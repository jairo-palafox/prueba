################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP379                                                      #
#Ojetivo       => Programa lanzador de integrador para la consulta de historico#
#                 anexo 1.                                                     #
#Fecha inicio  => 23 de Julio, 2015.                                           #
#Requerimiento => 841                                                          #
################################################################################
DATABASE safre_viv

DEFINE g_pid            DECIMAL(9,0)
DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_folio          DECIMAL(9,0)
DEFINE g_archivo        CHAR(50)
DEFINE g_usuario_cod    CHAR(20)

MAIN

    DEFINE v_estado                        SMALLINT
    DEFINE r_bnd_edo_act_archivo           SMALLINT

    LET g_usuario_cod = ARG_VAL(1)
    LET g_pid = ARG_VAL(2)
    LET g_proceso_cod = ARG_VAL(3)
    LET g_opera_cod = ARG_VAL(4)
    LET g_folio = ARG_VAL(5)
    LET g_archivo = ARG_VAL(6)

    --Texto a la bitacora
    CALL fn_display_proceso(0,"INTEGRACION")
    --Integracion de los datos del archivo
    CALL fn_integra_archivo() RETURNING v_estado

    -- Actualiza el estado del archivo procesado
    CALL fn_act_edo_archivo(g_archivo,g_folio,2,g_usuario_cod) 
         RETURNING r_bnd_edo_act_archivo

    IF v_estado = 0 THEN
        DISPLAY "Estado archivo: ",g_archivo,"(2): ",r_bnd_edo_act_archivo
        CALL fn_actualiza_opera_fin(g_pid,
                                    g_proceso_cod,
                                    g_opera_cod)
                                    RETURNING v_estado
        DISPLAY "Integración realizada con éxito."
    ELSE
        DISPLAY "El proceso de integracion ha finalizado pero con excepciones.\n."
        CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
    END IF
    CALL fn_display_proceso(1,"INTEGRACION")
    
END MAIN

#Funcion que realiza el conteo de los nss encontrados mediante la funcion fn_saldo_dia.
#Ademas guarda en una archivo los importes de vivienda 92, 97 y tesofe, si enuentra el nss.
#Se guarda de igual forma el signo - si esta sobregirado
FUNCTION fn_integra_archivo()

    --Variables empleadas para guardar los valores regresados por fn_saldo_dia 
    DEFINE v_resultado      SMALLINT
    DEFINE v_saldo_aivs     DECIMAL(16,6)
    DEFINE v_saldo_pesos    DECIMAL(16,6)
    --Empleado para las instrucciones SQL
    DEFINE v_query          STRING
    --NSS recuperadodel archivo
    DEFINE v_nss            CHAR(11)
    --AIV's vivienda 92
    DEFINE v_aiv_92         DECIMAL(16,6)
    --AIV's vivienda 97
    DEFINE v_aiv_97         DECIMAL(16,6)
    --Signo vivienda 92
    DEFINE v_viv92_signo    STRING
    --Signo vivienda 97
    DEFINE v_viv97_signo    STRING
    --Signo tesofe
    DEFINE v_tesofe_signo   STRING
    --Creacion del archivo de salida
    DEFINE v_channel_sal    base.Channel
    --Creacion del archivo de salida
    DEFINE v_v_channel_ne   base.Channel
    --Ruta de envio
    DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio
    --Ruta de archivo
    DEFINE v_ruta_arch      STRING    
    --Texto a escribir en el archivo
    DEFINE v_texto          STRING
    --Total de registros ingresados
    DEFINE v_tot_reg_det    BIGINT
    --Total de registros de acuerdo al sumario
    DEFINE v_tot_reg_sum    BIGINT
    --Total de registros reconocidos
    DEFINE v_tot_reg_rec    BIGINT
    --Total de registros no reconocidos
    DEFINE v_tot_reg_no_rec BIGINT
    --Resultado de la integracion
    DEFINE r_estado         SMALLINT

    CONSTANT viv_92 = 8
    CONSTANT viv_97 = 4
    CONSTANT tesofe = 47

    --Inicializar variables
    LET v_channel_sal = base.Channel.create()
    LET v_v_channel_ne = base.Channel.create()
    LET v_tot_reg_rec = 0
    LET v_tot_reg_no_rec = 0
    LET r_estado = 0

    SELECT total_registros
        INTO v_tot_reg_sum
        FROM safre_tmp:tmp_ret_anexo1_cons_sum

    SELECT COUNT(*)
        INTO  v_tot_reg_det
        FROM safre_tmp:tmp_ret_anexo1_cons_det

    --Si el numero de registros en la base no coincide con el sumario
    IF  v_tot_reg_sum != v_tot_reg_det THEN
        DISPLAY ""
        DISPLAY "               INCONSISTENCIA"
        DISPLAY "El numero de registros cargados no coincide con el sumario"
        DISPLAY "Registros del sumario:     ",v_tot_reg_sum
        DISPLAY "Registros encontrados:     ",v_tot_reg_det
        DISPLAY ""
        LET r_estado = 1

    ELSE

        --Se obtiene la ruta de envio y ejecutable
        SELECT ruta_envio, ruta_bin
            INTO   v_ruta_env_arch
            FROM   seg_modulo
            WHERE  modulo_cod = "ret"

        DISPLAY ""
        DISPLAY ""
        DISPLAY "------------------------------------------------------------------"
        DISPLAY "               Archivos Generados"
        DISPLAY ""
        --Se prepara el archivo de salida
        LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/","salida_",g_archivo
        DISPLAY "Archivo de salida:     salida_",g_archivo
        CALL v_channel_sal.openFile(v_ruta_arch,"w")
        --Archivo con nss no encontrados
        LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/","ne_",g_archivo
        DISPLAY "NSS no encontrados:    ne_",g_archivo
        CALL v_v_channel_ne.openFile(v_ruta_arch,"w")
        DISPLAY "------------------------------------------------------------------"
        DISPLAY ""
        DISPLAY ""

        --Se prepara la ejecucion a la funcion fn_saldo_dia
        LET v_query = "EXECUTE FUNCTION fn_saldo_dia(?,NULL,?,TODAY)"
        PREPARE prp_saldo_dia FROM v_query

        DECLARE cur_anexo1 CURSOR FOR SELECT nss
                                      FROM safre_tmp:tmp_ret_anexo1_cons_det

        LET v_texto = "tipo_registro|NSS|importe aivs 92|signo aiv's 92|importe aivs 97|signo aivs 97|importe tesofe(47)|signo tesofe(47)"
        CALL v_channel_sal.writeLine(v_texto)
        LET v_texto = "tipo_registro|NSS"
        CALL v_v_channel_ne.writeLine(v_texto)

        FOREACH cur_anexo1 INTO v_nss
            --Vivienda 92
            EXECUTE prp_saldo_dia USING v_nss,viv_92 INTO v_resultado,v_saldo_aivs,v_saldo_pesos
            --Si encuentra el NSS en la primera ejecucion, debe encontrarlo en las siguientes
            IF( v_resultado != 1) THEN
                LET v_aiv_92 = v_saldo_aivs
                --Asignamos el signo
                LET v_viv92_signo = ""
                IF v_aiv_92 < 0 THEN
                    LET v_viv92_signo = "-"
                END IF
                
                --Vivienda 97
                EXECUTE prp_saldo_dia USING v_nss,viv_97 INTO v_resultado,v_saldo_aivs,v_saldo_pesos
                LET v_aiv_97 = v_saldo_aivs
                --Asignamos el signo
                LET v_viv97_signo = ""
                IF v_aiv_97 < 0 THEN
                    LET v_viv97_signo = "-"
                END IF
                
                --Tesofe
                EXECUTE prp_saldo_dia USING v_nss,tesofe INTO v_resultado,v_saldo_aivs,v_saldo_pesos
                --Asignamos el signo
                LET v_tesofe_signo = ""
                IF v_saldo_aivs < 0 THEN
                    LET v_tesofe_signo = "-"
                END IF
                
                --Texto que ira en el archivo de salida
                LET v_texto = "2|",v_nss,"|",v_aiv_92 USING "&&&&&&&&&&.&&&&","|",v_viv92_signo,"|",
                                   v_aiv_97 USING "&&&&&&&&&&.&&&&","|",v_viv97_signo,"|",
                                   v_saldo_aivs USING "&&&&&&&&&&.&&&&","|",v_tesofe_signo
                CALL v_channel_sal.writeLine(v_texto)
                LET v_tot_reg_rec = v_tot_reg_rec + 1
            ELSE
                LET v_texto = "2",v_nss
                --Archivo con NSS no encontrados
                 CALL v_v_channel_ne.writeLine(v_texto)
                 LET v_tot_reg_no_rec = v_tot_reg_no_rec + 1
            END IF
        END FOREACH

        LET v_texto = "9"||v_tot_reg_rec
        LET v_texto = v_texto.trim()
        CALL v_channel_sal.writeLine(v_texto)

        LET v_texto = "9"||v_tot_reg_no_rec
        LET v_texto = v_texto.trim()
        CALL v_v_channel_ne.writeLine(v_texto)
        
        CALL v_channel_sal.close()
        CALL v_v_channel_NE.close()
    END IF

    RETURN r_estado
    
END FUNCTION