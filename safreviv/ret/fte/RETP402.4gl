################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP402                                                      #
#Ojetivo       => Realizar la integracion del archivo de consulta de           #
#                 solicitudes, generando un archivo con NSS, Codigo y          #
#                 descripcion de Marca, fecha de proceso y diagnostico.        #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINF-845                                                  #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

DEFINE g_pid            DECIMAL(9,0)
DEFINE g_proceso_cod    SMALLINT
DEFINE g_opera_cod      SMALLINT
DEFINE g_folio          DECIMAL(9,0)
DEFINE g_archivo        CHAR(50)
DEFINE g_usuario_cod    CHAR(20)

MAIN

    DEFINE v_estado     SMALLINT
    DEFINE r_bnd_edo_act_archivo           SMALLINT

    CALL ARG_VAL(1) RETURNING g_usuario_cod
    CALL ARG_VAL(2) RETURNING g_pid
    CALL ARG_VAL(3) RETURNING g_proceso_cod
    CALL ARG_VAL(4) RETURNING g_opera_cod
    CALL ARG_VAL(5) RETURNING g_folio
    CALL ARG_VAL(6) RETURNING g_archivo

    -- Texto a la bitacora
    CALL fn_display_proceso(0,"INTEGRACION")
    
    -- Integracion de los datos del archivo
    CALL fn_integra_archivo() RETURNING v_estado

    -- Actualiza el estado del archivo procesado
    CALL fn_act_edo_archivo(g_archivo,g_folio,2,g_usuario_cod) 
         RETURNING r_bnd_edo_act_archivo

    -- Se verifica si hubo errores
    IF v_estado = 0 THEN
        DISPLAY "Integración realizada con éxito."
    ELSE
        DISPLAY "El proceso de integracion ha finalizado pero con errores."
        --CALL fn_error_opera(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
    END IF

    CALL fn_actualiza_opera_fin(g_pid,
                                g_proceso_cod,
                                g_opera_cod)
                                RETURNING v_estado

    CALL fn_display_proceso(1,"INTEGRACION")
    
END MAIN

#Funcion que integra el archivo con los nss de consulta
FUNCTION fn_integra_archivo()

    -- Valor devuelto
    DEFINE r_estado         SMALLINT

    -- Valores a insertar en el archivo de salida
    DEFINE v_detalle_solicitud  DYNAMIC ARRAY OF RECORD
                nss             CHAR(11)               ,
                marca           CHAR(50)               ,
                tipo            CHAR(10)               ,
                fecha           DATE                   ,
                diagnostico     CHAR(80)               ,
                hora_consulta   DATETIME HOUR TO SECOND,
                viv92_aivs      DECIMAL(18,2)          ,
                viv97_aivs      DECIMAL(18,2)          ,
                viv92_pesos     DECIMAL(12,2)          ,
                viv97_pesos     DECIMAL(12,2)          ,
                tpo_cambio      DECIMAL(8,5)
    END RECORD

    -- Variables auxiliares
    DEFINE v_sql            STRING
    DEFINE v_numero_nss     INTEGER
    DEFINE v_string         base.StringBuffer
    DEFINE v_tipo           STRING
    DEFINE v_diagnostico    STRING

    LET v_string = base.StringBuffer.create()

    LET v_numero_nss = 1


    -- Se establece la busqueda de acuerdo al proceso que mando llamar la integracion
    LET v_sql = "SELECT ad.nss,sm.marca||\" - \"||sm.descripcion_marca AS marca_desc,       "||   
                "       rcp.indicador_marca,smh.f_inicio,rcp.estado_cuenta,rcp.h_intento,   "||
                "       rlg.aivs_viv92,rlg.aivs_viv97,rlg.importe_viv92,rlg.importe_viv97,0 "||
                "FROM   ret_ley73_generico     rlg,                                         "||
                "       afi_derechohabiente    ad ,                                         "||
                "       sfr_marca_historica    smh,                                         "||
                "       sfr_marca              sm ,                                         "||
                "       ret_ctr_marca_procesar rcp,                                         "

    IF g_proceso_cod = g_archivo_solicitud_marca_procesar THEN
        LET v_sql = v_sql||"       safre_tmp:tmp_ret_ley73_cons_mar_det trl                      "
    ELSE
        IF g_proceso_cod = g_archivo_solicitud_desmarca_procesar THEN
            LET v_sql = v_sql||"       safre_tmp:tmp_ret_ley73_cons_des_det trl                      "
        END IF
    END IF
    
    LET v_sql = v_sql||"WHERE  rlg.gpo_ley73          = 1                                    "||
                       "  AND  rlg.id_derechohabiente = ad.id_derechohabiente                "||
                       "  AND  ad.nss                 = trl.nss                              "||
                       "  AND  smh.marca              = sm.marca                             "||
                       "  AND  rlg.id_solicitud       = smh.n_referencia                     "||
                       "  AND  rlg.id_solicitud       = rcp.id_solicitud                     "

    IF g_proceso_cod = g_archivo_solicitud_marca_procesar THEN
        --LET v_sql = v_sql||"  AND  smh.f_fin IS NULL "
        LET v_sql = v_sql||"  AND  indicador_marca = 1"
    ELSE
        IF g_proceso_cod = g_archivo_solicitud_desmarca_procesar THEN
            --LET v_sql = v_sql||"  AND  smh.f_fin IS NOT NULL "
            LET v_sql = v_sql||"  AND  indicador_marca = 2   "
        END IF
    END IF

    LET v_sql = v_sql||" ORDER BY nss,rcp.indicador_marca,rcp.estado_cuenta"

    PREPARE prp_consulta_nss FROM v_sql
    DECLARE cur_consulta_nss CURSOR FOR prp_consulta_nss
    
    FOREACH cur_consulta_nss INTO v_detalle_solicitud[v_numero_nss].*

        -- Se obtiene el tipon de cambio
        IF (v_detalle_solicitud[v_numero_nss].viv92_aivs + v_detalle_solicitud[v_numero_nss].viv97_aivs) > 0 THEN
            LET v_detalle_solicitud[v_numero_nss].tpo_cambio =
                (v_detalle_solicitud[v_numero_nss].viv92_pesos + v_detalle_solicitud[v_numero_nss].viv97_pesos)/ 
                (v_detalle_solicitud[v_numero_nss].viv92_aivs  + v_detalle_solicitud[v_numero_nss].viv97_aivs )
        END IF

        -- Se elimina los espacios en blanco
        CALL v_string.clear()
        CALL v_string.append(v_detalle_solicitud[v_numero_nss].tipo)
        CALL v_string.replace(" ","",0)
        LET  v_tipo = v_string.toString()
        CALL v_string.clear()
        CALL v_string.append(v_detalle_solicitud[v_numero_nss].diagnostico)
        CALL v_string.replace(" ","",0)
        LET  v_diagnostico = v_string.toString()

        LET v_detalle_solicitud[v_numero_nss].diagnostico = fn_diagnostico_desc(v_diagnostico)

        -- Se pone el indicador de marca
        IF v_tipo = "1" THEN
            LET v_detalle_solicitud[v_numero_nss].tipo = "Marca"
        ELSE
            IF v_tipo = "2" THEN
                LET v_detalle_solicitud[v_numero_nss].tipo = "Desmarca"
            END IF
        END IF

        LET v_numero_nss = v_numero_nss + 1

    END FOREACH

    DISPLAY "NSS encontrados    :"||(v_numero_nss-1)

    IF v_numero_nss > 1 THEN
        CALL v_detalle_solicitud.deleteElement(v_numero_nss)
        CALL fn_genera_archivo(v_detalle_solicitud)        
        LET r_estado = 0
    ELSE
        LET r_estado = 1
    END IF

    RETURN r_estado

END FUNCTION

#Funcion que genera un archivo plano con el resultado de la consulta,
#separados por pipes.
FUNCTION fn_genera_archivo(p_detalle_solicitud)

    -- Valores a insertar en el archivo de salida
    DEFINE p_detalle_solicitud  DYNAMIC ARRAY OF RECORD
                nss             CHAR(11)               ,
                marca           CHAR(50)               ,
                tipo            CHAR(10)               ,
                fecha           DATE                   ,
                diagnostico     CHAR(80)               ,
                hora_consulta   DATETIME HOUR TO SECOND,
                viv92_aivs      DECIMAL(18,2)          ,
                viv97_aivs      DECIMAL(18,2)          ,
                viv92_pesos     DECIMAL(12,2)          ,
                viv97_pesos     DECIMAL(12,2)          ,
                tpo_cambio      DECIMAL(8,5)
    END RECORD

    -- Variables auxiliares
    DEFINE v_channel_sal    base.Channel -- Creacion del archivo de salida
    DEFINE v_texto          STRING
    DEFINE v_string         base.StringBuffer
    DEFINE v_ruta_env_arch  LIKE seg_modulo.ruta_envio -- Ruta de envio
    DEFINE v_ruta_arch      STRING -- Ruta de archivo
    DEFINE v_contador       INTEGER
    DEFINE v_tipo_cambio    DECIMAL(8,5)
    DEFINE v_hora_consulta  DATE

    --Se obtiene la ruta de envio y ejecutable
    SELECT ruta_envio, ruta_bin
    INTO   v_ruta_env_arch
    FROM   seg_modulo
    WHERE  modulo_cod = "ret"

    LET v_string = base.StringBuffer.create()

    DISPLAY ""
    DISPLAY ""
    DISPLAY "------------------------------------------------------------------"
    DISPLAY "               Archivos Generados"
    DISPLAY ""
    -- Se prepara el archivo de salida
    LET v_channel_sal = base.Channel.create()
    -- Eliminamos la extension
    CALL v_string.append(g_archivo)
    LET v_texto = v_string.subString(1,v_string.getIndexOf(".con",1)-1)
    LET v_ruta_arch = v_ruta_env_arch CLIPPED,"/",v_texto,"_consulta",".rmdnp"
    DISPLAY "Archivo de salida:     ",v_texto,"_consulta",".rmdnp"
    CALL v_channel_sal.openFile(v_ruta_arch,"w")
    DISPLAY ""
    DISPLAY ""

    -- Se escribe el encabezado
    LET v_texto = "NSS|"                          ||
                  "Código y descripción de marca|"||
                  "Marcaje/Desmarcaje|"           ||
                  "Fecha de proceso de marca|"    ||
                  "Diagnostico de marca|"         ||
                  "Hora de consulta|"             ||
                  "AIVS 92|"                      ||
                  "AIVS 97|"                      ||
                  "PESOS 92|"                     ||
                  "PESOS 97|"                     ||
                  "Tipo de cambio informado "
    CALL v_channel_sal.writeLine(v_texto)

    -- Se escribe el detalle
    FOR v_contador = 1 TO p_detalle_solicitud.getLength()

        LET v_texto =  p_detalle_solicitud[v_contador].nss                                   ,"|",
                       p_detalle_solicitud[v_contador].marca                                 ,"|",
                       p_detalle_solicitud[v_contador].tipo                                  ,"|",
                      (p_detalle_solicitud[v_contador].fecha         USING "dd/mm/yyyy"     ),"|",
                       p_detalle_solicitud[v_contador].diagnostico                           ,"|",
                       p_detalle_solicitud[v_contador].hora_consulta                         ,"|",
                      (p_detalle_solicitud[v_contador].viv92_aivs    USING "&&&&&&&&&&.&&&&"),"|",
                      (p_detalle_solicitud[v_contador].viv97_aivs    USING "&&&&&&&&&&.&&&&"),"|",
                      (p_detalle_solicitud[v_contador].viv92_pesos   USING "&&&&&&&&&&.&&"  ),"|",
                      (p_detalle_solicitud[v_contador].viv97_pesos   USING "&&&&&&&&&&.&&"  ),"|",
                      (p_detalle_solicitud[v_contador].tpo_cambio    USING "&&&.&&&&&"      )
        CALL v_channel_sal.writeLine(v_texto)
    END FOR

    CALL v_channel_sal.close()

END FUNCTION

#Funcion que regresa la descripcion del diagnostico
FUNCTION fn_diagnostico_desc(p_diagnostico)

    DEFINE p_diagnostico CHAR(10)

    CASE p_diagnostico
        WHEN "101"
            RETURN "101-101 Aceptada"
        WHEN "102"
            RETURN "101-102 NSS no se encuentra registrado en la BDNSAR"
        WHEN "201"
            RETURN "101-201 La cuenta se encuentra en proceso de 43 BIS"
        WHEN "202"
            RETURN "101-202 En proceso de Traspaso A-A"
        WHEN "203"
            RETURN "101-203 En proceso de Unificacion de cuentas"
        WHEN "204"
            RETURN "101-204 En proceso de Fusion de Afore"
        WHEN "205"
            RETURN "101-205 En proceso de separacion de cuentas"
        WHEN "207"
            RETURN "101-207 En proceso de transferencias de recursos"
        WHEN "208"
            RETURN "101-208 En proceso de disposicion de recursos"
        WHEN "211"
            RETURN "101-211 En proceso de Devolucion de pagos efectuados sin Justificacion Legal"
        WHEN "212"
            RETURN "101-212 En proceso de retiros parciales"
        WHEN "213"
            RETURN "101-213 En proceso de tramite judicial iniciado por Afore"
        WHEN "216"
            RETURN "101-216 Cuenta en proceso de aclaracion por conciliacion"
        WHEN "217"
            RETURN "101-217 Cuenta en proceso de seleccion SIEFORE"
        WHEN "219"
            RETURN "101-219 Cuenta en proceso de modificación"
        WHEN "220"
            RETURN "101-220 Cuenta en proceso de crédito de vivienda"
        WHEN "221"
            RETURN "101-221 Cuenta en proceso de crédito de vivienda"
        WHEN "223"
            RETURN "101-223 Cuenta en proceso de saldo previo"
        WHEN "224"
            RETURN "101-224 No se encuentra marcado"
        WHEN "225"
            RETURN "101-225 Existe alguna notificación de pago por Ventanilla INFONAVIT"
        WHEN "226"
            RETURN "101-226 Existe alguna notificación de pago por Ventanilla INFONAVIT"
        OTHERWISE
            RETURN ""
    END CASE

END FUNCTION