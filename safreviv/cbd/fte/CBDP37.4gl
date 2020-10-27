##############################################################
#MODULO             CBD                                     ##
#PROGRAMA           CBDP37                                  ##
#OBJETIVO           LANZADO PRELIQUIDACION DE PROCESO 2116  ##
#FECHA              septiembre 20015                        ##
##############################################################

DATABASE safre_viv
DEFINE g_pid decimal(9,0)
DEFINE g_proceso_cod SMALLINT 
DEFINE g_opera_cod SMALLINT 
DEFINE g_folio DECIMAL(9,0)
DEFINE g_archivo char(40)
DEFINE g_usuario_cod char(20)
--Arreglo que contendrá los datos de los registros rechazados (estado=0)
--para posteriormente crear un archivo con estos
DEFINE g_arr_registros_rechazados DYNAMIC ARRAY of RECORD 
        estado            integer,
        nss               char(11),
        subcuenta         smallint,
        fondo_inversion   smallint,
        tipo_movimiento   smallint,
        monto_acciones    decimal(16,6),
        monto_pesos       decimal(12,2),
        f_fecha           date  
    END record



MAIN 
    DEFINE v_estado         SMALLINT
    DEFINE v_nombre_archivo STRING 
    DEFINE v_ruta           varchar(100)
    
    LET g_usuario_cod=arg_val(1)
    LET g_pid=arg_val(2)
    LET g_proceso_cod = arg_val(3)
    LET g_opera_cod = arg_val(4)
    LET g_folio = arg_val(5)
    LET g_archivo=arg_val(6)
    

    WHENEVER ERROR CONTINUE
        UPDATE cbd_ctr_ajuste_saldo
        SET estado=5 --La operación falló, se modifica el estado del proceso en error 
        WHERE folio=g_folio
    WHENEVER ERROR STOP

    CALL fn_display_proceso(0,"Inicia Preliquidación")
    CALL fn_integra_archivo() RETURNING v_estado

    IF v_estado = 0 THEN 
    
        CALL fn_actualiza_opera_fin(g_pid,g_proceso_cod,g_opera_cod) RETURNING v_estado
        DISPLAY "Preliquidación realizada con exito"
        DISPLAY "Realizando updates  correspondientes"
        
        PREPARE prp_upd FROM "UPDATE glo_folio 
                                SET status=1
                                WHERE folio=?"
        EXECUTE prp_upd USING g_folio


        PREPARE prp_actualiza FROM "UPDATE cbd_ctr_ajuste_saldo
                                    SET estado=2
                                    WHERE folio=?"
                                  
        EXECUTE prp_actualiza  USING g_folio
        DISPLAY "Creando archivo de registros rechazados.."
        CALL fn_crea_archivo_rechazo() RETURNING v_nombre_archivo, v_ruta
      
        
        DISPLAY "Archivo RECHAZADOS_" ||v_nombre_archivo ||".rj_saldo creado en la ruta: "|| v_ruta
        
    ELSE 
        DISPLAY "El proceso de la preliquidación ha finalizado pero con excepciones verificar en el sistema saci.\nVer"
        CALL fn_error_opera(g_pid, g_proceso_cod,g_opera_cod) RETURNING v_estado
        
    END IF
    CALL fn_display_proceso(1,"Inicia Preliquidación")


END MAIN 



FUNCTION fn_integra_archivo()
     DEFINE v_sql_procedure             STRING 
     DEFINE v_status                    SMALLINT 
     DEFINE r_cod_error                 SMALLINT 
     DEFINE r_error_isam                INTEGER 
     DEFINE r_mensaje_error             varchar(255)
     DEFINE v_cont                      INTEGER

    LET v_sql_procedure= "EXECUTE FUNCTION fn_cbd_preliquida_archivo_2116(?,?)"
    LET v_cont=1

    PREPARE prp_procedure FROM v_sql_procedure
    DECLARE cur_rechazados CURSOR for prp_procedure

    FOREACH cur_rechazados USING g_folio, g_usuario_cod
            INTO r_cod_error, 
                 r_error_isam,
                 r_mensaje_error,
                 g_arr_registros_rechazados[v_cont].nss,
                 g_arr_registros_rechazados[v_cont].subcuenta,
                 g_arr_registros_rechazados[v_cont].fondo_inversion,
                 g_arr_registros_rechazados[v_cont].tipo_movimiento,
                 g_arr_registros_rechazados[v_cont].monto_acciones,
                 g_arr_registros_rechazados[v_cont].monto_pesos,
                 g_arr_registros_rechazados[v_cont].f_fecha
                 
                    LET v_cont=v_cont+1
    END FOREACH 
    
    IF r_cod_error = 0 THEN 
        DISPLAY ""
        DISPLAY r_mensaje_error
    ELSE 
        DISPLAY "#######Exception safre######"
        DISPLAY "Codigo error ", r_cod_error
        DISPLAY "error isam ", r_error_isam
        DISPLAY "Detalle ", r_mensaje_error
        LET v_status=1
    END if
    RETURN v_status
END FUNCTION 

FUNCTION fn_crea_archivo_rechazo()
    DEFINE v_texto                      STRING
    DEFINE channel_1                    base.Channel
    DEFINE v_ruta_ch1                   STRING
    DEFINE v_ruta_envio                 varchar(100) 
    DEFINE v_cont                       SMALLINT 
    DEFINE v_nom_archivo                string

    SELECT ruta_envio #Obtenemos la ruta de envio 
    INTO v_ruta_envio
    FROM seg_modulo 
    WHERE modulo_cod = 'cbd'
    
    LET v_nom_archivo=g_archivo--Se cambia el tipo el tipo de dato para su mejor manipulación
    LET v_nom_archivo=v_nom_archivo.subString(1,v_nom_archivo.getIndexOf(".",1)-1)
         LET v_ruta_ch1=v_ruta_envio
         
         --Obtenemos el nombre de el archivo sin extensión 
         LET v_ruta_ch1=v_ruta_ch1 CLIPPED,"/RECHAZADOS_",v_nom_archivo clipped,".rj_saldo"
         LET channel_1=base.Channel.create()
         CALL channel_1.openfile(v_ruta_ch1, "w")
         
         FOR v_cont=1 TO g_arr_registros_rechazados.getLength()-1
         LET v_texto="|",g_arr_registros_rechazados[v_cont].nss,"|"
                     ,g_arr_registros_rechazados[v_cont].subcuenta clipped,"|"
                     ,g_arr_registros_rechazados[v_cont].fondo_inversion clipped,"|"
                     ,g_arr_registros_rechazados[v_cont].tipo_movimiento clipped,"|"
                     ,g_arr_registros_rechazados[v_cont].monto_acciones,"|"
                     ,g_arr_registros_rechazados[v_cont].monto_pesos,"|"
                     ||year(g_arr_registros_rechazados[v_cont].f_fecha) clipped
                     ||month(g_arr_registros_rechazados[v_cont].f_fecha) clipped
                     ||day(g_arr_registros_rechazados[v_cont].f_fecha) clipped||"|"

                CALL channel_1.writeLine(v_texto)
         END FOR 
        
         CALL channel_1.close()
    RETURN v_nom_archivo, v_ruta_ch1
END function 

