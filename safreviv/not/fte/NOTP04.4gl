#####################################################################################################
#Modulo       => Notificaciones   'not'                                                             #
#Programa     => NOTP04                                                                             #
#Objetivo     => Programa que integra el archivo de pagos omisos                                    #                              
#Fecha_inicio =>                                                                                    # 
#####################################################################################################

DATABASE safre_viv

GLOBALS "NOTP04.inc"

PRIVATE DEFINE p_usuario         CHAR(20)  -- parametro de entrada 
PRIVATE DEFINE p_pid             LIKE bat_ctr_proceso.pid  -- ID del proceso
PRIVATE DEFINE p_proceso_cod     SMALLINT 
PRIVATE DEFINE p_opera_cod   SMALLINT
PRIVATE DEFINE v_folio           DECIMAL(9,0)
PRIVATE DEFINE p_archivo         VARCHAR(200)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

MAIN
   DEFINE v_resultado          SMALLINT
   DEFINE v_query              STRING
   
   -- se recuperan los parametros del lanzador
   LET p_usuario = ARG_VAL(1)
   LET p_pid = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod = ARG_VAL(4)
   LET v_folio = ARG_VAL(5)
   LET p_archivo = ARG_VAL(6)


   CALL fn_recupera_inf_proceso(p_proceso_cod, p_opera_cod) 
                               RETURNING v_proceso_desc,v_extension, 
                                         v_opera_desc,v_layout, 
                                         v_ruta_rescate,v_ruta_listados,
                                         v_usuario_proceso 

   #Encabezado para el archivo de monitoreo
   DISPLAY "*******************************************************************"
   DISPLAY " PROCESO            : ",v_proceso_desc
   DISPLAY " OPERACIÓN          : ",v_opera_desc
   DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
   DISPLAY " HORA               : ",TIME(CURRENT)
   DISPLAY "*******************************************************************"

    -- funcion que genera el folio
   CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario) RETURNING v_folio   -- se llama a la funcion que genera el folio

   UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid
   UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = P_pid

   UPDATE glo_ctr_archivo SET folio = v_folio 
   WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_archivo

   LET v_query = "EXECUTE PROCEDURE sp_not_integra_omisos_trm(?)"
   PREPARE exe_funcion FROM v_query
   EXECUTE exe_funcion USING v_folio INTO v_resultado

   IF SQLCA.SQLCODE <> 0 THEN
      DISPLAY ""
      DISPLAY "Ocurrió un ERROR al intentar integrar el archivo de omisos trm: "
      DISPLAY SQLERRMESSAGE

      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
      RETURNING v_resultado

      UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
      WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_archivo
   ELSE
      IF v_resultado <> 0 THEN
         #Si el resultado es distinto a cero el proceso envio un error de validacion
         DISPLAY ""
         CASE v_resultado
            WHEN ERROR_FECHAS
               DISPLAY "Archivo RECHAZADO: Las fechas de operación en encabezado y sumario no coinciden"
            WHEN ERROR_ARCHIVO_SUMARIO
               DISPLAY "Archivo RECHAZADO: El total de detalles en el archivo no corresponde con el reportado en el sumario"
            WHEN ERROR_INTEGRA_DETALLES
               DISPLAY "ERROR en la integración: No se integraron todos los registros del archivo, Favor de validar con el área de sistemas"
            OTHERWISE
               DISPLAY "Ocurrió un ERROR al intentar integrar el archivo de omisos trm: "
               DISPLAY "Codigo de error: ", v_resultado
         END CASE 
         
         CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
         RETURNING v_resultado

         UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
         WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_archivo
      ELSE
         #Si el resultado es cero significa que el proceso termino correctamente
         CALL fn_genera_reporte()
         
         # Finaliza la operacion
         CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
         RETURNING v_resultado
         IF(v_resultado <> 0)THEN         
            # Actualiza a estado erróneo
            DISPLAY ""
            DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."
            
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING v_resultado

            UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_ERROR 
            WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_archivo
            
         ELSE
            UPDATE glo_ctr_archivo SET estado = ESTADO_ARCHIVO_INTEGRADO
            WHERE proceso_cod = p_proceso_cod AND estado = 1 AND nombre_archivo = p_archivo

            CALL fn_notifica_proceso(v_folio, p_proceso_cod, p_usuario)
            
            DISPLAY ""
            DISPLAY "*******************************************************************"
            DISPLAY ""
            DISPLAY "Termino la ejecución del proceso: "
            DISPLAY ""
            DISPLAY " PROCESO            : ",v_proceso_desc
            DISPLAY " OPERACIÓN          : ",v_opera_desc
            DISPLAY " FECHA              : ",TODAY USING 'dd-mm-yyyy'
            DISPLAY " HORA               : ",TIME(CURRENT)
            DISPLAY ""
            DISPLAY "*******************************************************************"
         END IF	
      END IF
   END IF
END MAIN

FUNCTION fn_genera_reporte()
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

	DEFINE v_ruta_exe          LIKE seg_modulo.ruta_bin         -- Ruta del ejecutable
   DEFINE v_ruta_listado      LIKE seg_modulo.ruta_listados    -- Ruta de listados
	DEFINE v_nombre	         STRING

   SELECT ruta_bin, ruta_listados
     INTO v_ruta_exe,
          v_ruta_listado
     FROM seg_modulo 
    WHERE modulo_cod = 'not'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET v_nombre = v_ruta_exe CLIPPED, "/NOTP04.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   START REPORT rp_cifras_pagos_omisos TO XML HANDLER vhandler
      OUTPUT TO REPORT rp_cifras_pagos_omisos(v_folio)
   FINISH REPORT rp_cifras_pagos_omisos

   DISPLAY "*******************************************************************"
   DISPLAY ""
   DISPLAY "Termino la generación del archivo de resumen de registro de pagos  ", v_ruta_listado
   DISPLAY ""
   DISPLAY "*******************************************************************"
   
END FUNCTION 

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados    -- Ruta de listados
   DEFINE v_listado          STRING
   
   DEFINE v_reporte          STRING
   DEFINE v_formato          STRING
   DEFINE v_preview          INTEGER
    
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'not'

   LET v_listado = v_ruta_listados CLIPPED , "/" ,
                        p_usuario CLIPPED , "-", -- usuario
                        "NOTP04" CLIPPED, "-", -- programa
                        p_pid USING "&&&&&","-", -- PID
                        p_proceso_cod USING "&&&&&", "-", -- codigo del proceso
                        p_opera_cod   USING "&&&&&",".pdf" -- codigo de la operación

   IF ( fgl_report_loadCurrentSettings(v_reporte) ) THEN  -- if  the file loaded OK
      CALL fgl_report_setOutputFileName(v_listado)
      CALL fgl_report_selectDevice(v_formato)
      CALL fgl_report_selectPreview(v_preview)
   ELSE
       DISPLAY "Error: No se pudo encontrar el archivo ", v_reporte
       EXIT PROGRAM
   END IF

   RETURN fgl_report_commitCurrentSettings()
   
END FUNCTION

REPORT rp_cifras_pagos_omisos(p_folio)
   DEFINE p_folio                DECIMAL(9,0)
   DEFINE v_fecha                DATE
   DEFINE v_nombre_archivo       VARCHAR(40)
   DEFINE v_num_total            INTEGER
   DEFINE v_total_aportacion     DECIMAL(12,2)
   DEFINE v_total_amortizacion   DECIMAL(12,2)

   FORMAT

   FIRST PAGE HEADER
      LET v_fecha = TODAY

      SELECT nombre_archivo INTO v_nombre_archivo FROM glo_ctr_archivo where folio = v_folio

      SELECT count(*), SUM(aportacion), SUM(amortizacion)
      INTO v_num_total, v_total_aportacion, v_total_amortizacion
      FROM not_det_omisos_trm
      WHERE folio = v_folio

      PRINTX v_fecha USING 'dd-mm-yyyy'
      PRINTX p_folio
      PRINTX p_usuario
      PRINTX v_nombre_archivo
      PRINTX v_num_total
      PRINTX v_total_aportacion
      PRINTX v_total_amortizacion
      
END REPORT 