#Proyecto          => SAFRE VIVIENDA 2A ETAPA                                 #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => NOT                                                     #
#Programa          => NOTP01                                                  #
#Objetivo          => PROGRAMA PARA PREPARAR LAS NOTIFICACIONES A ENVIAR		#
#Fecha Inicio      => 28-ENERO-2015                                           #
###############################################################################
DATABASE safre_viv

#Parametros generales del proceso
PRIVATE DEFINE p_pid                      DECIMAL(9,0)                           -- PID del proceso
PRIVATE DEFINE p_proceso_cod              SMALLINT                               -- codigo del proceso
PRIVATE DEFINE p_opera_cod                SMALLINT                               -- codigo de la operacion
PRIVATE DEFINE p_usuario_cod              CHAR(20)                               -- clave del usuario firmado
PRIVATE DEFINE p_nombre_archivo           CHAR(40)                               -- nombre dle archivo
PRIVATE DEFINE v_folio                    DECIMAL(9,0)
PRIVATE DEFINE v_folio_origen             DECIMAL(9,0)

PRIVATE DEFINE v_proceso_desc             CHAR(40)
PRIVATE DEFINE v_extension                CHAR(10)
PRIVATE DEFINE v_opera_desc               CHAR(40)
PRIVATE DEFINE v_layout                   SMALLINT
PRIVATE DEFINE v_usuario_proceso          CHAR(20)
PRIVATE DEFINE v_ruta_rescate             STRING
PRIVATE DEFINE v_ruta_listados            CHAR(40)

PRIVATE DEFINE v_proceso_cod_origen       SMALLINT
PRIVATE DEFINE v_nom_funcion              VARCHAR(100)
PRIVATE DEFINE v_nom_tabla                VARCHAR(100)

PRIVATE DEFINE v_mensajes_sms             INTEGER
PRIVATE DEFINE v_mensajes_correo          INTEGER
PRIVATE DEFINE v_total_detalles           INTEGER

MAIN
   DEFINE v_resultado                     INTEGER
   #DEFINE v_mensaje                       VARCHAR(100)

   DEFINE v_fn_notifica_proceso           STRING


   -- se recuperan los parametros 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_pid            = ARG_VAL(2)
   LET p_proceso_cod    = ARG_VAL(3)
   LET p_opera_cod      = ARG_VAL(4)
   LET v_folio_origen   = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)

   WHENEVER ERROR CONTINUE
   
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

   #Se actualiza el estado del proceso a ejecutar
   UPDATE not_ctr_proceso set estado = 2, f_proceso = TODAY 
   WHERE folio = v_folio_origen AND proceso_cod_notifica = p_proceso_cod

	-- se solicita el numero de folio asociado a la operacion
	-- parametros: proceso, operacion, folio_referencia, usuario
	CALL fn_genera_folio_dis(p_proceso_cod,p_opera_cod,v_folio_origen,p_usuario_cod)
	RETURNING v_folio

	#Se actualiza el folio del proceso               
	UPDATE bat_ctr_proceso SET folio = v_folio WHERE pid = p_pid
	UPDATE bat_ctr_operacion SET folio = v_folio WHERE pid = p_pid

	WHENEVER ERROR CONTINUE

      DISPLAY ""
		DISPLAY "Inicia el proceso que preparara las notificaciones a enviar del folio ", v_folio_origen

      SELECT
         noti.proceso_cod_origen,
         noti.nom_funcion,
         noti.nom_tabla
      INTO
         v_proceso_cod_origen,
         v_nom_funcion,
         v_nom_tabla
      FROM cat_notificacion noti
      INNER JOIN glo_folio fol ON fol.proceso_cod = noti.proceso_cod_origen
      WHERE fol.folio = v_folio_origen
      AND noti.proceso_cod_notifica = p_proceso_cod
            
		LET v_fn_notifica_proceso = "EXECUTE FUNCTION ", v_nom_funcion CLIPPED, "(?,?)"
      #DISPLAY v_fn_notifica_proceso
		PREPARE exe_v_fn_notifica_proceso FROM v_fn_notifica_proceso
		EXECUTE exe_v_fn_notifica_proceso USING v_folio_origen, v_folio 
                                        INTO v_resultado, v_mensajes_sms, v_mensajes_correo, v_total_detalles
        
		IF SQLCA.SQLCODE <> 0 THEN
         DISPLAY ""
			DISPLAY "Ocurrió un ERROR al intentar preparar los mensajes de notificación: "
			DISPLAY SQLERRMESSAGE

         CALL fn_reporta_error()
         
			CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
			RETURNING v_resultado
			
			#CALL fn_actualiza_error()
		ELSE
			#Se valida que la funcion no regrese algun error de validacion
			IF v_resultado <> 0 THEN
				#Si el resultado es distinto a cero el proceso envio un error de validacion
            DISPLAY ""
				DISPLAY "Ocurrió un ERROR al intentar preparar los mensajes de notificación: "
				DISPLAY "Codigo de error: ", v_resultado
            
            CALL fn_reporta_error()
            
            CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
            RETURNING v_resultado
			ELSE
				#Si el resultado es cero significa que el proceso termino correctamente
            {IF v_mensajes_sms IS NULL THEN
               LET v_mensajes_sms = 0
            END IF 
            IF v_mensajes_correo IS NULL THEN
               LET v_mensajes_correo = 0
            END IF 
            IF v_total_detalles IS NULL THEN
               LET v_total_detalles = 0
            END IF} 
            CALL fn_genera_reporte(v_folio)

				# Finaliza la operacion
				CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
				RETURNING v_resultado
				IF(v_resultado <> 0)THEN         
					# Actualiza a estado erróneo
               DISPLAY ""
					DISPLAY "Ocurrio un ERROR al intentar actualizar el estado de la operacion..."

               CALL fn_reporta_error()
               
					CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) 
					RETURNING v_resultado
            ELSE
               #Actualizamos la ejecucion de la siguiente operacion para que se ejecute de forma automatica
               UPDATE bat_ctr_operacion SET ind_tipo_ejecucion = 1 WHERE pid = p_pid AND opera_cod > 1

               DISPLAY ""
               DISPLAY "Finalizo la ejecución de la función que prepara los mensajes a notificar"
               #DISPLAY "Mensaje de respuesta: ", v_mensaje

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
	WHENEVER ERROR STOP
END MAIN

PRIVATE FUNCTION fn_reporta_error()
   DEFINE v_usuario              CHAR(20)
   DEFINE v_folio_pendiente      DECIMAL(9,0)
   DEFINE v_ruta_ejecutable      VARCHAR(40) -- Ruta del ejecutable
   DEFINE v_ruta_listados        VARCHAR(40) -- Ruta del log
   DEFINE r_resultado_opera      INTEGER
   DEFINE v_nom_archivo          CHAR(40)
   DEFINE v_opera_cod_notifica   SMALLINT -- codigo de operacion
   DEFINE v_pid                  DECIMAL(9,0) -- ID del proceso
   DEFINE v_comando              STRING
   DEFINE v_consulta             STRING
   
   #Actualizamos el estado del proceso a error
   UPDATE not_ctr_proceso set estado = 9 WHERE folio = v_folio_origen AND proceso_cod_notifica = p_proceso_cod

   #Se valida si existe algun proceso formado para ejecucion
   LET v_consulta =  "SELECT FIRST 1 folio, usuario_cod ",
                     "FROM not_ctr_proceso ",
                     "WHERE proceso_cod_notifica = ? ",
                     "AND estado = 1 "
   PREPARE exe_consulta FROM v_consulta
   EXECUTE exe_consulta USING p_proceso_cod INTO v_folio_pendiente, v_usuario
   

   IF (v_folio_pendiente IS NOT NULL AND v_folio_pendiente <> 0) THEN
      #Se encontro un folio por notificar
      
      #Obtiene las rutas ejecutable
      SELECT ruta_bin
      INTO v_ruta_ejecutable
      FROM seg_modulo 
      WHERE modulo_cod = 'not'

      --Obtiene ruta listados
      SELECT ruta_listados
      INTO v_ruta_listados
      FROM seg_modulo 
      WHERE modulo_cod = 'bat'

      LET v_opera_cod_notifica = 1
      LET v_nom_archivo = 'notifica_proceso'

      # se valida si se puede generar el proceso
      CALL fn_valida_operacion(0,p_proceso_cod,v_opera_cod_notifica) RETURNING r_resultado_opera
      IF ( r_resultado_opera <> 0 ) THEN
         CALL fn_muestra_inc_operacion(r_resultado_opera)
      ELSE
         # se genera el pid para el proceso
         INITIALIZE v_pid TO NULL
         CALL fn_genera_pid(p_proceso_cod,v_opera_cod_notifica,v_usuario)
                RETURNING v_pid

         CALL fn_inicializa_proceso(v_pid,p_proceso_cod,v_opera_cod_notifica,0,
                                                "NOTP01",v_nom_archivo,v_usuario)
                                       RETURNING r_resultado_opera
         IF ( r_resultado_opera <> 0 ) THEN
            CALL fn_muestra_inc_operacion(r_resultado_opera)
         ELSE
            # Inicia operación
            CALL fn_actualiza_opera_ini(v_pid,p_proceso_cod,v_opera_cod_notifica,v_folio_pendiente,"NOTP01",
                                  v_nom_archivo,v_usuario) RETURNING r_resultado_opera
            # En el caso de que exista una inconsistencia al iniciar el proceso, se
            # Muestra un mensaje con la descripcion
            IF(r_resultado_opera)THEN
               CALL fn_muestra_inc_operacion(r_resultado_opera)
            ELSE
               LET v_comando = "nohup fglrun ",v_ruta_ejecutable CLIPPED,"/NOTP01.42r ",
                                                v_usuario," ",
                                                v_pid," ",
                                                p_proceso_cod," ",
                                                v_opera_cod_notifica," ",
                                                v_folio_pendiente," '",
                                                v_nom_archivo,
                                                "' 1>", v_ruta_listados CLIPPED ,
                                                "/nohup:",v_pid USING "&&&&&",":",
                                                         p_proceso_cod USING "&&&&&",":",
                                                         v_opera_cod_notifica USING "&&&&&" ," 2>&1 &"

               RUN v_comando
            END IF
         END IF
      END IF
   END IF
END FUNCTION

PRIVATE FUNCTION fn_genera_reporte(v_folio)
   DEFINE preview             SMALLINT
   DEFINE vhandler            om.SaxDocumentHandler

   DEFINE v_ruta_exe          LIKE seg_modulo.ruta_bin         -- Ruta del ejecutable
   DEFINE v_ruta_listado      LIKE seg_modulo.ruta_listados    -- Ruta de listados
   DEFINE v_nombre	         STRING
   DEFINE v_folio             DECIMAL(9,0)

   SELECT ruta_bin, ruta_listados
     INTO v_ruta_exe,
          v_ruta_listado
     FROM seg_modulo 
    WHERE modulo_cod = 'not'

   LET preview = FALSE
   INITIALIZE vhandler TO NULL

   LET v_nombre = v_ruta_exe CLIPPED, "/NOTP01.4rp"
   LET vhandler = fn_configuracion(v_nombre, "PDF", preview )

   START REPORT rep_notifica_enviadas TO XML HANDLER vhandler
      OUTPUT TO REPORT rep_notifica_enviadas(v_folio)
   FINISH REPORT rep_notifica_enviadas
END FUNCTION

PRIVATE FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
   DEFINE v_ruta_listados    LIKE seg_modulo.ruta_listados    -- Ruta de listados
   DEFINE v_listado          STRING 
   
   DEFINE v_reporte          STRING
   DEFINE v_formato          STRING
   DEFINE v_preview          INTEGER
    
   -- /ds/safreviv_lst/not
   SELECT ruta_listados
     INTO v_ruta_listados
     FROM seg_modulo
    WHERE modulo_cod = 'not'
   
   LET v_listado =   v_ruta_listados CLIPPED , "/" ,
                     p_usuario_cod CLIPPED , "-", -- usuario
                     "NOTP01" CLIPPED, "-", -- programa
                     p_pid         USING "&&&&&","-", -- PID
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

REPORT rep_notifica_enviadas(v_folio)
   DEFINE v_folio            DECIMAL(9,0)
   DEFINE v_fecha            DATE 

   DEFINE v_descripcion      VARCHAR(200)
   DEFINE v_f_actualiza      DATE
   DEFINE v_folio_refencia   DECIMAL(9,0)

   DEFINE v_total_mensajes   INTEGER

   DEFINE v_query            STRING
   
   FORMAT 

   FIRST PAGE HEADER
      LET v_fecha = TODAY
      LET v_f_actualiza = TODAY

--------------------------------------------------------------------------------
      # Folio de Referencia y Descripcion de proceso 
      SELECT glo2.folio, cat.proceso_desc
      INTO v_folio_refencia, v_descripcion
      FROM glo_folio glo1
      INNER JOIN glo_folio glo2 on glo2.folio = glo1.folio_referencia
      INNER JOIN cat_proceso cat ON cat.proceso_cod = glo2.proceso_cod
      WHERE glo1.folio = v_folio

      # Total de Registros de Notificacion 
      LET v_query = 'SELECT COUNT(*) ',
                              '  FROM ', v_nom_tabla
      PREPARE exe_cuenta_mensajes FROM v_query
      EXECUTE exe_cuenta_mensajes INTO v_total_mensajes
--------------------------------------------------------------------------------

   PRINTX v_descripcion
   PRINTX v_f_actualiza USING "dd-mm-yyyy"
   PRINTX v_folio_refencia
   PRINTX v_mensajes_sms
   PRINTX v_mensajes_correo
   PRINTX v_total_detalles
   PRINTX v_total_mensajes

   PRINTX v_folio
   PRINTX p_usuario_cod
   PRINTX v_fecha USING "dd-mm-yyyy"
END REPORT 