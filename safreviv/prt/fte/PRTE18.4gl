--==============================================================================
-- Version: 1.0.0
-- Fecha última modificación: 02 Junio 2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTE18                                                   #
#Descripcion       => Batch de preliquidación de devolucion de saldos receptora#
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 02 Junio 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"

DEFINE p_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod,  # Código de operacion
       p_folio       LIKE glo_folio.folio,
       p_usuario_cod LIKE seg_usuario.usuario_cod,
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo
   
MAIN
DEFINE r_error          SMALLINT,
       r_reultado_opera SMALLINT,
       p_titulo         STRING, # titulo del mensaje enviado en el correo
       p_mensaje        STRING,  # cuerpo del mensaje enviado
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_operacion_desc  LIKE cat_operacion.opera_desc,
       v_total_registros INTEGER,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       r_ruta            LIKE seg_modulo.ruta_listados,
       v_comando  STRING
     
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

      # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT opera_desc
     INTO v_operacion_desc
     FROM cat_operacion
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod = p_opera_cod

   SELECT COUNT(*)
     INTO v_total_registros
     FROM prt_traspaso_receptora
    WHERE folio_liquida = p_folio
     
   DISPLAY "\n"
   DISPLAY "PROCESO:   ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_operacion_desc
   DISPLAY "FOLIO:     ",p_folio
   DISPLAY "TOTAL DE REGISTROS A PRELIQUIDAR:",v_total_registros USING "##,###,##&"
   DISPLAY "\n"

   
   #Llamada a ejecución de procedimiento almacenado
   CALL fn_ejecuta_preliquidacion(p_folio,p_usuario_cod) RETURNING r_error

   IF NOT( r_error )THEN
         
      LET p_mensaje = "Preliquidación realizada con éxito"
      

      CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, r_ruta
      --CALL fn_rutas("bat") RETURNING r_ruta, r_ruta_lst
      
      LET v_comando = "fglrun ",r_ruta_ejecutable CLIPPED,"/PRTI03.42r ",p_usuario_cod, " ",
                                                                         p_pid, " ",
                                                                         p_proceso_cod," ",
                                                                         p_opera_cod," ",
                                                                         p_folio," ",
                                                                         "PRTE18"
                      {"' 1>>", r_ruta_lst CLIPPED,
                      "/nohup:",p_pid USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod USING "&&&&&",
                      " 2>&1 &"}
      RUN v_comando
      IF( STATUS )THEN
         DISPLAY "Ocurrió un error al ejecutar el reporte de la preliquidación"
      ELSE
         --DISPLAY "Se ha enviado la operación.\nPodrá revisar el detalle en el monitoreo de procesos"
      END IF
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_reultado_opera
      IF(r_reultado_opera <> 0)THEN
         CALL fn_desplega_inc_operacion(r_reultado_opera)
      END IF
   ELSE
      # Si ocurrio un error se actualiza el estatus como erroneo
      CALL fn_error_opera(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod) RETURNING r_reultado_opera
      IF( r_reultado_opera <> 0 )THEN
         CALL fn_desplega_inc_operacion(r_reultado_opera)
      END IF  
      LET p_mensaje = "El proceso de preliquidación ha finalizado con errores."
   END IF
   
   # Envío de correo con estado de finalizacionde operacion
   LET p_titulo = "Finalización de operación - Preliquidación de devolución receptora"
   CALL fn_correo_proceso(p_pid, 
                          p_proceso_cod, 
                          p_opera_cod, 
                          NULL, 
                          p_titulo,
                          p_mensaje)
END MAIN

# Descripción: Ejecucion de SP de preliquidación de devolución receptora
FUNCTION fn_ejecuta_preliquidacion(p_folio,p_usuario)
DEFINE p_folio     LIKE prt_cza_receptora.folio_liquida,
       p_usuario   LIKE seg_usuario.usuario_cod,
       v_consulta  STRING,
       v_sql_error SMALLINT,
       v_msn_error VARCHAR(40),
       v_error     BOOLEAN,
       r_registros_procesados    INTEGER,
       r_registros_preliquidados INTEGER,
       r_total_abonos            DECIMAL(22,2),
       r_total_saldo_insoluto    DECIMAL(22,2)

   WHENEVER ERROR CONTINUE
   # inicializacion de flujo correcto
   LET v_error = FALSE 
   
   LET v_consulta = "EXECUTE FUNCTION fn_prt_preliquidar_dev_receptora(?,?)"
   PREPARE prp_ejecuta_preliquidacion FROM v_consulta
   EXECUTE prp_ejecuta_preliquidacion USING p_folio,
                                            p_usuario_cod
                                       INTO v_sql_error,
                                            v_msn_error,
                                            r_registros_procesados,
                                            r_registros_preliquidados,
                                            r_total_abonos,
                                            r_total_saldo_insoluto
   
   IF(v_sql_error <> 0)THEN
      LET v_error = TRUE  # Ocurrió error
      DISPLAY ""
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_sql_error
      DISPLAY "MENSAJE:",v_msn_error
      DISPLAY ""
   ELSE
      DISPLAY ""
      DISPLAY "FOLIO PROCESADO:              ",p_folio USING "###,###,##&"
      DISPLAY "REGISTROS PROCESADOS:         ",r_registros_procesados    USING "###,###,##&"
      DISPLAY "REGISTROS PRELIQUIDADOS:      ",r_registros_preliquidados USING "###,###,##&"
      DISPLAY "MONTO TOTAL TRASPASOS:        ",r_total_abonos            USING "###,###,##&.&&"
      DISPLAY "MONTO TOTAL INSOLUTO CRÉDITO: ",r_total_saldo_insoluto    USING "###,###,##&.&&"
      DISPLAY ""
   END IF
   RETURN v_error
END FUNCTION