--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 24-07-2013
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => HPSP01                                                   #
#Objetivo          => Programa batch de reporte traspaso fondo servicios       #
#Autor             => Jesus Yañez Moreno                                       #
#Fecha inicio      => 18 Enero 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "HPSG02.4gl"

DEFINE p_usuario_cod LIKE seg_usuario.usuario_cod, # Usuario
       p_pid         LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio,   # numero de folio
       p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo

MAIN
DEFINE v_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       r_resultado_opera SMALLINT,
       v_mensaje         STRING,
       v_consulta        STRING,
       v_folio_rpt       LIKE glo_folio.folio,
       v_folio_dummy     DECIMAL(9,0),
       v_usuario_dummy   VARCHAR(20),
       v_registros       INTEGER
DEFINE v_si_resultado    SMALLINT
DEFINE v_isam_err        INTEGER
DEFINE v_err_txt         CHAR(200) 
DEFINE v_c_msj           VARCHAR(200)
DEFINE l_txt             VARCHAR(1000) 

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   LET v_folio_dummy = 11740
   LET v_usuario_dummy = "prueba_hps"   
   
   LET l_txt = "EXECUTE PROCEDURE sp_hps_aplica_fondo_servicio(?,?) "
   PREPARE qry_aplica_fondo_servicio FROM l_txt

   LET l_txt = "EXECUTE FUNCTION fn_hps_preliquidar_traspaso_servicios(?,?) "
   PREPARE qry_preliquidar_traspaso_servicios FROM l_txt

   
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   DISPLAY "INTEGRANDO SALDOS A TRASPASAR SUBCUENTAS SERVICIOS" 

   EXECUTE qry_aplica_fondo_servicio USING v_folio_dummy, v_usuario_dummy 
                                     INTO  v_si_resultado ,
                                           v_isam_err     ,
                                           v_c_msj
   IF v_si_resultado <> 0 THEN
   
      # Actualiza a estado erróneo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = v_c_msj
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte Traspaso Fondos Subcuentas Servicios',
                             v_mensaje)

   END IF
                                                               
   INITIALIZE v_folio_rpt TO NULL
   
   LET v_consulta = "\n SELECT MAX(folio)",
                    "\n   FROM glo_folio",
                    "\n  WHERE proceso_cod = ?",
                    "\n    AND opera_cod = ?",
                    "\n    AND status = 0" # registrado
   PREPARE prp_recupera_folio FROM v_consulta
   EXECUTE prp_recupera_folio USING p_proceso_cod,
                                    p_opera_cod
                               INTO v_folio_rpt

   DISPLAY "\n"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "folio  : ",v_folio_rpt
   DISPLAY "Proceso: ",v_proceso_desc
   DISPLAY "\n"

  
   DISPLAY "INICIANDO PROVISION"

   EXECUTE qry_preliquidar_traspaso_servicios USING v_folio_rpt ,p_usuario_cod 
                                              INTO  v_si_resultado ,
                                                    v_isam_err     ,
                                                    v_c_msj     
   IF v_si_resultado <> 0 THEN
   
      # Actualiza a estado erróneo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = v_c_msj
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte Traspaso Fondos Subcuentas Servicios',
                             v_mensaje)
   END IF

   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "GENERANDO REPORTE"
   DISPLAY "\n"

   LET v_registros = 0
   # Libreria(HPSI31) que genera el reporte de mandatos
   CALL fn_mdt_rpt_aplicacion_mdt(v_folio_rpt,
                                  50, # abono mandato
                                  p_usuario_cod,
                                  p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  TODAY,
                                  "HPSP01") RETURNING v_registros

IF( v_registros > 0)THEN
 
   DISPLAY "\n"
   DISPLAY "REPORTE CONCLUIDO"
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

   CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN
      LET v_mensaje = "La operación ha finalizado correctamente"
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte Abonos Mandatos',
                             v_mensaje)
   ELSE              
      # Actualiza a estado erróneo
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      LET v_mensaje = "Ocurrió un error al actualizar el estado de la operación"
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte Abonos Mandatos',
                             v_mensaje)
   END IF
ELSE
   CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         # Muestra el mensaje de inconsistencia en archivo y consola
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"

END IF
END MAIN
