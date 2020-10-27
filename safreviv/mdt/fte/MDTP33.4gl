--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10-07-2013
--==============================================================================

################################################################################
#Modulo            => MDT                                                      #
#Programa          => MDTP31                                                   #
#Objetivo          => Programa batch de reporte de abonos                      #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Julio 2013                                            #
################################################################################
DATABASE safre_viv

GLOBALS "MDTG02.4gl"

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
       v_consulta        STRING

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   LET v_consulta = "\n SELECT DISTINCT MAX(mdt.folio_dispersion)",
                    "\n   FROM mdt_det_aplica_mandato mdt JOIN mdt_det_aplica_monto mto",
                    "\n     ON mto.id_det_aplica_mandato = mto.id_det_aplica_mandato",
                    "\n  WHERE mto.estado = ?"
   PREPARE prp_rec_folio FROM v_consulta
   EXECUTE prp_rec_folio USING g_estado_abonado_pago_mdt
                          INTO p_folio
 
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'mdt'

   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   DISPLAY "\n"
   DISPLAY "Proceso: ",v_proceso_desc
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "INICIANDO PROCESO DE REPORTE MANDTOS"
   DISPLAY "Folio:",p_folio
   DISPLAY "\n"

   # Libreria(MDTI31) que genera el reporte de mandatos
   CALL fn_mdt_rpt_aplicacion_mdt(p_folio,
                                  g_estado_abonado_pago_mdt, # Var global de estado(100) mandatos abonados 
                                  p_usuario_cod,
                                  p_pid,
                                  p_proceso_cod,
                                  p_opera_cod,
                                  TODAY,
                                  "MDTP33") 
   
   DISPLAY "\n"
   DISPLAY "REPORTE CONCLUIDO"
   DISPLAY "FIN PROCESO"
   DISPLAY "Fecha  : ",TODAY
   DISPLAY "Hora   : ",TIME(CURRENT)
   DISPLAY "\n"


   CALL  fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)RETURNING r_resultado_opera
   IF(r_resultado_opera = 0)THEN
      LET v_mensaje = "El proceso ha finalizado correctamente"
      # Envia correo de estado de operación               
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '', # Archivo adjunto
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte mandatos',
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
                             'Finalización de operación - '||v_proceso_desc CLIPPED||' - Reporte mandatos',
                             v_mensaje)
   END IF

END MAIN