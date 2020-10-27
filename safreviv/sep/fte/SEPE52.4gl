--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/05/2012
--==============================================================================

################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPL52                                                        #
#Objetivo     => Intergración de archivo de compensacion deudor                #
#Fecha inicio => 19 Septiembre 2012                                            #
################################################################################
DATABASE safre_viv

DEFINE p_usuario     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid         LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod   LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio       LIKE glo_ctr_archivo.folio, # numero de folio
       v_nom_archivo LIKE glo_ctr_archivo.nombre_archivo # nombre del archivo a integrar
       
MAIN
DEFINE v_ruta_ejecutable  LIKE seg_modulo.ruta_bin,
       r_folio            LIKE glo_folio.folio,
       v_proceso_desc     LIKE cat_proceso.proceso_desc,
       v_conteo_registros INTEGER,
       v_consulta         STRING,
       v_total_cuentas_procesadas    INTEGER,
       v_total_cuentas_no_procesadas INTEGER,
       v_fecha_actual     DATE,
       v_msg_error        VARCHAR(254),
       v_error_sql        INTEGER,
       r_resultado_opera  SMALLINT

   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET v_nom_archivo = ARG_VAL(6)


   LET v_fecha_actual = TODAY
   SELECT ruta_bin
     INTO v_ruta_ejecutable
     FROM seg_modulo
    WHERE modulo_cod = 'sep'

   # genera folio 
   CALL fn_genera_folio(p_proceso_cod,p_opera_cod,p_usuario)
                        RETURNING r_folio
   
   # recupera la descripción del proceso
   SELECT proceso_desc
     INTO v_proceso_desc
     FROM cat_proceso
    WHERE proceso_cod = p_proceso_cod

   SELECT COUNT(*)
     INTO v_conteo_registros 
     FROM safre_tmp:tmp_sep_det_compensacion_deud
    WHERE 1 = 1

   # Imprime en log datos de integración
   DISPLAY "\n"
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "ARCHIVO: ",v_nom_archivo
   DISPLAY "FOLIO: ",r_folio
   DISPLAY "TOTAL DE REGISTROS A PROCESAR:",v_conteo_registros 
   DISPLAY "\n"

   # Ejecuta SP de integracion diagnóstico Op27
   LET v_consulta = "EXECUTE FUNCTION safre_viv:fn_sep_integra_compensacion_deudor(?,?,?)"
   PREPARE prp_integra_compensacion_deud FROM v_consulta
   EXECUTE prp_integra_compensacion_deud USING r_folio,
                                               v_nom_archivo,
                                               p_usuario
                                          INTO v_total_cuentas_procesadas,v_total_cuentas_no_procesadas,v_error_sql,v_msg_error

   IF(SQLCA.SQLCODE <> 0 OR v_error_sql <> 0)THEN
      DISPLAY "OCURRIÓ UN ERROR AL EJECUTAR EL SP"
      DISPLAY "CÓDIGO: ",v_error_sql
      DISPLAY "MENSAJE: ",v_msg_error
      CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
                                RETURNING r_resultado_opera
      # si ocurrió un error con la actualizacion de la operacion operacion 
      # muestra el mensaje
      IF(r_resultado_opera)THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
      EXIT PROGRAM
   END IF
      # Estadística de SP
   DISPLAY "TOTAL DE NSS PROCESADOS: ",v_total_cuentas_procesadas
   DISPLAY "TOTAL DE NSS NO ENCONTRADOS: ",v_total_cuentas_no_procesadas
   DISPLAY "\n"

   UPDATE bat_ctr_operacion
      SET folio = r_folio
    WHERE pid = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod = v_opera_carga_compensacion_deudor
      
   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
      EXIT PROGRAM
   ELSE
      # Envío de correo de notificación de proceso finalizado
      CALL fn_correo_proceso(p_pid, 
                             p_proceso_cod, 
                             p_opera_cod, 
                             '',
                             'Integración de Compensación deudor',
                             'ID Proceso   : '||p_pid||
                             'Proceso      : '||p_proceso_cod||
                             'Operacion    : '||p_opera_cod||
                             'Fecha Inicio : '||v_fecha_actual||
                             'Fecha Fin    : '||DATE
                             )
      # Ejecuta elaboración de reporte
      {LET v_comando = "fglrun ",v_ruta_ejecutable CLIPPED,"/SEPI13.42r '",
                                p_usuario CLIPPED, "' ",
                                p_pid CLIPPED, " ",
                                p_proceso_cod CLIPPED," ",
                                p_opera_cod CLIPPED," ",
                                r_folio CLIPPED, " '",
                                v_nom_archivo CLIPPED,"'"
      
      RUN v_comando
      IF(STATUS)THEN
         DISPLAY "Ocurrió un error al generar el reporte"
      END IF}
   END IF
   
END MAIN