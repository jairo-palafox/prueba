--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/02/2016
--==============================================================================

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTP28                                                   #
#Objetivo          => Batch de integración de traspaso saldos receptora        # 
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 16 Febrero 2016                                          #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"
GLOBALS "PRTWS02.inc"

DEFINE p_usuario      LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid          LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod  LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod    LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio        LIKE glo_ctr_archivo.folio, # numero de folio
       p_nom_archivo  LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       p_origen       BOOLEAN

MAIN

   # Se recuperan los parámetros
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)
   LET p_origen      = ARG_VAL(7) # Manual 1 o Batch 2

   CALL fn_inicializa_consultas()
   CALL fn_reenvia_solicitudes_crm()

END MAIN

FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   LET v_consulta = " UPDATE prt_reenvios_solicitud ",
                    "    SET estado_envio = ?,",
                    "        f_reenvio = ?,",
                    "        codigo_resp = ?,",
                    "        mensaje_resp = ?",
                    "  WHERE id_solicitud = ?",
                    "    AND tipo_solicitud = ?",
                    "    AND diagnostico_sol = ?"
   PREPARE prp_act_edo_reenvio FROM v_consulta

   LET v_consulta = " UPDATE prt_reenvios_solicitud ",
                    "    SET f_reenvio = ?,",
                    "        codigo_resp = ?,",
                    "        mensaje_resp = ?",
                    "  WHERE id_solicitud = ?",
                    "    AND tipo_solicitud = ?",
                    "    AND diagnostico_sol = ?"
   PREPARE prp_act_fecha_reenvio FROM v_consulta 

   LET v_consulta = " UPDATE STATISTICS FOR TABLE prt_reenvios_solicitud"
   PREPARE prp_act_reenvios FROM v_consulta

   LET v_consulta = " SELECT proceso_desc ",
                    "   FROM cat_proceso",
                    "  WHERE proceso_cod = ? "
   PREPARE prp_rec_desc_proceso FROM v_consulta

   LET v_consulta = " SELECT opera_desc ",
                    "   FROM cat_operacion",
                    "  WHERE proceso_cod = ? ",
                    "    AND opera_cod = ? "
   PREPARE prp_rec_desc_operacion FROM v_consulta

   LET v_consulta = " SELECT FIRST 1 descripcion_general ",
                    "   FROM prt_diagnostico",
                    "  WHERE diagnostico_externo = ? ",
                    "    AND destino_diagnostico = ? "
   PREPARE prp_rec_desc_diag FROM v_consulta

END FUNCTION

# Descripción: 
FUNCTION fn_reenvia_solicitudes_crm()
DEFINE v_consulta        STRING,
       r_resultado_opera SMALLINT,
       v_reenvio RECORD
         v_id_solictud   LIKE prt_reenvios_solicitud.id_solicitud,
         v_nss           LIKE prt_solicitud_cedente.nss,
         v_n_caso        LIKE prt_solicitud_cedente.n_caso,
         v_diagnostico   LIKE prt_reenvios_solicitud.diagnostico_sol
       END RECORD,
       v_res_reenvios_rpt DYNAMIC ARRAY OF RECORD
         v_id_solictud    LIKE prt_reenvios_solicitud.id_solicitud,
         v_nss            LIKE prt_solicitud_cedente.nss,
         v_n_caso         LIKE prt_solicitud_cedente.n_caso,
         v_diagnostico    LIKE prt_diagnostico.diagnostico_externo,
         v_desc_diag      LIKE prt_diagnostico.descripcion_general,
         v_estado_reenvio LIKE prt_diagnostico.descripcion_general,
         v_codigo_resp    VARCHAR(20),
         v_mensaje_resp   VARCHAR(100)
       END RECORD,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_desc      LIKE cat_operacion.opera_desc,
       v_respuesta_adai      RECORD
          v_resultado_ws        INTEGER,
          v_resultado_operacion CHAR(5),
          v_mensaje             VARCHAR(100)
       END RECORD,
       v_indice           SMALLINT,
       v_fecha_actual_com DATETIME YEAR TO SECOND,
       v_total_aceptados  SMALLINT,
       v_total_rechazados SMALLINT

   EXECUTE prp_rec_desc_proceso USING p_proceso_cod
                                 INTO  v_proceso_desc

   EXECUTE prp_rec_desc_operacion USING p_proceso_cod,
                                        p_opera_cod
                                   INTO v_opera_desc
    
   --DISPLAY "\n"
   DISPLAY "PROCESO: ",v_proceso_desc
   DISPLAY "OPERACIÓN: ",v_opera_desc
   DISPLAY "\n"

   LET v_consulta = "SELECT ren.id_solicitud,",
                    "       sol.nss,",
                    "       sol.n_caso,",
                    "       ren.diagnostico_sol",
                    "  FROM prt_reenvios_solicitud ren JOIN prt_solicitud_cedente sol",
                    "    ON ren.id_solicitud = sol.id_prt_solicitud_cedente",
                    " WHERE ren.estado_envio = ?",
                    "   AND ren.tipo_solicitud = ?"
                    --"   AND sol.estado IN (?,?)"
   # si la ejecución fué manual, verifica si hay que descartar algún registro
   IF( p_origen = 1 )THEN #manual
      LET v_consulta = v_consulta || "   AND ren.id_solicitud NOT IN (SELECT id_solicitud FROM safre_tmp:prt_tmp_reg_no_reenvia WHERE 1 = 1)"
   END IF

   # Contadores de totales
   LET v_total_aceptados  = 0
   LET v_total_rechazados = 0
   LET v_indice           = 0

   PREPARE prp_rec_reenvios FROM v_consulta
   DECLARE cur_rec_reenvios CURSOR FOR prp_rec_reenvios
   FOREACH cur_rec_reenvios USING C_REENVIO_NO_ENVIADO,
                                  C_FLUJO_CEDENTE
                             INTO v_reenvio.*

      LET v_indice = v_indice + 1
      DISPLAY "\n"
      DISPLAY "Procesando reenvío"
      DISPLAY "NSS: "||v_reenvio.v_nss
      DISPLAY "Número de caso: "||v_reenvio.v_n_caso
      DISPLAY "Diagnóstico: "||v_reenvio.v_diagnostico

      LET v_res_reenvios_rpt[v_indice].v_id_solictud = v_reenvio.v_id_solictud
      LET v_res_reenvios_rpt[v_indice].v_nss         = v_reenvio.v_nss
      LET v_res_reenvios_rpt[v_indice].v_n_caso      = v_reenvio.v_n_caso
      LET v_res_reenvios_rpt[v_indice].v_diagnostico = v_reenvio.v_diagnostico
      EXECUTE prp_rec_desc_diag USING v_reenvio.v_diagnostico,
                                      C_DESTINO_DIAG_P_I
                                 INTO v_res_reenvios_rpt[v_indice].v_desc_diag
      DISPLAY "Descripción diagnóstico: "||v_res_reenvios_rpt[v_indice].v_desc_diag
      --DISPLAY ""
      DISPLAY "Envíando notificación..."

      INITIALIZE v_respuesta_adai.* TO NULL
      # Invoca cliente ws para enviar notificación del resultado de la marca a ADAI
      CALL recibeMarcaCedente(v_reenvio.v_n_caso,
                              C_ID_ESTATUS_NOTIFICA_MARCA,
                              v_reenvio.v_diagnostico CLIPPED) # Envia el diagnóstico de procesar directamente a ADAI
                                                             RETURNING v_respuesta_adai.v_resultado_ws,
                                                                       v_respuesta_adai.v_resultado_operacion,
                                                                       v_respuesta_adai.v_mensaje
      
      IF( v_respuesta_adai.v_resultado_ws = 0 AND
          v_respuesta_adai.v_resultado_operacion = C_ID_RESPUESTA_ADAI)THEN

         LET v_fecha_actual_com = CURRENT YEAR TO SECOND
         EXECUTE prp_act_edo_reenvio USING C_REENVIO_ENVIADO,
                                           v_fecha_actual_com,
                                           v_respuesta_adai.v_resultado_operacion,
                                           v_respuesta_adai.v_mensaje,
                                           v_reenvio.v_id_solictud,
                                           C_FLUJO_CEDENTE,
                                           v_reenvio.v_diagnostico
         DISPLAY "Resultado reenvío: INFORMADO"
         LET v_total_aceptados = v_total_aceptados + 1
         LET v_res_reenvios_rpt[v_indice].v_estado_reenvio = "INFORMADO"
      ELSE
         DISPLAY "--- Control de servicio"
         DISPLAY "Resultado de servicio:"||v_respuesta_adai.v_resultado_ws
         DISPLAY "Resultado de operación:"||v_respuesta_adai.v_resultado_operacion
         
         LET v_fecha_actual_com = CURRENT YEAR TO SECOND
         EXECUTE prp_act_fecha_reenvio USING v_fecha_actual_com,
                                             v_respuesta_adai.v_resultado_operacion,
                                             v_respuesta_adai.v_mensaje,
                                             v_reenvio.v_id_solictud,
                                             C_FLUJO_CEDENTE,
                                             v_reenvio.v_diagnostico
         DISPLAY "Resultado reenvío: SIN INFORMAR"
         DISPLAY "Mensaje: "||v_respuesta_adai.v_mensaje
         DISPLAY "---\n"
         LET v_total_rechazados = v_total_rechazados + 1
         LET v_res_reenvios_rpt[v_indice].v_estado_reenvio = "SIN INFORMAR"
      END IF
      LET v_res_reenvios_rpt[v_indice].v_codigo_resp  = v_respuesta_adai.v_resultado_operacion
      LET v_res_reenvios_rpt[v_indice].v_mensaje_resp = v_respuesta_adai.v_mensaje
      --DISPLAY "\n"

   END FOREACH

   FREE cur_rec_reenvios

   DISPLAY "Total registros enviados: "||v_total_aceptados
   DISPLAY "Total registros no enviados: "||v_total_rechazados
   DISPLAY "Total registros procesados: "||(v_total_aceptados + v_total_rechazados)
   DISPLAY "\n"

   # Actualiza estadisticas de tabla
   EXECUTE prp_act_reenvios

   CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                               RETURNING r_resultado_opera
   # si ocurrió un error con la actualizacion de la operacion operacion 
   # muestra el mensaje
   IF(r_resultado_opera)THEN
      CALL fn_desplega_inc_operacion(r_resultado_opera)
   END IF

   IF( v_res_reenvios_rpt.getLength() > 0 )THEN
      CALL fn_genera_reporte_reenvios(v_total_aceptados,
                                      v_total_rechazados,
                                      v_res_reenvios_rpt)
   END IF
   
END FUNCTION

# Descripción: Genera reporte de resultado de reenvíos
FUNCTION fn_genera_reporte_reenvios(p_total_aceptados,p_total_rechazados,p_res_reenvios_rpt)
DEFINE p_total_aceptados  SMALLINT,
       p_total_rechazados SMALLINT,
       p_res_reenvios_rpt DYNAMIC ARRAY OF RECORD
         v_id_solictud    LIKE prt_reenvios_solicitud.id_solicitud,
         v_nss            LIKE prt_solicitud_cedente.nss,
         v_n_caso         LIKE prt_solicitud_cedente.n_caso,
         v_diagnostico    LIKE prt_diagnostico.diagnostico_externo,
         v_desc_diag      LIKE prt_diagnostico.descripcion_general,
         v_estado_reenvio LIKE prt_diagnostico.descripcion_general,
         v_codigo_resp    VARCHAR(20),
         v_mensaje_resp   VARCHAR(100)
       END RECORD,
       r_ruta_listados   LIKE seg_modulo.ruta_listados,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_nom_reporte     STRING,
       v_manejador_rpt   OM.SaxDocumentHandler,
       v_indice          SMALLINT
       
   CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, 
                                  r_ruta_listados
                                  
   # CREACIÓN DE REPORTE
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/PRTP281.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario     CLIPPED,"-",
                          "PRTP28-",
                          p_pid         USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod   USING "&&&&&"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)

      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_rpt_resultados_reenvios TO XML HANDLER v_manejador_rpt

         --DISPLAY "p_res_reenvios_rpt.getLength():",p_res_reenvios_rpt.getLength()
         FOR v_indice = 1 TO p_res_reenvios_rpt.getLength()

            OUTPUT TO REPORT fn_rpt_resultados_reenvios(p_total_aceptados,
                                                        p_total_rechazados,
                                                        p_res_reenvios_rpt[v_indice].*)
         END FOR

      FINISH REPORT fn_rpt_resultados_reenvios

   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

# Descripción: --ORDER BY p_res_reenvios_rpt.v_estado_reenvio
REPORT fn_rpt_resultados_reenvios(p_total_aceptados,p_total_rechazados,p_res_reenvios_rpt)
DEFINE p_total_aceptados  SMALLINT,
       p_total_rechazados SMALLINT,
       p_res_reenvios_rpt RECORD
         v_id_solictud    LIKE prt_reenvios_solicitud.id_solicitud,
         v_nss            LIKE prt_solicitud_cedente.nss,
         v_n_caso         LIKE prt_solicitud_cedente.n_caso,
         v_diagnostico    LIKE prt_diagnostico.diagnostico_externo,
         v_desc_diag      LIKE prt_diagnostico.descripcion_general,
         v_estado_reenvio LIKE prt_diagnostico.descripcion_general,
         v_codigo_resp    VARCHAR(20),
         v_mensaje_resp   VARCHAR(100)
       END RECORD,
       v_pagina       SMALLINT,
       v_fecha_actual STRING

   ORDER BY p_res_reenvios_rpt.v_estado_reenvio
   FORMAT

      FIRST PAGE HEADER
         LET v_fecha_actual = TODAY USING "dd/mm/yyyy"
         PRINTX v_fecha_actual,
                p_total_aceptados,
                p_total_rechazados

      ON EVERY ROW
         PRINTX p_res_reenvios_rpt.v_nss,
                p_res_reenvios_rpt.v_n_caso,
                p_res_reenvios_rpt.v_diagnostico,
                p_res_reenvios_rpt.v_desc_diag,
                p_res_reenvios_rpt.v_estado_reenvio,
                p_res_reenvios_rpt.v_codigo_resp,
                p_res_reenvios_rpt.v_mensaje_resp

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina
       
END REPORT