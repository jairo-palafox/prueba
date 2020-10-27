--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/07/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTI06                                                     #
#Objetivo        => Consulta cifras control y traspasos                        #
#Fecha Inicio    => 10 Julio 2015                                              #
################################################################################
SCHEMA "safre_viv"

DEFINE p_usuario         VARCHAR(20),--LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid             LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod     LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod       LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio           LIKE glo_ctr_archivo.folio, # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       p_flujo           SMALLINT,
       p_tipo_cedente    SMALLINT,
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_ruta_listados   LIKE seg_modulo.ruta_listados,
       g_ruta_envio      LIKE seg_modulo.ruta_envio,
       g_nom_rpt_txt     STRING
       
MAIN

   LET p_usuario      = ARG_VAL(1)
   LET p_pid          = ARG_VAL(2)
   LET p_proceso_cod  = ARG_VAL(3)
   LET p_opera_cod    = ARG_VAL(4)
   LET p_folio        = ARG_VAL(5)
   LET p_nom_archivo  = ARG_VAL(6)
   LET p_flujo        = ARG_VAL(7)
   LET p_tipo_cedente = ARG_VAL(8)

   CONNECT TO "safre_viv"
   CALL fn_inicializa_consultas()
   # p_flujo, 1 -> Cedente, 2 -> Receptora
   IF( p_flujo = 1 )THEN
      # p_tipo_cedente, 1 -> Aceptadas, 2 -> Rechazadas
      IF( p_tipo_cedente = 1 )THEN
         CALL fn_genera_reportes_cedente_ace()
      ELSE
         CALL fn_genera_reportes_cedente_rch()
      END IF
   ELSE
      CALL fn_genera_reportes_receptora()
   END IF
   DISCONNECT "safre_viv"

END MAIN

# Descripción: inicializa consultas del programa
FUNCTION fn_inicializa_consultas()
DEFINE v_consulta STRING

   SELECT ruta_bin,
          ruta_listados,
          ruta_envio
     INTO g_ruta_ejecutable,
          g_ruta_listados,
          g_ruta_envio
     FROM seg_modulo
    WHERE modulo_cod = 'prt'
    

   LET v_consulta = " SELECT proceso_desc",
                    "   FROM cat_proceso",
                    "  WHERE proceso_cod = ?"
   PREPARE prp_recupera_proceso FROM v_consulta

   LET v_consulta = " SELECT opera_desc",
                    "   FROM cat_operacion",
                    "  WHERE proceso_cod = ?",
                    "    AND opera_cod = ?"
   PREPARE prp_recupera_operacion FROM v_consulta

   LET v_consulta = " SELECT id_solicitud,",
                    "        nss,",
                    "        curp,",
                    "        credito,",
                    "        sdo_insoluto,",
                    "        estado_marca,",
                    "        f_originacion,",
                    "        f_ini_tra,",
                    "        saldo_viv97,",
                    "        aivs_solicitado,",
                    "        pesos_solicitado,",
                    "        f_marca,",
                    "        folio_tramite,",
                    "        aivs_procesar,",
                    "        pesos_procesar,",
                    "        diag_procesar,",              
                    "        desc_diag_procesar,",
                    "        aivs_cedido,",
                    "        pesos_cedido,",
                    "        fecha_proceso,",
                    "        diag_notif,",
                    "        diagnostico",
                    "   FROM safre_tmp:prt_tmp_con_trp_prt_ced",
                    "  WHERE 1 = 1"
   PREPARE prp_recupera_registros_ced FROM v_consulta

   LET v_consulta = " SELECT id_solicitud,",
                    "        nss,",
                    "        curp,",
                    "        credito,",
                    "        f_marca,",
                    "        folio_tramite,",                    
                    "        sdo_insoluto,",
                    "        aivs_recibido,",
                    "        pesos_recibido,",
                    "        fecha_proceso,",
                    "        pesos_dispersion,",
                    "        fecha_dispersion",
                    "   FROM safre_tmp:prt_tmp_con_trp_prt_rec",
                    "  WHERE 1 = 1 ",
                    "  ORDER BY id_solicitud "                    
   PREPARE prp_recupera_registros_rec FROM v_consulta

END FUNCTION 

# Descripción: Especifica parametros y construcción de reporte acepatadas cedente
FUNCTION fn_genera_reportes_cedente_ace()
DEFINE p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_totales_sdo_insoluto     LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
       v_totales_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
       v_totales_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
       v_totales_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
       v_totales_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
       v_totales_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
       v_totales_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
       v_tipo_rpt      STRING,
       v_nom_reporte   STRING,       
       v_manejador_rpt OM.SaxDocumentHandler,
       r_resultado_opera INTEGER,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       --v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       --v_opera_desc      LIKE cat_operacion.opera_desc,
       v_hora            DATETIME HOUR TO SECOND,
       v_bnd_reallizado  BOOLEAN,
       v_pid_txt         STRING,
       v_canal           BASE.Channel,
       v_registro        STRING,
       v_contador        INTEGER

   LET v_bnd_reallizado = FALSE
   
   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTI061.4rp") )THEN
      CALL fgl_report_selectDevice("PDF")

      LET v_pid_txt     = p_pid CLIPPED
      LET v_proceso_cod = p_proceso_cod USING "&&&&&"
      LET v_opera_cod   = p_opera_cod USING "&&&&&"
      LET g_nom_rpt_txt = g_ruta_envio CLIPPED ||"/"||v_pid_txt||v_proceso_cod||v_opera_cod||".txt"
      LET v_nom_reporte = p_usuario CLIPPED, 
                          "-PRTC06-",
                          v_pid_txt,"-",
                          v_proceso_cod USING "&&&&&","-",
                          v_opera_cod USING "&&&&&",".pdf"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(g_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      LET v_hora = CURRENT HOUR TO SECOND
                                      
      DISPLAY "GENERACIÓN DE REPORTES MOVIMIENTOS CEDENTE PORTABILIDAD"
      DISPLAY "USUARIO:   ",p_usuario
      DISPLAY "FECHA:     ",TODAY USING "dd/mm/yyyy"
      DISPLAY "HORA:      ",v_hora
      DISPLAY ""

      LET v_contador = 0
      LET v_totales_sdo_insoluto     = 0     
      LET v_totales_saldo_viv97_inf  = 0
      LET v_totales_aivs_solicitado  = 0
      LET v_totales_pesos_solicitado = 0
      LET v_totales_aivs_procesar    = 0
      LET v_totales_pesos_procesar   = 0
      LET v_totales_pesos_cedido     = 0
      
      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("|")
      CALL v_canal.openFile(g_nom_rpt_txt,"w")
      CALL v_canal.writeLine("NSS|"||
                             "CURP|"||
                             "No. Crédito Fovissste|"||
                             "Saldo insoluto Fovissste|"||
                             "Fecha originación Fovissste|"||
                             "Fecha inicio Trámite|"||
                             "Saldo Viv 97 Infonavit|"||
                             "AIVS Solicitado|"||
                             "PESOS Solicitado|"||
                             "Fecha marca|"||
                             "Folio trámite Procesar|"||
                             "AIVS Procesar|"||
                             "PESOS Procesar|"||
                             "Diagnóstico Procesar|"||
                             "Descripción diagnóstico Procesar|"||
                             "PESOS transferidos a Fovissste|"||
                             "Fecha proceso|")
      DECLARE cur_recupera_registros_ced_ace CURSOR FOR prp_recupera_registros_ced
      START REPORT fn_rpt_consulta_portabilidad_ced TO XML HANDLER v_manejador_rpt         
         FOREACH cur_recupera_registros_ced_ace INTO p_registros.*
            LET v_contador = v_contador + 1
            OUTPUT TO REPORT fn_rpt_consulta_portabilidad_ced(v_contador,p_registros.*,v_hora)
            LET v_registro = p_registros.v_nss,"|",
                             p_registros.v_curp,"|",
                             p_registros.v_credito,"|",
                             p_registros.v_sdo_insoluto,"|",
                             p_registros.v_f_originacion USING "dd/mm/yyyy","|",
                             p_registros.v_f_ini_tramite USING "dd/mm/yyyy","|",
                             p_registros.v_saldo_viv97_inf,"|",
                             p_registros.v_aivs_solicitado,"|",
                             p_registros.v_pesos_solicitado,"|",
                             p_registros.v_f_marca USING "dd/mm/yyyy","|",
                             p_registros.v_folio_tram_proce,"|",
                             p_registros.v_aivs_procesar,"|",
                             p_registros.v_pesos_procesar,"|",
                             p_registros.v_diag_procesar,"|",
                             p_registros.v_desc_diag_procesar,"|",
                             p_registros.v_pesos_cedido,"|",
                             p_registros.v_fecha_proceso USING "dd/mm/yyyy","|"
                                   
            CALL v_canal.writeLine(v_registro)

            IF( p_registros.v_sdo_insoluto IS NOT NULL )THEN LET v_totales_sdo_insoluto     = v_totales_sdo_insoluto     + p_registros.v_sdo_insoluto END IF
            IF( p_registros.v_saldo_viv97_inf IS NOT NULL )THEN LET v_totales_saldo_viv97_inf  = v_totales_saldo_viv97_inf  + p_registros.v_saldo_viv97_inf END IF
            IF( p_registros.v_aivs_solicitado IS NOT NULL )THEN LET v_totales_aivs_solicitado  = v_totales_aivs_solicitado  + p_registros.v_aivs_solicitado END IF
            IF( p_registros.v_pesos_solicitado IS NOT NULL )THEN LET v_totales_pesos_solicitado = v_totales_pesos_solicitado + p_registros.v_pesos_solicitado END IF
            IF( p_registros.v_aivs_procesar IS NOT NULL )THEN LET v_totales_aivs_procesar    = v_totales_aivs_procesar    + p_registros.v_aivs_procesar END IF
            IF( p_registros.v_pesos_procesar IS NOT NULL )THEN LET v_totales_pesos_procesar   = v_totales_pesos_procesar   + p_registros.v_pesos_procesar END IF
            IF( p_registros.v_pesos_cedido IS NOT NULL )THEN LET v_totales_pesos_cedido     = v_totales_pesos_cedido     + p_registros.v_pesos_cedido END IF
         END FOREACH
      FINISH REPORT fn_rpt_consulta_portabilidad_ced

      LET v_registro = "TOTALES|",
                       v_contador,"|",
                       "|",
                       v_totales_sdo_insoluto,"|",
                       "|",
                       "|",
                       v_totales_saldo_viv97_inf,"|",
                       v_totales_aivs_solicitado,"|",
                       v_totales_pesos_solicitado,"|",
                       "|",
                       "|",
                       v_totales_aivs_procesar,"|",
                       v_totales_pesos_procesar,"|",
                       "|",
                       "|",
                       v_totales_pesos_cedido,"|",
                       "|"
      CALL v_canal.writeLine(v_registro)
      
      FREE cur_recupera_registros_ced_ace
      CALL v_canal.close()
            
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         DISPLAY "REPORTE DE REGISTROS ACEPTADOS CEDENTE"
         DISPLAY "REPORTE GENERADO EN: ",g_nom_rpt_txt
      ELSE
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF              
   ELSE
      DISPLAY "No fue posible generar el reporte"
      CALL fn_error_opera(p_pid,
                          p_proceso_cod,
                          p_opera_cod)
                     RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   END IF

END FUNCTION

# Descripción: Especifica parametros y construcción de reporte rechazadas cedente
FUNCTION fn_genera_reportes_cedente_rch()
DEFINE p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_total_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
       v_nom_reporte   STRING,       
       v_manejador_rpt OM.SaxDocumentHandler,
       r_resultado_opera INTEGER,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       --v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       --v_opera_desc      LIKE cat_operacion.opera_desc,
       v_hora            DATETIME HOUR TO SECOND,
       v_bnd_reallizado  BOOLEAN,
       v_pid_txt         STRING,
       v_canal           BASE.Channel,
       v_registro        STRING,
       v_contador        INTEGER

   LET v_bnd_reallizado = FALSE
   
   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTI063.4rp") )THEN
      CALL fgl_report_selectDevice("PDF")

      LET v_pid_txt     = p_pid CLIPPED
      LET v_proceso_cod = p_proceso_cod USING "&&&&&"
      LET v_opera_cod   = p_opera_cod USING "&&&&&"
      LET g_nom_rpt_txt = g_ruta_envio CLIPPED ||"/"||v_pid_txt||v_proceso_cod||v_opera_cod||".txt"
      LET v_nom_reporte = p_usuario CLIPPED, 
                          "-PRTC06-",
                          v_pid_txt,"-",
                          v_proceso_cod USING "&&&&&","-",
                          v_opera_cod USING "&&&&&",".pdf"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(g_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      LET v_hora = CURRENT HOUR TO SECOND
                                      
      DISPLAY "GENERACIÓN DE REPORTES MOVIMIENTOS CEDENTE PORTABILIDAD"
      DISPLAY "USUARIO:   ",p_usuario
      DISPLAY "FECHA:     ",TODAY USING "dd/mm/yyyy"
      DISPLAY "HORA:      ",v_hora
      DISPLAY ""

      LET v_total_sdo_insoluto = 0
      LET v_contador = 0
      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("|")
      CALL v_canal.openFile(g_nom_rpt_txt,"w")
      CALL v_canal.writeLine("NSS|"||
                             "CURP|"||
                             "No. Crédito Fovissste|"||
                             "Saldo insoluto Fovissste|"||
                             "Fecha marca|"||
                             "Diagnóstico notificación|"||
                             "Descripción diagnóstico|")
      DECLARE cur_recupera_registros_ced_rch CURSOR FOR prp_recupera_registros_ced      
      START REPORT fn_rpt_consulta_portabilidad_ced TO XML HANDLER v_manejador_rpt         
         FOREACH cur_recupera_registros_ced_rch INTO p_registros.*
            LET v_contador = v_contador + 1
            OUTPUT TO REPORT fn_rpt_consulta_portabilidad_ced(v_contador,p_registros.*,v_hora)
            LET v_registro = p_registros.v_nss,"|",
                             p_registros.v_curp,"|",
                             p_registros.v_credito,"|",
                             p_registros.v_sdo_insoluto,"|",
                             p_registros.v_f_marca USING "dd/mm/yyyy","|",
                             p_registros.v_diag_notif,"|",
                             p_registros.v_diag_desc,"|"
                                   
            CALL v_canal.writeLine(v_registro)
            LET v_total_sdo_insoluto = v_total_sdo_insoluto + p_registros.v_sdo_insoluto 
         END FOREACH
      FINISH REPORT fn_rpt_consulta_portabilidad_ced

      LET v_registro = "TOTALES|",
                       v_contador,"|",
                       v_total_sdo_insoluto,"|",
                       "|",
                       "|",
                       "|",
                       "|"
      CALL v_canal.writeLine(v_registro)
      FREE cur_recupera_registros_ced_rch
      CALL v_canal.close()
            
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         DISPLAY "REPORTE DE REGISTROS RECHAZADOS CEDENTE"
         DISPLAY "REPORTE GENERADO EN: ",g_nom_rpt_txt
      ELSE
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF              
   ELSE
      DISPLAY "No fue posible generar el reporte"
      CALL fn_error_opera(p_pid,
                          p_proceso_cod,
                          p_opera_cod)
                     RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   END IF

END FUNCTION  

# Descripción: Especifica parametros y construcción de reporte receptora
FUNCTION fn_genera_reportes_receptora()
DEFINE p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida
       END RECORD,
       v_total_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
       v_total_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
       v_total_pesos_dispersion LIKE cta_movimiento.monto_pesos,
       v_nom_reporte     STRING,       
       v_manejador_rpt   OM.SaxDocumentHandler,
       r_resultado_opera INTEGER,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       --v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       --v_opera_desc      LIKE cat_operacion.opera_desc,
       v_hora            DATETIME HOUR TO SECOND,
       v_bnd_reallizado  BOOLEAN,
       v_pid_txt         STRING,
       v_canal           BASE.Channel,
       v_registro        STRING,
       v_contador        INTEGER

   LET v_bnd_reallizado = FALSE
   
   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTI062.4rp") )THEN
      CALL fgl_report_selectDevice("PDF")

      LET v_pid_txt     = p_pid CLIPPED
      LET v_proceso_cod = p_proceso_cod USING "&&&&&"
      LET v_opera_cod   = p_opera_cod USING "&&&&&"
      LET g_nom_rpt_txt = g_ruta_envio CLIPPED ||"/"||v_pid_txt||v_proceso_cod||v_opera_cod||".txt"
      LET v_nom_reporte = p_usuario CLIPPED, 
                          "-PRTC06-",
                          v_pid_txt,"-",
                          v_proceso_cod USING "&&&&&","-",
                          v_opera_cod USING "&&&&&",".pdf"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(g_ruta_listados CLIPPED||"/"||v_nom_reporte)
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()

      
      {EXECUTE prp_recupera_proceso USING p_proceso_cod
                                    INTO v_proceso_desc
                                          
      EXECUTE prp_recupera_operacion USING p_proceso_cod,
                                           p_opera_cod
                                      INTO v_opera_desc}
      LET v_hora = CURRENT HOUR TO SECOND
                                      
      DISPLAY "GENERACIÓN DE REPORTES MOVIMIENTOS RECEPTORA PORTABILIDAD"
      --DISPLAY "PROCESO:   ",v_proceso_desc
      --DISPLAY "OPERACIÓN: ",v_opera_desc
      DISPLAY "USUARIO:   ",p_usuario
      DISPLAY "FECHA:     ",TODAY USING "dd/mm/yyyy"
      DISPLAY "HORA:      ",v_hora
      DISPLAY ""

      LET v_contador = 0
      LET v_total_sdo_insoluto   = 0
      LET v_total_pesos_recibido = 0
      LET v_total_pesos_dispersion = 0
      
      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("|")
      CALL v_canal.openFile(g_nom_rpt_txt,"w")
      CALL v_canal.writeLine("NSS|"||
                             "CURP|"||
                             "No. Crédito Infonavit|"||
                             "Fecha marca|"||
                             "Folio trámite Procesar|"||
                             "Saldo insoluto Infonavit|"||
                             "PESOS Recibido|"||
                             "Fecha proceso|"||
                             "PESOS Cartera|"||
                             "Fecha Cartera|")
      DECLARE cur_recupera_registros_rec CURSOR FOR prp_recupera_registros_rec      
      START REPORT fn_rpt_consulta_portabilidad_rec TO XML HANDLER v_manejador_rpt         
         FOREACH cur_recupera_registros_rec INTO p_registros.*
            LET v_contador = v_contador + 1
            OUTPUT TO REPORT fn_rpt_consulta_portabilidad_rec(v_contador,p_registros.*,v_hora)
            LET v_registro = p_registros.v_nss,"|",
                             p_registros.v_curp,"|",
                             p_registros.v_credito,"|",
                             p_registros.v_f_marca USING "dd/mm/yyyy","|",
                             p_registros.v_folio_tramite,"|",
                             p_registros.v_sdo_insoluto,"|",
                             p_registros.v_pesos_recibido,"|",
                             p_registros.v_fecha_proceso USING "dd/mm/yyyy","|",
                             p_registros.v_pesos_dispersion,"|",
                             p_registros.v_f_liquida_disp USING "dd/mm/yyyy","|"
            CALL v_canal.writeLine(v_registro)
            
            LET v_total_sdo_insoluto   = v_total_sdo_insoluto + p_registros.v_sdo_insoluto
            IF( p_registros.v_pesos_recibido IS NOT NULL)THEN LET v_total_pesos_recibido = v_total_pesos_recibido + p_registros.v_pesos_recibido END IF
            IF( p_registros.v_pesos_dispersion IS NOT NULL)THEN LET v_total_pesos_dispersion = v_total_pesos_dispersion + p_registros.v_pesos_dispersion END IF
         END FOREACH
      FINISH REPORT fn_rpt_consulta_portabilidad_rec
      LET v_registro = "TOTALES|",
                       v_contador,"|",
                       "|",
                       "|",
                       "|",
                       v_total_sdo_insoluto,"|",
                       v_total_pesos_recibido,"|",
                       "|",
                       v_total_pesos_dispersion,"|",
                       "|"
      CALL v_canal.writeLine(v_registro)
      FREE cur_recupera_registros_rec
      CALL v_canal.close()
            
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         DISPLAY "REPORTE DE REGISTROS RECEPTORA"
         DISPLAY "REPORTE GENERADO EN: ",g_nom_rpt_txt
      ELSE
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF              
   ELSE
      DISPLAY "No fue posible generar el reporte"
      CALL fn_error_opera(p_pid,
                          p_proceso_cod,
                          p_opera_cod)
                     RETURNING r_resultado_opera
      IF( r_resultado_opera )THEN
         CALL fn_desplega_inc_operacion(r_resultado_opera)
      END IF
   END IF

END FUNCTION 

# Descripción: Genera reporte PDF de registros portabilidad
REPORT fn_rpt_consulta_portabilidad_ced(p_contador,p_registros,p_hora)
DEFINE p_contador INTEGER,
       p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
          v_estado_marca VARCHAR(20),
          v_f_originacion    LIKE prt_solicitud_cedente.f_originacion_fovissste,
          v_f_ini_tramite    LIKE prt_solicitud_cedente.f_ini_tramite,
          v_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
          v_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
          v_f_marca          LIKE sfr_marca_historica.f_inicio,
          v_folio_tram_proce LIKE bus_tramite.folio_procesar,
          v_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
          v_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
          v_diag_procesar    LIKE prt_traspaso_cedente.diag_procesar,
          v_desc_diag_procesar LIKE prt_diagnostico.descripcion_general,
          v_aivs_cedido      LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso    LIKE prt_preliquida.f_liquida,
          v_diag_notif   VARCHAR(20),
          v_diag_desc    VARCHAR(100)
       END RECORD,
       v_total_sdo_insoluto LIKE prt_solicitud_cedente.saldo_insoluto_credito_fovissste,
       v_total_saldo_viv97_inf  LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
       v_total_aivs_solicitado  LIKE prt_solicitud_cedente.aivs_saldo_viv97_infonavit,
       v_total_pesos_solicitado LIKE prt_solicitud_cedente.pesos_saldo_viv97_infonavit,
       v_total_aivs_procesar    LIKE prt_solicitud_cedente.aivs_saldo_viv97_afore,
       v_total_pesos_procesar   LIKE prt_solicitud_cedente.pesos_saldo_viv97_afore,
       v_total_pesos_cedido     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
       p_hora         DATETIME HOUR TO SECOND,
       v_pagina       SMALLINT,
       v_fecha_actual CHAR(10),
       v_estado_marca VARCHAR(20),
       v_titulo       STRING,
       v_total_registros INTEGER

      
   FORMAT

      FIRST PAGE HEADER
         IF( p_registros.v_estado_marca = "ACEPTADA")THEN
            LET v_titulo = "REPORTE DE REGISTROS ACEPTADAS PORTABILIDAD CEDENTE"
         ELSE
            LET v_titulo = "REPORTE DE REGISTROS RECHAZADOS PORTABILIDAD CEDENTE"
         END IF
         LET v_estado_marca = p_registros.v_estado_marca
         LET v_fecha_actual = TODAY USING "dd/mm/yyyy"
         LET v_total_registros = COUNT(*)
         PRINTX v_titulo,
                p_usuario,
                v_fecha_actual,
                p_hora,
                v_estado_marca,
                v_total_registros

      PAGE HEADER
         PRINTX v_titulo,
                p_usuario,
                v_fecha_actual,
                p_hora,
                v_estado_marca,
                v_total_registros      

      ON EVERY ROW
         PRINTX p_contador,
                p_registros.v_id_solicitud,
                p_registros.v_nss,
                p_registros.v_curp,
                p_registros.v_credito,
                p_registros.v_sdo_insoluto,
                p_registros.v_estado_marca,
                p_registros.v_f_originacion USING "dd/mm/yyyy",
                p_registros.v_f_ini_tramite USING "dd/mm/yyyy",
                p_registros.v_saldo_viv97_inf,
                p_registros.v_aivs_solicitado,
                p_registros.v_pesos_solicitado,
                p_registros.v_f_marca       USING "dd/mm/yyyy",
                p_registros.v_folio_tram_proce,                
                p_registros.v_aivs_procesar,
                p_registros.v_pesos_procesar,
                p_registros.v_diag_procesar,
                p_registros.v_desc_diag_procesar,
                p_registros.v_pesos_cedido,
                p_registros.v_fecha_proceso USING "dd/mm/yyyy",
                p_registros.v_diag_notif,
                p_registros.v_diag_desc

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         LET v_total_sdo_insoluto     = SUM(p_registros.v_sdo_insoluto)
         LET v_total_saldo_viv97_inf  = SUM(p_registros.v_saldo_viv97_inf)
         LET v_total_aivs_solicitado  = SUM(p_registros.v_aivs_solicitado)
         LET v_total_pesos_solicitado = SUM(p_registros.v_pesos_solicitado)
         LET v_total_aivs_procesar    = SUM(p_registros.v_aivs_procesar)
         LET v_total_pesos_procesar   = SUM(p_registros.v_pesos_procesar)
         LET v_total_pesos_cedido     = SUM(p_registros.v_pesos_cedido)
         
         PRINTX v_total_sdo_insoluto,
                v_total_saldo_viv97_inf,
                v_total_aivs_solicitado,
                v_total_pesos_solicitado,
                v_total_aivs_procesar,
                v_total_pesos_procesar,
                v_total_pesos_cedido
      
END REPORT

# Descripción: Genera reporte PDF de registros portabilidad receptora
REPORT fn_rpt_consulta_portabilidad_rec(p_contador,p_registros,p_hora)
DEFINE p_contador INTEGER,
       p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss          LIKE prt_solicitud_cedente.nss,
          v_curp         LIKE prt_solicitud_cedente.curp,
          v_credito      LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_f_marca      LIKE sfr_marca_historica.f_inicio,
          v_folio_tramite  LIKE bus_tramite.folio_procesar,
          v_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
          v_aivs_recibido  LIKE prt_traspaso_receptora.mto_aivs_fov2008,
          v_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
          v_fecha_proceso  LIKE prt_preliquida.f_liquida,
          v_pesos_dispersion LIKE cta_movimiento.monto_pesos,
          v_f_liquida_disp LIKE cta_movimiento.f_liquida
       END RECORD,
       v_total_sdo_insoluto   LIKE prt_solicitud_receptora.saldo_insoluto_credito_infonavit,
       v_total_pesos_recibido LIKE prt_traspaso_receptora.mto_pesos_fov2008,
       v_total_pesos_dispersion LIKE cta_movimiento.monto_pesos,
       p_hora         DATETIME HOUR TO SECOND,
       v_pagina       SMALLINT,
       v_fecha_actual CHAR(10),
       v_titulo       STRING,
       v_total_registros INTEGER
      
   FORMAT

      FIRST PAGE HEADER
         LET v_titulo = "REPORTE DE REGISTROS PORTABILIDAD RECEPTORA"
         LET v_fecha_actual = TODAY USING "dd/mm/yyyy"
         LET v_total_registros = COUNT(*)
         PRINTX v_titulo,
                p_usuario,
                v_fecha_actual,
                p_hora,
                v_total_registros

      ON EVERY ROW
         PRINTX p_contador,
                p_registros.v_id_solicitud,
                p_registros.v_nss,
                p_registros.v_curp,
                p_registros.v_credito,
                p_registros.v_f_marca USING "dd/mm/yyyy",
                p_registros.v_folio_tramite,
                p_registros.v_sdo_insoluto,
                p_registros.v_pesos_recibido,
                p_registros.v_fecha_proceso USING "dd/mm/yyyy",
                p_registros.v_pesos_dispersion,
                p_registros.v_f_liquida_disp USING "dd/mm/yyyy"

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         LET v_total_sdo_insoluto   = SUM(p_registros.v_sdo_insoluto)
         LET v_total_pesos_recibido = SUM(p_registros.v_pesos_recibido)
         LET v_total_pesos_dispersion = SUM(p_registros.v_pesos_dispersion)
         PRINTX v_total_sdo_insoluto,
                v_total_pesos_recibido,
                v_total_pesos_dispersion
      
END REPORT