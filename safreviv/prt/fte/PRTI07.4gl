--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16/07/2015
--==============================================================================

################################################################################
#Módulo          => PRT                                                        #
#Programa        => PRTI07                                                     #
#Objetivo        => Consulta subsecuentes                                      #
#Fecha Inicio    => 16 Julio 2015                                              #
################################################################################
SCHEMA "safre_viv"

DEFINE p_usuario         VARCHAR(20),--LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid             LIKE bat_ctr_proceso.pid, # identificador de proceso
       p_proceso_cod     LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod       LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio           LIKE glo_ctr_archivo.folio, # número de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       p_flujo           SMALLINT,
       g_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       g_ruta_listados   LIKE seg_modulo.ruta_listados,
       g_ruta_envio      LIKE seg_modulo.ruta_envio,
       g_nom_rpt_txt     STRING
       
MAIN

   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)
   LET p_flujo       = ARG_VAL(7)

   CONNECT TO "safre_viv"
   CALL fn_inicializa_consultas()
   IF( p_flujo = 1 )THEN
      CALL fn_genera_reportes_cedente()
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
                    "        movimiento,",
                    "        pesos,",
                    "        fecha_proceso",
                    "   FROM safre_tmp:prt_tmp_con_trp_prt_subsec_ced",
                    "  WHERE 1 = 1"
   PREPARE prp_recupera_registros_ced FROM v_consulta

   LET v_consulta = " SELECT id_solicitud,",
                    "        nss,",
                    "        curp,",
                    "        credito,",
                    "        movimiento,",
                    "        pesos,",
                    "        fecha_proceso",
                    "   FROM safre_tmp:prt_tmp_con_trp_prt_subsec_rec",
                    "  WHERE 1 = 1"
   PREPARE prp_recupera_registros_rec FROM v_consulta

END FUNCTION 

# Descripción: Especifica parametros y construcción de reporte
FUNCTION fn_genera_reportes_cedente()
DEFINE p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          --v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       v_total_pesos   LIKE prt_solicitud_cedente.pesos_viv97_cedido,
       v_nom_reporte   STRING,       
       v_manejador_rpt OM.SaxDocumentHandler,
       r_resultado_opera INTEGER,
       v_proceso_cod     LIKE cat_proceso.proceso_cod,
       v_proceso_desc    LIKE cat_proceso.proceso_desc,
       v_opera_cod       LIKE cat_operacion.opera_cod,
       v_opera_desc      LIKE cat_operacion.opera_desc,
       v_hora            DATETIME HOUR TO SECOND,
       v_bnd_reallizado  BOOLEAN,
       v_pid_txt         STRING,
       v_canal           BASE.Channel,
       v_registro        STRING,
       v_contador        INTEGER

   LET v_bnd_reallizado = FALSE
   
   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTI071.4rp") )THEN
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

      
      EXECUTE prp_recupera_proceso USING p_proceso_cod
                                    INTO v_proceso_desc
                                          
      EXECUTE prp_recupera_operacion USING p_proceso_cod,
                                           p_opera_cod
                                      INTO v_opera_desc

      DISPLAY "GENERACIÓN DE REPORTES MOVIMIENTOS SUBSECUENTES CEDENTE PORTABILIDAD"
      --DISPLAY "PROCESO:   ",v_proceso_desc
      --DISPLAY "OPERACIÓN: ",v_opera_desc
      DISPLAY "USUARIO:   ",p_usuario
      DISPLAY "FECHA:     ",TODAY USING "dd/mm/yyyy"
      DISPLAY "HORA:      ",CURRENT HOUR TO SECOND
      DISPLAY ""

      LET v_contador = 0
      LET v_total_pesos = 0
      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("|")
      CALL v_canal.openFile(g_nom_rpt_txt,"w")
      CALL v_canal.writeLine("NSS|"||
                             "CURP|"||
                             "No. Crédito Fovissste|"||
                             "Movimiento|"||
                             "PESOS|"||
                             "Fecha proceso|")
      DECLARE cur_recupera_registros_ced CURSOR FOR prp_recupera_registros_ced      
      START REPORT fn_rpt_consulta_portabilidad TO XML HANDLER v_manejador_rpt         
         FOREACH cur_recupera_registros_ced INTO p_registros.*
            LET v_contador = v_contador + 1
            OUTPUT TO REPORT fn_rpt_consulta_portabilidad(v_contador,p_registros.*,v_hora)
            LET v_registro = p_registros.v_nss,"|",
                             p_registros.v_curp,"|",
                             p_registros.v_credito,"|",
                             p_registros.v_movimiento,"|",
                             p_registros.v_pesos,"|",                             
                             p_registros.v_fecha_proceso USING "dd/mm/yyyy","|"
                                   
            CALL v_canal.writeLine(v_registro)
            IF( p_registros.v_pesos IS NOT NULL )THEN LET v_total_pesos = v_total_pesos + p_registros.v_pesos END IF
         END FOREACH
      FINISH REPORT fn_rpt_consulta_portabilidad

      LET v_registro = "TOTALES|",
                       v_contador,"|",
                       "|",
                       "|",
                       v_total_pesos,"|",                             
                       "|"
      CALL v_canal.writeLine(v_registro)
      
      FREE cur_recupera_registros_ced
      CALL v_canal.close()
            
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         DISPLAY "REPORTE DE REGISTROS SUBSECUENTES CEDENTE"
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
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          --v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       v_total_pesos     LIKE prt_solicitud_cedente.pesos_viv97_cedido,
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
   
   IF( fgl_report_loadCurrentSettings(g_ruta_ejecutable CLIPPED ||"/PRTI071.4rp") )THEN
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
      DISPLAY "GENERACIÓN DE REPORTES MOVIMIENTOS SUBSECUENTES RECEPTORA PORTABILIDAD"
      DISPLAY "USUARIO:   ",p_usuario
      DISPLAY "FECHA:     ",TODAY USING "dd/mm/yyyy"
      DISPLAY "HORA:      ",v_hora
      DISPLAY ""

      LET v_contador = 0
      LET v_total_pesos = 0
      LET v_canal = base.Channel.create()
      CALL v_canal.setDelimiter("|")
      CALL v_canal.openFile(g_nom_rpt_txt,"w")
      CALL v_canal.writeLine("NSS|"||
                             "CURP|"||
                             "No. Crédito Infonavit|"||
                             "Movimiento|"||
                             "PESOS recibidos|"||                             
                             "Fecha proceso|")
      DECLARE cur_recupera_registros_rec CURSOR FOR prp_recupera_registros_rec      
      START REPORT fn_rpt_consulta_portabilidad TO XML HANDLER v_manejador_rpt         
         FOREACH cur_recupera_registros_rec INTO p_registros.*
            LET v_contador = v_contador + 1
            OUTPUT TO REPORT fn_rpt_consulta_portabilidad(v_contador,p_registros.*,v_hora)
            LET v_registro = p_registros.v_nss,"|",
                             p_registros.v_curp,"|",
                             p_registros.v_credito,"|",
                             p_registros.v_movimiento,"|",
                             p_registros.v_pesos,"|",
                             p_registros.v_fecha_proceso USING "dd/mm/yyyy","|"
            CALL v_canal.writeLine(v_registro)
            IF( p_registros.v_pesos IS NOT NULL )THEN LET v_total_pesos = v_total_pesos + p_registros.v_pesos END IF
         END FOREACH
      FINISH REPORT fn_rpt_consulta_portabilidad

      LET v_registro = "TOTALES|",
                       v_contador,"|",
                       "|",
                       "|",
                       v_total_pesos,"|",
                       "|"
      CALL v_canal.writeLine(v_registro)
            
      FREE cur_recupera_registros_rec
      CALL v_canal.close()
            
      CALL fn_actualiza_opera_fin(p_pid,
                                  p_proceso_cod,
                                  p_opera_cod) RETURNING r_resultado_opera
      IF( r_resultado_opera = 0 )THEN
         DISPLAY "REPORTE DE REGISTROS SUBSECUENTES RECEPTORA"
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
REPORT fn_rpt_consulta_portabilidad(p_contador,p_registros,p_hora)
DEFINE p_contador INTEGER,
       p_registros RECORD
          v_id_solicitud LIKE prt_solicitud_cedente.id_prt_solicitud_cedente,
          v_nss     LIKE prt_solicitud_cedente.nss,
          v_curp    LIKE prt_solicitud_cedente.curp,
          v_credito LIKE prt_solicitud_cedente.id_credito_fovissste,
          v_movimiento LIKE cat_movimiento.movimiento_desc,
          --v_aivs          LIKE prt_solicitud_cedente.aivs_viv97_cedido,
          v_pesos          LIKE prt_solicitud_cedente.pesos_viv97_cedido,
          v_fecha_proceso LIKE prt_preliquida.f_liquida
       END RECORD,
       v_total_pesos  LIKE prt_solicitud_cedente.pesos_viv97_cedido,
       p_hora         DATETIME HOUR TO SECOND,
       v_pagina       SMALLINT,
       v_fecha_actual CHAR(10),
       v_titulo       STRING,
       v_total_registros INTEGER,
       v_nom_credito  STRING,
       v_nom_pesos    STRING

   FORMAT

      FIRST PAGE HEADER
         IF( p_flujo = 1)THEN
            LET v_titulo = "REPORTE DE REGISTROS SUBSECUENTES PORTABILIDAD CEDENTE"
            LET v_nom_credito = "No. Crédito Fovissste"
            LET v_nom_pesos   = "PESOS transferidos a Fovissste"
         ELSE
            LET v_titulo = "REPORTE DE REGISTROS SUBSECUENTES PORTABILIDAD RECEPTORA"
            LET v_nom_credito = "No. Crédito Infonavit"
            LET v_nom_pesos   = "PESOS recibidos"
         END IF
         LET v_fecha_actual = TODAY USING "dd/mm/yyyy"
         LET p_hora = CURRENT HOUR TO SECOND
         LET v_total_registros = COUNT(*)
         PRINTX v_titulo,
                p_usuario,
                v_fecha_actual,
                p_hora,
                v_total_registros,
                v_nom_credito,
                v_nom_pesos

      ON EVERY ROW
         PRINTX p_contador,
                p_registros.v_id_solicitud,
                p_registros.v_nss,
                p_registros.v_curp,
                p_registros.v_credito,
                p_registros.v_movimiento,
                p_registros.v_pesos,
                p_registros.v_fecha_proceso USING "dd/mm/yyyy"

      PAGE TRAILER
         # imprime número de la página
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         LET v_total_pesos = SUM(p_registros.v_pesos)
         PRINTX v_total_pesos
         
END REPORT