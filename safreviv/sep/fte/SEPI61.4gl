--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-10-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI61                                                   #
#Objetivo          => Reporte de preliquidación de expedientes sólo infonavit  #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 10 Octubre 2012                                          #
################################################################################

DATABASE safre_viv

DEFINE p_usuario_cod     LIKE seg_usuario.usuario_cod, # Usuario que realiza la integracion
       p_pid             LIKE bat_ctr_proceso.pid,     # identificador de proceso
       p_proceso_cod     LIKE cat_proceso.proceso_cod, # Código del proceso
       p_opera_cod       LIKE cat_operacion.opera_cod, # Código de la operacion
       p_folio           INTEGER,                      # numero de folio
       p_nom_archivo     LIKE glo_ctr_archivo.nombre_archivo, # nombre del archivo a integrar
       r_ruta_lst        LIKE seg_modulo.ruta_listados,
       r_ruta_ejecutable LIKE seg_modulo.ruta_bin,
       v_nom_reporte     STRING,
       v_manejador_rpt   OM.SaxDocumentHandler

MAIN
DEFINE v_consulta STRING

   # Se recuperan los parámetros
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_nom_archivo = ARG_VAL(6)

   CALL fn_recupera_expedientes_preliquidados()
END MAIN

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI61                                                   #
#Descripcion       => Recupera expedientes preliquidados                       #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 16 Octubre 2012                                          #
################################################################################
FUNCTION fn_recupera_expedientes_preliquidados()
DEFINE v_fecha_actual STRING,
       v_consulta     STRING,
       v_encabezado   RECORD
          v_fecha         STRING,
          v_folio         LIKE glo_folio.folio,
          v_total_parejas INTEGER
       END RECORD,
       v_detalle_rpt RECORD
          v_num           INTEGER,
          v_id_expediente LIKE sep_movimiento_invadido.id_expediente,
          v_invadido      CHAR(11),
          v_asociado      CHAR(11),
          v_subcuenta     SMALLINT,
          v_monto_pesos   DECIMAL(12,2),
          v_monto_aivs    DECIMAL(16,6)
       END RECORD,
       v_indice           INTEGER

   LET v_fecha_actual = YEAR(TODAY)
   LET v_fecha_actual = MONTH(TODAY) USING "&&","-" CLIPPED,
                        DAY(TODAY)   USING "&&","-" CLIPPED,
                        v_fecha_actual.trim()

   LET v_encabezado.v_fecha   = v_fecha_actual
   LET v_encabezado.v_folio   = p_folio

   SELECT COUNT(*)
     INTO v_encabezado.v_total_parejas
     FROM sep_preliquida_solo_infonavit
    WHERE folio_liquida = p_folio

   IF NOT(v_encabezado.v_total_parejas > 0)THEN
      DISPLAY "No se encontraron registros para generar reporte"
      EXIT PROGRAM
   END IF

   # Construye reporte
   CALL fn_rutas("sep") RETURNING r_ruta_ejecutable, r_ruta_lst
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/SEPI611.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED, 
                          "-SEPP61-", 
                          p_pid         USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod   USING "&&&&&"

      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      LET v_consulta = "\n SELECT UNIQUE prq.id_referencia,", -- id_expediente
                       "\n        mov.invadido,",
                       "\n        mov.asociado,",
                       "\n        prq.subcuenta,",
                       "\n        abs(prq.monto_pesos),",
                       "\n        abs(prq.monto_acciones)",
                       "\n   FROM sep_preliquida_solo_infonavit prq ",
                       "\n        JOIN sep_movimiento_invadido mov",
                       "\n     ON prq.id_referencia = mov.id_expediente",
                       "\n  WHERE prq.folio_liquida = ?"
      PREPARE prp_rec_expedientes_preliquidados FROM v_consulta
      DECLARE cur_rec_expedientes_preliquidados CURSOR FOR prp_rec_expedientes_preliquidados
      LET v_indice = 1
      START REPORT fn_genera_rpt_preliquidacion_exp_solo_infonavit TO XML HANDLER v_manejador_rpt
         FOREACH cur_rec_expedientes_preliquidados USING p_folio
                                                    INTO v_detalle_rpt.v_id_expediente,
                                                         v_detalle_rpt.v_invadido,
                                                         v_detalle_rpt.v_asociado,
                                                         v_detalle_rpt.v_subcuenta,
                                                         v_detalle_rpt.v_monto_pesos,
                                                         v_detalle_rpt.v_monto_aivs
            LET v_detalle_rpt.v_num = v_indice
            OUTPUT TO REPORT fn_genera_rpt_preliquidacion_exp_solo_infonavit(v_encabezado.*,
                                                                             v_detalle_rpt.*)
            LET v_indice = v_indice + 1

         END FOREACH
         FREE cur_rec_expedientes_preliquidados
      FINISH REPORT fn_genera_rpt_preliquidacion_exp_solo_infonavit
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF
     
END FUNCTION

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPI61                                                   #
#Descripcion       => Imprime datos en reporte                                 #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 16 Octubre 2012                                          #
################################################################################
REPORT fn_genera_rpt_preliquidacion_exp_solo_infonavit(v_encabezado,v_detalle_rpt)
DEFINE v_pagina SMALLINT,
       v_encabezado   RECORD
          v_fecha         STRING,
          v_folio         LIKE glo_folio.folio,
          v_total_parejas INTEGER
       END RECORD,
       v_detalle_rpt RECORD
          v_num           INTEGER,
          v_id_expediente LIKE sep_movimiento_invadido.id_expediente,
          v_invadido      CHAR(11),
          v_asociado      CHAR(11),
          v_subcuenta     SMALLINT,
          v_monto_pesos   DECIMAL(12,2),
          v_monto_aivs    DECIMAL(16,6)
       END RECORD,
       v_total_aivs       DECIMAL(16,6),
       v_total_pesos      DECIMAL(12,2)

   FORMAT

      FIRST PAGE HEADER
         PRINTX v_encabezado.*

      ON EVERY ROW  
         PRINTX v_detalle_rpt.*

      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

      ON LAST ROW
         LET v_total_aivs = SUM(v_detalle_rpt.v_monto_aivs)
         LET v_total_pesos = SUM(v_detalle_rpt.v_monto_pesos)
         PRINTX v_total_aivs
         PRINTX v_total_pesos

END REPORT