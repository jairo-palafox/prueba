--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 26/03/2015
--==============================================================================

################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTI03                                                   #
#Objetivo          => Genera reporte de preliquidación traspasos receptora     #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 23 Marzo 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

DEFINE p_pid                  LIKE bat_ctr_proceso.pid,     
       p_proceso_cod          LIKE cat_proceso.proceso_cod, 
       p_opera_cod LIKE cat_operacion.opera_cod, 
       p_usuario_cod          LIKE seg_usuario.usuario_cod, 
       p_folio                LIKE glo_folio.folio,
       p_lanzador             LIKE glo_ctr_archivo.nombre_archivo,
       r_ruta_lst             LIKE seg_modulo.ruta_listados,
       r_ruta_ejecutable      LIKE seg_modulo.ruta_bin,
       v_nom_reporte          STRING,
       v_manejador_rpt        OM.SaxDocumentHandler

CONSTANT C_ESTADO_INTEG_TRASPASO_RECEP SMALLINT = 10
CONSTANT C_ESTADO_PRELI_TRASPASO_RECEP SMALLINT = 20

MAIN
   LET p_usuario_cod = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_folio       = ARG_VAL(5)
   LET p_lanzador    = ARG_VAL(6)

   CALL fn_rutas("prt") RETURNING r_ruta_ejecutable, 
                                  r_ruta_lst

   --CALL fn_inicializa_consulta()
   CALL fn_recupera_registros()
   
END MAIN

FUNCTION fn_inicializa_consulta()
DEFINE v_consulta STRING

   {LET v_consulta = " SELECT COUNT(*)",
                    "   FROM prt_traspaso_receptora",
                    "  WHERE folio_liquida = ?",
                    "    AND estado = ?"
   PREPARE prp_recupera_conteo_preliquidacion FROM v_consulta}

END FUNCTION

# Descripción: Recupera información de preliquidación traspasos receptora
FUNCTION fn_recupera_registros()
DEFINE v_consulta         STRING,
       v_traspasos RECORD
         v_nss             LIKE afi_derechohabiente.nss,
         v_movimiento      LIKE cat_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_monto_pesos     LIKE prt_preliquida.monto_pesos,
         v_monto_aivs      LIKE prt_preliquida.monto_acciones
       END RECORD
       
   # CREACIÓN DE REPORTE
   IF(fgl_report_loadCurrentSettings(r_ruta_ejecutable CLIPPED||"/PRTI031.4rp"))THEN
      # Salida del reporte
      CALL fgl_report_selectDevice("PDF")
      LET v_nom_reporte = p_usuario_cod CLIPPED,"-",
                          p_lanzador    CLIPPED,"-",
                          p_pid         USING "&&&&&", "-", 
                          p_proceso_cod USING "&&&&&", "-", 
                          p_opera_cod   USING "&&&&&"
                          
      # ruta de salida del reporte
      CALL fgl_report_setOutputFileName(r_ruta_lst CLIPPED||"/"||v_nom_reporte)
         
      # Indica que no hay previsualizacion
      CALL fgl_report_selectPreview(0)
      
      # se asigna la configuración en el menejo del reporte
      LET v_manejador_rpt = fgl_report_commitCurrentSettings()
      START REPORT fn_rpt_preliquidacion_trasp_recept TO XML HANDLER v_manejador_rpt

         {LET v_consulta = " SELECT nss,",
                          "        mto_pesos_fov2008,",
                          "        sdo_insoluto_infonavit",
                          "   FROM prt_traspaso_receptora",
                          "  WHERE folio_liquida = ?",
                          "    AND estado = ?"}

         LET v_consulta = " SELECT afi.nss,",
                          "        pre.movimiento,",
                          "        mov.movimiento_desc,",
                          "        pre.monto_pesos,",
                          "        pre.monto_acciones",
                          "   FROM prt_preliquida pre JOIN afi_derechohabiente afi",
                          "     ON pre.id_derechohabiente = afi.id_derechohabiente",
                          "        LEFT OUTER JOIN cat_movimiento mov",
                          "     ON pre.movimiento = mov.movimiento",
                          "  WHERE pre.folio_liquida = ?"
         PREPARE prp_recupera_reg FROM v_consulta
         DECLARE cur_recupera_reg CURSOR FOR prp_recupera_reg
         FOREACH cur_recupera_reg USING p_folio
                                        --C_ESTADO_PRELI_TRASPASO_RECEP
                                   INTO v_traspasos.*
            OUTPUT TO REPORT fn_rpt_preliquidacion_trasp_recept(p_folio,v_traspasos.*)
         END FOREACH
         FREE cur_recupera_reg
         
      FINISH REPORT fn_rpt_preliquidacion_trasp_recept
      
   ELSE
      DISPLAY "No fue posible generar el reporte"
   END IF

END FUNCTION

# Descripción: Genera reporte de preliquidación
REPORT fn_rpt_preliquidacion_trasp_recept(p_folio,
                                          p_traspasos)
DEFINE p_folio     LIKE glo_folio.folio,
       p_traspasos RECORD
         v_nss             LIKE afi_derechohabiente.nss,
         v_movimiento      LIKE cat_movimiento.movimiento,
         v_movimiento_desc LIKE cat_movimiento.movimiento_desc,
         v_monto_pesos     LIKE prt_preliquida.monto_pesos,
         v_monto_aivs      LIKE prt_preliquida.monto_acciones
       END RECORD,
       v_total_pesos       LIKE prt_preliquida.monto_pesos,
       v_total_aivs        LIKE prt_preliquida.monto_acciones,
       v_pagina                 SMALLINT,
       v_fecha_actual           DATE,
       v_total_procesados       INTEGER,
       v_total_no_preliquidados INTEGER,
       v_total_preliquidados    INTEGER,
       v_titulo_rpt             STRING,
       v_tabla                  STRING,
       v_consulta               STRING
       
   FORMAT

      FIRST PAGE HEADER
         CASE p_proceso_cod
            WHEN C_PROCESO_COD_TRANS_SDO_RECEPTORA
               LET v_tabla = "   FROM prt_traspaso_receptora"
               LET v_titulo_rpt = "REPORTE PRELIQUIDACIÓN DE TRASPASOS DE SALDOS RECEPTORA"
               
            WHEN C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA
               LET v_tabla = "   FROM prt_traspaso_receptora"
               LET v_titulo_rpt = "REPORTE PRELIQUIDACIÓN DE TRASPASOS SUBSECUENTES RECEPTORA"
               
            WHEN C_PROCESO_COD_DEV_SDO_RECEPTORA
               LET v_tabla = "   FROM prt_traspaso_receptora"
               LET v_titulo_rpt = "REPORTE PRELIQUIDACIÓN DE DEVOLUCIÓN DE SALDOS RECEPTORA"

            WHEN C_PROCESO_COD_DEV_SDO_CEDENTE
               LET v_tabla = "   FROM prt_traspaso_cedente"
               LET v_titulo_rpt = "REPORTE PRELIQUIDACIÓN DE DEVOLUCIÓN DE SALDOS CEDENTE"
               
         END CASE
         LET v_consulta = " SELECT COUNT(*)",
                          v_tabla,
                          "  WHERE folio_liquida = ?",
                          "    AND estado = ?"
         PREPARE prp_recupera_conteo_preliquidacion FROM v_consulta
         EXECUTE prp_recupera_conteo_preliquidacion USING p_folio,
                                                          C_ESTADO_TRASPASO_PRELIQUIDADA
                                                     INTO v_total_preliquidados

         EXECUTE prp_recupera_conteo_preliquidacion USING p_folio,
                                                          C_ESTADO_TRASPASO_INTEGRADA
                                                     INTO v_total_no_preliquidados
         LET v_total_procesados = v_total_preliquidados + v_total_no_preliquidados

         LET v_fecha_actual = TODAY
         PRINTX v_titulo_rpt,
                v_fecha_actual USING "dd-mm-yyyy",
                p_folio,
                v_total_preliquidados,
                v_total_procesados

      BEFORE GROUP OF p_traspasos.v_movimiento
         LET v_total_pesos = SUM(p_traspasos.v_monto_pesos)
         LET v_total_aivs  = SUM(p_traspasos.v_monto_aivs)
         PRINTX p_traspasos.v_movimiento_desc,
                v_total_pesos,
                v_total_aivs
         
      ON EVERY ROW
         PRINTX p_traspasos.*
         
      PAGE TRAILER
         LET v_pagina = PAGENO 
         PRINTX v_pagina

END REPORT
