----------------------------------------------------------------------
-- Modulo        => TIA
-- Componente    => TIAS00.4gl
-- Funcionalidad => Genera reporte de carga inicial del historico
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 26-jun-2015
----------------------------------------------------------------------



FUNCTION fn_lanza_reporte(v_reporte, v_salida)
  DEFINE
    v_salida                 STRING,
    v_reporte                STRING,
    v_preview                SMALLINT,
    v_handler                om.SaxDocumentHandler
  DEFINE v_cve_mov           CHAR(2)
  DEFINE v_ind_consiste      SMALLINT
  DEFINE v_total             INTEGER
  DEFINE v_saldo             DECIMAL(17,2)

  LET g_nombre_reporte    = g_usuario_cod CLIPPED, "-MIGL38-",
                            g_pid USING "&&&&&", "-",
                            g_proceso_cod USING "&&&&&", "-",
                            g_opera_cod USING "&&&&&",
                            ".pdf"

  LET g_ruta_reporte      = g_seg_modulo.ruta_listados CLIPPED, "/",
                            g_nombre_reporte CLIPPED

  LET v_preview           = FALSE

  INITIALIZE v_handler TO NULL

  IF v_reporte IS NOT NULL AND
     v_salida  IS NOT NULL THEN
     LET v_handler = fn_configuracion(v_reporte, v_salida, v_preview)
     START REPORT rep_dh_carga TO XML HANDLER v_handler

         DECLARE cur_reporte CURSOR FOR SELECT cve_mov_realizado, ind_consistencia,
                                               COUNT(*), SUM(saldo_inf_pesos)
                                          FROM safre_viv:cta_bd92_decreto
                                         GROUP BY 1,2
                                         ORDER BY 1,2

         FOREACH cur_reporte INTO v_cve_mov, v_ind_consiste, v_total, v_saldo
            OUTPUT TO REPORT rep_dh_carga(v_cve_mov, v_ind_consiste, v_total, v_saldo)
         END FOREACH
     
     FINISH REPORT rep_dh_carga
  END IF
END FUNCTION

FUNCTION fn_configuracion(v_reporte, v_formato, v_preview)
  DEFINE
    v_reporte                STRING,
    v_formato                STRING,
    v_preview                INTEGER

  -- CARGAR EL ARCHIVO 4rp
  IF NOT fgl_report_loadCurrentSettings(v_reporte) THEN
     RETURN NULL
  END IF

  CALL fgl_report_selectDevice(v_formato)
  CALL fgl_report_selectPreview(v_preview)
  CALL fgl_report_setOutputFileName(g_ruta_reporte)
  -- CALL fgl_report_setSharePortWithGDC(false)

  -- use the report
  RETURN fgl_report_commitCurrentSettings()

END FUNCTION

REPORT rep_dh_carga (p_cve_mov, p_ind_consiste, p_total, p_saldo)

   DEFINE p_cve_mov           CHAR(2)
   DEFINE p_ind_consiste      SMALLINT
   DEFINE p_total             INTEGER
   DEFINE p_saldo             DECIMAL(17,2)

   DEFINE tot_detalle_bd      DECIMAL(9,0)
   DEFINE tot_detalle_mov     DECIMAL(9,0)

   DEFINE v_saldo_bd          DECIMAL(22,6)
   DEFINE v_saldo_mov         DECIMAL(22,6)

   DEFINE tot_detalle_990     DECIMAL(9,0)
   DEFINE v_saldo_bd_990      DECIMAL(22,6)

   DEFINE tot_traspaso        DECIMAL(9,0)
   DEFINE sdo_traspaso        DECIMAL(22,6)
   DEFINE tot_retiro          DECIMAL(9,0)
   DEFINE sdo_retiro          DECIMAL(22,6)
   DEFINE tot_disponible      DECIMAL(9,0)
   DEFINE sdo_disponible      DECIMAL(22,6)

   DEFINE tot_traspaso_0      DECIMAL(9,0)
   DEFINE sdo_traspaso_0      DECIMAL(22,6)
   DEFINE tot_traspaso_1      DECIMAL(9,0)
   DEFINE sdo_traspaso_1      DECIMAL(22,6)

   DEFINE tot_retiro_0        DECIMAL(9,0)
   DEFINE sdo_retiro_0        DECIMAL(22,6)
   DEFINE tot_retiro_1        DECIMAL(9,0)
   DEFINE sdo_retiro_1        DECIMAL(22,6)

   DEFINE tot_disponible_0    DECIMAL(9,0)
   DEFINE sdo_disponible_0    DECIMAL(22,6)
   DEFINE tot_disponible_1    DECIMAL(9,0)
   DEFINE sdo_disponible_1    DECIMAL(22,6)
  
 FORMAT

  FIRST PAGE HEADER

   SELECT COUNT(*), SUM(saldo_inf_pesos)
     INTO tot_detalle_bd, v_saldo_bd
     FROM safre_viv:cta_bd92_decreto

   SELECT COUNT(*), SUM(monto_pesos)
     INTO tot_detalle_mov, v_saldo_mov
     FROM safre_viv:cta_decreto
    WHERE folio_liquida = r_folio_lote

   SELECT COUNT(*), SUM(saldo_inf_pesos)
     INTO tot_detalle_990, v_saldo_bd_990
     FROM safre_viv:cta_bd92_decreto
    WHERE clave_icefa = '990'
      AND cve_mov_realizado = '01'
    
   PRINTX g_fecha_apertura USING "dd-mm-yyyy"
   PRINTX g_usuario_cod
   PRINTX r_folio_lote

   PRINTX tot_detalle_bd
   PRINTX tot_detalle_mov
   PRINTX tot_detalle_990
   PRINTX v_saldo_mov
   PRINTX v_saldo_bd
   PRINTX v_saldo_bd_990

  ON EVERY ROW
   CASE p_cve_mov
      WHEN "00"
         CASE p_ind_consiste
            WHEN 0
               LET tot_disponible_0 = p_total   
               LET sdo_disponible_0 = p_saldo
            WHEN 1
               LET tot_disponible_1 = p_total   
               LET sdo_disponible_1 = p_saldo
         END CASE
      WHEN "01"
         DISPLAY "TRASPASO"
         CASE p_ind_consiste
            WHEN 0
               DISPLAY "CONSISTE 0"
               LET tot_traspaso_0 = p_total   
               LET sdo_traspaso_0 = p_saldo
            WHEN 1
               DISPLAY "CONSISTE 1"
               LET tot_traspaso_1 = p_total   
               LET sdo_traspaso_1 = p_saldo
         END CASE
      WHEN "02"
         CASE p_ind_consiste
            WHEN 0
               LET tot_retiro_0 = p_total   
               LET sdo_retiro_0 = p_saldo
            WHEN 1
               LET tot_retiro_1 = p_total   
               LET sdo_retiro_1 = p_saldo
         END CASE
   END CASE
   
   
  ON LAST ROW 
   LET tot_traspaso = tot_traspaso_0 + tot_traspaso_1 
   LET sdo_traspaso = sdo_traspaso_0 + sdo_traspaso_1
   LET tot_retiro = tot_retiro_0 + tot_retiro_1   
   LET sdo_retiro = sdo_retiro_0 + sdo_retiro_1
   LET tot_disponible = tot_disponible_0 + tot_disponible_1   
   LET sdo_disponible = sdo_disponible_0 + sdo_disponible_1 
   
   PRINTX tot_traspaso     
   PRINTX sdo_traspaso     
   PRINTX tot_retiro       
   PRINTX sdo_retiro       
   PRINTX tot_disponible   
   PRINTX sdo_disponible   

   PRINTX tot_traspaso_0   
   PRINTX sdo_traspaso_0   
   PRINTX tot_traspaso_1   
   PRINTX sdo_traspaso_1   

   PRINTX tot_retiro_0     
   PRINTX sdo_retiro_0     
   PRINTX tot_retiro_1     
   PRINTX sdo_retiro_1     

   PRINTX tot_disponible_0 
   PRINTX sdo_disponible_0 
   PRINTX tot_disponible_1 
   PRINTX sdo_disponible_1 
END REPORT

