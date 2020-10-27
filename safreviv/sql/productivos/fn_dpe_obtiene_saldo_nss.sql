






CREATE FUNCTION "safreviv".fn_dpe_obtiene_saldo_nss(p_id_dpe_referencia DECIMAL(9,0), p_folio DECIMAL(9,0), 
      p_reg_patronal_imss CHAR(11),p_periodo_pago CHAR(6) )
   RETURNING DECIMAL(16,6), DECIMAL(16,6), SMALLINT,CHAR(200)
 
 DEFINE v_detalle_imp_viv_dev     DECIMAL(16,6);
 DEFINE v_detalle_avis_viv_dev    DECIMAL(16,6);
 DEFINE v_si_pendiente_preliquida SMALLINT;
 DEFINE v_c_cadena                CHAR(200);
 
  -- Tipos status de pagos
 DEFINE c_pago_total_nss                SMALLINT;
 DEFINE c_pago_parcial_nss              SMALLINT;
 DEFINE c_pago_por_preliquidar_total    SMALLINT;
 DEFINE c_pago_por_preliquidar_parcial  SMALLINT;
 DEFINE c_pago_preliquidado_total       SMALLINT;
 DEFINE c_pago_preliquidado_parcial     SMALLINT;
 DEFINE c_pago_liquidado_total          SMALLINT;
 DEFINE c_pago_liquidado_parcial        SMALLINT;
 DEFINE c_pago_enviado_procesar_total   SMALLINT;
 DEFINE c_pago_enviado_procesar_parcial SMALLINT;
 
   -- Constantes de estatus de diagnostico y de pagos
   LET c_pago_total_nss                = 0;
   LET c_pago_parcial_nss              = 1;
   LET c_pago_por_preliquidar_total    = 2;
   LET c_pago_por_preliquidar_parcial  = 3;
   LET c_pago_preliquidado_total       = 4;
   LET c_pago_preliquidado_parcial     = 5;
   LET c_pago_liquidado_total          = 6;
   LET c_pago_liquidado_parcial        = 7;
   LET c_pago_enviado_procesar_total   = 8;
   LET c_pago_enviado_procesar_parcial = 9;
   
   LET v_si_pendiente_preliquida = 0;
   
   -- Inicializa totales
   LET v_detalle_imp_viv_dev  = 0;
   LET v_detalle_avis_viv_dev = 0;
 
   LET v_c_cadena ="verifica si es un pago con pago pendiente de liquidar";
   -- verfica si cuenta con pagos pendientes de liquidar.
   --  ya que en ese caso NO se considera este registro
   --  para generarle un pago parcial hasta que se aplique este cargo.
   SELECT COUNT(*)
     INTO v_si_pendiente_preliquida
     FROM dpe_sol_trab_parcial
    WHERE id_dpe_referencia = p_id_dpe_referencia
      AND folio             = p_folio
      AND reg_patronal_imss = p_reg_patronal_imss
      AND periodo_pago      = p_periodo_pago
      AND diagnostico       = c_pago_por_preliquidar_parcial;
   
   IF(v_si_pendiente_preliquida > 0)THEN
      
      LET v_c_cadena ="se encontro un pago pendiente de liquidar";
      
      -- NO se puede generar pago parcial de este nss
      --   hasta que se genere su liquidación
      
      RETURN v_detalle_imp_viv_dev, v_detalle_avis_viv_dev, 
             v_si_pendiente_preliquida, v_c_cadena;
   END IF

   LET v_c_cadena ="se obtiene los importe de saldos";
   SELECT SUM(imp_viv_dev), SUM(avis_viv_dev)
     INTO v_detalle_imp_viv_dev, v_detalle_avis_viv_dev
     FROM dpe_sol_trab_parcial
    WHERE id_dpe_referencia = p_id_dpe_referencia
      AND folio             = p_folio
      AND reg_patronal_imss = p_reg_patronal_imss
      AND periodo_pago      = p_periodo_pago
      AND diagnostico      <> c_pago_por_preliquidar_parcial;
   
   LET v_c_cadena =v_detalle_avis_viv_dev;
   
   RETURN v_detalle_imp_viv_dev, v_detalle_avis_viv_dev, v_si_pendiente_preliquida, v_c_cadena;
END FUNCTION -- fn_dpe_obtiene_saldo_nss
;


