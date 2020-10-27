






CREATE PROCEDURE "safreviv".sp_dis_rev_compensa_avance(p_folio   DECIMAL(9,0))

--Última modificación 05112018
--Declaración de variables
DEFINE v_monot_apo           DECIMAL(12,2);
DEFINE v_monto_amo           DECIMAL(12,2);
DEFINE v_id_detalle          INTEGER;
      
  FOREACH
    SELECT id_dis_det_avance_pago, monto_apo_avance, monto_amo_avance
      INTO v_id_detalle, v_monot_apo, v_monto_amo
      FROM dis_compensa_avance
     WHERE folio_dis = p_folio

    UPDATE dis_det_avance_pago
       SET estado                 = 30,
           monto_dif_apo          = v_monot_apo,
           monto_dif_amo          = v_monto_amo
     WHERE id_dis_det_avance_pago = v_id_detalle;

  END FOREACH;

  DELETE FROM dis_compensa_avance
  WHERE folio_dis        = p_folio;

  DELETE FROM dis_interface_hs
  WHERE folio_liquida    = p_folio;

  {DELETE FROM dis_interface_ef
  WHERE folio_liquida    = p_folio;}

  DELETE FROM dis_interface_ef_ad
  WHERE folio_liquida    = p_folio;

  DELETE FROM dse_devolucion
  WHERE folio_referencia = p_folio;

  --DELETE FROM dis_amortizacion_real;
  
  DELETE FROM dis_his_transaccion
  WHERE folio_liquida    = p_folio;

  DELETE FROM dis_his_hs
  WHERE folio            = p_folio; 

  DELETE FROM dis_info_inconsistente
  WHERE folio_liquida    = p_folio;
  
  -- Noviembre 2014
  -- PRODINF-434 (PROINFXVII-29)
  -- NRP's 99 
  -- Proceso: 1403 - REG PAGOS SOLO INFONAVIT (PAU) 
  DELETE FROM dis_cta_ind_pau
  WHERE folio_liquida = p_folio;
    
  -- Junio 2015
  -- Reverso Migración Aportaciones Subsecuentes a SACI (Créditos 43 BIS) 
  DELETE FROM dis_ctr_aps_tns
  WHERE folio_liquida    = p_folio;

  DELETE FROM dis_crd_tramite
  WHERE folio_liquida = p_folio;

  DELETE FROM dis_crd_ceros
  WHERE folio_liquida = p_folio;

END PROCEDURE;


