






CREATE FUNCTION "safreviv".fn_reverso_negocio_dis(p_folio       DECIMAL(9,0),
                                       p_proceso_cod SMALLINT,
                                       p_usuario_cod VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100)
                                       
--Última modificación 05112018
--Declaración de variables
DEFINE v_resultado             SMALLINT;
DEFINE v_mensaje               VARCHAR(100);

DEFINE v_monto_apo             DECIMAL(12,2);
DEFINE v_monto_amo             DECIMAL(12,2);
DEFINE v_id_detalle            INTEGER;

SET PDQPRIORITY HIGH;

  -- RESPALDO
  --Avance de Pagos
  INSERT INTO safre_tmp:tmp_dis_det_avance_pago
  SELECT a.*
  FROM   dis_det_avance_pago a,
         dis_compensa_avance b
  WHERE  a.id_dis_det_avance_pago = b.id_dis_det_avance_pago
  AND    b.folio_dis              = p_folio;

  --Compensa Avance de Pagos 
  INSERT INTO safre_tmp:tmp_dis_compensa_avance
  SELECT b.*
  FROM   dis_det_avance_pago a,
         dis_compensa_avance b
  WHERE  a.id_dis_det_avance_pago = b.id_dis_det_avance_pago
  AND    b.folio_dis              = p_folio;

  --Interface Cartera
  INSERT INTO safre_tmp:tmp_dis_interface_hs
  SELECT *
  FROM   dis_interface_hs
  WHERE  folio_liquida = p_folio;

  --Interface Entidades Financieras
  INSERT INTO safre_tmp:tmp_dis_interface_ef
  SELECT *
  FROM   dis_interface_ef
  WHERE  folio_liquida = p_folio;

  --Interface Entidades Financieras (Adelanto)
  INSERT INTO safre_tmp:tmp_dis_interface_ef_ad
  SELECT *
  FROM   dis_interface_ef_ad    
  WHERE  folio_liquida = p_folio;

  --Cortes por Período
  INSERT INTO safre_tmp:tmp_dis_his_hs
  SELECT *
  FROM   dis_his_hs 
  WHERE  folio = p_folio;

  --Inconsistencias
  INSERT INTO safre_tmp:tmp_dis_info_inconsistente
  SELECT *
  FROM   dis_info_inconsistente
  WHERE  folio_liquida = p_folio;

  --Aportaciones Subsecuentes (Otorgamiento Créditos 43Bis)
  INSERT INTO safre_tmp:tmp_dis_ctr_aps_tns
  SELECT *
  FROM   dis_ctr_aps_tns
  WHERE  folio_liquida = p_folio;

  --Créditos en Trámite
  INSERT INTO safre_tmp:tmp_dis_crd_tramite
  SELECT *
  FROM   dis_crd_tramite
  WHERE  folio_liquida = p_folio;

  --Pagos PAU (Créditos Liquidados y Sin Crédito)
  INSERT INTO safre_tmp:tmp_dis_cta_ind_pau
  SELECT *
  FROM   dis_cta_ind_pau
  WHERE  folio_liquida = p_folio;

   --Número de Crédito en Ceros
  INSERT INTO safre_tmp:tmp_dis_crd_ceros
  SELECT *
  FROM   dis_crd_ceros
  WHERE  folio_liquida = p_folio;
 
  FOREACH
    SELECT id_dis_det_avance_pago, monto_apo_avance, monto_amo_avance
    INTO   v_id_detalle, v_monto_apo, v_monto_amo
    FROM   dis_compensa_avance
    WHERE  folio_dis = p_folio

    UPDATE dis_det_avance_pago
    SET    estado                 = 30,
           monto_dif_apo          = v_monto_apo,
           monto_dif_amo          = v_monto_amo
    WHERE  id_dis_det_avance_pago = v_id_detalle;
  END FOREACH;

  DELETE 
  FROM   dis_compensa_avance
  WHERE  folio_dis        = p_folio;

  DELETE 
  FROM   dis_interface_hs
  WHERE  folio_liquida    = p_folio;

  DELETE 
  FROM   dis_interface_ef
  WHERE  folio_liquida    = p_folio;

  DELETE 
  FROM   dis_interface_ef_ad
  WHERE  folio_liquida    = p_folio;

  {DELETE 
  FROM   dis_amortizacion_real;}
  
  DELETE 
  FROM   dis_his_transaccion
  WHERE  folio_liquida    = p_folio;

  DELETE 
  FROM   dis_his_hs
  WHERE  folio            = p_folio; 

  DELETE 
  FROM   dis_info_inconsistente
  WHERE  folio_liquida    = p_folio;
  
  -- NRP's 99 
  -- Operación: REG PAGOS SOLO INFONAVIT (PAU) 
  DELETE 
  FROM   dis_cta_ind_pau
  WHERE  folio_liquida    = p_folio;
    
  DELETE 
  FROM   dis_ctr_aps_tns
  WHERE  folio_liquida    = p_folio;

  DELETE 
  FROM   dis_crd_tramite
  WHERE  folio_liquida    = p_folio;

  DELETE 
  FROM   dis_crd_ceros
  WHERE  folio_liquida    = p_folio;

  LET v_resultado = 0;
  LET v_mensaje   = "El reverso de negocio se ejecuto correctamente";

  RETURN v_resultado, v_mensaje;

END FUNCTION;


