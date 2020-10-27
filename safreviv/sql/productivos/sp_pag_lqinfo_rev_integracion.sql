






CREATE PROCEDURE "safreviv".sp_pag_lqinfo_rev_integracion(p_folio DECIMAL(9,0))

  -- se borran los datos de las tablas historicas de LQINFO
  DELETE FROM pag_cza_recauda      WHERE folio = p_folio;
  DELETE FROM pag_cza_ent_recauda  WHERE folio = p_folio;
  DELETE FROM pag_cza_pag_patronal WHERE folio = p_folio;
  DELETE FROM pag_sum_pag_patronal WHERE folio = p_folio;
  DELETE FROM pag_sum_ent_recauda  WHERE folio = p_folio;
  DELETE FROM pag_sum_recauda      WHERE folio = p_folio;
  DELETE FROM cta_his_pagos        WHERE folio = p_folio;
  DELETE FROM cta_pag_complemento  WHERE folio = p_folio;

  -- se actualiza el estatus del archivo cargado
  UPDATE glo_folio
     SET status = -1
   WHERE folio = p_folio;
  
END PROCEDURE;


