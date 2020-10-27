






CREATE PROCEDURE "safreviv".sp_pag_fc_rev_integracion(p_folio DECIMAL(9,0))

  -- se borran los datos de las tablas historicas de FC
  DELETE FROM pag_cza_fc  WHERE folio = p_folio;
  DELETE FROM pag_det_fc  WHERE folio = p_folio;
  DELETE FROM pag_sum_fc  WHERE folio = p_folio;
  
  -- se actualiza el estatus del archivo cargado
  UPDATE glo_folio
     SET status = -1
   WHERE folio = p_folio;  
  
END PROCEDURE;


