






CREATE PROCEDURE "safreviv".sp_ret_rev_int_canint(p_folio DECIMAL(9,0));
  
  -- se borra los registros de la cabeza canint
  DELETE FROM ret_cza_canint
   WHERE folio = p_folio;
   
  --se borran los registros relacionados al folio CANINT
  DELETE FROM ret_det_canint
   WHERE folio = p_folio;
  
END PROCEDURE;


