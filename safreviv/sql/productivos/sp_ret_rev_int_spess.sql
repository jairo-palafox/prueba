






CREATE PROCEDURE "safreviv".sp_ret_rev_int_spess(p_folio DECIMAL(9,0));
  
  -- se borra los registros de la cabeza datamart
  DELETE FROM ret_cza_datamart
   WHERE folio = p_folio;
   
  --se borran los registros relacionados al folio SPESS en  ret_datamart
  DELETE FROM ret_datamart
   WHERE folio = p_folio;
  
  --se borran los registros relacionados al folio SPESS en  ret_datamart_rch_carga
  DELETE FROM ret_datamart_rch_carga
   WHERE folio = p_folio;

END PROCEDURE;


