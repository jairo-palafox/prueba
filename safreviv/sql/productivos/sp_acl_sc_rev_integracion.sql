






CREATE PROCEDURE "safreviv".sp_acl_sc_rev_integracion(p_folio DECIMAL(9,0))

   -- se borran los datos de las tablas historicas
   DELETE FROM cta_his_pagos       WHERE folio = p_folio;
   DELETE FROM cta_pag_complemento WHERE folio = p_folio;
   DELETE FROM acl_sum_sc_nss      WHERE folio = p_folio;
   
   delete from acl_his_duplicados  where folio = p_folio;  --20052019
   
   DELETE FROM acl_pag_registrado  WHERE folio = p_folio;
   
   delete from acl_pag_rechazo     where folio = p_folio;  --20052019 
   
   DELETE FROM cta_rechazos_acl    WHERE folio = p_folio;  --saci2018-67
   DELETE FROM cta_especial_acl    WHERE folio = p_folio;  --saci2018-67
  
  -- se actualiza el estatus del archivo cargado
  UPDATE glo_folio
     SET status = -1
   WHERE folio = p_folio;    
  
END PROCEDURE;


