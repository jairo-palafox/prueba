






CREATE PROCEDURE "safreviv".sp_acl_enaclara_rev_integracion(p_folio DECIMAL(9,0))

  --se borran los registros relacionados al folio 
  DELETE FROM cta_his_pagos
  WHERE folio = p_folio;
  
  DELETE FROM cta_pag_complemento
  WHERE folio = p_folio;

  DELETE FROM acl_sum_aclaracion
  WHERE folio = p_folio;

   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   UPDATE glo_ctr_archivo
   SET    estado = 1
   WHERE  folio = p_folio
   AND    proceso_cod = 101 -- Aclaraciones ENACLARA
   AND    opera_cod = 1;

  
END PROCEDURE;


