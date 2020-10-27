






CREATE PROCEDURE "safreviv".sp_rev_liquida_ci_aclaraciones(p_folio DECIMAL(9,0));

  --se borran los registros relacionados al folio 
  DELETE FROM cta_movimiento
  WHERE folio_liquida = p_folio;

   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio;

  {UPDATE safre_mig:glo_ctr_archivo
   SET    estado = 1
   WHERE folio = p_folio
   AND   proceso_cod = 110 -- carga de aclaraciones
   AND   opera_cod = 1;}

END PROCEDURE;


