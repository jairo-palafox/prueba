






CREATE PROCEDURE "safreviv".sp_ret_rev_int_ci_tramite_judicial(p_folio DECIMAL(9,0));
  DEFINE v_id_derechohabiente  DECIMAL(9,0) ;

  --se borran los registros 
  DELETE FROM ret_pago_juridico;

   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   UPDATE safre_mig:glo_ctr_archivo
   SET    estado = 1
   WHERE folio = p_folio
   AND   proceso_cod = 1514 -- carga inicial retiros por trámite judicial
   AND   opera_cod = 1;

END PROCEDURE;


