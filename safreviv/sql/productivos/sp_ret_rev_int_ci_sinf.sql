






CREATE PROCEDURE "safreviv".sp_ret_rev_int_ci_sinf(p_folio DECIMAL(9,0));
	DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
  DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud        DECIMAL(9,0);

  --se borran los registros relacionados al folio 
  DELETE FROM ret_solo_infonavit
  WHERE folio = p_folio;
  
   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   UPDATE safre_mig:glo_ctr_archivo
   SET    estado = 1
   WHERE folio = p_folio
   AND   proceso_cod = 1513 -- carga inicial retiros por disposicion de recursos
   AND   opera_cod = 1;

END PROCEDURE;


