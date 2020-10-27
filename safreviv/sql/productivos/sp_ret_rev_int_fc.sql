






CREATE PROCEDURE "safreviv".sp_ret_rev_int_fc(p_folio DECIMAL(9,0));
	DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
  DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud        DECIMAL(9,0);

  LET v_marca_entra = 807;
  LET v_id_derechohabiente = 0;
  LET v_id_solicitud  = 0;
  --se hace el ciclo de todas las soliictudes en estado 30 = integradas

  FOREACH
  	  
    SELECT id_derechohabiente,id_solicitud
      INTO v_id_derechohabiente,v_id_solicitud
      FROM ret_fortalecimiento_credito
     WHERE folio = p_folio
       --AND estado_solicitud = 30
                       
      --se invoca SP que reversa la marca de la cuenta consultada
      EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                              v_marca_entra  ,
                              v_id_solicitud ,
                              p_folio        );
  END FOREACH;
  --se borran los registros relacionados al folio en ret_disposicion
  DELETE FROM ret_fortalecimiento_credito
   WHERE folio = p_folio;

END PROCEDURE;


