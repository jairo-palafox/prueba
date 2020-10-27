






CREATE PROCEDURE "safreviv".sp_ret_rev_int_ci_ley73(p_folio DECIMAL(9,0));
	DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
  DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud        DECIMAL(9,0);

   --set debug file to "/ds/safreviv_int/BD/reverso_integracion_ci_trmley73.debug";

   -- se reversa la marca de los derechohabientes 
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud  = 0;
   --se hace el ciclo de todas las soliictudes en estado 30 = integradas
   
   --trace "desmarcando";
   
   FOREACH
     SELECT id_derechohabiente,id_solicitud
       INTO v_id_derechohabiente,v_id_solicitud
       FROM ret_pago_trm
      WHERE folio = p_folio
          
       --trace "desmarcando solicitud: " || v_id_solicitud;
          
       -- marca tramite judicial pagado laudo/amparo
       LET v_marca_entra = 590;
                  
       --se invoca SP que reversa la marca de la cuenta consultada
       EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                                            v_marca_entra  ,
                                            v_id_solicitud ,
                                            p_folio        );
                                            
       -- marca tramite judicial en proceso
       LET v_marca_entra = 591;
                  
       --se invoca SP que reversa la marca de la cuenta consultada
       EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                                            v_marca_entra  ,
                                            v_id_solicitud ,
                                            p_folio        );
   
   END FOREACH;

   --trace "eliminando registros";

   --se borran los registros relacionados al folio en ret_disposicion
   DELETE FROM ret_pago_trm
   WHERE folio = p_folio;
  
   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   UPDATE safre_mig:glo_ctr_archivo
   SET    estado = 1
   WHERE folio = p_folio
   AND   proceso_cod = 1515
   AND   opera_cod = 1;

END PROCEDURE;


