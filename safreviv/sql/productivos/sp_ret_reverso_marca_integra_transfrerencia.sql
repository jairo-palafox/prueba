






CREATE PROCEDURE "safreviv".sp_ret_reverso_marca_integra_transfrerencia(
                p_folio               DECIMAL(9,0)                
                );
	DEFINE id_derechohabiente    DECIMAL(9,0) ;	
  DEFINE p_marca_entra         SMALLINT;
  DEFINE p_n_referencia        SMALLINT;

  LET p_marca_entra = 806;
  LET p_n_referencia = 0 ;
  --se hace el ciclo de todas las soliictudes en estado 30 = integradas
  
  FOREACH cu_cursor_reversa_marac FOR
  
    SELECT id_derehohabiente
      INTO id_derechohabiente
      FROM ret_transferencia
     WHERE folio = p_folio
       AND estado_solicitud = 30
                 
      --se invoca SP que reversa la marca de la cuenta consultada
      CALL sp_reversa_marca ( id_derechohabiente,
                              p_marca_entra  ,
                              p_n_referencia ,
                              p_folio        );
  END FOREACH; 
  FREE cu_cursor_reversa_marac;
  
  --borrar las tablas    
  --se borran los registros relacionados al folio en ret_disposicion
  DELETE FROM ret_transferencia
   WHERE folio = p_folio;
  --se borran los registros relacionados al encabezado en ret_cza_disposicion
  DELETE  FROM ret_cza_transferencia
   WHERE folio = p_folio;
  --se borran los registros relacionados al encabezado en ret_disposicion_rch   
  DELETE FROM ret_transferencia_rch
    WHERE folio = p_folio;  
  --se borran los registros relacionados al encabezado en ret_cza_disposicion_rch   
  DELETE FROM ret_cza_transferencia_rch
    WHERE folio = p_folio;

END PROCEDURE;


