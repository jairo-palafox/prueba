






CREATE PROCEDURE "safreviv".sp_ret_rev_int_tipo_n(p_folio DECIMAL(9,0))
	DEFINE v_id_decreto            DECIMAL(9,0) ;	
	DEFINE v_id_solicitud          DECIMAL(9,0);
	DEFINE v_marca_entra           SMALLINT;      

  LET v_marca_entra = 804; 
  LET v_id_decreto = 0;
  LET v_id_solicitud = 0;
  --se hace el ciclo de todas las soliictudes en estado 30 = integradas
  
  FOREACH   
  SELECT id_decreto, id_solicitud
  INTO   v_id_decreto,v_id_solicitud
  FROM   ret_tipo_n
  WHERE  folio = p_folio

     --se invoca SP que reversa la marca de la cuenta consultada
     EXECUTE PROCEDURE sp_reversa_marca_decreto (v_id_decreto   ,
                                                 v_marca_entra  ,
                                                 v_id_solicitud ,
                                                 p_folio        );
  END FOREACH;
  
  --borrar las tablas    
  --se borran los registros relacionados al folio en ret_disposicion
  DELETE FROM  ret_tipo_n
   WHERE folio = p_folio;
  --se borran los registros relacionados al encabezado en ret_cza_disposicion
  DELETE  FROM  ret_cza_tipo_n
   WHERE folio = p_folio;
  --se borran los registros relacionados al encabezado en ret_disposicion_rch   
  DELETE FROM  ret_tipo_n_rch
    WHERE folio = p_folio;  
  --se borran los registros relacionados al encabezado en ret_cza_disposicion_rch   
  DELETE FROM  ret_cza_tipo_n_rch
    WHERE folio = p_folio;
    
  -- se le quita el folio al registro de archivo
  UPDATE glo_ctr_archivo
  SET    folio = NULL,
         estado = 1 -- cargado
  WHERE  folio = p_folio;
  
  -- se cambia el status del folio a -1
  UPDATE glo_folio
  SET    status = -1
  WHERE  folio = p_folio;


END PROCEDURE;


