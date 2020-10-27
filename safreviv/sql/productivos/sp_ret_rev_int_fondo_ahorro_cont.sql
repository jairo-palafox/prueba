






CREATE PROCEDURE "safreviv".sp_ret_rev_int_fondo_ahorro_cont(p_folio DECIMAL(9,0));
                               
  DEFINE v_id_derechohabiente    DECIMAL(9,0) ;
  --DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud          DECIMAL(9,0);

  --LET v_marca_entra = 805;
  LET v_id_derechohabiente = 0;
  LET v_id_solicitud  = 0;

  -- se realiza el reverso de la marca
  FOREACH
  SELECT id_derechohabiente,
         id_solicitud
    INTO v_id_derechohabiente,
         v_id_solicitud
    FROM ret_fondo_ahorro
   WHERE folio = p_folio
     AND estado_solicitud = 15
                       
     -- se invoca SP que reversa la marca de la cuenta consultada
     EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                             802, -- marca de fondo de ahorro
                             v_id_solicitud ,
                             p_folio        );
  END FOREACH;
    
  --se borran los registros relacionados al folio en ret_fondo72
  FOREACH 
  SELECT id_solicitud
  INTO v_id_solicitud
  FROM ret_fondo_ahorro
  WHERE folio = p_folio

     -- se borra la solicitud de ret_det_fondo72
     DELETE 
     FROM ret_det_fondo72
     WHERE id_solicitud = id_solicitud;
  END FOREACH;

  -- se borran las solicitudes cargadas
  DELETE
  FROM  ret_fondo_ahorro
  WHERE folio = p_folio;

  -- el folio se cambia a reversado
  UPDATE glo_folio
  SET    status = -1
  WHERE  folio = p_folio;

END PROCEDURE;


