






CREATE PROCEDURE "safreviv".sp_ret_rev_int_ci_disposicion(p_nombre_archivo CHAR(20));
	DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
  DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud        DECIMAL(9,0);
  DEFINE v_folio               DECIMAL(9,0);
  
  -- se obtienen todos los folios
  FOREACH
  SELECT folio
  INTO   v_folio
  FROM   ret_cza_disposicion
  WHERE  nombre_archivo = p_nombre_archivo

     --se borran los registros relacionados al folio 
     DELETE FROM ret_disposicion
     WHERE folio = v_folio;
     
     DELETE FROM ret_cza_disposicion
     WHERE folio = v_folio;
     
     DELETE FROM ret_transferencia_rch
     WHERE folio = v_folio;
     
     -- se actualiza el folio a reversado
     UPDATE safre_mig:glo_folio
     SET    status = -1
     WHERE  folio = v_folio;
  END FOREACH

  -- la glo_ctr_archivo se queda con el ultimo folio
  UPDATE safre_mig:glo_ctr_archivo
  SET    estado = 1
  WHERE --folio = v_folio
  --AND   
  proceso_cod = 1512 -- carga inicial retiros por disposicion de recursos
  AND   opera_cod = 1;

END PROCEDURE;


