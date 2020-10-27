






CREATE PROCEDURE "safreviv".sp_ret_rev_integra_cargos_ssv(
                                           p_folio               DECIMAL(9,0)
                                          ,p_marca               SMALLINT 
                                          ,p_proceso_cod         SMALLINT
                                          ,p_nombre_archivo      CHAR(40)
                                          );
  DEFINE v_id_derechohabiente    DECIMAL(9,0) ;
  DEFINE v_marca_entra           SMALLINT;  
  DEFINE v_id_solicitud          DECIMAL(9,0);
  DEFINE v_s_qry                 varchar(250);

  LET v_marca_entra        = p_marca; --803;
  LET v_id_derechohabiente = 0;
  LET v_id_solicitud       = 0;
  LET v_s_qry              = null ;
  
  --se hace el ciclo de todas las soliictudes en estado 15 = integradas
  FOREACH  	  
      SELECT a.id_derechohabiente, b.id_solicitud
      INTO   v_id_derechohabiente,v_id_solicitud
      FROM   afi_derechohabiente a, ret_cargos_ssv_siaff b
      WHERE  a.nss = b.nss
      AND    b.folio = p_folio
                              
      --se invoca SP que reversa la marca de la cuenta consultada
      EXECUTE PROCEDURE sp_reversa_marca ( v_id_derechohabiente, 
                              v_marca_entra  ,
                              v_id_solicitud ,
                              p_folio        );
  END FOREACH;
  


  DELETE 
  FROM   afi_derechohabiente
  WHERE  nss IN (SELECT nss
                 FROM   ret_cargos_ssv_siaff
                 WHERE  folio = p_folio
                 AND    tipo_sol = "1.1"
                 AND    estado_solicitud = 10);

  DELETE 
  FROM   ret_cargos_ssv_siaff
  WHERE  folio = p_folio;
  
  UPDATE glo_folio
  SET    status      = -1
  WHERE  proceso_cod =  p_proceso_cod
  AND    status      =  0
  AND    folio       =  p_folio;
  
  -- se actualiza el estado del archivo a cargado
  UPDATE glo_ctr_archivo
  SET    estado = 1
  WHERE  nombre_archivo = p_nombre_archivo;    
   
END PROCEDURE;


