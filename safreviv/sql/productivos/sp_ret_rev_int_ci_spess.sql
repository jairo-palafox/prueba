






CREATE PROCEDURE "safreviv".sp_ret_rev_int_ci_spess(p_folio DECIMAL(9,0));
	DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
  DEFINE v_marca_entra         SMALLINT;  
  DEFINE v_id_solicitud        DECIMAL(9,0);

  --se borran los registros relacionados al folio en ret_disposicion
  DELETE FROM ret_datamart
  WHERE folio = p_folio;
  
  DELETE FROM ret_cza_datamart
  WHERE folio = p_folio;
  
  -- se regresa el proceso a procesando y la operacion a listo
   --UPDATE safre_mig:bat_ctr_proceso
   --SET    estado_cod  = 2
   --WHERE  proceso_cod = 1510;
   --
   --UPDATE safre_mig:bat_ctr_operacion
   --SET    estado_cod = 1,
   --       folio      = 0,
   --       fecha_ini  = NULL,
   --       fecha_fin  = NULL
   --WHERE proceso_cod = 1510
   --AND   opera_cod   = 2;
          
   -- se actualiza el folio a reversado
   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   UPDATE safre_mig:glo_ctr_archivo
   SET    estado = 1
   WHERE folio = p_folio
   AND   proceso_cod = 1510
   AND   opera_cod = 1;

END PROCEDURE;


