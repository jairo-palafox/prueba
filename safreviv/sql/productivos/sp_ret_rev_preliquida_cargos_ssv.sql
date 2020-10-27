






CREATE PROCEDURE "safreviv".sp_ret_rev_preliquida_cargos_ssv( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT                 
                 );
                   -- Control de Excepciones
  DEFINE sql_err                INTEGER;
  DEFINE isam_err               INTEGER;
  DEFINE err_txt                CHAR(200);
  DEFINE v_c_msj                CHAR(200);
  DEFINE v_si_resultado         SMALLINT;
  DEFINE v_id_derechohabiente   DECIMAL(9,0); 
  DEFINE v_id_solicitud         DECIMAL(9,0);
  DEFINE v_marca                SMALLINT;

  LET v_marca = 819; -- Cargos SSV

  FOREACH
      SELECT a.id_derechohabiente, b.id_solicitud
      INTO   v_id_derechohabiente, v_id_solicitud
      FROM   afi_derechohabiente a, ret_cargos_ssv_siaff b
      WHERE  a.nss = b.nss
      AND    b.folio = p_folio
      AND    b.estado_solicitud = 100
      AND    b.cod_rechazo = 121 -- sin saldo

      -- se reversa la desmarca de las cuentas
      EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                            v_marca,
                                            v_id_solicitud,
                                            p_folio);
   END FOREACH;

   --- SE BORRAN LOS REGISTROS DE RET_PRELIQUIDA
   DELETE 
   FROM   ret_preliquida
   WHERE  folio_liquida = p_folio;
   
    --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 0
   UPDATE glo_folio
      SET status = 0
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio = p_folio;

   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 2
   UPDATE glo_ctr_archivo 
      SET estado = 2  
    WHERE proceso_cod = p_proceso_cod
      AND estado = 3
      AND folio = p_folio;

   -- se cambian las solicitudes a estatus 50 preliquidada
   UPDATE ret_cargos_ssv_siaff
      SET estado_solicitud = 10 -- integradas 
         , cod_rechazo = 0
         , tipo_sol = "0.0"
    WHERE folio  = p_folio
    AND   tipo_sol <> "1.1"
      AND ( estado_solicitud = 50 -- preliquidadas
       OR (estado_solicitud = 100
           AND cod_rechazo = 121)); -- rechazadas sin saldo  

   UPDATE ret_cargos_ssv_siaff
      SET estado_solicitud = 10 -- integradas 
         , cod_rechazo = 0
   WHERE folio  = p_folio
    AND   tipo_sol = "1.1"
      AND estado_solicitud = 50; -- preliquidadas; -- rechazadas sin saldo  
           
 --RETURN v_si_resultado, err_txt;
END PROCEDURE;


