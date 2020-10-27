






CREATE PROCEDURE "safreviv".sp_ret_rev_liquida_cargos_ssv( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT                 
                 );
 -- Control de Excepciones
 DEFINE sql_err                INTEGER;
 DEFINE isam_err               INTEGER;
 DEFINE err_txt                VARCHAR(200);
 DEFINE v_c_msj                VARCHAR(200);
 DEFINE v_si_resultado         SMALLINT;
 DEFINE v_id_derechohabiente   DECIMAL(9,0); 
 DEFINE v_id_solicitud         DECIMAL(9,0);
 DEFINE v_marca                SMALLINT;

  --manejo de excepciones
  --ON EXCEPTION SET sql_err, isam_err, err_txt
  --    LET v_si_resultado = sql_err;
  --    
  --    RETURN v_si_resultado, isam_err, err_txt;
  --END EXCEPTION

   LET v_marca = 819; -- cargo a la SSV

   -- se borran los movimientos de cta_movimiento
   DELETE FROM cta_movimiento
   WHERE  folio_liquida = p_folio;
 
   FOREACH
      SELECT a.id_derechohabiente, b.id_solicitud
      INTO   v_id_derechohabiente, v_id_solicitud
      FROM   afi_derechohabiente a, ret_cargos_ssv_siaff b
      WHERE  a.nss = b.nss
      AND    b.folio = p_folio
      AND    b.estado_solicitud = 60

      -- se reversa la desmarca de las cuentas
      EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                            v_marca,
                                            v_id_solicitud,
                                            p_folio);
   END FOREACH;

   --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 1
   UPDATE glo_folio
   SET status = 1
   WHERE proceso_cod = p_proceso_cod
     AND status = 2
     AND folio = p_folio;
   
   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 2
   UPDATE glo_ctr_archivo 
      SET estado = 0
    WHERE proceso_cod = p_proceso_cod
      AND estado = 2
      AND folio = p_folio;

   -- se cambian las solicitudes a estatus 60 preliquidada
   UPDATE ret_cargos_ssv_siaff
      SET estado_solicitud = 50 -- preliquidadas
    WHERE folio  = p_folio
      AND estado_solicitud = 60 ;
      
END PROCEDURE;


