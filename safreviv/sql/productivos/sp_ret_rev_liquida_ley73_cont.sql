






CREATE PROCEDURE "safreviv".sp_ret_rev_liquida_ley73_cont( 
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

   LET v_marca = 803; -- retiros por Ley 73 cont

   -- se borran los movimientos de cta_movimiento
   DELETE FROM cta_movimiento
   WHERE  folio_liquida = p_folio;
 
   FOREACH
   SELECT id_derechohabiente, id_solicitud
     INTO v_id_derechohabiente, v_id_solicitud
     FROM ret_ley73
    WHERE folio = p_folio
      AND estado_solicitud  = 60

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
   
   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 3 
   UPDATE glo_ctr_archivo 
      SET estado = 0
    WHERE proceso_cod = p_proceso_cod
      AND estado = 2
      AND folio = p_folio;

   -- se cambian las solicitudes a estatus 60 preliquidada
   UPDATE ret_ley73
      SET estado_solicitud = 50 -- preliquidadas
    WHERE folio  = p_folio
      AND estado_solicitud = 60 ;
      
END PROCEDURE;


