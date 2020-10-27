






CREATE PROCEDURE "safreviv".sp_ret_reverso_preliquidacion_disposicion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT,
                 p_marca               SMALLINT
                 );
DEFINE sql_err                INTEGER;
DEFINE isam_err               INTEGER;
DEFINE err_txt                CHAR(200);
DEFINE v_c_msj                CHAR(200);
DEFINE v_si_resultado         SMALLINT;
DEFINE v_id_derechohabiente   DECIMAL(9,0);
DEFINE v_id_solicitud         DECIMAL(9,0);
DEFINE v_marca                SMALLINT;
DEFINE v_monto_viv92_invalido SMALLINT;
DEFINE v_monto_viv97_invalido SMALLINT;
DEFINE v_con_credito_43bis    SMALLINT;
   
   -- codigos de rechazo
   LET v_marca = p_marca; --805; -- retiros por disposicion
   LET v_monto_viv92_invalido = 766;
   LET v_monto_viv97_invalido = 767;
   LET v_con_credito_43bis    = 22;

   FOREACH
    SELECT id_derechohabiente, id_solicitud
      INTO v_id_derechohabiente, v_id_solicitud
      FROM ret_disposicion
     WHERE folio = p_folio
       AND estado_solicitud  = 100
       AND cod_rechazo IN (v_monto_viv92_invalido, v_monto_viv97_invalido, v_con_credito_43bis)
   
       -- se reversa la desmarca de las cuentas
       EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                             v_marca,
                                             v_id_solicitud,
                                             p_folio);
   END FOREACH;

   -- se borran los registros de historico de insuficiencia de saldos
   DELETE FROM ret_his_saldo
   WHERE  folio = p_folio;

   -- se actualiza la tabla glo_folio a estatus 0
   UPDATE glo_folio
      SET status = 0
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio = p_folio;

   -- se actualiza la tabla glo_ctr_archivo a estado 2
   UPDATE glo_ctr_archivo
      SET estado = 2
    WHERE proceso_cod = p_proceso_cod
      AND estado = 3
      AND folio = p_folio;

   -- se cambian las solicitudes a estatus 30 integradas
   UPDATE ret_disposicion
      SET estado_solicitud = 30 -- integardas
         , cod_rechazo = 0
    WHERE folio  = p_folio
      AND ( estado_solicitud = 50 -- preliquidadas
       OR (estado_solicitud = 100
           AND cod_rechazo IN (v_monto_viv92_invalido, v_monto_viv97_invalido, v_con_credito_43bis))); -- rechazadas sin saldo

END PROCEDURE;


