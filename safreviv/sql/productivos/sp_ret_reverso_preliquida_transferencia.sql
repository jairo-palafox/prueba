






CREATE PROCEDURE "safreviv".sp_ret_reverso_preliquida_transferencia( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT                 
                 );
DEFINE sql_err                INTEGER;
DEFINE isam_err               INTEGER;
DEFINE err_txt                CHAR(200);
DEFINE v_c_msj                CHAR(200);
DEFINE v_si_resultado         SMALLINT;
DEFINE v_id_derechohabiente   DECIMAL(9,0); 
DEFINE v_id_solicitud         DECIMAL(9,0);
DEFINE v_marca                SMALLINT;
DEFINE v_monto_viv97_invalido SMALLINT;

   LET v_marca = 806; -- retiros por transferencia
   LET v_monto_viv97_invalido = 767;
   
   FOREACH
    SELECT id_derechohabiente, id_solicitud
      INTO v_id_derechohabiente, v_id_solicitud
      FROM ret_transferencia 
     WHERE folio = p_folio
       AND estado_solicitud  = 100 -- rechazadas
       AND cod_rechazo       = v_monto_viv97_invalido -- sin saldo
   
       -- se reversa la desmarca de las cuentas
       EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                             v_marca,
                                             v_id_solicitud,
                                             p_folio);
   END FOREACH;
   
   -- se borra el historico de saldos insuficientes
   DELETE FROM ret_his_saldo
   WHERE  folio = p_folio;   
   
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
   UPDATE ret_transferencia
      SET estado_solicitud = 30 -- integradas 
          ,cod_rechazo = 0
    WHERE folio  = p_folio
      AND ( estado_solicitud = 50 -- preliquidadas
       OR (estado_solicitud = 100
           AND cod_rechazo = v_monto_viv97_invalido)); -- rechazadas sin saldo  
                       
 --RETURN v_si_resultado, err_txt;
END PROCEDURE;


