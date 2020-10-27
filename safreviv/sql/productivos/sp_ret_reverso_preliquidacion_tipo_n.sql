






CREATE PROCEDURE "safreviv".sp_ret_reverso_preliquidacion_tipo_n( 
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
DEFINE v_id_decreto           DECIMAL(9,0); 
DEFINE v_id_solicitud         DECIMAL(9,0);
DEFINE v_marca                SMALLINT;
DEFINE v_monto_viv92_invalido SMALLINT;

  LET v_marca = 804; -- retiros por disposicion
  LET v_monto_viv92_invalido = 766;

  FOREACH
   SELECT id_decreto, id_solicitud
     INTO v_id_decreto , v_id_solicitud
     FROM ret_tipo_n 
    WHERE folio = p_folio
      AND estado_solicitud  = 100
      AND cod_rechazo       = v_monto_viv92_invalido -- sin saldo

      -- se reversa la desmarca de las cuentas
      EXECUTE PROCEDURE sp_reversa_desmarca_decreto(v_id_decreto  ,
                                                    v_marca       ,
                                                    v_id_solicitud,
                                                    p_folio);
   END FOREACH;
   
   DELETE FROM ret_his_saldo
   WHERE folio = p_folio;
   
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
   UPDATE ret_tipo_n
      SET estado_solicitud = 30 -- integardas 
          ,cod_rechazo = 0
    WHERE folio  = p_folio
      AND ( estado_solicitud = 50 -- preliquidadas
       OR (estado_solicitud = 100
           AND cod_rechazo = v_monto_viv92_invalido)); -- rechazadas sin saldo  

END PROCEDURE;


