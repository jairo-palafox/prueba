






CREATE PROCEDURE "safreviv".sp_ret_reverso_liquidacion_tipo_n( 
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
 
   --manejo de excepciones
  --ON EXCEPTION SET sql_err, isam_err, err_txt
  --    LET v_si_resultado = sql_err;
  --    
  --    RETURN v_si_resultado, isam_err, err_txt;
  --END EXCEPTION

   LET v_marca = 804; -- retiros por disposicion de recursos
 
   FOREACH
   SELECT id_decreto, id_solicitud
     INTO v_id_decreto, v_id_solicitud
     FROM ret_tipo_n
    WHERE folio = p_folio
      AND estado_solicitud  = 60

      -- se reversa la desmarca de las cuentas
      EXECUTE PROCEDURE sp_reversa_desmarca_decreto(v_id_decreto  ,
                                                    v_marca       ,
                                                    v_id_solicitud,
                                                    p_folio);
   END FOREACH;

   DELETE FROM cta_decreto
   WHERE  folio_liquida = p_folio;

   --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 1
   UPDATE glo_folio
      SET status = 1 
    WHERE proceso_cod = p_proceso_cod
      AND status = 2
      AND folio  = p_folio;

   --SE ACTUALIZA LA TABLA GLO_CTR_ARCHIVO A ESTADO 3
   UPDATE glo_ctr_archivo 
      SET estado = 3
    WHERE proceso_cod = p_proceso_cod
      AND estado = 4
      AND folio = p_folio;

   -- se cambian las solicitudes a estatus 50 preliquidada
   UPDATE ret_tipo_n
      SET estado_solicitud = 50 -- preliquidadas
    WHERE folio  = p_folio
      AND estado_solicitud = 60 ;

--RETURN v_si_resultado, err_txt;
END PROCEDURE ;


