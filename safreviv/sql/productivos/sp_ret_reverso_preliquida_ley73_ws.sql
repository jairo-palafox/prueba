






CREATE PROCEDURE "safreviv".sp_ret_reverso_preliquida_ley73_ws( 
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

   
   -- se borra la preliquidación
   DELETE FROM ret_preliquida
   WHERE  folio_liquida = p_folio;   
   
    --Se borra el folio 
    DELETE glo_folio
    WHERE  folio = p_folio;
   
   -- se cambian las solicitudes a estatus 15 Autorizadas
   UPDATE ret_solicitud_generico
      SET estado_solicitud = 15 -- Autorizadas 
          ,cod_rechazo = 0
    WHERE folio  = p_folio
      AND estado_solicitud = 50; -- preliquidadas); -- rechazadas sin saldo  

   UPDATE ret_ley73_generico
      SET estado_solicitud = 15 -- Autorizadas 
          ,cod_rechazo = 0
    WHERE folio  = p_folio
      AND estado_solicitud = 50; -- preliquidadas); -- rechazadas sin saldo  
                       
 --RETURN v_si_resultado, err_txt;
END PROCEDURE;


