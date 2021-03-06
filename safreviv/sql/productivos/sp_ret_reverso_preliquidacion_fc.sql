






CREATE PROCEDURE "safreviv".sp_ret_reverso_preliquidacion_fc( 
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

   LET v_marca = 807; -- retiros por disposicion
   
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
   UPDATE ret_fortalecimiento_credito
      SET estado_solicitud = 30 -- integardas 
         , cod_rechazo = 0
    WHERE folio  = p_folio
      AND ( estado_solicitud = 50 -- preliquidadas
       OR (estado_solicitud = 100
           AND cod_rechazo = 10)); -- rechazadas sin saldo  
      
 --RETURN v_si_resultado, err_txt;
END PROCEDURE;


