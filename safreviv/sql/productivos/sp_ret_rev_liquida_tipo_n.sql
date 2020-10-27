






CREATE PROCEDURE "safreviv".sp_ret_rev_liquida_tipo_n( 
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


