






CREATE PROCEDURE "safreviv".sp_tia_reverso_liquidacion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT,
                 p_usuario_cod         CHAR(20)
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
 
 -- manejo de excepciones
  --ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = sql_err;
      
    --  RETURN v_si_resultado, isam_err, err_txt;
  --END EXCEPTION

  DELETE FROM cta_movimiento 
  WHERE folio_liquida = p_folio; 
  
  DELETE FROM cta_decreto
  WHERE folio_liquida  =  p_folio ;  

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

--	RETURN v_si_resultado, err_txt;
END PROCEDURE ;


