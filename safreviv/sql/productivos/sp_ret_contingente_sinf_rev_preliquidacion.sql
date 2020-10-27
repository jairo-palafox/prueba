






CREATE PROCEDURE "safreviv".sp_ret_contingente_sinf_rev_preliquidacion( 
                 p_folio               DECIMAL(9,0),
                 p_proceso_cod         SMALLINT                 
                 )

 RETURNING SMALLINT, INTEGER, VARCHAR(255)
 
  -- Control de Excepciones
  DEFINE sql_err                INTEGER;
  DEFINE isam_err               INTEGER;
  DEFINE err_txt                CHAR(200);
  DEFINE v_c_msj                CHAR(200);
  DEFINE v_si_resultado         SMALLINT;
  DEFINE v_id_derechohabiente   DECIMAL(9,0); 
  DEFINE v_id_solicitud         DECIMAL(9,0);
  DEFINE v_marca                SMALLINT;

  --manejo de excepciones
  ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
  END EXCEPTION

   -- no se borran los datos de la tabla de preliquidacion
   -- porque la tabla se regenera en cada ejecucion
   
   DELETE
     FROM ret_preliquida
    WHERE folio_liquida = p_folio;
   
    -- se devuelve el estado del folio a integrado
   UPDATE glo_folio
      SET status = 0
    WHERE folio = p_folio;
      
   -- se actualiza el estado del folio      
   UPDATE ret_solo_infonavit
      SET estado_solicitud = 10
    WHERE estado_solicitud = 50
      AND folio = p_folio  ;
      
   DELETE FROM ret_his_saldo
   WHERE  folio = p_folio;
   
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El reverso de preliquidación finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt;
END PROCEDURE;


