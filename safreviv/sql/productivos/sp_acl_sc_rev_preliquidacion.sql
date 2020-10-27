






CREATE PROCEDURE "safreviv".sp_acl_sc_rev_preliquidacion( 
                 p_d_folio             DECIMAL(9,0),
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

   -- se cambian los estados de pago
   DELETE FROM pag_ctr_pago
   WHERE folio = p_d_folio ;

   UPDATE cta_his_pagos   
   SET    result_operacion = 1
   WHERE  folio = p_d_folio
   AND    id_derechohabiente IN ( SELECT id_derechohabiente FROM acl_preliquida WHERE folio_liquida = p_d_folio) ;
   
   -- se borran los datos
   DELETE FROM acl_preliquida
   WHERE folio_liquida = p_d_folio ;
   
    --SE ACTUALIZA LA TABLA GLO_FOLIO A ESTATUS 0
   UPDATE glo_folio
      SET status = 0
    WHERE proceso_cod = p_proceso_cod
      AND status = 1
      AND folio = p_folio;
   
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de preliquidación finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt;
   
END PROCEDURE;


