






CREATE PROCEDURE "safreviv".sp_acl_enaclara_rev_preliquidacion(p_folio DECIMAL(9,0))

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
   END EXCEPTION;

   -- se reactivan los registros rechazados en preliquidacion
   UPDATE cta_his_pagos
   SET result_operacion = 1
   WHERE id_referencia IN ( SELECT id_referencia
                            FROM pag_excep_aclaracion
                            WHERE folio = p_folio )
   AND folio = p_folio;

   DELETE FROM pag_excep_aclaracion
   WHERE folio = p_folio;
                            
   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 0,
          result_operacion = 1
   WHERE id_referencia IN ( SELECT id_referencia 
                            FROM   pag_ctr_pago 
                            WHERE  folio = p_folio
                            AND    estado_pago = 70)
   AND folio = p_folio;

   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 0
   WHERE  id_referencia IN ( SELECT id_referencia 
                             FROM   pag_ctr_pago 
                             WHERE  folio = p_folio
                             AND    estado_pago = 50)
   AND folio = p_folio;

   -- se borra la tabla de control de pagos para el folio dado
   DELETE FROM pag_ctr_pago
   WHERE  folio = p_folio;
  
   -- se devuelven los indicadores de liquidacion a su estatus anterior
   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 3,
          folio_referencia = NULL
   WHERE  folio_referencia = p_folio
   AND    ind_liquidacion  = 4;

   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 1,
          folio_referencia = NULL
   WHERE  folio_referencia = p_folio
   AND    ind_liquidacion  = 3;

   -- se actualiza el folio a estado de integracion
   UPDATE glo_folio SET status = 0 WHERE folio = p_folio;
   
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El reverso de preliquidación finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt;
END PROCEDURE;


