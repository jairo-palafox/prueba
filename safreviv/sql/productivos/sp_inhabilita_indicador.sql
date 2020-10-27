






CREATE PROCEDURE "safreviv".sp_inhabilita_indicador(p_folio DECIMAL(9,0))

RETURNING SMALLINT, VARCHAR(255)

   DEFINE v_codigo_respuesta SMALLINT;
   DEFINE v_descripcion      VARCHAR(255);
   DEFINE sql_err            INTEGER;
   DEFINE isam_err           INTEGER;
   DEFINE err_txt            CHAR(200);

  --manejo de excepciones 
  ON EXCEPTION SET sql_err, isam_err, err_txt

      RETURN sql_err,  err_txt;
  END EXCEPTION

--   DELETE FROM pag_ctr_pago
--   WHERE folio = p_folio ;

   UPDATE cta_his_pagos
   SET    ind_liquidacion = -1
--          result_operacion = 0
   WHERE  folio = p_folio ;

--   UPDATE cta_his_pagos
--   SET    ind_liquidacion  = 1,
--          folio_referencia = NULL
--   WHERE  folio_referencia = p_folio
--   AND    ind_liquidacion IN (2,3,5) ;

--   UPDATE cta_his_pagos
--   SET    ind_liquidacion  = 3,
--          folio_referencia = NULL
--   WHERE  folio_referencia = p_folio
--   AND    ind_liquidacion = 4    ;

   LET v_codigo_respuesta = 0;
   LET v_descripcion = "SE INHABILITA FOLIO";

   RETURN v_codigo_respuesta, v_descripcion;

END PROCEDURE;


