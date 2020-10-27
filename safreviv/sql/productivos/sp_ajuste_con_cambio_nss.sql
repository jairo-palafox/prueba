






CREATE PROCEDURE "safreviv".sp_ajuste_con_cambio_nss (p_folio       DECIMAL(9,0),
                                           p_proceso_cod SMALLINT,
                                           p_usuario     CHAR(20))

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

   DELETE FROM pag_ctr_pago
   WHERE folio = p_folio ;

   UPDATE cta_his_pagos
   SET    ind_liquidacion = -1,
          result_operacion = 0
   WHERE  folio = p_folio ;

   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 1,
          folio_referencia = NULL
   WHERE  folio_referencia = p_folio
   AND    ind_liquidacion IN (2,3,5) ;

   UPDATE cta_his_pagos
   SET    ind_liquidacion  = 3,
          folio_referencia = NULL
   WHERE  folio_referencia = p_folio
   AND    ind_liquidacion = 4    ;

   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

   --DELETE FROM glo_ctr_archivo where folio = p_folio; Se comenta línea para mantener el registro del archivo

   LET v_codigo_respuesta = 0;
   LET v_descripcion = "REGLAS DE NEGOCIO REVERSADAS ...";

   RETURN v_codigo_respuesta, v_descripcion;

END PROCEDURE
;


