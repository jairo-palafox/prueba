






CREATE PROCEDURE "safreviv".sp_actualiza_destino()

   DEFINE v_id_derecho DECIMAL(9,0);
   DEFINE v_id_referen DECIMAL(9,0);
   DEFINE v_f_pago     DATE;
   DEFINE v_f_credito  DATE;

   FOREACH
      SELECT id_derechohabiente,
             id_referencia,
             f_pago
      INTO   v_id_derecho,
             v_id_referen,
             v_f_pago
      FROM   cta_his_pagos
      WHERE  folio = 9272

      SELECT f_credito
      INTO   v_f_credito
      FROM   cta_credito
      WHERE  id_derechohabiente = v_id_derecho
      AND    f_credito < v_f_pago;

      IF ( DBINFO('sqlca.sqlerrd2') > 0 ) THEN  --EXITOSO

         UPDATE cta_pag_complemento
         SET    destino_ap_viv = "1"
         WHERE  folio = 9272
         AND    id_derechohabiente = v_id_derecho
         AND    id_referencia      = v_id_referen;

      END IF

   END FOREACH

END PROCEDURE;


