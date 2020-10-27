






CREATE PROCEDURE "safreviv".sp_act_aivs_cta_mov12576()

  RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(100), DECIMAL(9,0), DECIMAL(9,0)
                 
   DEFINE v_folio              DECIMAL(9,0);
   DEFINE v_id_referencia      DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_subcuenta          SMALLINT;
   DEFINE v_movimiento         SMALLINT;
   DEFINE v_aiv_ap_pat         DECIMAL(18,6);
   DEFINE v_monto_acciones     DECIMAL(16,6);

   DEFINE v_cont_si decimal(9,0);
   DEFINE v_cont_no decimal(9,0);

       -- Control de Excepciones
   DEFINE sql_err        SMALLINT;
   DEFINE isam_err       SMALLINT;
   DEFINE err_txt        VARCHAR(255);
   DEFINE v_si_resultado SMALLINT;
   DEFINE v_salida       CHAR(100);

     --manejo de excepciones
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;
   END EXCEPTION  

   LET v_salida = NULL;
   LET v_cont_si = 0;
   LET v_cont_no = 0;

   --  query principal --
   FOREACH 
      SELECT mal.folio,
             mal.id_referencia,
             mal.id_derechohabiente,
             mov.subcuenta,
             mov.movimiento,
             mal.aiv_ap_pat,
             mov.monto_acciones
      INTO   v_folio,
             v_id_referencia, 
             v_id_derechohabiente,
             v_subcuenta,
             v_movimiento,
             v_aiv_ap_pat,
             v_monto_acciones
      FROM  cta_his_pagos mal,
            cta_movimiento mov
      WHERE mal.folio         = 12576
      AND   mal.aiv_ap_pat    > 0
      AND   mal.folio         = mov.folio_liquida
      AND   mal.id_referencia = mov.id_referencia
      AND  (mal.aiv_ap_pat   <> mov.monto_acciones)
      AND   mov.movimiento    = 1
      AND   mov.subcuenta     = 4
  
      LET v_salida = v_folio||" "||v_id_referencia||" "||v_id_derechohabiente;

      LET v_cont_si = v_cont_si + 1;

      UPDATE cta_movimiento
      SET    monto_acciones     = v_aiv_ap_pat
      WHERE  folio_liquida      = v_folio
      AND    id_referencia      = v_id_referencia
      AND    id_derechohabiente = v_id_derechohabiente
      AND    subcuenta          = v_subcuenta
      AND    movimiento         = v_movimiento
      AND    monto_acciones     = v_monto_acciones;

   END FOREACH;

   RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;

END PROCEDURE;


