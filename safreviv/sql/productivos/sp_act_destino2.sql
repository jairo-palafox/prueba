






CREATE PROCEDURE "safreviv".sp_act_destino2()

--  RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(100), DECIMAL(9,0), DECIMAL(9,0)
                 
   DEFINE v_folio             DECIMAL(9,0);
   DEFINE v_id_referencia     DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_destino_ap_viv CHAR(01);


   DEFINE v_cont_si decimal(9,0);
   DEFINE v_cont_no decimal(9,0);

       -- Control de Excepciones
   DEFINE sql_err        SMALLINT;
   DEFINE isam_err       SMALLINT;
   DEFINE err_txt        VARCHAR(255);
   DEFINE v_si_resultado SMALLINT;
   DEFINE v_salida       CHAR(100);

     --manejo de excepciones
--   ON EXCEPTION SET sql_err, isam_err, err_txt
--      LET v_si_resultado = sql_err;
--      RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;
--   END EXCEPTION  

   LET v_salida = NULL;
   LET v_cont_si = 0;
   LET v_cont_no = 0;

   --  query principal --
   FOREACH cur_his_pagos FOR
      SELECT folio,
             id_referencia,
             id_derechohabiente
      INTO   v_folio,
             v_id_referencia, 
             v_id_derechohabiente
      FROM  cta_his_pagos
      WHERE folio in (8989,
                      9007,
                      9067,
                      9087,
                      9107,
                      9217,
                      9347,
                      9409,
                      9510,
                      9527,
                      9549,
                      9551,
                      9588,
                      9638,
                      9688,
                      9926,
                      10045,
                      10090,
                      10198,
                      10231,
                      10265,
                      10274,
                      10297,
                      10545,
                      10599,
                      10708,
                      10762,
                      10789,
                      10802)
      AND   destino_ap_viv is null 
      
      LET v_salida = v_folio||" "||v_id_referencia||" "||v_id_derechohabiente;

      LET v_destino_ap_viv = NULL;

      SELECT destino_ap_viv
      INTO   v_destino_ap_viv
      FROM   cta_pag_complemento
      WHERE  folio              = v_folio
      AND    id_referencia      = v_id_referencia
      AND    id_derechohabiente = v_id_derechohabiente;

      IF v_destino_ap_viv IS NOT NULL THEN
         LET v_cont_si = v_cont_si + 1;
         UPDATE cta_his_pagos
         SET    destino_ap_viv = v_destino_ap_viv
         WHERE  CURRENT OF cur_his_pagos;
      ELSE
         LET v_cont_no = v_cont_no + 1;
      END IF;

   END FOREACH;

--   RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;

END PROCEDURE;


