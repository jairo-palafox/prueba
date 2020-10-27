






CREATE PROCEDURE "safreviv".sp_actualiza_salidas()

--  RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(100), DECIMAL(9,0), DECIMAL(9,0)
                 
   DEFINE v_folio             DECIMAL(9,0);
   DEFINE v_id_referencia     DECIMAL(9,0);

   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_folio_sua          DECIMAL(6,0);
   DEFINE v_periodo_pago       CHAR(06);
   DEFINE v_f_pago             DATE;
   DEFINE v_nrp                CHAR(11);
   DEFINE v_cve_ent_receptora  CHAR(03);
   DEFINE v_imp_ap_pat         DECIMAL(12,2);
   DEFINE v_imp_am_cre         DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext    DECIMAL(18,6);
 
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
             id_derechohabiente,
             folio_sua,
             periodo_pago,
             f_pago,
             nrp,
             cve_ent_receptora,
             imp_ap_pat,
             imp_am_cre,
             aiv_gen_pgo_ext
      INTO   v_folio,
             v_id_referencia, 
             v_id_derechohabiente,
             v_folio_sua,
             v_periodo_pago,
             v_f_pago,
             v_nrp,
             v_cve_ent_receptora,
             v_imp_ap_pat,
             v_imp_am_cre,
             v_aiv_gen_pgo_ext
      FROM  cta_his_pagos      
      WHERE destino_ap_viv is null
      AND   folio in (9638,
                      10223,
                      10274,
                      10789,
                      11503,
                      12119,
                      12683,
                      13275,
                      9527,
                      9549,
                      10265,
                      11165,
                      11409,
                      11476,
                      12083,
                      12677,
                      13268,
                      11172,
                      12166,
                      13271)

      LET v_destino_ap_viv = NULL;

      LET v_salida = v_folio||" "||v_id_referencia||" "||v_id_derechohabiente;

      SELECT destino_ap_viv
      INTO   v_destino_ap_viv
      FROM   cta_his_pagos
      WHERE  folio              = 9272
      AND    id_derechohabiente = v_id_derechohabiente
      AND    folio_sua          = v_folio_sua
      AND    periodo_pago       = v_periodo_pago
      AND    f_pago             = v_f_pago
      AND    nrp                = v_nrp
      AND    cve_ent_receptora  = v_cve_ent_receptora
      AND    imp_ap_pat         = v_imp_ap_pat
      AND    imp_am_cre         = v_imp_am_cre
      AND    aiv_gen_pgo_ext    = v_aiv_gen_pgo_ext;

      IF v_destino_ap_viv IS NOT NULL THEN
         LET v_cont_si = v_cont_si + 1;
         UPDATE cta_his_pagos
         SET    destino_ap_viv = v_destino_ap_viv
         WHERE  CURRENT OF cur_his_pagos; 

      ELSE
         LET v_cont_no = v_cont_no + 1;
      END IF;
            
   END FOREACH;

  -- RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;

END PROCEDURE;


