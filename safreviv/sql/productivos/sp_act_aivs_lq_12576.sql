






CREATE PROCEDURE "safreviv".sp_act_aivs_lq_12576()

  RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(100), DECIMAL(9,0), DECIMAL(9,0)
                 
   DEFINE v_folio              DECIMAL(9,0);
   DEFINE v_id_referencia      DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_folio_sua          DECIMAL(6,0);
   DEFINE v_periodo_pago       CHAR(06);
   DEFINE v_f_pago             DATE;
   DEFINE v_nrp                CHAR(11);
   DEFINE v_cve_ent_receptora  CHAR(03);
   DEFINE v_imp_ap_pat         DECIMAL(12,2);
   DEFINE v_imp_am_cre         DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext    DECIMAL(18,6);
   DEFINE v_aiv_ap_pat         DECIMAL(18,6);
   DEFINE v_ind_liquidacion    SMALLINT;


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
             mal.folio_sua,
             mal.periodo_pago,
             mal.f_pago,
             mal.nrp,
             mal.cve_ent_receptora,
             mal.imp_ap_pat,
             mal.imp_am_cre,
             mal.aiv_gen_pgo_ext,
             bue.aiv_ap_pat,
             mal.ind_liquidacion
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
             v_aiv_gen_pgo_ext,
             v_aiv_ap_pat,
             v_ind_liquidacion
      FROM  cta_his_pagos mal,
            cta_his_pagos bue
      WHERE mal.folio = 12576
      AND   mal.aiv_ap_pat > 0
     AND   bue.folio = 14471        ----- folio_bueno ----- 12774
     AND   mal.id_derechohabiente = bue.id_derechohabiente
     AND   mal.folio_sua          = bue.folio_sua
     AND   mal.periodo_pago       = bue.periodo_pago
     AND   mal.f_pago             = bue.f_pago
     AND   mal.nrp                = bue.nrp
     AND   mal.cve_ent_receptora  = bue.cve_ent_receptora
     AND   mal.imp_ap_pat         = bue.imp_ap_pat
     AND   mal.imp_am_cre         = bue.imp_am_cre
     AND   mal.aiv_gen_pgo_ext    = bue.aiv_gen_pgo_ext
     AND   (mal.aiv_ap_pat <>  bue.aiv_ap_pat)
--     AND   mal.ind_liquidacion = 0    --- Revisar este coment para produccion

      LET v_salida = v_folio||" "||v_id_referencia||" "||v_id_derechohabiente;

      LET v_cont_si = v_cont_si + 1;

      UPDATE cta_his_pagos
      SET    aiv_ap_pat = v_aiv_ap_pat
      WHERE  folio              = 12576    --- folio malo
      AND    id_derechohabiente = v_id_derechohabiente
      AND    folio_sua          = v_folio_sua
      AND    periodo_pago       = v_periodo_pago
      AND    f_pago             = v_f_pago
      AND    nrp                = v_nrp
      AND    cve_ent_receptora  = v_cve_ent_receptora
      AND    imp_ap_pat         = v_imp_ap_pat
      AND    imp_am_cre         = v_imp_am_cre
      AND    aiv_gen_pgo_ext    = v_aiv_gen_pgo_ext
      AND    ind_liquidacion    = v_ind_liquidacion;

   END FOREACH;

   RETURN v_si_resultado, isam_err, err_txt, v_salida, v_cont_si, v_cont_no;

END PROCEDURE;


