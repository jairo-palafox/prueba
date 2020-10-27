






CREATE PROCEDURE "safreviv".sp_dwh_extractor_pagos(p_f_corte   DATE)
RETURNING INTEGER, INTEGER, CHAR(200);

   DEFINE v_f_inicio          DATE;
   DEFINE v_f_paso            DATE;

   DEFINE v_nss               CHAR(11);

   DEFINE v_total_registros   INTEGER;

   DEFINE r_sql_err        INTEGER;
   DEFINE r_isam_err       INTEGER;
   DEFINE r_err_txt        CHAR(200);

   ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt
      RETURN r_sql_err, r_isam_err, r_err_txt;
   END EXCEPTION

   LET r_sql_err  = 0;
   LET r_isam_err = 0;
   LET r_err_txt  = "";

   LET v_f_paso = p_f_corte - 1 UNITS MONTH; 
   LET v_f_inicio = MDY(MONTH(v_f_paso),1,YEAR(v_f_paso));

   SET PDQPRIORITY HIGH;
   SELECT id_derechohabiente ,
          periodo_pago       ,
          folio_sua          ,
          f_pago             ,
          imp_ap_pat         ,
          imp_am_cre         ,
          aiv_ap_pat         ,
          f_proceso          ,
          f_valor            ,
          destino_ap_viv
     FROM cta_his_pagos
    WHERE f_proceso BETWEEN v_f_inicio AND p_f_corte
      AND origen_archivo = 1
      AND ind_liquidacion IN (0,2,3)
     INTO TEMP tmp_pagos_bimestre;

   CREATE INDEX xpk_tmp_pag_dwh ON tmp_pagos_bimestre (id_derechohabiente);
   UPDATE STATISTICS FOR TABLE tmp_pagos_bimestre;

   SELECT UNIQUE nss
     FROM safre_sdo@vivws_tcp:saci_indicador
     INTO TEMP tmp_nss_indicador;

   CREATE UNIQUE INDEX xpk_tmp_nss_indicador ON tmp_nss_indicador (nss);

   INSERT INTO safre_sdo@vivws_tcp:saci_pago
   SELECT afi.nss,
          pag.periodo_pago       ,
          pag.folio_sua          ,
          pag.f_pago             ,
          pag.imp_ap_pat         ,
          pag.imp_am_cre         ,
          pag.aiv_ap_pat         ,
          pag.f_proceso          ,
          pag.f_valor            ,
          pag.destino_ap_viv     ,
          p_f_corte
     FROM tmp_nss_indicador tmp,
          afi_derechohabiente afi,
          tmp_pagos_bimestre pag
       WHERE tmp.nss = afi.nss
         AND afi.id_derechohabiente = pag.id_derechohabiente;

   SET PDQPRIORITY DEFAULT;

   SELECT count(*)
     INTO v_total_registros
     FROM safre_sdo@vivws_tcp:saci_pago;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh VALUES (p_f_corte,
                                   "saci_pago",
                                   TODAY,
                                   v_total_registros,
                                   1); 

END PROCEDURE;


