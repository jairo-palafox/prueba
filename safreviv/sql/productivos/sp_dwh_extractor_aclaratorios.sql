






CREATE PROCEDURE "safreviv".sp_dwh_extractor_aclaratorios()
RETURNING INTEGER, INTEGER, CHAR(200);


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


   SET PDQPRIORITY HIGH;

SELECT
     id_derechohabiente,
     nrp,
     fn_bimestre_pago(periodo_pago) bimestre_pago,
     YEAR(f_pago) anio_pago,
     MONTH(f_pago) mes_pago,
     localiza_trabajador,
     ind_liquidacion,
     result_operacion,
     cve_ent_receptora,
     tpo_aclaracion,
     imp_ap_pat,
     aiv_ap_pat,
     valor_aiv,
     imp_am_cre,
     int_gen_pgo_ext,
     aiv_gen_pgo_ext
FROM cta_his_pagos
  WHERE origen_archivo not in (1,4)
     INTO TEMP tmp_aclaratorios;

   CREATE INDEX xpk_tmp_ret_dwh ON tmp_aclaratorios (id_derechohabiente);
   UPDATE STATISTICS FOR TABLE tmp_aclaratorios;


   INSERT INTO safre_sdo@vivws_tcp:saci_aclaratorio
   SELECT afi.nss,
          tmp.nrp ,
          tmp.bimestre_pago  ,
          tmp.anio_pago,
          tmp.mes_pago,
          tmp.localiza_trabajador ,
          tmp.ind_liquidacion,
          tmp.result_operacion,
          tmp.cve_ent_receptora,
          tmp.tpo_aclaracion ,
          tmp.imp_ap_pat ,
          tmp.aiv_ap_pat ,
          tmp.valor_aiv,
          tmp.imp_am_cre ,
          tmp.int_gen_pgo_ext ,
          tmp.aiv_gen_pgo_ext
     FROM tmp_aclaratorios tmp,
          afi_derechohabiente afi
        WHERE  tmp.id_derechohabiente=afi.id_derechohabiente ;

   SET PDQPRIORITY DEFAULT;

   SELECT count(*)
     INTO v_total_registros
     FROM safre_sdo@vivws_tcp:saci_aclaratorio;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh VALUES (TODAY,
                                   "saci_aclaratorio",
                                   TODAY,
                                   v_total_registros,
                                   1);

   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE;


