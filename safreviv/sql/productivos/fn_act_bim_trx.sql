






CREATE FUNCTION "safreviv".fn_act_bim_trx()
   RETURNING SMALLINT

   DEFINE v_error               smallint;
   DEFINE v_id_transaccion      decimal(9,0);

   DEFINE v_id_derechohabiente  decimal(9,0);
   DEFINE v_f_respuesta         date;
   DEFINE v_cve_ent_financiera  smallint;
   DEFINE v_num_ctr_int_ef      char(18);
   DEFINE v_f_vencimiento       date;
   DEFINE v_impt_utilizado      decimal(12,2);
   DEFINE v_impt_ap_subsec      decimal(12,2);
   DEFINE v_bimestre            char(6);
   DEFINE v_f_proceso           date;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   ---SET DEBUG FILE TO '/safreviv_int/BD/fn_act_bim_trx.trace';
   ---TRACE ON;

   create temp table ocg_conf_id_trx
   (id_ocg_ctr_transaccion decimal(9,0));

  insert into ocg_conf_id_trx values(99999999);

   LET v_error = 0;
   LET v_id_transaccion = 0;

   FOREACH
      select tmp.id_derechohabiente ,
             tmp.f_respuesta        ,
             tmp.cve_ent_financiera ,
             tmp.num_ctr_int_ef     ,
             tmp.f_vencimiento      ,
             tmp.impt_utilizado_garantia,
             tmp.impt_ap_subsec     ,
             tmp.bimestre           ,
             tmp.f_proceso
        into v_id_derechohabiente   ,
             v_f_respuesta          ,
             v_cve_ent_financiera   ,
             v_num_ctr_int_ef       ,
             v_f_vencimiento        ,
             v_impt_utilizado       ,
             v_impt_ap_subsec       ,
             v_bimestre             ,
             v_f_proceso 
        from ocg_trx tmp

      FOREACH
       SELECT FIRST 1 id_ocg_ctr_transaccion
         INTO v_id_transaccion
         FROM ocg_ctr_transaccion
        where id_derechohabiente = v_id_derechohabiente
          and folio = 1
          and f_transaccion = v_f_respuesta
          and vivienda_97 = v_impt_ap_subsec
          and periodo_pago <> v_bimestre
          and id_ocg_ctr_transaccion not in(
                   select id_ocg_ctr_transaccion
                     from ocg_conf_id_trx)
      END FOREACH;

      if v_id_transaccion > 0 then
          update ocg_ctr_transaccion
             set periodo_pago = v_bimestre
           where id_ocg_ctr_transaccion = v_id_transaccion
             and id_derechohabiente = v_id_derechohabiente
             and folio = 1
             and f_transaccion = v_f_respuesta
             and vivienda_97 = v_impt_ap_subsec
             and periodo_pago <> v_bimestre;
      end if

      insert into ocg_conf_id_trx values(v_id_transaccion);

      let v_id_transaccion = "";
      let v_bimestre       = "";

   END FOREACH

   RETURN v_error;

END FUNCTION;


