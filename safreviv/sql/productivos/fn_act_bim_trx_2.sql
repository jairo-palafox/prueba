






CREATE FUNCTION "safreviv".fn_act_bim_trx_2()
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

   DEFINE v_vivienda_97         decimal(12,2);
   DEFINE v_periodo_pago        char(6);
   DEFINE v_f_transaccion       date;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   ---SET DEBUG FILE TO '/safreviv_int/BD/fn_act_bim_trx.trace';
   ---TRACE ON;

   LET v_error = 0;
   LET v_id_transaccion = 0;

   FOREACH
      select id_derechohabiente,
             vivienda_97
        into v_id_derechohabiente,
             v_vivienda_97
        from safre_tmp:reg_ctr

      FOREACH
       SELECT id_derechohabiente,
              impt_ap_subsec,
              bimestre,
              f_respuesta
         into v_id_derechohabiente,
              v_impt_ap_subsec,
              v_bimestre,
              v_f_respuesta
         FROM ocg_trx
        where id_derechohabiente = v_id_derechohabiente
          and impt_ap_subsec = v_vivienda_97

       insert into safre_tmp:tmp_bim
       values(v_id_derechohabiente,
              v_impt_ap_subsec,
              v_bimestre,
              v_f_respuesta);
      END FOREACH

      foreach
       SELECT id_ocg_ctr_transaccion,
              id_derechohabiente,
              vivienda_97,
              periodo_pago,
              f_transaccion
         INTO v_id_transaccion,
              v_id_derechohabiente,
              v_vivienda_97,
              v_periodo_pago,
              v_f_transaccion
         FROM ocg_ctr_transaccion
        where folio = 1
          and id_derechohabiente = v_id_derechohabiente
          and vivienda_97 = v_vivienda_97

       insert into safre_tmp:tmp_trx
       values(v_id_transaccion,
              v_id_derechohabiente,
              v_vivienda_97,
              v_periodo_pago,
              v_f_transaccion);
      END FOREACH;

      let v_id_transaccion = "";
      let v_bimestre       = "";
   END FOREACH;

   RETURN v_error;

END FUNCTION;


