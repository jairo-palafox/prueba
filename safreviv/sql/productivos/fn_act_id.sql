






CREATE FUNCTION "safreviv".fn_act_id()

   RETURNING SMALLINT

   DEFINE v_id_f    decimal(9,0);
   DEFINE v_id      decimal(9,0);
   DEFINE v_cve_ef  smallint;
   DEFINE v_num_ctr char(18);
   DEFINE v_error   smallint;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_act_id.trace';
   TRACE ON ;

   let v_id_f    = "";
   let v_id      = "";
   let v_cve_ef  = "";
   let v_num_ctr = "";

   FOREACH
      select id_derechohabiente,
             cve_ent_financiera,
             num_ctr_int_ef
        into v_id,
             v_cve_ef,
             v_num_ctr
        from ocg_ctr_transaccion
       where id_ocg_formalizacion is null

      select id_ocg_formalizacion
        into v_id_f
        from ocg_formalizacion
       where id_derechohabiente = v_id
         and cve_ent_financiera = v_cve_ef
         and num_ctr_int_ef     = v_num_ctr;

      update ocg_ctr_transaccion
         set id_ocg_formalizacion = v_id_f
       where id_derechohabiente   = v_id
         and cve_ent_financiera   = v_cve_ef
         and num_ctr_int_ef       = v_num_ctr
         and id_ocg_formalizacion is null;

      let v_id_f    = "";
      let v_id      = "";
      let v_cve_ef  = "";
      let v_num_ctr = "";

   END FOREACH

   RETURN v_error;
END FUNCTION;


