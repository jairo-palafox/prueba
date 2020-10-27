






CREATE FUNCTION "safreviv".fn_ocg_acreditado()
   RETURNING SMALLINT

DEFINE v_id_ocg_formalizacion decimal(9,0);
DEFINE v_id_ocg_liquidacion   decimal(9,0);
DEFINE v_error                SMALLINT;
DEFINE v_id_dh                decimal(9,0);
DEFINE v_ef                   smallint;
DEFINE v_nci                  char(18);
DEFINE v_nss                  char(11);
DEFINE v_id_ocg_detalle       decimal(9,0);
DEFINE v_curp                 CHAR(18);
DEFINE v_nombre_af            char(40);
DEFINE v_ap_paterno_af        char(40);
DEFINE v_ap_materno_af        char(40);
DEFINE v_tpo_credito          char(1);

ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_acreditado.trace';
   TRACE ON;

   let v_error = 0;


   FOREACH

      select id_ocg_liquidacion,
             id_derechohabiente,
             cve_ent_financiera,
             num_ctr_int_ef,
             tpo_credito
        into v_id_ocg_liquidacion,
             v_id_dh,
             v_ef,
             v_nci,
             v_tpo_credito
        from ocg_liquidacion
       where situacion = 155
         and id_ocg_formalizacion is null
   --      and id_ocg_tramite is null
         and id_ocg_liquidacion = 608330

      select nss,
             rfc,
             nombre_af,
             ap_paterno_af,
             ap_materno_af
        into v_nss,
             v_curp,
             v_nombre_af,
             v_ap_paterno_af,
             v_ap_materno_af
        from afi_derechohabiente
       where id_derechohabiente = v_id_dh;

      LET v_id_ocg_detalle       = seq_ocg_detalle.nextval;
      LET v_id_ocg_formalizacion = seq_ocg_formalizacion.nextval;

      insert into ocg_detalle
           values (v_id_ocg_detalle,
                   2,
                   v_id_dh,
                   5,
                   today,
                   v_ef,
                   v_nss
                  );
     
      insert into ocg_formalizacion
           values (v_id_ocg_formalizacion,
                   v_id_ocg_detalle,
                   '',
                   v_id_dh,
                   v_ef,
                   v_nci,
                   '',
                   v_curp,
                   v_ap_paterno_af,
                   v_ap_materno_af,
                   v_nombre_af,
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   v_tpo_credito,
                   '',
                   '',
                   1,
                   30,
                   '',
                   155,
                   '',
                   '',
                   '',
                   ''
                  );

      insert into ocg_acreditado
           values (v_id_ocg_formalizacion,
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   '',
                   30,
                   155
                   ); 

      update ocg_liquidacion
         set id_ocg_formalizacion = v_id_ocg_formalizacion
       where id_ocg_liquidacion = v_id_ocg_liquidacion;


   let v_id_ocg_formalizacion = null;
   let v_id_ocg_liquidacion   = null;
   let v_id_dh                = null;
   let v_ef                   = null;
   let v_nci                  = null;
   let v_id_ocg_detalle       = null;
   let v_curp                 = null;
   let v_ap_paterno_af        = null;
   let v_ap_materno_af        = null;
   let v_nombre_af            = null;
                      
   END FOREACH

   RETURN v_error;
END FUNCTION;


