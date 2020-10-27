






CREATE FUNCTION "safreviv".fn_ocg_mig_tramite()
   RETURNING SMALLINT

   DEFINE v_error               smallint;
   DEFINE v_ent_financiera      smallint;
   DEFINE v_id_dh               decimal(9,0);
   DEFINE v_rfc                 char(13);
   DEFINE v_paterno             char(40);
   DEFINE v_materno             char(40);
   DEFINE v_nombre              char(40);
   DEFINE v_viv97               decimal(12,2);
   DEFINE v_f_saldo             date;
   DEFINE v_tpo_credito         char(1);
   DEFINE v_diagnostico         char(2);
   DEFINE v_situacion           char(3);
   DEFINE v_cons_tramite        decimal(9,0);
   DEFINE v_cons_detalle        decimal(9,0);
   DEFINE v_f_proceso           date;
   DEFINE v_nss                 char(11);
   DEFINE v_f_vigencia          date;
   DEFINE v_f_respuesta         date;
   DEFINE v_f_envio             date;
   DEFINE v_f_carga             date;
   DEFINE v_f_liq_cofi          date;
   DEFINE v_nss_migra           char(11);
   DEFINE v_id_ocg_detalle      decimal(9,0);
   DEFINE v_id_ocg_tramite      decimal(9,0);
   DEFINE v_id_ocg_formalizacion decimal(9,0);
   DEFINE v_valor_avaluo        decimal(15,2);
   DEFINE v_monto_credito       decimal(15,2);
   DEFINE v_f_otorga            date;
   DEFINE v_nss_especial_migra  char(11);
   DEFINE v_cuenta_nss          smallint;
   DEFINE v_id_ocg_detalle1     decimal(9,0);
   DEFINE v_id_ocg_ctr_archivo1 decimal(9,0);
   DEFINE v_id_derechohabiente1 decimal(9,0);
   DEFINE v_subproceso1         smallint;
   DEFINE v_cve_ent_financiera1 smallint;
   DEFINE v_nss1                char(11);
   DEFINE v_f_proceso1          date;
   DEFINE v_estado              smallint;


ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   LET v_error = 0;
   LET v_nss = "";

 --  SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_mig_tramite.trace';
 --  TRACE ON;

{
-- funcionalidad para migrar sp001 de aceptados y no reconocidos

   drop table if exists tmp_tramite_migra;
   create table tmp_tramite_migra
   (
  tpo_registro char(2),
  f_carga char(8),
  f_respuesta char(8),
  f_vigencia char(8),
  situacion char(1),
  subproceso char(3),
  f_envio char(8),
  ent_financiera char(3),
  nss char(11),
  num_ctr_int char(18),
  rfc char(13),
  ap_paterno char(40),
  ap_materno char(40),
  nombre char(40),
  ssv97 char(10),
  f_saldo char(8),
  diagnostico char(2),
  escritura char(8),
  notario char(4),
  ent_fed_notario char(2),
  mcpio_notario char(3),
  reg_publico_propiedad char(15),
  folio_real char(8),
  partida char(6),
  foja char(8),
  volumen char(6),
  libro char(6),
  tomo char(6),
  seccion char(6),
  ent_fed_inmueble char(2),
  domicilio_inmueble char(30),
  valor_avaluo char(15),
  monto_credito char(15),
  plazo_meses char(5),
  tpo_moneda char(2),
  tasa_base char(20),
  margen char(20),
  f_otorga char(8),
  impt_solic_garantia char(15),
  f_vencimiento char(8),
  impt_utilizado_garantia char(12),
  impt_ap_subsec char(12),
  bimestre char(6),
  impt_ap_devuelto char(15),
  f_liberacion char(8),
  impt_gtia_devuelto char(15),
  f_registro_carta char(8),
  usuario_reg_carta char(8),
  incons_1 char(2),
  incons_2 char(2),
  incons_3 char(2),
  incons_4 char(2),
  incons_5 char(2),
  incons_6 char(2),
  incons_7 char(2),
  incons_8 char(2),
  incons_9 char(2),
  incons_10 char(2),
  incons_11 char(2),
  incons_12 char(2),
  incons_13 char(2),
  incons_14 char(2),
  incons_15 char(2),
  incons_16 char(2),
  incons_17 char(2),
  incons_18 char(2),
  incons_19 char(2),
  incons_20 char(2),
  f_proceso char(8),
  f_dev_pago char(8),
  causa_liquida char(2),
  tpo_credito char(1),
  cve_nss_asociado char(1),
  nss_conyuge char(11)
);

   insert into tmp_tramite_migra
   select *
     from safre_tmp:tmp_detalle_mig
    where situacion in ("A","N");

   let v_cuenta_nss = 0;

   FOREACH
      select nss,count(*)
        into v_nss_especial_migra,
             v_cuenta_nss
        from tmp_tramite_migra
      group by 1
      having count(*) > 1

      IF v_cuenta_nss > 1 THEN

         update tmp_tramite_migra
            set diagnostico = 99
         where nss = v_nss_especial_migra;

         let v_nss_especial_migra = "";
         let v_cuenta_nss         = 0;
      END IF
   END FOREACH
}
  FOREACH
      select t.ent_financiera,
             a.id_derechohabiente,
             t.rfc,
             t.ap_paterno,
             t.ap_materno,
             t.nombre,
             t.ssv97,
             t.f_saldo,
             t.tpo_credito,
             t.f_vigencia,
             t.f_respuesta,
             '01',
             situacion,
             f_envio,
             f_carga,
             foja
        into v_ent_financiera,
             v_id_dh,
             v_rfc,
             v_paterno,
             v_materno,
             v_nombre,
             v_viv97,
             v_f_saldo,
             v_tpo_credito,
             v_f_vigencia,
             v_f_respuesta,
             v_diagnostico,
             v_situacion,
             v_f_envio,
             v_f_carga,
             v_f_liq_cofi
        from tmp_tramite_migra t,afi_derechohabiente a
       where t.nss = a.nss
         and t.situacion in ("A","N")
         and t.diagnostico = 99

            LET v_cons_detalle = seq_ocg_detalle.NEXTVAL;
            LET v_cons_tramite = seq_ocg_tramite.NEXTVAL;

            IF v_situacion = "A" THEN
               LET v_situacion = 50;
            END IF

            IF v_situacion = "N" THEN
               LET v_situacion = 25;
            END IF


            IF v_tpo_credito is null or
               v_tpo_credito = ' ' then
               let v_tpo_credito = 'A';
            END IF

            IF v_diagnostico = 1 THEN
               LET v_estado = 70;
            ELSE
               LET v_estado = 60;
            END IF

            INSERT INTO ocg_tramite
                 values (v_cons_tramite,
                         v_cons_detalle,
                         v_ent_financiera,
                         v_id_dh,
                         v_rfc,
                         '',
                         v_paterno,
                         v_materno,
                         v_nombre,
                         '',
                         v_viv97,
                         v_f_saldo,
                         v_tpo_credito,
                         v_f_vigencia,
                         v_f_respuesta,
                         v_diagnostico,
                         '',
                         v_situacion);

               INSERT INTO ocg_fecha_mig
                    VALUES (v_cons_tramite,
                            v_cons_detalle,
                            v_id_dh,
                            v_f_envio,
                            v_f_carga,
                            v_f_respuesta,
                            v_f_liq_cofi,
                            "1",
                            today);

               LET v_id_dh        = "";
               LET v_rfc          = "";
               LET v_paterno      = "";
               LET v_materno      = "";
               LET v_nombre       = "";
               LET v_viv97        = "";
               LET v_f_saldo      = "";
               LET v_tpo_credito  = "";
               LET v_diagnostico  = "";
               LET v_situacion    = "";
               LET v_f_vigencia   = "";
               LET v_f_respuesta  = "";
               LET v_f_envio      = "";
               LET v_f_carga      = "";
               LET v_f_liq_cofi   = "";


   END FOREACH

-- funacionalidad para agregar datos de tramite en detalle para A
   FOREACH


     -- insert into ocg_detalle
      select tr.id_ocg_detalle,
             '0',
             tr.id_derechohabiente,
             '1',
             tm.f_proceso,
             tr.cve_ent_financiera,
             tm.nss
        into v_id_ocg_detalle1,
             v_id_ocg_ctr_archivo1,
             v_id_derechohabiente1,
             v_subproceso1,
             v_f_proceso1,
             v_cve_ent_financiera1,
             v_nss1
        from ocg_tramite tr, tmp_tramite_migra tm,afi_derechohabiente af
       where tm.nss = af.nss
         and tr.id_derechohabiente = af.id_derechohabiente
         and tr.id_derechohabiente in (select id_derechohabiente from
             afi_derechohabiente where nss in (select nss from tmp_tramite_migra                                                                                                                                  ))
         and tr.situacion = 50
         and tm.diagnostico = 99


         insert into ocg_detalle
              values(v_id_ocg_detalle1,
                     v_id_ocg_ctr_archivo1,
                     v_id_derechohabiente1,
                     v_subproceso1,
                     v_f_proceso1,
                     v_cve_ent_financiera1,
                     v_nss1);

          let v_id_ocg_detalle1       = "";
          let v_id_ocg_ctr_archivo1   = "";
          let v_id_derechohabiente1   = "";
          let v_subproceso1           = "";
          let v_cve_ent_financiera1   = "";
          let v_nss1                  = "";
          let v_f_proceso1            = "";

   END FOREACH


    FOREACH

      select tr.id_ocg_detalle,
             '0',
             tr.id_derechohabiente,
             '1',
             tm.f_proceso,
             tr.cve_ent_financiera,
             tm.nss
        into v_id_ocg_detalle1,
             v_id_ocg_ctr_archivo1,
             v_id_derechohabiente1,
             v_subproceso1,
             v_f_proceso1,
             v_cve_ent_financiera1,
             v_nss1
        from ocg_tramite tr, tmp_tramite_migra tm,afi_derechohabiente af
       where tm.nss = af.nss
         and tr.id_derechohabiente = af.id_derechohabiente
         and tr.id_derechohabiente in (select id_derechohabiente from
             afi_derechohabiente where nss in (select nss from tmp_tramite_migra                                                                                                                                  ))
         and tr.situacion = 25
         and tm.diagnostico = 99

         insert into ocg_detalle
              values(v_id_ocg_detalle1,
                     v_id_ocg_ctr_archivo1,
                     v_id_derechohabiente1,
                     v_subproceso1,
                     v_f_proceso1,
                     v_cve_ent_financiera1,
                     v_nss1);

          let v_id_ocg_detalle1       = "";
          let v_id_ocg_ctr_archivo1   = "";
          let v_id_derechohabiente1   = "";
          let v_subproceso1           = "";
          let v_cve_ent_financiera1   = "";
          let v_nss1                  = "";
          let v_f_proceso1            = "";

   END FOREACH


   RETURN v_error;
END FUNCTION;


