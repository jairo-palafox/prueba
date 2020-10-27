






CREATE FUNCTION "safreviv".fn_ocg_mig_historia(p_usuario CHAR(20))
   RETURNING SMALLINT

   DEFINE v_error                   smallint;
   DEFINE v_today                   date;
   DEFINE v_p_usuario               char(20);
   DEFINE v_nss                     char(11);
   DEFINE v_id_incons               char(9) ;
   DEFINE v_subproceso              char(3) ;
   DEFINE v_ent_financiera          char(3) ;
   DEFINE v_num_ctr_int             char(18);
   DEFINE v_ap_paterno              char(40);
   DEFINE v_ap_materno              char(40);
   DEFINE v_nombre                  char(40);
   DEFINE v_rfc                     char(13);
   DEFINE v_ssv97                   char(15);
   DEFINE v_f_saldo                 char(10);
   DEFINE v_diagnostico             char(3) ;
   DEFINE v_tpo_credito             char(2) ;
   DEFINE v_f_proceso               char(10);
   DEFINE v_inconsistencia          char(50);
   DEFINE v_curp                    char(18);
   DEFINE v_escritura               char(8) ;
   DEFINE v_notario                 char(4) ;
   DEFINE v_ent_fed_notario         char(8) ;
   DEFINE v_mcpio_notario           char(8) ;
   DEFINE v_reg_publico_propiedad   char(15);
   DEFINE v_folio_real              char(8) ;
   DEFINE v_partida                 char(6) ;
   DEFINE v_foja                    char(8) ;
   DEFINE v_volumen                 char(6) ;
   DEFINE v_libro                   char(6) ;
   DEFINE v_tomo                    char(6) ;
   DEFINE v_seccion                 char(6) ;
   DEFINE v_ent_fed_inmueble        char(4) ;
   DEFINE v_domicilio_inmueble      char(50);
   DEFINE v_valor_avaluo            char(15);
   DEFINE v_monto_credito           char(15);
   DEFINE v_plazo_meses             char(5) ;
   DEFINE v_tpo_moneda              char(2) ;
   DEFINE v_tasa_base               char(20);
   DEFINE v_margen                  char(20);
   DEFINE v_f_otorga                char(10);
   DEFINE v_f_registro_carta        char(10);
   DEFINE v_estado                  char(3) ;
   DEFINE v_usuario_reg_carta       char(40);
   DEFINE v_situacion               char(3) ;
   DEFINE v_f_vigencia              char(10);
   DEFINE v_f_respuesta             char(10);
   DEFINE v_impt_solic_garantia     char(15);
   DEFINE v_f_vencimiento           char(10);
   DEFINE v_impt_utilizado_garantia char(15);
   DEFINE v_solic_saldo             char(3) ;
   DEFINE v_cve_ent_financiera      char(3) ;
   DEFINE v_bimestre_ap_subsec      char(2) ;
   DEFINE v_importe_ap_subsec       char(15);
   DEFINE v_f_liberacion_gtia       char(10);
   DEFINE v_importe_gtia_devuelto   char(15);
   DEFINE v_id_causa_liquida        char(3) ;
   DEFINE v_f_deposito              char(10);
   DEFINE v_mun_inmueble            char(40);
   DEFINE v_genero                  char(3);
   DEFINE v_f_envio                 char(10);
   DEFINE v_impt_ap_subsec          char(15);
   DEFINE v_bimestre                char(2);
   DEFINE v_causa_liquida           char(2);
   DEFINE v_cve_nss_asociado        char(11);
   DEFINE v_num_ctr_int_ef          char(18);
   DEFINE v_incons_1   CHAR(3);
   DEFINE v_incons_2   CHAR(3);
   DEFINE v_incons_3   CHAR(3);
   DEFINE v_incons_4   CHAR(3);
   DEFINE v_incons_5   CHAR(3);
   DEFINE v_incons_6   CHAR(3);
   DEFINE v_incons_7   CHAR(3);
   DEFINE v_incons_8   CHAR(3);
   DEFINE v_incons_9   CHAR(3);
   DEFINE v_incons_10  CHAR(3);
   DEFINE v_incons_11  CHAR(3);
   DEFINE v_incons_12  CHAR(3);
   DEFINE v_incons_13  CHAR(3);
   DEFINE v_incons_14  CHAR(3);
   DEFINE v_incons_15  CHAR(3);
   DEFINE v_incons_16  CHAR(3);
   DEFINE v_incons_17  CHAR(3);
   DEFINE v_incons_18  CHAR(3);
   DEFINE v_incons_19  CHAR(3);
   DEFINE v_incons_20  CHAR(3);
 ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_ocg_mig_historia.trace';
   --TRACE ON;

   LET v_error = 0;
   LET v_today     = today;
   LET v_p_usuario = p_usuario;

   FOREACH
      SELECT nss,
             '',
             subproceso,
             ent_financiera,
             num_ctr_int,
             ap_paterno,
             ap_materno, 
             nombre,
             rfc,
             ssv97,
             f_saldo,
             diagnostico,
             tpo_credito,
             f_proceso,
             situacion,
             incons_1,
             incons_2,
             incons_3,
             incons_4,
             incons_5 ,
             incons_6,
             incons_7,
             incons_8,
             incons_9,
             incons_10,
             incons_11,
             incons_12,
             incons_13,
             incons_14,
             incons_15,
             incons_16,
             incons_17,
             incons_18,
             incons_19,
             incons_20
        INTO v_nss,
             v_id_incons,
             v_subproceso,
             v_ent_financiera,
             v_num_ctr_int,
             v_ap_paterno,
             v_ap_materno,
             v_nombre,
             v_rfc,
             v_ssv97,
             v_f_saldo,
             v_diagnostico,
             v_tpo_credito,
             v_f_proceso,
             v_situacion,
             v_incons_1,
             v_incons_2,
             v_incons_3,
             v_incons_4,
             v_incons_5,
             v_incons_6,
             v_incons_7,
             v_incons_8,
             v_incons_9,
             v_incons_10,
             v_incons_11,
             v_incons_12,
             v_incons_13,
             v_incons_14,
             v_incons_15,
             v_incons_16,
             v_incons_17,
             v_incons_18,
             v_incons_19,
             v_incons_20
        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 001

      LET v_id_incons = seq_mig_incons.NEXTVAL;

      INSERT INTO ocg_mig_inconsistencia
           VALUES(v_id_incons,
                  v_nss,
                  v_subproceso,
                  v_incons_1,
                  v_incons_2,
                  v_incons_3,
                  v_incons_4,
                  v_incons_5,
                  v_incons_6,
                  v_incons_7,
                  v_incons_8,
                  v_incons_9,
                  v_incons_10,
                  v_incons_11,
                  v_incons_12,
                  v_incons_13,
                  v_incons_14,
                  v_incons_15,
                  v_incons_16,
                  v_incons_17,
                  v_incons_18,
                  v_incons_19,
                  v_incons_20,
                  today);

      INSERT INTO ocg_mig_tramite
           VALUES(v_nss,
                  v_id_incons,
                  v_subproceso,
                  v_ent_financiera,
                  v_ap_paterno,
                  v_ap_materno,
                  v_nombre,
                  v_rfc,
                  v_ssv97,
                  v_f_saldo,
                  v_diagnostico,
                  v_tpo_credito,
                  v_f_proceso,
                  v_today,
                  v_situacion,
                  v_p_usuario
                  );

      LET v_nss            = "";
      LET v_id_incons      = "";
      LET v_subproceso     = "";
      LET v_ent_financiera = "";
      LET v_num_ctr_int    = "";
      LET v_ap_paterno     = "";
      LET v_ap_materno     = "";
      LET v_nombre         = "";
      LET v_rfc            = "";
      LET v_ssv97          = "";
      LET v_f_saldo        = "";
      LET v_diagnostico    = "";
      LET v_tpo_credito    = "";
      LET v_f_proceso      = "";
      LET v_inconsistencia = "";
      LET v_incons_1       = "";
      LET v_incons_2       = "";
      LET v_incons_3       = "";
      LET v_incons_4       = "";
      LET v_incons_5       = "";
      LET v_incons_6       = "";
      LET v_incons_7       = "";
      LET v_incons_8       = "";
      LET v_incons_9       = "";
      LET v_incons_10      = "";
      LET v_incons_11      = "";
      LET v_incons_12      = "";
      LET v_incons_13      = "";
      LET v_incons_14      = "";
      LET v_incons_15      = "";
      LET v_incons_16      = "";
      LET v_incons_17      = "";
      LET v_incons_18      = "";
      LET v_incons_19      = "";
      LET v_incons_20      = "";

   END FOREACH
{
   FOREACH
      SELECT nss,
             '',
             ent_financiera,
             num_ctr_int,
             rfc,
             '',
             ap_paterno,
             ap_materno,
             nombre,
             ssv97,
             escritura,
             notario,
             ent_fed_notario,
             mcpio_notario,
             reg_publico_propiedad,
             folio_real,
             partida,
             foja,
             volumen,
             libro,
             tomo,
             seccion,
             ent_fed_inmueble,
             seccion,
             domicilio_inmueble,
             valor_avaluo,
             monto_credito,
             plazo_meses,
             tpo_moneda,
             tasa_base,
             margen,
             tpo_credito,
             f_otorga,
             f_registro_carta,
             diagnostico,
             '',
             usuario_reg_carta,
             situacion,
             f_vigencia,
             f_respuesta,
             f_saldo,
             seccion,
             incons_1,incons_2,incons_3,incons_4,incons_5 ,
             incons_6,incons_7,incons_8,incons_9,incons_10,
             incons_11,incons_12,incons_13,incons_14,incons_15,
             incons_16,incons_17,incons_18,incons_19,incons_20
        INTO v_nss,
             v_id_incons,
             v_ent_financiera,
             v_num_ctr_int,
             v_rfc,
             v_curp,
             v_ap_paterno,
             v_ap_materno,
             v_nombre,
             v_ssv97,
             v_escritura,
             v_notario,
             v_ent_fed_notario,
             v_mcpio_notario,
             v_reg_publico_propiedad,
             v_folio_real,
             v_partida,
             v_foja,
             v_volumen,
             v_libro,
             v_tomo,
             v_seccion,
             v_ent_fed_inmueble,
             v_mun_inmueble,
             v_domicilio_inmueble,
             v_valor_avaluo,
             v_monto_credito,
             v_plazo_meses,
             v_tpo_moneda,
             v_tasa_base,
             v_margen,
             v_tpo_credito,
             v_f_otorga,
             v_f_registro_carta,
             v_diagnostico,
             v_estado,
             v_usuario_reg_carta,
             v_situacion,
             v_f_vigencia,
             v_f_respuesta,
             v_f_saldo,
             v_genero,
             v_inconsistencia
        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 002

      INSERT INTO ocg_mig_formalizacion
           VALUES(v_nss,
                  v_id_incons,
                  v_ent_financiera,
                  v_num_ctr_int,
                  v_rfc,
                  v_curp,
                  v_ap_paterno,
                  v_ap_materno,
                  v_nombre,
                  v_ssv97,
                  v_escritura,
                  v_notario,
                  v_ent_fed_notario,
                  v_mcpio_notario,
                  v_reg_publico_propiedad,
                  v_folio_real,
                  v_partida,
                  v_foja,
                  v_volumen,
                  v_libro,
                  v_tomo,
                  v_seccion,
                  v_ent_fed_inmueble,
                  v_seccion,
                  v_domicilio_inmueble,
                  v_valor_avaluo,
                  v_monto_credito,
                  v_plazo_meses,
                  v_tpo_moneda,
                  v_tasa_base,
                  v_margen,
                  v_tpo_credito,
                  v_f_otorga,
                  v_f_registro_carta,
                  v_diagnostico,
                  v_estado,
                  v_usuario_reg_carta,
                  v_situacion,
                  v_f_vigencia,
                  v_f_respuesta,
                  v_f_saldo,
                  v_seccion,
                  v_inconsistencia,
                  v_today,
                  v_p_usuario
                  );

      LET v_nss                   = "";
      LET v_id_incons             = "";
      LET v_ent_financiera        = "";
      LET v_num_ctr_int           = "";
      LET v_rfc                   = "";
      LET v_curp                  = "";
      LET v_ap_paterno            = "";
      LET v_ap_materno            = "";
      LET v_nombre                = "";
      LET v_ssv97                 = "";
      LET v_escritura             = "";
      LET v_notario               = "";
      LET v_ent_fed_notario       = "";
      LET v_mcpio_notario         = "";
      LET v_reg_publico_propiedad = "";
      LET v_folio_real            = "";
      LET v_partida               = "";
      LET v_foja                  = "";
      LET v_volumen               = "";
      LET v_libro                 = "";
      LET v_tomo                  = "";
      LET v_seccion               = "";
      LET v_ent_fed_inmueble      = "";
      LET v_domicilio_inmueble    = "";
      LET v_valor_avaluo          = "";
      LET v_monto_credito         = "";
      LET v_plazo_meses           = "";
      LET v_tpo_moneda            = "";
      LET v_tasa_base             = "";
      LET v_margen                = "";
      LET v_tpo_credito           = "";
      LET v_f_otorga              = "";
      LET v_f_registro_carta      = "";
      LET v_diagnostico           = "";
      LET v_estado                = "";
      LET v_usuario_reg_carta     = "";
      LET v_situacion             = "";
      LET v_f_vigencia            = "";
      LET v_f_respuesta           = "";
      LET v_f_saldo               = "";
      LET v_seccion               = "";
      LET v_inconsistencia        = "";
      --verificar genero y municippio de inmubele en seccion
   END FOREACH

   FOREACH
      SELECT nss,
             '',
             ent_financiera,
             num_ctr_int,
             impt_solic_garantia,
             f_vencimiento,
             impt_utilizado_garantia,
             tpo_credito,
             '',
             diagnostico,
             '',
             '',
             incons_1,incons_2,incons_3,incons_4,incons_5 ,
             incons_6,incons_7,incons_8,incons_9,incons_10,
             incons_11,incons_12,incons_13,incons_14,incons_15,
             incons_16,incons_17,incons_18,incons_19,incons_20
        INTO v_nss,
             v_id_incons,
             v_ent_financiera,
             v_num_ctr_int,
             v_impt_solic_garantia,
             v_f_vencimiento,
             v_impt_utilizado_garantia,
             v_tpo_credito,
             v_solic_saldo,
             v_diagnostico,
             v_estado,
             v_situacion,
             v_inconsistencia
        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 003

      INSERT INTO ocg_mig_uso_garantia
           VALUES(v_nss,
                  v_id_incons,
                  v_ent_financiera,
                  v_num_ctr_int,
                  v_impt_solic_garantia,
                  v_f_vencimiento,
                  v_impt_utilizado_garantia,
                  v_tpo_credito,
                  v_solic_saldo,
                  v_diagnostico,
                  v_estado,
                  v_situacion,
                  v_inconsistencia,
                  v_today,
                  v_p_usuario
                  );

      LET v_nss                     = "";
      LET v_id_incons               = "";
      LET v_ent_financiera          = "";
      LET v_num_ctr_int             = "";
      LET v_impt_solic_garantia     = "";
      LET v_f_vencimiento           = "";
      LET v_impt_utilizado_garantia = "";
      LET v_tpo_credito             = "";
      LET v_solic_saldo             = "";
      LET v_diagnostico             = "";
      LET v_situacion               = "";
      LET v_inconsistencia          = "";
      LET v_estado                  = "";

   END FOREACH

   FOREACH
      SELECT nss,
             '',
             f_respuesta,
             f_envio,
             ent_financiera,
             num_ctr_int,
             ssv97,
             diagnostico,
             f_vencimiento,
             impt_utilizado_garantia,
             impt_ap_subsec,
             bimestre,
             incons_1,incons_2,incons_3,incons_4,incons_5 ,
             incons_6,incons_7,incons_8,incons_9,incons_10,
             incons_11,incons_12,incons_13,incons_14,incons_15,
             incons_16,incons_17,incons_18,incons_19,incons_20,
             f_proceso,
             causa_liquida,
             cve_nss_asociado
        INTO v_nss,
             v_id_incons,
             v_f_respuesta,
             v_f_envio,
             v_ent_financiera,
             v_num_ctr_int,
             v_ssv97,
             v_diagnostico,
             v_f_vencimiento,
             v_impt_utilizado_garantia,
             v_impt_ap_subsec,
             v_bimestre,
             v_inconsistencia,
             v_f_proceso,
             v_causa_liquida,
             v_cve_nss_asociado
        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 004

      INSERT INTO ocg_mig_ap_subsec
           VALUES(v_nss,
                  v_id_incons,
                  v_f_respuesta,
                  v_f_envio,
                  v_ent_financiera,
                  v_num_ctr_int,
                  v_ssv97,
                  v_diagnostico,
                  v_f_vencimiento,
                  v_impt_utilizado_garantia,
                  v_impt_ap_subsec,
                  v_bimestre,
                  v_inconsistencia,
                  v_f_proceso,
                  v_causa_liquida,
                  v_today,
                  v_p_usuario
                  );
   END FOREACH

   FOREACH
      SELECT nss,
             '',
             ent_financiera,
             num_ctr_int,
             bimestre,
             impt_ap_subsec,
             f_liberacion,
             impt_gtia_devuelto,
             causa_liquida,
             f_dev_pago,
             tpo_credito,
             diagnostico,
             '',
             '',
             incons_1,
             incons_2,
             incons_3,
             incons_4,
             incons_5 ,
             incons_6,
             incons_7,
             incons_8,
             incons_9,
             incons_10,
             incons_11,
             incons_12,
             incons_13,
             incons_14,
             incons_15,
             incons_16,
             incons_17,
             incons_18,
             incons_19,
             incons_20
        INTO v_nss,
             v_id_incons,
             v_cve_ent_financiera,
             v_num_ctr_int_ef,
             v_bimestre_ap_subsec,
             v_importe_ap_subsec,
             v_f_liberacion_gtia,
             v_importe_gtia_devuelto,
             v_id_causa_liquida,
             v_f_deposito,
             v_tpo_credito,
             v_diagnostico,
             v_estado,
             v_situacion,
             v_incons_1,
             v_incons_2,
             v_incons_3,
             v_incons_4,
             v_incons_5,
             v_incons_6,
             v_incons_7,
             v_incons_8,
             v_incons_9,
             v_incons_10,
             v_incons_11,
             v_incons_12,
             v_incons_13,
             v_incons_14,
             v_incons_15,
             v_incons_16,
             v_incons_17,
             v_incons_18,
             v_incons_19,
             v_incons_20

        FROM safre_tmp:tmp_detalle_mig
       WHERE subproceso = 005

      INSERT INTO ocg_mig_liquidacion
           VALUES(v_nss,
                  v_id_incons,
                  v_cve_ent_financiera,
                  v_num_ctr_int_ef,
                  v_bimestre_ap_subsec,
                  v_importe_ap_subsec,
                  v_f_liberacion_gtia,
                  v_importe_gtia_devuelto,
                  v_id_causa_liquida,
                  v_f_deposito,
                  v_tpo_credito,
                  v_diagnostico,
                  v_estado,
                  v_situacion,
                  v_inconsistencia,
                  v_today,
                  v_p_usuario
                  );

      LET v_nss                   = "";
      LET v_id_incons             = "";
      LET v_cve_ent_financiera    = "";
      LET v_num_ctr_int_ef        = "";
      LET v_bimestre_ap_subsec    = "";
      LET v_importe_ap_subsec     = "";
      LET v_f_liberacion_gtia     = "";
      LET v_importe_gtia_devuelto = "";
      LET v_id_causa_liquida      = "";
      LET v_f_deposito            = "";
      LET v_tpo_credito           = "";
      LET v_diagnostico           = "";
      LET v_estado                = "";
      LET v_situacion             = "";
      LET v_inconsistencia        = "";
      LET v_incons_1              = "";
      LET v_incons_2              = "";
      LET v_incons_3              = "";
      LET v_incons_4              = "";
      LET v_incons_5              = "";
      LET v_incons_6              = "";
      LET v_incons_7              = "";
      LET v_incons_8              = "";
      LET v_incons_9              = "";
      LET v_incons_10             = "";
      LET v_incons_11             = "";
      LET v_incons_12             = "";
      LET v_incons_13             = "";
      LET v_incons_14             = "";
      LET v_incons_15             = "";
      LET v_incons_16             = "";
      LET v_incons_17             = "";
      LET v_incons_18             = "";
      LET v_incons_19             = "";
      LET v_incons_20             = "";

   END FOREACH
}
 RETURN v_error;
END FUNCTION;


