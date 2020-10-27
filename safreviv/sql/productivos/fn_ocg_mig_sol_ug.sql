






CREATE FUNCTION "safreviv".fn_ocg_mig_sol_ug(p_usuario            CHAR(20),
                                  p_id_ocg_ctr_archivo DECIMAL(9,0))

   RETURNING SMALLINT,INTEGER,VARCHAR(250),CHAR(11),DECIMAL(10,0);

   DEFINE vt_f_carga                CHAR(8);
   DEFINE vt_f_respuesta            CHAR(8);
   DEFINE vt_f_vigencia             CHAR(8);
   DEFINE vt_situacion              CHAR(1);
   DEFINE vt_subproceso             CHAR(3);
   DEFINE vt_f_envio                CHAR(8);
   DEFINE vt_ent_financiera         CHAR(3);
   DEFINE vt_nss                    CHAR(11);
   DEFINE vt_num_ctr_int            CHAR(18);
   DEFINE vt_impt_solic_garantia    CHAR(15);
   DEFINE vt_f_vencimiento          CHAR(8);
   DEFINE vt_impt_utilizado_gtia    CHAR(12);
   DEFINE vt_f_proceso              CHAR(8);
   DEFINE vt_tpo_credito            CHAR(1);
   DEFINE vt_foja                   CHAR(8);
   DEFINE vt_seccion                CHAR(6);
   DEFINE vt_usuario_reg_carta      CHAR(8);

   DEFINE vh_f_carga                DATE;
   DEFINE vh_f_respuesta            DATE;
   DEFINE vh_f_vigencia             DATE;
   DEFINE vh_situacion              SMALLINT;
   DEFINE vh_subproceso             SMALLINT;
   DEFINE vh_f_envio                DATE;
   DEFINE vh_ent_financiera         CHAR(3);
   DEFINE vh_nss                    CHAR(11);
   DEFINE vh_num_ctr_int            CHAR(18);
   DEFINE vh_impt_solic_garantia    DECIMAL(12,2);
   DEFINE vh_f_vencimiento          DATE;
   DEFINE vh_impt_utilizado_gtia    DECIMAL(12,2);
   DEFINE vh_f_proceso              DATE;
   DEFINE vh_tpo_credito            CHAR(1);
   DEFINE vh_diagnostico            CHAR(2);

   DEFINE v_solicitud_saldo         SMALLINT;
   DEFINE v_id_referencia_cta       SMALLINT;
   DEFINE v_folio_referencia        SMALLINT;
   DEFINE v_proceso_cod             SMALLINT;
   DEFINE v_folio                   SMALLINT;
   DEFINE v_f_transaccion           DATE;
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_f_pago                  DATE;
   DEFINE v_concepto                SMALLINT;
   DEFINE v_edo_trx                 SMALLINT;
   DEFINE v_curp                    CHAR(18);
   DEFINE v_f_liquida_cofi          DATE;
   DEFINE v_f_formalizacion         DATE;

   DEFINE v_tot_reg                 DECIMAL(10,0);

   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_id_ocg_detalle          DECIMAL(9,0);
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0);
   DEFINE v_id_ocg_sol_ug           DECIMAL(9,0);
   DEFINE v_id_ocg_ctr_arh          DECIMAL(9,0);
   DEFINE v_id_ocg_ctr_transaccion  DECIMAL(9,0);

   DEFINE v_f_registro              DATE;
   DEFINE v_estado                  SMALLINT;
   DEFINE v                         SMALLINT;
   DEFINE v_reg_rch                 SMALLINT;
   DEFINE v_numero                  SMALLINT;
   DEFINE v_sqry                    CHAR(100);

   DEFINE v_cod_error               SMALLINT;
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);
   DEFINE v_c_nss                   CHAR(11);

   ON EXCEPTION SET v_cod_error
      -- Devolverá el código de error que ocasione la excepción
      LET v_c_msj = "REGISTRO CON EXCEPCIONES";

      RETURN v_cod_error  ,
             v_isam_err   ,
             v_c_msj      ,
             vt_nss       ,
             v_tot_reg;
   END EXCEPTION

   SET DEBUG FILE TO '/safreviv_int/BD/ocgIntegSolUG.trace';
   TRACE ON;

   ----Iniciliza variables
   LET v_cod_error               = 0;
   LET v_isam_err                = 0;
   LET v_c_msj                   = "REGISTROS PROCESADOS";
   LET v_c_nss                   = "";

   LET vt_f_carga                = "";
   LET vt_f_respuesta            = "";
   LET vt_f_vigencia             = "";
   LET vt_situacion              = "";
   LET vt_subproceso             = "";
   LET vt_f_envio                = "";
   LET vt_ent_financiera         = "";
   LET vt_nss                    = "";
   LET vt_num_ctr_int            = "";
   LET vt_impt_solic_garantia    = "";
   LET vt_f_vencimiento          = "";
   LET vt_impt_utilizado_gtia    = "";
   LET vt_f_proceso              = "";
   LET vt_tpo_credito            = "";
   LET vt_foja                   = "";
   LET vt_seccion                = "";
   LET vt_usuario_reg_carta      = "";

   LET vh_f_carga                = "";
   LET vh_f_respuesta            = "";
   LET vh_f_vigencia             = "";
   LET vh_situacion              = 110;
   LET vh_subproceso             = 3;
   LET vh_f_envio                = "";
   LET vh_ent_financiera         = "";
   LET vh_nss                    = "";
   LET vh_num_ctr_int            = "";
   LET vh_impt_solic_garantia    = 0;
   LET vh_f_vencimiento          = "";
   LET vh_impt_utilizado_gtia    = 0;
   LET vh_f_proceso              = "";
   LET vh_tpo_credito            = "";
   LET vh_diagnostico            = "01";

   LET v_tot_reg                 = 0;

   LET v_id_derechohabiente      = 0;
   LET v_id_ocg_detalle          = 0;
   LET v_id_ocg_tramite          = "";
   LET v_id_ocg_formalizacion    = 0;
   LET v_id_ocg_sol_ug           = 0;
   LET v_id_ocg_ctr_arh          = 1;
   LET v_id_ocg_ctr_transaccion  = 0;

   LET v_solicitud_saldo         = 1;
   LET v_id_referencia_cta       = 1;
   LET v_folio_referencia        = 1;
   LET v_proceso_cod             = 3906;
   LET v_folio                   = 1;
   LET v_f_transaccion           = "";
   LET v_periodo_pago            = "";
   LET v_f_pago                  = "";
   LET v_concepto                = 417;
   LET v_edo_trx                 = 80;
   LET v_f_liquida_cofi          = "";

   LET v_estado                  = 20;
   LET v_f_registro              = TODAY;

   LET v_id_ocg_formalizacion    = 0;

   --------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS INEXISTENTES EN CATÁLOGO --
   -- DE DERECHOHABIENTES                                --
   --------------------------------------------------------

   DROP TABLE IF EXISTS tmp_sp3;

   SELECT *
     FROM tmp_detalle_mig
    WHERE subproceso = "003"
      AND situacion in("D", "E", "T")
   INTO TEMP tmp_sp3;

   create index tmp_sp31 on tmp_sp3(nss);

   update statistics for table tmp_sp3;

   ------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS EXISTENTES EN CATÁLOGO --
   -- DE DERECHOHABIENTES                              --
   ------------------------------------------------------

   FOREACH
      SELECT t3.f_carga                ,
             t3.f_respuesta            ,
             t3.f_vigencia             ,
             t3.situacion              ,
             t3.subproceso             ,
             t3.f_envio                ,
             t3.ent_financiera         ,
             t3.nss                    ,
             t3.num_ctr_int            ,
             t3.impt_solic_garantia    ,
             t3.f_vencimiento          ,
             t3.impt_utilizado_garantia,
             t3.f_proceso              ,
             t3.tpo_credito            ,
             t3.foja                   ,
             t3.seccion                ,
             t3.usuario_reg_carta      ,
             a.id_derechohabiente      ,
             a.curp
        INTO vt_f_carga                ,
             vt_f_respuesta            ,
             vt_f_vigencia             ,
             vt_situacion              ,
             vt_subproceso             ,
             vt_f_envio                ,
             vt_ent_financiera         ,
             vt_nss                    ,
             vt_num_ctr_int            ,
             vt_impt_solic_garantia    ,
             vt_f_vencimiento          ,
             vt_impt_utilizado_gtia    ,
             vt_f_proceso              ,
             vt_tpo_credito            ,
             vt_foja                   ,
             vt_seccion                ,
             vt_usuario_reg_carta      ,
             v_id_derechohabiente      ,
             v_curp
        FROM tmp_sp3 t3,
             afi_ocg a
       WHERE t3.nss = a.nss
      ORDER BY t3.situacion DESC

      LET vh_f_proceso           = vt_f_proceso;
      LET vh_f_envio             = vt_f_envio;
      LET vh_f_carga             = vt_f_carga;
      LET vh_f_respuesta         = vt_f_respuesta;
      LET vh_f_vencimiento       = vt_f_vencimiento;
      LET vh_ent_financiera      = vt_ent_financiera;
      LET vh_num_ctr_int         = vt_num_ctr_int;
      LET vh_impt_solic_garantia = vt_impt_solic_garantia;
      LET vh_impt_solic_garantia = vh_impt_solic_garantia / 100;
      LET vh_impt_utilizado_gtia = vt_impt_utilizado_gtia;
      LET vh_tpo_credito         = vt_tpo_credito;

      IF vh_tpo_credito = "" OR vh_tpo_credito = " "THEN
         LET vh_tpo_credito = "A";
      END IF

      IF vt_seccion = "SOLINF" AND vt_usuario_reg_carta = "OPERESGE" THEN
         LET v_concepto = 317;
      END IF

      IF vt_seccion = "REZARE" AND vt_usuario_reg_carta = "ARP50018" THEN
         LET v_concepto = 317;
      END IF

      IF vt_situacion = "T" THEN
         LET vh_situacion = 110;
      ELIF vt_situacion = "D" THEN
         LET vh_situacion = 50;
      ELSE
         LET vh_situacion = 90;
      END IF

      SELECT MAX(f_formalizacion)
        INTO v_f_formalizacion
        FROM safre_tmp:tmp_acred_43 t
       WHERE t.id_derechohabiente = v_id_derechohabiente
         AND t.f_formalizacion <= vh_f_proceso;

      SELECT MIN(t1.id_ocg_formalizacion)
        INTO v_id_ocg_formalizacion
        FROM safre_tmp:tmp_acred_43 t1
       WHERE t1.id_derechohabiente = v_id_derechohabiente
         AND t1.f_formalizacion = v_f_formalizacion;

      IF v_id_ocg_formalizacion IS NULL OR v_id_ocg_formalizacion = 0 THEN
         SELECT MIN(t1.id_ocg_formalizacion)
           INTO v_id_ocg_formalizacion
           FROM safre_tmp:tmp_acred_43 t1
          WHERE t1.id_derechohabiente = v_id_derechohabiente;
      END IF

    {
      SELECT MIN(acr.id_ocg_formalizacion)
        INTO v_id_ocg_formalizacion
        FROM safre_viv:ocg_acreditado acr, safre_viv:ocg_formalizacion frl
       WHERE frl.id_derechohabiente   = v_id_derechohabiente
         AND frl.id_ocg_formalizacion = acr.id_ocg_formalizacion
         AND acr.f_formalizacion = (SELECT MAX(f_formalizacion)
                                      FROM safre_viv:ocg_acreditado ac2, safre_viv:ocg_formalizacion fr2
                                     WHERE fr2.id_derechohabiente   = v_id_derechohabiente
                                       AND fr2.id_ocg_formalizacion = ac2.id_ocg_formalizacion
                                       AND ac2.f_formalizacion      < vh_f_proceso);
    }

      LET v_id_ocg_detalle = safre_viv:seq_ocg_detalle.NEXTVAL;
      LET v_id_ocg_sol_ug  = safre_viv:seq_ocg_solic_ug.NEXTVAL;

      ---INSERT INTO safre_viv:ocg_detalle
      INSERT INTO tmp_ocg_detalle_ug
      VALUES (v_id_ocg_detalle,
              v_id_ocg_ctr_arh,
              v_id_derechohabiente,
              vh_subproceso,
              vh_f_proceso,
              vh_ent_financiera,
              vt_nss);

      --INSERT INTO safre_viv:ocg_solicitud_uso_garantia
      INSERT INTO tmp_ocg_solicitud_uso_garantia
      VALUES(v_id_ocg_sol_ug,
             v_id_ocg_detalle,
             v_id_ocg_formalizacion,
             v_id_ocg_tramite,
             v_id_derechohabiente,
             vh_ent_financiera,
             vh_num_ctr_int,
             vh_impt_solic_garantia,
             vh_f_vencimiento,
             vh_impt_utilizado_gtia,
             vh_tpo_credito,
             v_solicitud_saldo,
             vh_diagnostico,
             v_estado,
             vh_situacion);

      ---INSERT INTO safre_viv:ocg_fecha_mig
      INSERT INTO tmp_ocg_fecha_mig_ug
      VALUES(v_id_ocg_sol_ug,
             v_id_ocg_detalle,
             v_id_derechohabiente,
             vh_f_envio,
             vh_f_carga,
             vh_f_respuesta,
             v_f_liquida_cofi,
             vh_subproceso,
             v_f_registro);

      IF vt_situacion = "T" THEN
         LET v_id_ocg_ctr_transaccion = safre_viv:seq_dis_ctr_aps_tns.NEXTVAL;
         LET v_periodo_pago           = YEAR(vh_f_vencimiento)||LPAD(MONTH(vh_f_vencimiento),2,0);
         LET v_f_pago                 = vh_f_respuesta;

         --INSERT INTO safre_viv:ocg_ctr_transaccion
         INSERT INTO tmp_ocg_ctr_transaccion_ug
         VALUES(v_id_ocg_ctr_transaccion,
                v_id_ocg_formalizacion,
                v_id_ocg_detalle,
                v_id_derechohabiente,
                v_id_referencia_cta,
                v_folio_referencia,
                v_proceso_cod,
                vh_ent_financiera,
                vh_num_ctr_int,
                v_folio,
                v_f_transaccion,
                vt_nss,
                v_curp,
                vh_impt_utilizado_gtia,
                v_periodo_pago,
                v_f_pago,
                v_concepto,
                vh_f_proceso,
                v_edo_trx);
      END IF

      LET v_tot_reg = v_tot_reg + 1;

      LET vt_f_carga                = "";
      LET vt_f_respuesta            = "";
      LET vt_f_vigencia             = "";
      LET vt_situacion              = "";
      LET vt_subproceso             = "";
      LET vt_f_envio                = "";
      LET vt_ent_financiera         = "";
      LET vt_nss                    = "";
      LET vt_num_ctr_int            = "";
      LET vt_impt_solic_garantia    = "";
      LET vt_f_vencimiento          = "";
      LET vt_impt_utilizado_gtia    = "";
      LET vt_f_proceso              = "";
      LET vt_tpo_credito            = "";
      LET vt_foja                   = "";
      LET vt_seccion                = "";
      LET vt_usuario_reg_carta      = "";
      LET vh_f_carga                = "";
      LET vh_f_respuesta            = "";
      LET vh_f_vigencia             = "";
      LET vh_f_envio                = "";
      LET vh_ent_financiera         = "";
      LET vh_nss                    = "";
      LET vh_num_ctr_int            = "";
      LET vh_impt_solic_garantia    = 0;
      LET vh_f_vencimiento          = "";
      LET vh_impt_utilizado_gtia    = 0;
      LET vh_f_proceso              = "";
      LET vh_tpo_credito            = "";
      LET v_id_derechohabiente      = 0;
      LET v_f_transaccion           = "";
      LET v_periodo_pago            = "";
      LET v_f_pago                  = "";
      LET v_f_liquida_cofi          = "";
      LET v_id_ocg_formalizacion    = 0;
      LET v_concepto                = 417;
   END FOREACH;

   RETURN v_cod_error  ,
          v_isam_err   ,
          v_c_msj      ,
          vt_nss       ,
          v_tot_reg;

END FUNCTION;


