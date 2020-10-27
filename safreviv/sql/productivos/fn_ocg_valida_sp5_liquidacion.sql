






CREATE FUNCTION "safreviv".fn_ocg_valida_sp5_liquidacion(p_id_ocg_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT,INTEGER,INTEGER

   DEFINE v_tpo_trabajador          CHAR(1);
   DEFINE v_tpo_credito             CHAR(1);
   DEFINE v_inconsistencia          CHAR(2);
   DEFINE v_diagnostico             CHAR(2);
   DEFINE v_bim_apor_subsec         CHAR(6);
   DEFINE v_f_envio                 CHAR(10);
   DEFINE v_fecha_envio             DATE;
   DEFINE v_f_libera_garantia       CHAR(10);
   DEFINE v_f_libera_gtia           DATE;
   DEFINE v_f_deposito              CHAR(10);
   DEFINE v_nss                     CHAR(11);
   DEFINE v_num_ctrl_ef             CHAR(18);
   DEFINE v_ax_num_ctrl_ef          CHAR(18);
   DEFINE v_f_proceso               DATE;
   DEFINE v_cve_ent_financiera      DECIMAL(3,0);
   DEFINE v_cve_ent_fin_valida      DECIMAL(3,0);
   DEFINE v_ax_cve_ent_financiera   DECIMAL(3,0);
   DEFINE v_id_ocg_liquidacion      DECIMAL(9,0);
   DEFINE v_id_dh                   DECIMAL(9,0);
   DEFINE v_id_ocg_detalle          DECIMAL(9,0);
   DEFINE v_ax_id_ocg_ctr_arch      DECIMAL(9,0);
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0);
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_id_ocg_tramite_t        DECIMAL(9,0);
   DEFINE v_imp_subsec_devuelto     DECIMAL(13,2);
   DEFINE v_imp_ocg_devuelto        DECIMAL(13,2);
   DEFINE v_cnt_aceptados           INTEGER;
   DEFINE v_cnt_rechazados          INTEGER;
   DEFINE v_tot_regs                INTEGER;
   DEFINE v_cnt_cve_ef              SMALLINT;
   DEFINE v_subproceso              SMALLINT;
   DEFINE v_cnt_vig                 SMALLINT;
   DEFINE v_ind_edo_cuenta          SMALLINT;
   DEFINE v_idx_nss                 SMALLINT;
   DEFINE v_error                   SMALLINT;
   DEFINE v_bnd_inconsistencia      SMALLINT;
   DEFINE v_estado                  SMALLINT;         -- 0 aceptado , 1 rechazado
   DEFINE v_id_causa_liquida        CHAR(1);
   DEFINE v_situacion               SMALLINT;
   DEFINE v_f_formalizacion         DATE;
   DEFINE v_cta_cred_liq            INTEGER;
   DEFINE v_cta_cred_vig            INTEGER;
   DEFINE v_bnd_dev_gta             SMALLINT;
   DEFINE v_bnd_dev_ap              SMALLINT;
   DEFINE v_bnd_liq                 SMALLINT;
   DEFINE v_id_ocg_devolucion       DECIMAL(9,0);
   DEFINE v_imp_revaluado_15        DECIMAL(13,2);
   DEFINE v_imp_f_proceso           DECIMAL(13,2);
   DEFINE v_id_dis                  DECIMAL(9,0);
   DEFINE v_periodo_dis             CHAR(6);
   DEFINE v_f_pago                  DATE;
   DEFINE v_imp_ap_pat              DECIMAL(12,2);
   DEFINE v_aiv_ap_pat              DECIMAL(18,6);
   DEFINE v_f_transaccion           DATE;
   DEFINE v_f_factura               DATE;
   DEFINE v_precio_fondo_today      DECIMAL(19,14);
   DEFINE v_precio_fondo_15         DECIMAL(19,14);
   DEFINE v_ocg_ctr_transaccion     DECIMAL(9,0);
   DEFINE v_cta_liq                 INTEGER;
   DEFINE v_mes                     CHAR(2);
   DEFINE v_periodo                 CHAR(8);
   DEFINE v_ax_cod_error            SMALLINT;
   DEFINE v_f_libera_garantia_d     DATE;
   DEFINE v_dia                     SMALLINT;
   DEFINE v_f_transaccion_d         DATE;
   DEFINE v_f_actual                DATE;
   DEFINE v_rch_marca               SMALLINT;
   DEFINE v_edo_devolucion          SMALLINT;
   DEFINE v_id_ocg_dev              DECIMAL(9,0);
   DEFINE v_cve                     DECIMAL(3,0);
   DEFINE v_imp_ap                  DECIMAL(12,2);
   DEFINE v_gta                     DECIMAL(12,2);
   DEFINE v_f_dep                   DATE;
   DEFINE v_monto_devuelto          DECIMAL(12,2);
   DEFINE v_imp_sol                 DECIMAL(12,2);
   DEFINE v_monto_devolucion        DECIMAL(12,2);
   DEFINE v_monto_disponible        DECIMAL(12,2);
   DEFINE v_bnd_dev                 SMALLINT;
   DEFINE v_imp_pendiente           DECIMAL(12,2);
   DEFINE v_edo_registro            SMALLINT;
   DEFINE v_importe                 DECIMAL(12,2);
   DEFINE v_concepto                SMALLINT;
   DEFINE v_nss_unificado           CHAR(11);
   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_id_dh_unificado         DECIMAL(9,0);
   DEFINE v_diag                    SMALLINT;
   DEFINE v_bnd_cred_liq            SMALLINT;
   DEFINE v_bimestre                SMALLINT;
   DEFINE v_bnd_liq_aux             SMALLINT;
   DEFINE v_curp                    CHAR(18);
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_num_ctr_int             CHAR(18);
   DEFINE v_id_ocg_trx              DECIMAL(9,0);

   ON EXCEPTION SET v_error
      LET v_cnt_aceptados      = 0;
      LET v_cnt_rechazados     = 0;
      RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_valida_sp5_liquidacion.trace';
   TRACE ON;

   LET v_error              = 0;
   LET v_subproceso         = "005";
   LET v_cnt_vig            = 0;
   LET v_f_proceso          = TODAY;
   LET v_ax_id_ocg_ctr_arch = p_id_ocg_ctr_arch;
   LET v_cnt_aceptados      = 0;
   LET v_cnt_rechazados     = 0;
   LET v_idx_nss            = 1;
   LET v_bnd_inconsistencia = 0;
   LET v_f_actual           = TODAY;
   LET v_bnd_dev_ap         = 0;
   LET v_bnd_dev_gta        = 0;
   LET v_bnd_liq            = 0;
   LET v_nss_unificado      = "";
   LET v_nss_unificador     = "";
   LET v_id_dh_unificador   = "";
   LET v_id_dh_unificado    = "";
   LET v_diag               = 0;
   LET v_bnd_cred_liq       = 0;
   LET v_f_libera_garantia  = "";
   LET v_tpo_credito        = "";

   LET v_id_ocg_formalizacion = "";
   LET v_id_ocg_tramite       = "";
   LET v_f_formalizacion      = "";
   LET v_id_ocg_tramite_t     = "";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_envio = ""
    WHERE subproceso = v_subproceso
      AND f_envio = "        ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_envio = ""
    WHERE subproceso = v_subproceso
      AND f_envio = "00000000";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET bim_apor_subsec = ""
    WHERE subproceso = v_subproceso
      AND bim_apor_subsec = "      ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET bim_apor_subsec = ""
    WHERE subproceso = v_subproceso
      AND bim_apor_subsec = "000000";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET bim_apor_subsec = ""
    WHERE subproceso = v_subproceso
      AND bim_apor_subsec = "0";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_deposito = ""
    WHERE subproceso = v_subproceso
      AND f_deposito = "        ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_deposito = ""
    WHERE subproceso = v_subproceso
      AND f_deposito = "00000000";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_deposito = ""
    WHERE subproceso = v_subproceso
      AND f_deposito = "0";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_libera_garantia = ""
    WHERE subproceso = v_subproceso
      AND f_libera_garantia = "        ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_libera_garantia = ""
    WHERE subproceso = v_subproceso
      AND f_libera_garantia = "0";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_libera_garantia = ""
    WHERE subproceso = v_subproceso
      AND f_libera_garantia = "00000000";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET f_libera_garantia = NULL
    WHERE subproceso = v_subproceso
      AND f_libera_garantia = "";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET imp_subsec_devuelto = ""
    WHERE subproceso = v_subproceso
      AND imp_subsec_devuelto = "               ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET imp_subsec_devuelto = ""
    WHERE subproceso = v_subproceso
      AND imp_subsec_devuelto = "000000000000000";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET imp_ocg_devuelto = ""
    WHERE subproceso = v_subproceso
      AND imp_ocg_devuelto = "               ";

   UPDATE safre_tmp:tmp_rec_det_ocg43
      SET imp_ocg_devuelto = ""
    WHERE subproceso = v_subproceso
      AND imp_ocg_devuelto = "000000000000000";

   FOREACH
      SELECT cve_ent_financiera ,
             nss                ,
             num_ctrl_ef        ,
             f_envio[5,6]||"/"|| f_envio[7,8]||"/"|| f_envio[1,4],
             bim_apor_subsec    ,
             imp_subsec_devuelto,
             causa_liquidacion  ,
             f_deposito[5,6]||"/"|| f_deposito[7,8]||"/"|| f_deposito[1,4],
             cred_convenidos    ,
             f_libera_garantia[5,6]||"/"|| f_libera_garantia[7,8] ||"/"|| f_libera_garantia[1,4],
             imp_ocg_devuelto
        INTO v_cve_ent_financiera ,
             v_nss                ,
             v_num_ctrl_ef        ,
             v_f_envio            ,
             v_bim_apor_subsec    ,
             v_imp_subsec_devuelto,
             v_id_causa_liquida   ,
             v_f_deposito         ,
             v_tpo_credito        ,
             v_f_libera_garantia  ,
             v_imp_ocg_devuelto
        FROM safre_tmp:tmp_rec_det_ocg43
       WHERE subproceso = v_subproceso
     ORDER BY cve_ent_financiera, nss

      -- Se asigna a la variable el valor de la secuencia
      LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      LET v_id_ocg_liquidacion = seq_ocg_liquidacion.nextval;
      LET v_id_ocg_devolucion  = seq_ocg_devolucion.nextval;
      LET v_estado             = 30;
      LET v_bnd_inconsistencia = 0;
      LET v_situacion          = 140;
      LET v_diagnostico        = 01;
      LET v_bnd_dev_ap         = 0;
      LET v_bnd_dev_gta        = 0;
      LET v_bnd_liq            = 0;
      LET v_f_libera_gtia      = v_f_libera_garantia;
      LET v_fecha_envio        = v_f_envio;
      LET v_bnd_liq_aux        = 0;
      LET v_periodo            = "";

      ---IF v_tpo_credito = "" OR v_tpo_credito = " " THEN
         ---LET v_tpo_credito = "A";
      ---END IF

      IF v_tpo_credito <> "7" AND v_tpo_credito <> "8" AND v_tpo_credito <> "C" THEN
         LET v_tpo_credito = "A";
      END IF

      IF v_tpo_credito = "" OR v_tpo_credito = " " THEN
         LET v_tpo_credito = "A";
      END IF

      IF v_bim_apor_subsec = ' ' THEN
         LET v_bim_apor_subsec = NULL;
      END IF

      IF v_imp_subsec_devuelto = ' ' THEN
         LET v_imp_subsec_devuelto = NULL;
      END IF

      IF v_id_causa_liquida = '  ' THEN
         LET v_id_causa_liquida = NULL;
      END IF

      IF v_imp_ocg_devuelto = ' ' THEN
         LET v_imp_ocg_devuelto = NULL;
      END IF

      IF (v_imp_subsec_devuelto IS NOT NULL) AND
         (v_imp_ocg_devuelto IS NULL) THEN
         LET v_importe  = v_imp_subsec_devuelto;
         LET v_concepto = 508;
      END IF

      IF (v_imp_subsec_devuelto IS NULL) AND
         (v_imp_ocg_devuelto IS NOT NULL) THEN
         LET v_importe  = v_imp_ocg_devuelto;
         LET v_concepto = 608;
      END IF

      LET v_imp_subsec_devuelto = v_imp_subsec_devuelto / 100;
      LET v_imp_ocg_devuelto    = v_imp_ocg_devuelto / 100;

      IF (v_f_libera_garantia = "  /  /  ") OR
         (v_f_libera_garantia = "00/00/00") THEN
         LET v_f_libera_garantia = NULL;
         LET v_f_libera_gtia = NULL;
      END IF

      IF (v_f_deposito = "  /  /  ") OR
         (v_f_deposito = "00/00/00")THEN
         LET v_f_deposito = NULL;
      END IF

      --IF v_imp_ocg_devuelto IS NULL OR v_imp_ocg_devuelto < 0 THEN
        --LET v_imp_ocg_devuelto = 0;
      --END IF

      --TRACE 'fecha deposito : ' || v_f_deposito;
      --TRACE 'fecha envio    : ' || v_f_envio;
      --TRACE 'fecha lib gta : ' || v_f_libera_garantia;

      -- Se obtiene el id_derechohabiente
      SELECT id_derechohabiente,
             tipo_trabajador,
             ind_estado_cuenta
        INTO v_id_dh,
             v_tpo_trabajador,
             v_ind_edo_cuenta
        FROM afi_derechohabiente
       WHERE nss = v_nss;

     IF v_id_dh IS NULL THEN
        LET v_id_dh = 0;
     END IF

     -- Si el NSS es incorrecto        #1
      FOR v_idx_nss = 1 TO LENGTH(v_nss)
         IF SUBSTR(v_nss,v_idx_nss,1) NOT MATCHES '[0-9]' THEN
            --TRACE 'El nss no cumple con la validaci.n: ' || v_nss;
            --TRACE 'En la posici.n : ' || v_idx_nss;

            LET v_inconsistencia = "02";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_liquidacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
            EXIT FOR;
         END IF
      END FOR;

      -- Se verifica que exista en la base de derechohabientes     #2
      IF v_id_dh < 0 OR v_id_dh IS NULL THEN
         LET v_inconsistencia = "02";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_liquidacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );

         LET v_inconsistencia = "43";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_liquidacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica si la cuenta es inhabilitada por unificaci.n  #4
      IF v_ind_edo_cuenta <> 0 THEN
         EXECUTE PROCEDURE fn_busca_nss_unificador (v_nss)
                      INTO v_nss_unificador, v_id_dh_unificador, v_diag;

         IF v_diag = 1 THEN
            LET v_nss_unificado   = v_nss;
            LET v_id_dh_unificado = v_id_dh;
            LET v_nss             = v_nss_unificador;
            LET v_id_dh           = v_id_dh_unificador;
            LET v_ind_edo_cuenta  = 0;

           {
            IF EXISTS(
               -- Se verifica que el nss unificado tenga un credito 43 bis vigente        #5
               SELECT UNIQUE f.id_derechohabiente
                 FROM ocg_acreditado a, ocg_formalizacion f
                WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                  AND f.id_derechohabiente   = v_id_dh_unificado
                  AND f.situacion BETWEEN 60 AND 80
                  AND f.diagnostico        = 1) THEN

               UPDATE ocg_formalizacion
                  SET id_derechohabiente = v_id_dh
                WHERE id_ocg_formalizacion IN(
                      SELECT f.id_ocg_formalizacion
                        FROM ocg_acreditado a, ocg_formalizacion f
                       WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                         AND f.id_derechohabiente   = v_id_dh_unificado
                         AND f.situacion BETWEEN 60 AND 80
                         AND f.diagnostico        = 1)
                  AND id_derechohabiente = v_id_dh_unificado;

               UPDATE ocg_tramite
                  SET id_derechohabiente = v_id_dh
                WHERE id_ocg_tramite IN(
                    SELECT f.id_ocg_tramite
                      FROM ocg_acreditado a, ocg_formalizacion f
                      WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                        AND f.id_derechohabiente   = v_id_dh_unificado
                        AND f.situacion BETWEEN 60 AND 80
                        AND f.diagnostico        = 1)
                  AND id_derechohabiente = v_id_dh_unificado;

               UPDATE ocg_detalle
                  SET id_derechohabiente = v_id_dh
                WHERE id_ocg_detalle IN(
                      SELECT f.id_ocg_detalle
                      FROM ocg_acreditado a, ocg_formalizacion f
                      WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                        AND f.id_derechohabiente   = v_id_dh_unificado
                        AND f.situacion BETWEEN 60 AND 80
                        AND f.diagnostico        = 1)
                  AND id_derechohabiente = v_id_dh_unificado;

               UPDATE ocg_fecha_mig
                 SET id_derechohabiente = v_id_dh
               WHERE id_ocg_referencia IN(
                     SELECT f.id_ocg_formalizacion
                     FROM ocg_acreditado a, ocg_formalizacion f
                     WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                     AND f.id_derechohabiente = v_id_dh_unificado
                     AND f.situacion BETWEEN 60 AND 80
                     AND f.diagnostico        = 1)
                 AND id_derechohabiente = v_id_dh_unificado;

               UPDATE ocg_liquidacion_cofi
                  SET id_derechohabiente = v_id_dh
                WHERE id_derechohabiente = v_id_dh_unificado;

               UPDATE ocg_solicitud_uso_garantia
                  SET id_derechohabiente = v_id_dh
                WHERE id_ocg_detalle IN(
                      SELECT f.id_ocg_detalle
                      FROM ocg_acreditado a, ocg_formalizacion f
                      WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                        AND f.id_derechohabiente   = v_id_dh_unificado
                        AND f.situacion BETWEEN 60 AND 80
                        AND f.diagnostico        = 1)
                  AND id_derechohabiente = v_id_dh_unificado;
            END IF
           }

            INSERT INTO safre_tmp:tmp_ocg_uni
            VALUES(v_nss_unificado,
                   v_nss_unificador,
                   5,
                   TODAY);
         ELSE
            LET v_inconsistencia = "43";  --"03";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_liquidacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         END IF
      END IF

      -- Se inserta en la tabla de detalle
      INSERT INTO ocg_detalle
           VALUES( v_id_ocg_detalle,
                   v_ax_id_ocg_ctr_arch,
                   v_id_dh,
                   v_subproceso,
                   v_f_proceso,
                   v_cve_ent_financiera,
                   v_nss );

      -- Se verifica que el nss sea tpo_trabajador = "I" imss #3
      IF v_tpo_trabajador <> "I" THEN
         LET v_inconsistencia = "01";
         LET v_bnd_inconsistencia = 1;

         INSERT INTO ocg_inconsistencia
              VALUES( v_id_ocg_liquidacion,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso );
      END IF

      -- Se verifica si la cuenta es inhabilitada por unificaci.n  #4
      IF v_ind_edo_cuenta <> 0 THEN
      ELSE

         -- en caso de venir informaci.n para liquidaci.n y devoluci.n, se toma registro como devoluci.n
         IF (v_id_causa_liquida     IS NOT NULL) AND
            (v_f_libera_garantia    IS NOT NULL) AND
            (v_bim_apor_subsec      IS NOT NULL) AND
            (v_f_deposito           IS NOT NULL) THEN
            IF (v_imp_subsec_devuelto IS NOT NULL) OR
               (v_imp_ocg_devuelto    IS NOT NULL) THEN
               LET v_id_causa_liquida = NULL;
               LET v_f_libera_garantia= NULL;
               LET v_bnd_liq          = 0   ;
            END IF
         END IF

         -- se identifica si es liquidaci.n o devolucion de saldo #6

         IF (v_id_causa_liquida  IS NOT NULL) AND
            (v_f_libera_garantia IS NOT NULL) THEN
            IF (v_bim_apor_subsec     IS NULL )AND
               (v_imp_subsec_devuelto IS NULL )AND
               (v_f_deposito          IS NULL )AND
               (v_imp_ocg_devuelto    IS NULL )THEN  --- es liquidaci.n de cr.dito

               LET v_bnd_liq_aux = 1;

               -- Se verifica que el nss tenga un credito 43 bis vigente        #5
               SELECT COUNT(*)
                 INTO v_cnt_vig
                 FROM ocg_acreditado a, ocg_formalizacion f
                WHERE a.id_ocg_formalizacion = f.id_ocg_formalizacion
                  AND f.id_derechohabiente = v_id_dh
                  AND f.situacion = 80  ---BETWEEN 60 AND 80
                  AND f.diagnostico        = 1;

               --TRACE 'v_cnt_vig : '|| v_cnt_vig;

               IF v_cnt_vig >= 1 THEN
                  LET v_bnd_liq     = 1;
                  LET v_bnd_dev_ap  = 0;
                  LET v_bnd_dev_gta = 0;
               END IF
            ELSE
               LET v_bnd_liq = 0;
            END IF
         ELSE
            SELECT COUNT(*)
              INTO v_cta_liq
              FROM ocg_liquidacion
             WHERE id_derechohabiente = v_id_dh
               AND situacion  BETWEEN 140 AND 160
               AND diagnostico = 1;

            IF v_cta_liq >= 1 THEN
               IF (v_bim_apor_subsec     IS NOT NULL )AND
                  (v_f_deposito          IS NOT NULL )AND
                  (v_imp_subsec_devuelto IS NOT NULL )THEN
                  LET v_bnd_dev_ap = 1;
               ELSE
                  IF (v_bim_apor_subsec     IS NOT NULL )AND
                     (v_f_deposito          IS NOT NULL )AND
                     (v_imp_ocg_devuelto    IS NOT NULL )THEN
                     LET v_bnd_dev_gta = 1;
                  END IF
               END IF
               SELECT MAX(id_ocg_formalizacion),
                      MAX(id_ocg_tramite)
                 INTO v_id_ocg_formalizacion,
                      v_id_ocg_tramite
                 FROM ocg_liquidacion
                WHERE id_derechohabiente = v_id_dh
                  AND situacion BETWEEN 140 AND 160
                  AND diagnostico = 1;
            ELSE
               IF v_f_libera_garantia IS NULL THEN
                  LET v_inconsistencia = "51";
                  LET v_bnd_inconsistencia = 1;

                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF

               IF v_id_causa_liquida IS NULL THEN
                  LET v_inconsistencia = "54";
                  LET v_bnd_inconsistencia = 1;
                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF

{
               IF v_tpo_credito IS NULL THEN
                  LET v_inconsistencia = "42";

                  INSERT INTO ocg_inconsistencia
                       VALUES(v_id_ocg_liquidacion,
                              v_subproceso,
                              v_inconsistencia,
                              v_f_proceso );
               END IF
}
            END IF

            IF (v_bnd_liq    = 1) THEN
               IF (v_bnd_dev_ap = 1) OR
                  (v_bnd_dev_gta= 1) THEN
                  LET v_inconsistencia = "58";
                  LET v_bnd_inconsistencia = 1;

                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               ENd IF
            END IF
--**************************************************************
         END IF

         SELECT COUNT(*)
           INTO v_cnt_cve_ef
           FROM cat_entidad_financiera
          WHERE cve_ent_financiera = v_cve_ent_financiera;

         -- Se valida que la entidad financiera exista en el catalogo        #8
         IF v_cnt_cve_ef = 0 THEN
            LET v_inconsistencia = "26";
            LET v_bnd_inconsistencia = 1;

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_liquidacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );
         ELSE
            -- Se valida que sea la misma entidad financiera que en el proceso de formalizaci.n #8.1
            SELECT num_ctr_int_ef,
                   cve_ent_financiera
              INTO v_ax_num_ctrl_ef,
                   v_ax_cve_ent_financiera
              FROM ocg_formalizacion
             WHERE id_derechohabiente = v_id_dh
               AND diagnostico = 1
               AND situacion >= 60
               AND situacion < 140;

            IF (v_bnd_dev_ap = 1) OR
               (v_bnd_dev_gta= 1) THEN
            ELSE
               IF v_ax_cve_ent_financiera <> v_cve_ent_financiera THEN
                  LET v_inconsistencia     = "26";
                  LET v_bnd_inconsistencia = 1;

                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF
            END IF
         END IF

        -- Se valida que la fecha de env.o sea menor o igual a hoy        #9
        IF v_fecha_envio > v_f_proceso THEN
           LET v_inconsistencia = "46";
           LET v_bnd_inconsistencia = 1;

           INSERT INTO ocg_inconsistencia
                VALUES( v_id_ocg_liquidacion,
                        v_subproceso,
                        v_inconsistencia,
                        v_f_proceso );
        END IF

        -- Se valida la marca de cr.dito #10
        IF v_bnd_liq = 1 THEN
           --TRACE "V_ID_CAUSA_LIQUIDA : "||v_id_causa_liquida;
           IF v_id_causa_liquida <> 1 THEN
              IF v_id_causa_liquida <> 2 THEN
                 IF v_id_causa_liquida <> 3 THEN
                    IF v_id_causa_liquida <> 4 THEN
                       IF v_id_causa_liquida <> 5 THEN
                          LET v_inconsistencia = "54";
                          LET v_bnd_inconsistencia = 1;
                          INSERT INTO ocg_inconsistencia
                               VALUES(v_id_ocg_liquidacion,
                                      v_subproceso,
                                      v_inconsistencia,
                                      v_f_proceso );
                       END IF
                    END IF
                 END IF
              END IF
           END IF
        --END IF

        -- Se recupera el id_ocg_formalizacion
           SELECT f.id_ocg_formalizacion,
                  ---f.num_ctr_int_ef,
                  f.id_ocg_tramite,
                  f.f_registro_carta
             INTO v_id_ocg_formalizacion,
                  ---v_num_ctrl_ef,
                  v_id_ocg_tramite,
                  v_f_formalizacion
             FROM ocg_acreditado a, ocg_formalizacion f
            WHERE f.id_derechohabiente   = v_id_dh
              AND a.id_ocg_formalizacion = f.id_ocg_formalizacion
              AND f.situacion BETWEEN 60 AND 80
              AND a.situacion            = f.situacion
              AND f.diagnostico          = 1;

        END IF
{
        IF v_id_ocg_formalizacion IS NULL THEN
           LET v_id_ocg_formalizacion = 0;   -- Pendiente  se usa el id para probar

        -- se valida que el numero de control interno sea el mismo que en formalizaci.n
        ELIF v_ax_num_ctrl_ef = v_num_ctrl_ef THEN
           LET v_inconsistencia     = "47";
           LET v_bnd_inconsistencia = 1;

           INSERT INTO ocg_inconsistencia
                VALUES( v_id_ocg_liquidacion,
                        v_subproceso,
                        v_inconsistencia,
                        v_f_proceso );
        END IF
}


        -- sel valida que no venga nula la fecha de liberaci.n #11
        IF v_bnd_liq = 1 THEN
           IF v_f_libera_garantia IS NULL THEN
              LET v_inconsistencia     = "51";
              LET v_bnd_inconsistencia = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES( v_id_ocg_liquidacion,
                           v_subproceso,
                           v_inconsistencia,
                           v_f_proceso );
           END IF

          -- se valida que la fecha de liberacion sea mayor a la fecha de formalizaci.n #12
           IF v_tpo_credito <> "7" AND v_tpo_credito <> "8" THEN
              IF v_f_libera_gtia < v_f_formalizacion THEN
                 LET v_inconsistencia     = "51";
                 LET v_bnd_inconsistencia = 1;

                 INSERT INTO ocg_inconsistencia
                      VALUES( v_id_ocg_liquidacion,
                              v_subproceso,
                              v_inconsistencia,
                              v_f_proceso );
              END IF
           END IF

        -- se valida que la fecha de liberacion sea menor a la fecha de envio #13
           IF v_f_libera_gtia > v_fecha_envio THEN
              LET v_inconsistencia     = "51";
              LET v_bnd_inconsistencia = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES( v_id_ocg_liquidacion,
                           v_subproceso,
                           v_inconsistencia,
                           v_f_proceso );
           END IF
        END IF

        IF v_bnd_dev_ap = 1 THEN
           IF v_bim_apor_subsec[5,6] > 06 THEN
              LET v_inconsistencia     = "";
              LET v_bnd_inconsistencia = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES( v_id_ocg_liquidacion,
                           v_subproceso,
                           v_inconsistencia,
                           v_f_proceso );

              LET v_periodo = v_bim_apor_subsec;
           ELSE
              {
              LET v_bimestre =  v_bim_apor_subsec[5,6];
              SELECT mes_desde
                INTO v_mes
                FROM cat_bimestre
               WHERE bimestre = v_bimestre;
               }
              LET v_periodo = v_bim_apor_subsec;   --v_mes||v_bim_apor_subsec[1,4];

           END IF
        END IF

        IF v_bnd_dev_gta = 1 THEN
           IF v_bim_apor_subsec[5,6] > 12 THEN
              LET v_inconsistencia     = " ";
              LET v_bnd_inconsistencia = 1;

              INSERT INTO ocg_inconsistencia
                   VALUES( v_id_ocg_liquidacion,
                           v_subproceso,
                           v_inconsistencia,
                           v_f_proceso );
                 LET v_periodo = v_bim_apor_subsec;
           ELSE
              {
              LET v_bimestre =  v_bim_apor_subsec[5,6];

              SELECT mes_desde
                INTO v_mes
                FROM cat_bimestre
               WHERE bimestre = v_bimestre;
               }
              LET v_periodo =  v_bim_apor_subsec; --v_bim_apor_subsec[5,6]||v_bim_apor_subsec[1,4];
           END IF
        END IF

        -- Se valida si existieron inconsistencias
        IF v_bnd_inconsistencia = 1 THEN
           LET v_diagnostico    = 02;
           LET v_cnt_rechazados = v_cnt_rechazados + 1;
           LET v_estado         = 60;
           LET v_situacion      = 20;
        ELSE
           LET v_cnt_aceptados = v_cnt_aceptados + 1;
        END IF

        --TRACE 'bandera liq     : ' || v_bnd_liq;
        --TRACE 'bandera dev_ap  : ' || v_bnd_dev_ap;
        --TRACE 'bandera dev_gta : ' || v_bnd_dev_gta;

      IF (v_bnd_liq    = 0) THEN
         IF (v_bnd_dev_ap = 0) AND
            (v_bnd_dev_gta= 0) THEN
{
         IF (v_tpo_credito = "7" ) OR
            (v_tpo_credito = "8" ) AND
            (v_bnd_inconsistencia <> 1) THEN
}
            IF NOT EXISTS (SELECT id_ocg_formalizacion
                             FROM ocg_formalizacion
                            WHERE id_derechohabiente = v_id_dh
                              AND diagnostico = 1
                              AND situacion   BETWEEN 55 AND 80) AND
                          (v_bnd_inconsistencia <> 1 )THEN

               LET v_id_ocg_formalizacion = null;

               --TRACE 'tpo_credito2 : ' || v_tpo_credito;

               IF EXISTS (SELECT id_ocg_tramite
                            FROM ocg_tramite
                           WHERE id_derechohabiente = v_id_dh
                             ---AND cve_ent_financiera = v_cve_ent_financiera
                             AND diagnostico = 1
                             AND situacion   = 50
                             AND tpo_credito in (7,8)) THEN

                          SELECT cve_ent_financiera
                            INTO v_cve_ent_fin_valida
                            FROM ocg_tramite
                           WHERE id_derechohabiente = v_id_dh
                             ---AND cve_ent_financiera = v_cve_ent_financiera
                             AND diagnostico = 1
                             AND situacion   = 50
                             AND tpo_credito in (7,8);

                          IF v_cve_ent_fin_valida = v_cve_ent_financiera THEN
                             SELECT MAX(id_ocg_tramite)
                               INTO v_id_ocg_tramite_t
                               FROM ocg_tramite
                              WHERE id_derechohabiente = v_id_dh
                                AND cve_ent_financiera = v_cve_ent_financiera
                                AND diagnostico = 1
                                AND situacion   = 50
                                AND tpo_credito in (7,8);

                  --TRACE 'tpo_credito3 : ' || v_tpo_credito;

                             IF v_bnd_liq_aux = 1 THEN
                                LET v_estado      = 30;
                                LET v_situacion   = 153;
                                LET v_diagnostico = 1 ;

                                UPDATE ocg_tramite
                                   SET situacion = 153,
                                       estado       = 30
                                 WHERE id_ocg_tramite = v_id_ocg_tramite_t
                                   AND id_derechohabiente = v_id_dh
                                   AND cve_ent_financiera = v_cve_ent_financiera
                                   AND diagnostico = 1
                                   AND situacion   in(30,50);

                                EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                                    206,
                                                                    v_id_ocg_tramite_t,
                                                                    0,
                                                                    0,
                                                                    "safreviv",
                                                                    3903) INTO v_rch_marca;
                             ELSE
                                 LET v_estado      = 60;
                                 LET v_situacion   = 20;
                                 LET v_diagnostico = 2 ;
                             END IF

                          INSERT INTO ocg_fecha_mig
                               VALUES(v_id_ocg_liquidacion,
                                      V_id_ocg_detalle,
                                      v_id_dh,
                                      v_f_proceso,
                                      v_f_proceso,
                                      '',
                                      '',
                                      5,
                                      v_f_proceso);

                          INSERT INTO ocg_liquidacion
                               VALUES(v_id_ocg_liquidacion,
                                      v_id_ocg_detalle,
                                      v_id_ocg_formalizacion,
                                      v_id_ocg_tramite_t,
                                      v_id_dh,
                                      v_cve_ent_financiera,
                                      v_num_ctrl_ef,
                                      v_periodo,
                                      v_imp_subsec_devuelto,
                                      v_f_libera_gtia,
                                      v_imp_ocg_devuelto,
                                      v_id_causa_liquida,
                                      v_f_deposito,
                                      v_tpo_credito,
                                      v_diagnostico,
                                      v_estado,
                                      v_situacion
                                      );

                          CONTINUE FOREACH;
                  ELSE
                     LET v_bnd_inconsistencia = 1;
                     LET v_inconsistencia = "26"; --antes la 51, se cambio (06-12-2017)

                     INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
                  END IF
               ELSE
                  IF EXISTS (SELECT l.id_derechohabiente
                               FROM ocg_acreditado a, ocg_liquidacion l
                              WHERE l.id_derechohabiente = v_id_dh
                                AND a.id_ocg_formalizacion = l.id_ocg_formalizacion
                                AND l.situacion >= 140) THEN

                     SELECT max(id_ocg_formalizacion)
                       INTO v_id_ocg_formalizacion
                       FROM ocg_formalizacion
                      WHERE id_derechohabiente = v_id_dh
                        AND diagnostico = 1
                        AND situacion BETWEEN 140 AND 160;

                     LET v_inconsistencia = "53";
                  ELSE
                     IF EXISTS (SELECT id_ocg_liquidacion
                                  FROM ocg_liquidacion
                                 WHERE id_derechohabiente = v_id_dh
                                   AND situacion in (153,155)
                                   AND diagnostico = 1) THEN

                        LET v_inconsistencia = "53";
                     ELSE
                        LET v_inconsistencia = "43";
                     END IF
                  END IF

                  LET v_bnd_inconsistencia = 1;

                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF
            ELSE
               IF EXISTS (SELECT id_ocg_formalizacion
                             FROM ocg_formalizacion
                            WHERE id_derechohabiente = v_id_dh
                              AND diagnostico = 1
                              AND situacion in (55,60,70)) THEN

                  LET v_bnd_inconsistencia = 1;

                  LET v_inconsistencia = "60";
                  INSERT INTO ocg_inconsistencia
                       VALUES( v_id_ocg_liquidacion,
                               v_subproceso,
                               v_inconsistencia,
                               v_f_proceso );
               END IF
            END IF

               IF v_bnd_inconsistencia = 1 THEN

                  INSERT INTO ocg_fecha_mig
                       VALUES(v_id_ocg_liquidacion,
                              v_id_ocg_detalle,
                              v_id_dh,
                              v_f_proceso,
                              v_f_proceso,
                              '',
                              '',
                              5,
                              v_f_proceso);

                   INSERT INTO ocg_liquidacion
                        VALUES (v_id_ocg_liquidacion,
                               v_id_ocg_detalle,
                               v_id_ocg_formalizacion,
                               v_id_ocg_tramite,
                               v_id_dh,
                               v_cve_ent_financiera,
                               v_num_ctrl_ef,
                               v_periodo,
                               v_imp_subsec_devuelto,
                               v_f_libera_gtia,
                               v_imp_ocg_devuelto,
                               v_id_causa_liquida,
                               v_f_deposito,
                               v_tpo_credito,
                               2,
                               60,
                               20
                               );

                  LET v_cve_ent_financiera  = "";
                  LET v_nss                 = "";
                  LET v_num_ctrl_ef         = "";
                  LET v_f_envio             = "";
                  LET v_bim_apor_subsec     = "";
                  LET v_imp_subsec_devuelto = "";
                  LET v_id_causa_liquida    = "";
                  LET v_f_deposito          = "";
                  LET v_tpo_credito         = "";
                  LET v_f_libera_garantia   = "";
                  LET v_imp_ocg_devuelto    = "";
                  LET v_id_ocg_formalizacion = "";
                  LET v_id_ocg_tramite       = "";
                  LET v_f_formalizacion      = "";
                  LET v_id_ocg_tramite_t     = "";

                  CONTINUE FOREACH;

               END IF

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     5,
                     v_f_proceso);

               IF v_bnd_inconsistencia = 1 THEN

         INSERT INTO ocg_liquidacion
               VALUES (v_id_ocg_liquidacion,
                       v_id_ocg_detalle,
                       v_id_ocg_formalizacion,
                       v_id_ocg_tramite,
                       v_id_dh,
                       v_cve_ent_financiera,
                       v_num_ctrl_ef,
                       v_periodo,
                       v_imp_subsec_devuelto,
                       v_f_libera_gtia,
                       v_imp_ocg_devuelto,
                       v_id_causa_liquida,
                       v_f_deposito,
                       v_tpo_credito,
                       2,
                       60,
                       20
                       );
                 END IF

               IF v_bnd_inconsistencia = 0 THEN
         INSERT INTO ocg_liquidacion
               VALUES (v_id_ocg_liquidacion,
                       v_id_ocg_detalle,
                       v_id_ocg_formalizacion,
                       v_id_ocg_tramite,
                       v_id_dh,
                       v_cve_ent_financiera,
                       v_num_ctrl_ef,
                       v_periodo,
                       v_imp_subsec_devuelto,
                       v_f_libera_gtia,
                       v_imp_ocg_devuelto,
                       v_id_causa_liquida,
                       v_f_deposito,
                       v_tpo_credito,
                       1,
                       30,
                       140
                       );
                END IF

         UPDATE ocg_tramite
            SET situacion          = 140,
                estado             = 30
          WHERE diagnostico        = 1
            AND situacion         >= 60
            AND id_derechohabiente = v_id_dh
            AND id_ocg_tramite     = v_id_ocg_tramite;

          UPDATE ocg_formalizacion
             SET situacion            = 140,
                 estado               = 30
           WHERE diagnostico          = 1
             AND situacion           >= 60
             AND id_derechohabiente   = v_id_dh
            -- AND id_ocg_tramite       = v_id_ocg_tramite
             AND id_ocg_formalizacion = v_id_ocg_formalizacion;

          UPDATE ocg_acreditado
             SET situacion            = 140,
                 estado               = 30,
                 f_liquida_credito    = TODAY
           WHERE situacion           >= 60
             AND id_ocg_formalizacion = v_id_ocg_formalizacion;


         END IF
      END IF

      IF (v_bnd_liq    = 1) AND
         (v_bnd_dev_ap = 0) AND
         (v_bnd_dev_gta= 0) AND
         (v_bnd_inconsistencia <> 1) THEN

         LET v_periodo  = v_bim_apor_subsec;

         -- Se inserta en la tabla ocg_liquidacion
         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     5,
                     v_f_proceso);

         INSERT INTO ocg_liquidacion
              VALUES(v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_ocg_formalizacion,
                     v_id_ocg_tramite,
                     v_id_dh,
                     v_cve_ent_financiera,
                     v_num_ctrl_ef,
                     v_periodo,
                     v_imp_subsec_devuelto,
                     v_f_libera_gtia,
                     v_imp_ocg_devuelto,
                     v_id_causa_liquida,
                     v_f_deposito,
                     v_tpo_credito,
                     v_diagnostico,
                     v_estado,
                     v_situacion
                     );

         UPDATE ocg_tramite
            SET situacion          = 140,
                estado             = 30
          WHERE diagnostico        = 1
            AND situacion         >= 60
            AND id_derechohabiente = v_id_dh
            AND id_ocg_tramite     = v_id_ocg_tramite;

          UPDATE ocg_formalizacion
             SET situacion            = 140,
                 estado               = 30
           WHERE diagnostico          = 1
             AND situacion           >= 60
             AND id_derechohabiente   = v_id_dh
            -- AND id_ocg_tramite       = v_id_ocg_tramite
             AND id_ocg_formalizacion = v_id_ocg_formalizacion;

          UPDATE ocg_acreditado
             SET situacion            = 140,
                 estado               = 30,
                 f_liquida_credito    = TODAY
           WHERE situacion           >= 60
             AND id_ocg_formalizacion = v_id_ocg_formalizacion;

        -- EXECUTE FUNCTION fn_desmarca_cuenta (v_id_dh,232,v_id_ocg_tramite,0,0,"",3903)
        -- INTO v_ax_cod_error;


         EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                          206,
                          v_id_ocg_formalizacion,
                          0,
                          0,
                          "safreviv",
                          3903) INTO v_rch_marca;


          IF (v_f_libera_garantia[4,5]) > 01 THEN
              LET v_dia = v_f_libera_garantia[4,5];
              --TRACE'v_dia : '||v_dia;
              LET v_f_libera_garantia_d = (v_f_libera_garantia );
              LET v_f_libera_garantia_d = (v_f_libera_garantia_d + 1 );
              LET v_f_libera_garantia_d = (v_f_libera_garantia_d -v_dia );
          ELSE
              LET v_f_libera_garantia_d = v_f_libera_garantia;
          END IF

         --TRACE'v_f_libera_garantia : '||v_f_libera_garantia_d;
        -- TRACE'v_dia : '||v_dia;

         FOREACH
            SELECT id_ocg_ctr_transaccion,
                   f_transaccion
              INTO v_ocg_ctr_transaccion,
                   v_f_transaccion
              FROM ocg_ctr_transaccion
             WHERE id_derechohabiente = v_id_dh
               AND f_transaccion >= v_f_libera_garantia_d
               AND estado in (60,70,80)

            IF v_f_transaccion IS NOT NULL THEN
               SELECT periodo_pago,
                      f_pago,
                      vivienda_97,
                      f_transaccion
                 INTO v_periodo_dis,
                      v_f_pago,
                      v_imp_ap_pat,
                      v_f_transaccion
                 FROM ocg_ctr_transaccion
                WHERE id_ocg_ctr_transaccion = v_ocg_ctr_transaccion;

               SELECT precio_fondo
                 INTO v_precio_fondo_today
                 FROM glo_valor_fondo
                WHERE f_valuacion = v_f_transaccion
                  AND fondo = 11;

               LET v_aiv_ap_pat = (v_imp_ap_pat/v_precio_fondo_today);
               LET v_imp_revaluado_15 = v_imp_ap_pat;
               LET v_precio_fondo_15  = v_precio_fondo_today;

               LET v_f_transaccion_d = (v_f_transaccion + 16);

                   --TRACE'v_f_transaccion : '||v_f_transaccion;

               IF v_f_transaccion_d <= v_f_actual THEN
                  --TRACE'v_f_transaccion : '||v_f_transaccion;

                  SELECT precio_fondo
                    INTO v_precio_fondo_15
                    FROM glo_valor_fondo
                   WHERE f_valuacion = v_f_actual
                     AND fondo = 11;

                  LET v_imp_revaluado_15 = (v_aiv_ap_pat*v_precio_fondo_15);
                  --LET v_imp_f_proceso  = (v_aiv_ap_pat*v_precio_fondo_today);
               END IF

                LET v_f_transaccion_d = (v_f_transaccion - 16);

               LET v_diagnostico      = 1;

                -- Se inserta en la tabla ocg_devolucion_prospecto
               INSERT INTO safre_tmp:tmp_ocg_devolucion_prospecto
                    VALUES(v_id_ocg_devolucion,
                           v_ocg_ctr_transaccion,
                           v_id_ocg_detalle,
                           v_id_ocg_formalizacion,
                           v_id_dh,
                           v_nss,
                           v_cve_ent_financiera,
                           v_num_ctrl_ef,
                           v_periodo_dis,
                           v_imp_ap_pat,
                           v_aiv_ap_pat,
                           v_imp_revaluado_15,
                           v_precio_fondo_15,
                           v_f_transaccion,
                           v_f_libera_garantia_d,
                           v_diagnostico,
                           v_estado,
                           v_situacion
                           );

            END IF
         END FOREACH;

      ELSE
          IF (v_bnd_liq    = 1) AND
             (v_bnd_dev_ap = 0) AND
             (v_bnd_dev_gta= 0) AND
             (v_bnd_inconsistencia = 1) THEN

             LET v_periodo  = v_bim_apor_subsec;

             -- Se inserta en la tabla ocg_liquidacion

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     5,
                     v_f_proceso);

             INSERT INTO ocg_liquidacion
                   VALUES(v_id_ocg_liquidacion,
                          v_id_ocg_detalle,
                          v_id_ocg_formalizacion,
                          v_id_ocg_tramite,
                          v_id_dh,
                          v_cve_ent_financiera,
                          v_num_ctrl_ef,
                          v_periodo,
                          v_imp_subsec_devuelto,
                          v_f_libera_gtia,
                          v_imp_ocg_devuelto,
                          v_id_causa_liquida,
                          v_f_deposito,
                          v_tpo_credito,
                          2,
                          60,
                          20
                          );

          END IF
       END IF

      LET v_bnd_dev = 0;

      IF (v_bnd_liq     = 0) AND
         (v_bnd_dev_gta = 1) OR
         (v_bnd_dev_ap  = 1) THEN
         LET v_bnd_dev = 1;
      ELSE
         IF (v_bnd_liq     = 1) AND
            (v_bnd_dev_gta = 0) AND
            (v_bnd_dev_ap  = 1) THEN
            LET v_bnd_dev = 1;
         END IF
      END IF

      --TRACE 'bnd devolucion : ' || v_bnd_dev;

      IF v_bnd_dev = 1 THEN

         IF EXISTS (SELECT cve_ent_financiera
                      FROM ocg_devolucion_ef
                     WHERE cve_ent_financiera = v_cve_ent_financiera
                       AND f_devolucion       = v_f_deposito
                       AND estado in (110,150)) THEN
            LET v_edo_devolucion = 160;
         ELSE
            LET v_bnd_inconsistencia = 1;
            LET v_edo_devolucion     = 120;
            LET v_inconsistencia     = "61";

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_liquidacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );

            LET v_inconsistencia = "58";

            INSERT INTO ocg_inconsistencia
                 VALUES( v_id_ocg_liquidacion,
                         v_subproceso,
                         v_inconsistencia,
                         v_f_proceso );

         END IF

         -- Se inserta en la tabla ocg_devolucion

         IF v_bnd_inconsistencia = 1 THEN
            LET v_diagnostico = 2;
            LET v_estado      = 60;
            LET v_situacion   = 20 ;

         ELSE
             LET v_diagnostico = 1;
             LET v_estado      = 70;
             LET v_situacion   = 30 ;
         END IF


         INSERT INTO ocg_devolucion
              VALUES(v_id_ocg_devolucion,
                     v_id_ocg_detalle,
                     v_id_ocg_formalizacion,
                     v_id_ocg_tramite,
                     v_id_dh,
                     v_cve_ent_financiera,
                     v_num_ctrl_ef,
                     v_periodo,
                     v_imp_subsec_devuelto,
                     v_imp_ocg_devuelto,
                     "",
                     v_f_deposito,
                     v_tpo_credito,
                     v_diagnostico,
                     v_estado,
                     v_situacion,
                     v_edo_devolucion,
                     TODAY
                     );

         INSERT INTO ocg_fecha_mig
              VALUES(v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_dh,
                     v_f_proceso,
                     v_f_proceso,
                     '',
                     '',
                     5,
                     v_f_proceso);

         IF v_bnd_inconsistencia = 1 THEN
            LET v_diagnostico = 2;
            LET v_estado      = 60;
            LET v_situacion   = 165 ;
         ELSE
             LET v_diagnostico = 1;
             LET v_estado      = 70;
             LET v_situacion   = 175 ;
         END IF


         INSERT INTO ocg_liquidacion
             VALUES (v_id_ocg_liquidacion,
                     v_id_ocg_detalle,
                     v_id_ocg_formalizacion,
                     v_id_ocg_tramite,
                     v_id_dh,
                     v_cve_ent_financiera,
                     v_num_ctrl_ef,
                     v_periodo,
                     v_imp_subsec_devuelto,
                     v_f_libera_gtia,
                     v_imp_ocg_devuelto,
                     v_id_causa_liquida,
                     v_f_deposito,
                     v_tpo_credito,
                     v_diagnostico,
                     v_estado,
                     v_situacion
                      );

      END IF

      LET v_cve_ent_financiera  = "";
      LET v_nss                 = "";
      LET v_num_ctrl_ef         = "";
      LET v_f_envio             = "";
      LET v_bim_apor_subsec     = "";
      LET v_imp_subsec_devuelto = "";
      LET v_id_causa_liquida    = "";
      LET v_f_deposito          = "";
      LET v_tpo_credito         = "";
      LET v_f_libera_garantia   = "";
      LET v_imp_ocg_devuelto    = "";
      LET v_id_ocg_formalizacion = "";
      LET v_id_ocg_tramite       = "";
      LET v_f_formalizacion      = "";
      LET v_id_ocg_tramite_t     = "";
   END IF;
   END FOREACH;

      LET v_id_ocg_dev       = "";
      LET v_cve              = "";
      LET v_imp_ap           = "";
      LET v_gta              = "";
      LET v_f_dep            = "";
      LET v_monto_devolucion = "";
      LET v_monto_devuelto   = "";
      LET v_imp_pendiente    = "";
      LET v_edo_registro     = "";

   FOREACH

      SELECT id_ocg_devolucion,
             id_ocg_formalizacion,
             id_derechohabiente,
             cve_ent_financiera,
             num_ctr_int_ef,
             importe_subsec_devuelto,
             importe_ocg_devuelto,
             f_deposito,
             edo_registro,
             importe_pendiente,
             periodo_pago
        INTO v_id_ocg_dev,
             v_id_ocg_formalizacion,
             v_id_dh,
             v_cve,
             v_num_ctr_int,
             v_imp_ap,
             v_gta,
             v_f_dep,
             v_edo_registro,
             v_imp_pendiente,
             v_periodo_pago
        FROM ocg_devolucion
       WHERE edo_registro in (160,180)
         AND estado = 70
         AND situacion = 30
         AND diagnostico = 1
         AND f_proceso = TODAY

      SELECT SUM(monto_devolucion),
             SUM(monto_devuelto)
        INTO v_monto_devolucion,
             v_monto_devuelto
        FROM ocg_devolucion_ef
       WHERE cve_ent_financiera = v_cve
         AND f_devolucion       = v_f_dep
         AND estado in (110,150);

      IF (v_monto_devolucion IS NULL) AND
         (v_monto_devuelto IS NULL) THEN
         CONTINUE FOREACH;
      END IF

      IF v_monto_devuelto IS NULL THEN
         LET v_monto_devuelto = 0;
      END IF

      LET v_monto_disponible = v_monto_devolucion - v_monto_devuelto;

         IF v_edo_registro = 7 THEN
            LET v_imp_sol = v_imp_pendiente;
         ELSE

            IF (v_imp_ap IS NOT NULL) OR
               (v_imp_ap > 0 ) THEN
               LET v_imp_sol  = v_imp_ap;
               LET v_concepto = 508;
            ELSE
               IF (v_gta IS NOT NULL) OR
                  (v_gta > 0 ) THEN
                  LET v_imp_sol  = v_gta;
                  LET v_concepto = 608;
               END IF
            END IF
         END IF

         IF v_monto_disponible >= v_imp_sol THEN
            LET v_monto_devuelto = v_monto_devuelto + v_imp_sol;
            LET v_importe        = v_imp_sol;

            UPDATE ocg_devolucion_ef
               SET monto_devuelto     = v_monto_devuelto
             WHERE cve_ent_financiera = v_cve
               AND f_devolucion       = v_f_dep;

           LET v_imp_pendiente = 0;

            UPDATE ocg_devolucion
               SET edo_registro = 170,
                   importe_pendiente = v_imp_pendiente
             WHERE id_ocg_devolucion = v_id_ocg_dev;

          END IF

         IF (v_monto_disponible < v_imp_sol) AND
            (v_monto_disponible > 0 ) THEN

            LET v_monto_devuelto = v_monto_devuelto + v_monto_disponible;
            LET v_importe        = v_monto_disponible;

            UPDATE ocg_devolucion_ef
               SET monto_devuelto     = v_monto_devuelto
             WHERE cve_ent_financiera = v_cve
               AND f_devolucion       = v_f_dep;

            LET v_imp_pendiente = v_imp_sol - v_monto_disponible;

             UPDATE ocg_devolucion
               SET edo_registro = 180,
                   importe_pendiente = v_imp_pendiente
             WHERE id_ocg_devolucion = v_id_ocg_dev;

         END IF

         LET v_monto_devuelto = "";

         SELECT monto_devolucion - monto_devuelto
           INTO v_monto_devuelto
           FROM ocg_devolucion_ef
          WHERE cve_ent_financiera = v_cve
            AND f_devolucion       = v_f_dep
            AND estado in (110,150);

         IF (v_monto_devuelto = 0) THEN

             UPDATE ocg_devolucion_ef
                SET estado = 140
              WHERE cve_ent_financiera = v_cve
                AND f_devolucion       = v_f_dep
                AND estado in (110,150);
         END IF

         IF (v_monto_devuelto >= 1) THEN

             UPDATE ocg_devolucion_ef
                SET estado = 150
              WHERE cve_ent_financiera = v_cve
                AND f_devolucion       = v_f_dep
                AND estado in (110,150);
         END IF


--***************************************************************** se inserta en transaccion desde que el registro queda conciliado nueva validacion 06/11/2017
      LET v_id_ocg_detalle     = seq_ocg_detalle.nextval;
      LET v_id_ocg_trx         = seq_dis_ctr_aps_tns.nextval ;

      SELECT nss,
             curp
        INTO v_nss,
             v_curp
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_id_dh;

      INSERT INTO ocg_detalle
           VALUES (v_id_ocg_detalle,
                   v_ax_id_ocg_ctr_arch,
                   v_id_dh,
                   4,
                   today,
                   v_cve,
                   v_nss);

      INSERT INTO ocg_ctr_transaccion
           VALUES (v_id_ocg_trx,
                   v_id_ocg_formalizacion,
                   v_id_ocg_detalle,
                   v_id_dh,
                   1,
                   1,
                   3906,
                   v_cve,
                   v_num_ctr_int,
                   1,
                   today,
                   v_nss,
                   v_curp,
                   v_importe,
                   v_periodo_pago,
                   today,
                   v_concepto,
                   today,
                   80);

      INSERT INTO ocg_fecha_mig
           VALUES (v_id_ocg_trx,
                   v_id_ocg_detalle,
                   v_id_dh,
                   "",
                   today,
                   "",
                   "",
                   4,
                   today);
--*****************************************************************
      LET v_bnd_liq          = 0;
      LET v_id_ocg_dev       = "";
      LET v_cve              = "";
      LET v_imp_ap           = "";
      LET v_gta              = "";
      LET v_f_dep            = "";
      LET v_monto_devolucion = "";
      LET v_monto_devuelto   = "";
      LET v_imp_pendiente    = "";
      LET v_edo_registro     = "";
      LET v_nss_unificado    = "";
      LET v_nss_unificador   = "";
      LET v_id_dh_unificador = "";
      LET v_id_dh_unificado  = "";
      LET v_diag             = 0;
      LET v_f_libera_gtia    = "";
      LET v_fecha_envio      = "";
      LET v_tpo_credito      = "";
      LET v_monto_disponible = 0;
      LET v_concepto         = "";
      LET v_nss              = "";
      LET v_curp             = "";
      LET v_importe          = "";
      LET v_periodo_pago     = "";
      LET v_num_ctr_int      = "";
      LET v_id_ocg_formalizacion = "";
      LET v_id_ocg_detalle   = "";
      LET v_id_dh            = "";
      LET v_id_ocg_trx       = "";

   END FOREACH;
 --  END IF

   -- Se actualiza el contador de la tabla de control de archisos
   LET v_tot_regs = v_cnt_aceptados + v_cnt_rechazados;
   UPDATE ocg_ctr_archivo
      SET tot_sp5 = v_tot_regs
    WHERE id_ocg_ctr_archivo = p_id_ocg_ctr_arch;

   -- Se actualizan estadisticas
   UPDATE STATISTICS FOR TABLE ocg_liquidacion;
   UPDATE STATISTICS FOR TABLE ocg_inconsistencia;
   UPDATE STATISTICS FOR TABLE ocg_devolucion;

   RETURN v_error,v_cnt_aceptados, v_cnt_rechazados;
END FUNCTION;


