






CREATE FUNCTION "selefp".fn_ocg_valida_sp5_liq_devolucion(p_id_ocg_ctr_arch DECIMAL(9,0))
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

   SET DEBUG FILE TO '/safreviv_int/archivos/fn_ocg_valida_sp5_liq_devolucion.trace';
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
         AND f_proceso = TODAY - 1

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


