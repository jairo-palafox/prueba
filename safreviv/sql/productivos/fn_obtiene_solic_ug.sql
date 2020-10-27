






CREATE FUNCTION "safreviv".fn_obtiene_solic_ug(p_usuario            CHAR(20),
                                    p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                    p_d_folio            DECIMAL(9,0))
RETURNING SMALLINT, INTEGER;

   --- resultado, total regs
   DEFINE v_resultado               SMALLINT;
   DEFINE v_total_reg               INTEGER;
   DEFINE v_integra_solic           SMALLINT;
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_importe                 DECIMAL(12,2);
   DEFINE v_f_vencimiento           DATE;
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_id_ocg_solicitud_ug     DECIMAL(9,0);
   DEFINE v_tpo_uso                 SMALLINT;
   DEFINE v_id_cre_uso_garantia     DECIMAL(9,0);
   DEFINE v_nss                     CHAR(11);
   DEFINE v_subproceso              SMALLINT;
   DEFINE v_tpo_originacion         SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE cre_id_cre_uso_garantia   DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE cre_folio_liquida         DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_tpo_transferencia     CHAR (2);
   DEFINE cre_tpo_uso               SMALLINT;
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_f_presentacion        DATE;
   DEFINE cre_f_movimiento          DATE; 
   DEFINE cre_periodo_pago          CHAR(6);
   DEFINE cre_importe_v97           DECIMAL(12,2);
   DEFINE cre_nss_afore             CHAR(11);
   DEFINE cre_rfc_afore             CHAR(13);
   DEFINE cre_paterno_afore         CHAR(40); 
   DEFINE cre_materno_afore         CHAR(40);
   DEFINE cre_nombre_afore          CHAR(40);
   DEFINE cre_nom_imss              CHAR(50);
   DEFINE cre_edo_procesar          SMALLINT;
   DEFINE cre_diagnostico           CHAR(3);
   DEFINE cre_estado                SMALLINT;
   DEFINE cre_f_proceso             DATE;

   ---variables para solicitudes rechazadad ocg
   DEFINE v_inconsistencia          SMALLINT;
   DEFINE v_situacion               SMALLINT;
   DEFINE v_diag                    SMALLINT;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_rech                    SMALLINT;
   DEFINE v_result_conv             SMALLINT;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      LET v_resultado = 1;
      LET v_total_reg = 0;

      -- se devuelve el resultado de la operación indicando que ocurrió un error
      RETURN v_resultado, v_total_reg;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/obtieneUG.trace';
   ---TRACE ON;

   LET v_resultado            = 0;
   LET v_total_reg            = 0;
   LET v_integra_solic        = 0;
   LET v_id_derechohabiente   = 0;
   LET v_importe              = 0;
   LET v_periodo_pago         = "";
   LET v_id_ocg_solicitud_ug  = 0;
   LET v_nss                  = 0;
   LET v_f_vencimiento        = "";
   LET v_tpo_uso              = "";
   LET v_subproceso           = 3;
   LET v_tpo_originacion      = 2;
   LET v_f_proceso            = TODAY;
   LET v_inconsistencia       = "";
   LET v_situacion            = 90;
   LET v_diag                 = 2;
   LET v_estado               = "";
   LET v_rech                 = 0;
   LET v_id_cre_uso_garantia  = "";
   LET cre_estado             = 10;
   LET cre_diagnostico        = NULL;

   FOREACH
      SELECT d.nss,
             s.id_derechohabiente,
             s.id_ocg_solicitud_ug,
             s.importe_solicitado,
             s.f_vencimiento,
             ----YEAR(s.f_vencimiento)||lpad(MONTH(s.f_vencimiento),2,0)||lpad(day(s.f_vencimiento),2,0),
             YEAR(s.f_vencimiento)||lpad(MONTH(s.f_vencimiento),2,0),
             s.solicitud_saldo
        INTO v_nss,
             v_id_derechohabiente,
             v_id_ocg_solicitud_ug,
             v_importe,
             v_f_vencimiento,
             v_periodo_pago,
             v_tpo_uso
        FROM ocg_solicitud_uso_garantia s,
             ocg_detalle d
       WHERE s.situacion      = 50
         AND s.id_ocg_detalle = d.id_ocg_detalle

      LET v_total_reg = v_total_reg + 1;

      IF v_tpo_uso <> 3 AND
         v_tpo_uso <> 2 THEN
         LET v_tpo_uso = 2;
      END IF

      IF v_tpo_uso IS NULL OR v_tpo_uso = "" THEN
         LET v_tpo_uso = 2;
      END IF

      IF NOT EXISTS (SELECT id_derechohabiente
                       FROM cre_acreditado
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND tpo_originacion    = v_tpo_originacion
                    ) THEN
         LET v_inconsistencia = 43;
         LET v_rech           = 1;
         LET cre_estado       = 240;
         LET cre_diagnostico  = "13";
      ELSE
         IF EXISTS (SELECT id_derechohabiente
                      FROM cre_uso_garantia
                     WHERE id_derechohabiente = v_id_derechohabiente
                       AND tpo_transferencia  IN("18","48")
                       AND periodo_pago       = v_periodo_pago
                       AND estado             IN(10,20,130,140)
                       AND id_cre_ctr_archivo IN(SELECT id_cre_ctr_archivo
                                                   FROM cre_ctr_archivo
                                                  WHERE operacion = 18)
                    ) THEN
            SELECT MAX(id_cre_uso_garantia)
              INTO v_id_cre_uso_garantia
              FROM cre_uso_garantia
             WHERE id_derechohabiente = v_id_derechohabiente
               AND tpo_transferencia  IN("18","48")
               AND periodo_pago       = v_periodo_pago
               AND estado             IN(10,20,130,140)
               AND id_cre_ctr_archivo IN(SELECT id_cre_ctr_archivo
                                           FROM cre_ctr_archivo
                                          WHERE operacion = 18);

            SELECT estado
              INTO v_estado
              FROM cre_uso_garantia
             WHERE id_cre_uso_garantia = v_id_cre_uso_garantia;

            IF v_estado = 10 OR v_estado = 20 THEN
               LET v_inconsistencia = 49;
            ELSE
               LET v_inconsistencia = 50;
            END IF

            LET cre_estado           = 240;
            LET cre_diagnostico      = "17";

            LET v_id_cre_uso_garantia = "";
            LET v_rech                = 1;
         ELSE
            -- se asignan los valores en las variables que se usarán para insertar el registro
            LET cre_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
            LET cre_id_cre_ctr_archivo  = p_ax_id_cre_ctr_arch;
            LET cre_folio_liquida       = 0;
            LET cre_id_derechohabiente  = v_id_derechohabiente;
            LET cre_tpo_transferencia   = "18";
            LET cre_tpo_uso             = v_tpo_uso; --(valor de solicitud_saldo)--check
            LET cre_num_credito         = 0;
            LET cre_f_presentacion      = TODAY;
            LET cre_f_movimiento        = MDY(MONTH(cre_f_presentacion), 1, YEAR(cre_f_presentacion)) + 1 UNITS MONTH;
            ---LET cre_f_movimiento       = YEAR(f_vencimiento)||lpad(MONTH(f_vencimiento),2,0)||lpad(day(f_vencimiento),2,0);

            --DateSerial(Year(dtFecha), Month(dtFecha), 1); --primer día natural del siguiente mes  --check
            LET cre_periodo_pago        = v_periodo_pago;
            LET cre_importe_v97         = v_importe;  -- check
            LET cre_nss_afore           = v_nss;
            LET cre_rfc_afore           = NULL;
            LET cre_paterno_afore       = NULL;
            LET cre_materno_afore       = NULL;
            LET cre_nombre_afore        = NULL;
            LET cre_nom_imss            = NULL;
            LET cre_edo_procesar        = 10;
            LET cre_f_proceso           = TODAY;

            -- se inserta registro en la tabla maestro
            INSERT INTO cre_uso_garantia(
                    id_cre_uso_garantia,
                    id_cre_ctr_archivo,
                    folio_liquida,
                    id_derechohabiente,
                    tpo_transferencia,
                    tpo_uso,
                    num_credito,
                    f_presentacion,
                    f_movimiento,
                    periodo_pago,
                    importe_v97,
                    nss_afore,
                    rfc_afore,
                    paterno_afore,
                    materno_afore,
                    nombre_afore,
                    nom_imss,
                    edo_procesar,
                    diagnostico,
                    estado,
                    f_proceso)
            VALUES (cre_id_cre_uso_garantia,
                    cre_id_cre_ctr_archivo,
                    cre_folio_liquida,
                    cre_id_derechohabiente,
                    cre_tpo_transferencia,
                    cre_tpo_uso,
                    cre_num_credito,
                    cre_f_presentacion,
                    cre_f_movimiento,
                    cre_periodo_pago,
                    cre_importe_v97,
                    cre_nss_afore,
                    cre_rfc_afore,
                    cre_paterno_afore,
                    cre_materno_afore,
                    cre_nombre_afore,
                    cre_nom_imss,
                    cre_edo_procesar,
                    cre_diagnostico,
                    cre_estado,
                    cre_f_proceso);
         END IF
      END IF

      IF v_rech = 0 THEN
         UPDATE ocg_solicitud_uso_garantia
            SET situacion           = v_situacion
          WHERE id_ocg_solicitud_ug = v_id_ocg_solicitud_ug;

         --inserta ocg_transaccion_cre
         INSERT INTO ocg_transaccion_cre
                     (subproceso,
                      id_referencia_ocg,
                      id_referencia_cre,
                      periodo_pago,
                      f_proceso)
              VALUES (v_subproceso,
                      v_id_ocg_solicitud_ug,
                      cre_id_cre_uso_garantia,
                      v_periodo_pago,
                      v_f_proceso);
      ELSE
         EXECUTE FUNCTION fn_ins_inconsistencia(v_id_ocg_solicitud_ug,
                                               v_inconsistencia)
                                          INTO v_result_conv;

        {
         LET v_situacion = 20;

         UPDATE ocg_solicitud_uso_garantia
            SET situacion   = v_situacion,
                diagnostico = v_diag
          WHERE id_ocg_solicitud_ug = v_id_ocg_solicitud_ug;

         INSERT INTO ocg_inconsistencia
                     (id_ocg_referencia,
                      subproceso,
                      inconsistencia,
                      f_proceso)
              VALUES (v_id_ocg_solicitud_ug,
                      v_subproceso,
                      v_inconsistencia,
                      v_f_proceso);
        }
      END IF

      LET v_inconsistencia      = "";
      LET v_situacion           = 90;
      LET v_diag                = 2;
      LET v_estado              = "";
      LET v_rech                = 0;
      LET v_id_cre_uso_garantia = "";
      LET cre_estado            = 10;
   END FOREACH;

   RETURN v_resultado, v_total_reg;

END FUNCTION;


