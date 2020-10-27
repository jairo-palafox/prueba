






CREATE FUNCTION "safreviv".fn_uso_actualiza_uni(p_nss_unificador       CHAR(11),
                                     p_id_dh_unificador     DECIMAL(9,0),
                                     p_id_ocg_ug_unificador DECIMAL(9,0),
                                     p_nss_unificado        CHAR(11),
                                     p_id_dh_unificado      DECIMAL(9,0),
                                     p_id_ocg_ug_unificado  DECIMAL(9,0),
                                     p_folio_uni            DECIMAL(10,0))

RETURNING SMALLINT;

   --VARIABLES PARA REGISTRO cre_uso_garantia
   DEFINE cre_id_cre_ug_ant         DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE cre_folio_liquida         DECIMAL(9,0);
   DEFINE cre_id_dh_ant             DECIMAL(9,0);
   DEFINE cre_tpo_transferencia     CHAR (2);
   DEFINE cre_tpo_uso               SMALLINT;
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_f_presentacion        DATE;
   DEFINE cre_f_movimiento          DATE;
   DEFINE cre_periodo_pago          CHAR(6);
   DEFINE cre_importe_v97           DECIMAL(12,2);
   DEFINE cre_nss_ant               CHAR(11);
   DEFINE cre_rfc_afore             CHAR(13);
   DEFINE cre_paterno_afore         CHAR(40);
   DEFINE cre_materno_afore         CHAR(40);
   DEFINE cre_nombre_afore          CHAR(40);
   DEFINE cre_nom_imss              CHAR(50);
   DEFINE cre_edo_procesar          SMALLINT;
   DEFINE cre_diagnostico           CHAR(3);
   DEFINE cre_estado                SMALLINT;
   DEFINE cre_f_proceso             DATE;

   DEFINE cre_id_cre_uso_garantia   DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_nss_afore             CHAR(11);
   DEFINE v_f_proc_uni              DATE;
   DEFINE v_subproceso              SMALLINT;

   DEFINE v_id_cre_uso_uni          DECIMAL(9,0);

   --VARIABLES DESMARCA
   DEFINE v_n_referencia            DECIMAL(9,0);
   DEFINE v_folio                   DECIMAL(10,0);
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_proceso_cod             SMALLINT;
   DEFINE v_error                   SMALLINT;
   DEFINE v_resultado               SMALLINT;
   DEFINE r_ax_sts_marcaje          SMALLINT;

   ON EXCEPTION SET v_error
      -- Devuelbe el código de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/desmarcaUsoGtiaUni.trace';
   ---TRACE ON;

   LET v_error                 = 0;
   LET v_resultado             = 4;
   LET v_n_referencia          = "";
   LET v_folio                 = "";
   LET v_marca_entra           = 223;
   LET v_estado_marca          = 40;
   LET v_marca_causa           = 223;
   LET v_usuario               = "usr43bis";
   LET v_proceso_cod           = "1201";
   LET v_f_proc_uni            = TODAY;
   LET v_subproceso            = 3;

   LET cre_id_cre_ug_ant       = "";
   LET cre_id_cre_ctr_archivo  = "";
   LET cre_folio_liquida       = "";
   LET cre_id_dh_ant           = "";
   LET cre_tpo_transferencia   = "";
   LET cre_tpo_uso             = "";
   LET cre_num_credito         = "";
   LET cre_f_presentacion      = "";
   LET cre_f_movimiento        = "";
   LET cre_periodo_pago        = "";
   LET cre_importe_v97         = "";
   LET cre_nss_ant             = "";
   LET cre_rfc_afore           = "";
   LET cre_paterno_afore       = "";
   LET cre_materno_afore       = "";
   LET cre_nombre_afore        = "";
   LET cre_nom_imss            = "";
   LET cre_edo_procesar        = "";
   LET cre_diagnostico         = "";
   LET cre_estado              = "";
   LET cre_f_proceso           = "";
   LET cre_id_cre_uso_garantia = "";
   LET cre_id_derechohabiente  = "";
   LET cre_nss_afore           = "";
   LET v_id_cre_uso_uni        = "";

   FOREACH
      SELECT *
        INTO cre_id_cre_ug_ant,
             cre_id_cre_ctr_archivo,
             cre_folio_liquida,
             cre_id_dh_ant,
             cre_tpo_transferencia,
             cre_tpo_uso,
             cre_num_credito,
             cre_f_presentacion,
             cre_f_movimiento,
             cre_periodo_pago,
             cre_importe_v97,
             cre_nss_ant,
             cre_rfc_afore,
             cre_paterno_afore,
             cre_materno_afore,
             cre_nombre_afore,
             cre_nom_imss,
             cre_edo_procesar,
             cre_diagnostico,
             cre_estado,
             cre_f_proceso
        FROM cre_uso_garantia
       WHERE id_derechohabiente = p_id_dh_unificado
         AND tpo_transferencia  = "18"
         AND estado             = 20
         AND edo_procesar       IN(10,70,80,85)

      IF cre_id_cre_ug_ant IS NULL THEN
         LET v_n_referencia = 4;
      ELSE
         IF cre_edo_procesar < 80 THEN
            UPDATE cre_uso_garantia
               SET estado              = 230
             WHERE id_cre_uso_garantia = cre_id_cre_ug_ant;

            LET cre_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
            LET cre_id_derechohabiente  = p_id_dh_unificador;
            LET cre_nss_afore           = p_nss_unificador;

            INSERT INTO cre_uso_garantia VALUES(cre_id_cre_uso_garantia,
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

            INSERT INTO ocg_transaccion_cre VALUES(v_subproceso,
                                                   p_id_ocg_ug_unificador,
                                                   cre_id_cre_uso_garantia,
                                                   cre_periodo_pago,
                                                   v_f_proc_uni);

            FOREACH
               SELECT n_referencia,
                      folio
                 INTO v_n_referencia,
                      v_folio
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = cre_id_dh_ant
                  AND marca              = v_marca_entra
                  AND n_referencia       = cre_id_cre_ug_ant

               IF v_n_referencia IS NOT NULL AND v_n_referencia <> 0 THEN
                  EXECUTE FUNCTION fn_desmarca_cuenta(cre_id_dh_ant,
                                                      v_marca_entra,
                                                      v_n_referencia,
                                                      v_estado_marca,
                                                      v_marca_causa,
                                                      v_usuario,
                                                      v_proceso_cod)
                                                 INTO r_ax_sts_marcaje;
               END IF
            END FOREACH;

            LET v_estado_marca = 0;
            LET v_marca_causa  = "";

            ---generar marca "Uso de garantía"
            CALL fn_marca_cuenta(cre_id_derechohabiente,
                                 v_marca_entra,
                                 cre_id_cre_uso_garantia,
                                 p_folio_uni,
                                 v_estado_marca,
                                 v_estado_marca,
                                 v_marca_causa,
                                 v_marca_causa,
                                 v_usuario,
                                 v_proceso_cod)
                       RETURNING r_ax_sts_marcaje;

            IF EXISTS(SELECT id_cre_uso_garantia
                        FROM cre_uso_garantia
                       WHERE id_cre_uso_garantia = cre_id_cre_ug_ant
                         AND estado IN(80,85)) THEN

                      DELETE
                        FROM cre_uso_garantia
                       WHERE id_cre_uso_garantia = cre_id_cre_ug_ant
                         AND estado IN(80,85);
            END IF
         ELSE
            IF NOT EXISTS (SELECT id_cre_uso_garantia
                             FROM cre_uso_gtia_uni
                            WHERE id_cre_uso_garantia = cre_id_cre_ug_ant) THEN

               INSERT INTO cre_uso_gtia_uni VALUES(p_nss_unificador,
                                                   p_id_dh_unificador,
                                                   p_id_ocg_ug_unificador,
                                                   p_nss_unificado,
                                                   p_id_dh_unificado,
                                                   cre_id_cre_ug_ant,
                                                   cre_edo_procesar,
                                                   p_folio_uni,
                                                   v_f_proc_uni);
            END IF
         END IF
      END IF

      LET v_n_referencia          = 3;
      LET v_folio                 = "";
      LET cre_id_cre_ug_ant       = "";
      LET cre_id_cre_ctr_archivo  = "";
      LET cre_folio_liquida       = "";
      LET cre_id_dh_ant           = "";
      LET cre_tpo_transferencia   = "";
      LET cre_tpo_uso             = "";
      LET cre_num_credito         = "";
      LET cre_f_presentacion      = "";
      LET cre_f_movimiento        = "";
      LET cre_periodo_pago        = "";
      LET cre_importe_v97         = "";
      LET cre_nss_ant             = "";
      LET cre_rfc_afore           = "";
      LET cre_paterno_afore       = "";
      LET cre_materno_afore       = "";
      LET cre_nombre_afore        = "";
      LET cre_nom_imss            = "";
      LET cre_edo_procesar        = "";
      LET cre_diagnostico         = "";
      LET cre_estado              = "";
      LET cre_f_proceso           = "";
      LET cre_id_cre_uso_garantia = "";
      LET cre_id_derechohabiente  = "";
      LET cre_nss_afore           = "";
   END FOREACH;

   UPDATE statistics FOR TABLE cre_uso_garantia;
   UPDATE statistics FOR TABLE ocg_transaccion_cre;

   RETURN v_resultado;

END FUNCTION;


