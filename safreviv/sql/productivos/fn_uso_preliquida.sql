






CREATE FUNCTION "safreviv".fn_uso_preliquida(p_folio_liq           DECIMAL(9,0),
                                  p_folio_uso           DECIMAL(9,0),
                                  p_uso_gtia            DECIMAL(12,2),
                                  p_id_cre_uso_garantia DECIMAL(9,0),
                                  p_id_derechohabiente  DECIMAL(9,0),
                                  p_valor_fondo         DECIMAL(19,14),
                                  p_periodo_pago        CHAR(6),
                                  p_f_liq               DATE,
                                  p_tpo_uso             SMALLINT,
                                  p_tpo_transf          SMALLINT,
                                  p_estado              SMALLINT,
                                  p_id_cre_ctr_archivo  DECIMAL(9,0),
                                  p_folio_archivo       DECIMAL(10,0))

RETURNING SMALLINT

   ----varables para saldo global
   DEFINE v_aivs_sdo                DECIMAL(12,2);
   DEFINE v_pesos_sdo               DECIMAL(12,2);
   DEFINE v_aivs_prl                DECIMAL(12,2);
   DEFINE v_pesos_prl               DECIMAL(12,2);
   DEFINE v_aivs_deudor             DECIMAL(12,2);
   DEFINE v_pesos_deudor            DECIMAL(12,2);
   DEFINE p_aivs_deudor             DECIMAL(12,2);

   ----variables para saldo por subcuenta
   DEFINE v_aivs_subcta             DECIMAL(12,2);
   DEFINE v_pesos_subcta            DECIMAL(12,2);

   ----variables para asignación de valores
   ----a movimientos, fondo, subcuentas y tipo de trabajador
   DEFINE v_movimiento              SMALLINT;
   DEFINE v_fondo_inv               SMALLINT;
   DEFINE v_subcuenta               SMALLINT;
   DEFINE v_prelacion               SMALLINT;
   DEFINE v_origen                  CHAR(20);

   DEFINE v_f_liquida               DATE;
   DEFINE v_f_registro              DATE;
   DEFINE v_h_registro              DATETIME HOUR TO SECOND;

   ----variable para status de ejecución
   DEFINE v_status                  SMALLINT;
   DEFINE v_bnd                     SMALLINT;

   --VARIABLES DESMARCA LIQUIDACIÓN NO PROCEDENTE
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_proceso_cod             SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;
   DEFINE v_bnd_proceso             SMALLINT;
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_isam                    INTEGER;
   DEFINE v_char                    CHAR(20);

   --variables para periodo-bimestre
   DEFINE v_per_bim                 CHAR(2);
   DEFINE v_per_orig                CHAR(2);

   --- variables de inserción en dse_devolucion
   DEFINE vdev_id_dse_devolucion       DECIMAL(9,0);
   DEFINE vdev_folio                   DECIMAL(9,0);
   DEFINE vdev_modulo_cod              CHAR(3);
   DEFINE vdev_id_derechohabiente      DECIMAL(9,0);
   DEFINE vdev_num_credito             DECIMAL(10,0);
   DEFINE vdev_tpo_transferencia       CHAR(2);
   DEFINE vdev_origen_devolucion       CHAR(2);
   DEFINE vdev_f_pago                  DATE;
   DEFINE vdev_f_movimiento            DATE;
   DEFINE vdev_periodo_pago            CHAR(6);
   DEFINE vdev_folio_referencia        DECIMAL(9,0);
   DEFINE vdev_subcuenta               DECIMAL(9,0);
   DEFINE vdev_monto_aivs              DECIMAL(22,2);
   DEFINE vdev_monto_pesos             DECIMAL(12,2);
   DEFINE vdev_monto_aportacion        DECIMAL(12,2);
   DEFINE vdev_aivs_aportacion         DECIMAL(16,6);
   DEFINE vdev_nss_separacion          CHAR(11);
   DEFINE vdev_estado                  SMALLINT;

   ---SET DEBUG FILE TO '/safreviv_int/archivos/preliquidaUsoGtia.trace';
   ---TRACE ON;

   LET v_aivs_sdo     = 0;
   LET v_pesos_sdo    = 0;
   LET v_aivs_subcta  = 0;
   LET v_pesos_subcta = 0;
   LET v_aivs_deudor  = 0;
   LET v_pesos_deudor = 0;
   LET v_bnd          = 0;
   LET p_aivs_deudor  = 0;
   LET v_status       = 0;

   LET v_fondo_inv    = 11;
   LET v_f_liquida    = p_f_liq;
   LET v_origen       = "USO GARANTÍA";

   LET v_estado_marca = 30;
   LET v_marca_causa  = 0;
   LET v_marca_entra  = 223;
   LET v_proceso_cod  = 1217;
   LET v_tpo_credito  = 3;

   --variables para restitución
   LET vdev_folio             = p_folio_archivo;
   LET vdev_modulo_cod        = "rgl";
   LET vdev_num_credito       = 0;
   LET vdev_tpo_transferencia = "19";
   LET vdev_origen_devolucion = 2;
   LET vdev_f_pago            = p_f_liq;
   LET vdev_f_movimiento      = MDY(MONTH(vdev_f_pago),1,YEAR(vdev_f_pago));
   LET vdev_f_movimiento      = vdev_f_movimiento + 1 UNITS MONTH;
   LET vdev_folio_referencia  = p_folio_liq;
   LET vdev_subcuenta         = 4;
   LET vdev_monto_aivs        = 0;
   LET vdev_monto_pesos       = 0;
   LET vdev_monto_aportacion  = 0;
   LET vdev_aivs_aportacion   = 0;
   LET vdev_nss_separacion    = "";
   LET vdev_estado            = 10;
   LET vdev_periodo_pago      = "";


   ---Valor aivs al día de la preliquidación
   IF p_valor_fondo IS NULL THEN
      LET v_status = 1;
   ELSE
      IF p_uso_gtia > 0 THEN
         LET p_aivs_deudor = ROUND((p_uso_gtia / p_valor_fondo),2);
      ELSE
         LET p_uso_gtia    = 0;
         LET p_aivs_deudor = 0;
      END IF

      ----Actualización de montos en importe solicitado
      UPDATE cre_saldo_deudor
         SET monto_aivs   = p_aivs_deudor,
             f_movimiento = v_f_liquida,
             f_proceso    = v_f_liquida
       WHERE id_cre_acreditado = p_id_cre_uso_garantia
         AND folio_referencia  = p_folio_uso
         AND movimiento        = 401
         AND id_referencia     = p_periodo_pago;

      ----Obtención saldo de la cuenta individual
      SELECT SUM(monto_acciones), ROUND((sum(monto_acciones*p_valor_fondo)),2)
        INTO v_aivs_sdo, v_pesos_sdo
        FROM cta_movimiento
       WHERE id_derechohabiente = p_id_derechohabiente
         AND subcuenta IN(4,44);

      IF v_aivs_sdo IS NULL OR v_aivs_sdo <= 0 THEN
         LET v_aivs_sdo  = 0;
         LET v_pesos_sdo = 0;
      END IF

      SELECT SUM(monto_acciones), SUM(monto_pesos)
        INTO v_aivs_prl, v_pesos_prl
        FROM cre_ug_preliquida
       WHERE id_derechohabiente = p_id_derechohabiente
         AND subcuenta IN(4,44)
         AND folio_liquida = p_folio_liq;

      IF v_aivs_prl IS NULL OR v_aivs_prl >= 0 THEN
         LET v_aivs_prl  = 0;
         LET v_pesos_prl = 0;
      END IF

      LET v_aivs_sdo  = v_aivs_sdo + v_aivs_prl;
      LET v_pesos_sdo = v_pesos_sdo + v_pesos_prl;

      IF v_aivs_sdo <= 0 THEN
         UPDATE cre_uso_garantia
            SET estado              = 240,
                diagnostico         = '08'
          WHERE id_cre_uso_garantia = p_id_cre_uso_garantia;

         LET v_status = 240;

         FOREACH
            SELECT s.usuario_marca
              INTO v_usuario
              FROM sfr_marca_activa s
             WHERE s.marca              = v_marca_entra
               AND s.id_derechohabiente = p_id_derechohabiente
               AND s.n_referencia       = p_id_cre_uso_garantia

            -- se invoca la función de desmarca
            EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derechohabiente,
                                                v_marca_entra,
                                                p_id_cre_uso_garantia,
                                                v_estado_marca,
                                                v_marca_causa,
                                                V_usuario,
                                                v_proceso_cod)
                                           INTO v_ax_cod_error;
         END FOREACH
      ELSE  ----Obtención saldo por subcuenta
         FOREACH
            SELECT m.subcuenta, c.prelacion, SUM(m.monto_acciones)
              INTO v_subcuenta, v_prelacion, v_aivs_subcta
              FROM cta_movimiento m, cat_subcuenta c
             WHERE m.id_derechohabiente = p_id_derechohabiente
               AND m.subcuenta = 4
               AND m.subcuenta = c.subcuenta
            GROUP BY m.subcuenta, c.prelacion
            ORDER BY c.prelacion

            IF v_aivs_subcta IS NULL OR v_aivs_subcta <= 0 THEN
               LET v_aivs_sdo  = 0;
               LET v_pesos_sdo = 0;
            END IF

            SELECT SUM(monto_acciones), SUM(monto_pesos)
              INTO v_aivs_prl, v_pesos_prl
              FROM cre_ug_preliquida
             WHERE id_derechohabiente = p_id_derechohabiente
               AND subcuenta          = v_subcuenta
               AND folio_liquida      = p_folio_liq;

            IF v_aivs_prl IS NULL OR v_aivs_prl >= 0 THEN
               LET v_aivs_prl  = 0;
               LET v_pesos_prl = 0;
            END IF

            LET v_aivs_subcta = v_aivs_subcta + v_aivs_prl;

            IF v_aivs_subcta IS NULL OR v_aivs_subcta <= 0 THEN
               LET v_aivs_sdo  = 0;
               LET v_pesos_sdo = 0;
            ELSE
               LET v_pesos_subcta = ROUND((v_aivs_subcta * p_valor_fondo),2);
            END IF

            IF p_estado = 20 THEN
               IF v_subcuenta = 4 THEN
                  LET v_movimiento = 62;
               ELSE
                  LET v_movimiento = 142;
               END IF
            ELSE
               LET v_movimiento = 2012;
            END IF

            IF p_uso_gtia  = 0 THEN
                EXIT FOREACH;
            END IF

            IF p_uso_gtia < v_pesos_subcta THEN   ----el saldo deudor es menor al saldo de la subcuenta
               LET v_aivs_subcta  = p_aivs_deudor * (-1);
               LET v_pesos_subcta = p_uso_gtia * (-1);

               LET p_uso_gtia  = 0;
               LET p_aivs_deudor = 0;
            ELSE   ----el saldo deudor es mayor al saldo de la subcuenta
               LET v_aivs_subcta  = v_aivs_subcta * (-1);
               LET v_pesos_subcta = v_pesos_subcta * (-1);

               LET p_aivs_deudor  = p_aivs_deudor + v_aivs_subcta;
               LET p_uso_gtia   = p_uso_gtia + v_pesos_subcta;
            END IF

            LET v_f_registro = TODAY;
            LET v_h_registro = CURRENT;

            LET v_aivs_deudor  = v_aivs_deudor + v_aivs_subcta;
            LET v_pesos_deudor = v_pesos_deudor + v_pesos_subcta;

            IF v_aivs_subcta < 0 THEN   ---liquidación deudor
               INSERT INTO cre_ug_preliquida
               VALUES ( v_f_liquida,
                        p_id_derechohabiente,
                        v_subcuenta,
                        v_fondo_inv,
                        v_movimiento,
                        p_folio_liq,
                        p_periodo_pago,
                        v_aivs_subcta,
                        v_pesos_subcta,
                        v_f_liquida,
                        v_f_registro,
                        v_h_registro,
                        v_origen);

               IF p_estado = 20 THEN
                  IF p_tpo_transf = 18 THEN
                     LET v_per_orig = p_periodo_pago;

                     IF p_tpo_uso = 3 THEN
                        SELECT bimestre
                          INTO v_per_bim
                          FROM ocg_ctrl_bimestres
                         WHERE periodo = p_periodo_pago[5,6];

                        LET p_periodo_pago = p_periodo_pago[1,4]||v_per_bim;
                     END IF

                     EXECUTE PROCEDURE sp_ins_dis_aps_tns(p_id_derechohabiente,
                                                          p_id_cre_uso_garantia,
                                                          p_folio_liq,
                                                          v_f_liquida,
                                                          v_pesos_subcta,
                                                          v_aivs_subcta,
                                                          p_periodo_pago,
                                                          v_tpo_credito,
                                                          p_tpo_uso,
                                                          v_per_orig)
                                                     INTO v_bnd_proceso,
                                                          v_isam,
                                                          v_char;
                  END IF
               ELSE
                  LET vdev_id_dse_devolucion  = seq_dse_devolucion.NEXTVAL;
                  LET vdev_id_derechohabiente = p_id_derechohabiente;
                  LET vdev_monto_pesos        = (v_pesos_subcta * -1);
                  LET vdev_subcuenta          = v_subcuenta;

                 INSERT INTO dse_devolucion (id_dse_devolucion,
                                             folio,
                                             modulo_cod,
                                             id_derechohabiente,
                                             num_credito,
                                             tpo_transferencia,
                                             origen_devolucion,
                                             f_pago,
                                             f_movimiento,
                                             periodo_pago,
                                             folio_referencia,
                                             subcuenta,
                                             monto_aivs,
                                             monto_pesos,
                                             monto_aportacion,
                                             aivs_aportacion,
                                             nss_separacion,
                                             estado)
                                     VALUES (vdev_id_dse_devolucion,
                                             vdev_folio,
                                             vdev_modulo_cod,
                                             vdev_id_derechohabiente,
                                             vdev_num_credito,
                                             vdev_tpo_transferencia,
                                             vdev_origen_devolucion,
                                             vdev_f_pago,
                                             vdev_f_movimiento,
                                             vdev_periodo_pago,
                                             vdev_folio_referencia,
                                             vdev_subcuenta,
                                             vdev_monto_aivs,
                                             vdev_monto_pesos,
                                             vdev_monto_aportacion,
                                             vdev_aivs_aportacion,
                                             vdev_nss_separacion,
                                             vdev_estado);
               END IF
            END IF
         END FOREACH;

         ----preliquidación deudor
         IF v_aivs_deudor < 0 THEN
            LET v_movimiento = 62;

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_uso_garantia,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_periodo_pago,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_grt
            VALUES ( p_id_cre_uso_garantia,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_periodo_pago,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);
         END IF
      END IF
   END IF

   RETURN v_status;

   --Finaliza la función de preliquidación de uso de garantía 43 bis
END FUNCTION;


