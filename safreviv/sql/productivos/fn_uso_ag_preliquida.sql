






CREATE FUNCTION "safreviv".fn_uso_ag_preliquida(p_folio_liq           DECIMAL(9,0),
                                     p_folio_agr           DECIMAL(9,0),
                                     p_uso_anualidad       DECIMAL(12,2),
                                     p_id_cre_uso_garantia DECIMAL(9,0),
                                     p_id_derechohabiente  DECIMAL(9,0),
                                     p_valor_fondo         DECIMAL(19,14),
                                     p_tpo_uso             SMALLINT,
                                     p_tpo_credito         SMALLINT,
                                     p_f_liq               DATE,
                                     p_estado              SMALLINT,
                                     p_id_cre_ctr_archivo  DECIMAL(9,0),
                                     p_folio_archivo       DECIMAL(10,0))

RETURNING SMALLINT

   ----varables para saldo global
   DEFINE v_aivs_sdo                DECIMAL(12,2);
   DEFINE v_pesos_sdo               DECIMAL(12,2);
   DEFINE v_aivs_deudor             DECIMAL(12,2);
   DEFINE v_pesos_deudor            DECIMAL(12,2);
   DEFINE p_aivs_deudor             DECIMAL(12,2);

   ----variables para saldo por subcuenta
   DEFINE v_aivs_subcta             DECIMAL(12,2);
   DEFINE v_pesos_subcta            DECIMAL(12,2);

   ----variables para asignaci�n de valores
   ----a movimientos, fondo, subcuentas y tipo de trabajador
   DEFINE v_movimiento              SMALLINT;
   DEFINE v_fondo_inv               SMALLINT;
   DEFINE v_subcuenta               SMALLINT;
   DEFINE v_prelacion               SMALLINT;
   DEFINE v_origen                  CHAR(20);

   DEFINE v_f_liquida               DATE;
   DEFINE v_f_registro              DATE;
   DEFINE v_h_registro              DATETIME HOUR TO SECOND;
   DEFINE v_f_valor                 DATE;

   ----variable para status de ejecuci�n
   DEFINE v_status                  SMALLINT;
   DEFINE v_bnd                     SMALLINT;
   DEFINE qry_string                CHAR(500);
   DEFINE v_subctas                 CHAR(12);

   --VARIABLES DESMARCA LIQUIDACI�N NO PROCEDENTE
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_proceso_cod             SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;

   DEFINE v_ax_error                SMALLINT; -- contiene el c�digo de error en caso de ocurrir
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);
   DEFINE v_ax_estatus              SMALLINT; -- status, retorno de la funci�n que preliquidaci�n

   --- variables de inserci�n en dse_devolucion
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

   ON EXCEPTION SET v_ax_error, v_isam_err, v_c_msj
      -- Devolver� el c�digo de error que ocasione la excepci�n
      RETURN v_ax_error; ---, v_ax_estatus, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/usoPreliquida.trace';
   ---SET DEBUG FILE TO '/safreviv_int/archivos/preliquidaUsoAgr.trace';
   ---TRACE ON;

   DROP TABLE IF EXISTS tmp_mov_ua;

   -- inicializaci�n de variables
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
   LET v_f_liquida    = p_f_liq; --TODAY;
   LET v_f_valor      = p_f_liq; --MDY(MONTH(v_f_liquida),1,YEAR(v_f_liquida));

   LET v_estado_marca = 30;
   LET v_marca_causa  = 0;
   LET v_marca_entra  = 225;
   LET v_proceso_cod  = 312;

   LET v_ax_error     = 0;
   LET v_ax_estatus   = 0;
   LET v_isam_err     = 0;
   LET v_c_msj        = 'El proceso finaliz� correctamente';

   --variables para restituci�n
   LET vdev_folio             = p_folio_archivo;
   LET vdev_modulo_cod        = "ral";
   LET vdev_num_credito       = 0;
   LET vdev_tpo_transferencia = "15";
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

   IF p_tpo_uso = 1 THEN
      LET v_origen = "USO ANUALIDAD";
   ELSE
      LET v_origen = "USO GARANT�A AG";
   END IF

   ---Valor aivs al d�a de la preliquidaci�n
   IF p_valor_fondo IS NULL THEN
      LET v_status = 1;
   ELSE
      IF p_uso_anualidad > 0 THEN
         LET p_aivs_deudor = ROUND((p_uso_anualidad / p_valor_fondo),2);
      ELSE
         LET p_uso_anualidad = 0;
         LET p_aivs_deudor   = 0;
      END IF

      ----Actualizaci�n de montos en deudor
      UPDATE cre_saldo_deudor
         SET monto_aivs   = p_aivs_deudor,
             f_movimiento = v_f_liquida,
             f_proceso    = v_f_liquida
       WHERE id_cre_acreditado = p_id_cre_uso_garantia
         AND folio_referencia  = p_folio_agr
         AND movimiento        = 411;

      ----Obtenci�n saldo de la cuenta individual
      IF p_tpo_credito = 12 OR
         p_tpo_credito = 18 OR
         p_tpo_credito = 19 THEN
         LET v_subctas = "(4,44)";
      ELSE
         LET v_subctas = "(4,44,8,42)";
      END IF

      LET qry_string = " SELECT * "||
                       " FROM cta_movimiento "||
                       " WHERE id_derechohabiente = "||p_id_derechohabiente||
                       " AND subcuenta IN "||v_subctas||
                       " INTO TEMP tmp_mov_ua";

      EXECUTE IMMEDIATE qry_string;

      SELECT SUM(monto_acciones), ROUND((sum(monto_acciones*p_valor_fondo)),2)
         INTO v_aivs_sdo, v_pesos_sdo
         FROM tmp_mov_ua
        WHERE id_derechohabiente = p_id_derechohabiente;

      IF v_aivs_sdo IS NULL OR v_aivs_sdo <= 0 THEN
         LET v_aivs_sdo  = 0;
         LET v_pesos_sdo = 0;

         UPDATE cre_uso_garantia
            SET estado              = 240,
                diagnostico         = '08'
          WHERE id_cre_uso_garantia = p_id_cre_uso_garantia
            AND id_derechohabiente  = p_id_derechohabiente;

         FOREACH
            SELECT s.usuario_marca
              INTO v_usuario
              FROM sfr_marca_activa s
             WHERE s.marca              = v_marca_entra
               AND s.id_derechohabiente = p_id_derechohabiente
               AND s.n_referencia       = p_id_cre_uso_garantia

            -- se invoca la funci�n de desmarca
            EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derechohabiente,
                                                v_marca_entra,
                                                p_id_cre_uso_garantia,
                                                v_estado_marca,
                                                v_marca_causa,
                                                V_usuario,
                                                v_proceso_cod)
                                           INTO v_ax_cod_error;
         END FOREACH
      ELSE  ----Obtenci�n saldo por subcuenta
         FOREACH
            SELECT m.subcuenta, c.prelacion, SUM(m.monto_acciones)
              INTO v_subcuenta, v_prelacion, v_aivs_subcta
              FROM tmp_mov_ua m, cat_subcuenta c
             WHERE m.id_derechohabiente = p_id_derechohabiente
               AND m.subcuenta          = c.subcuenta
            GROUP BY m.subcuenta, c.prelacion
            ORDER BY c.prelacion

            IF v_aivs_subcta IS NULL OR v_aivs_subcta <= 0 THEN
               LET v_aivs_sdo  = 0;
               LET v_pesos_sdo = 0;
            ELSE
               LET v_pesos_subcta = ROUND((v_aivs_subcta * p_valor_fondo),2);
            END IF

            IF p_tpo_credito = 25 THEN
               IF (v_subcuenta = 4 OR 
                   v_subcuenta = 8) THEN
                  LET v_movimiento = 1962;
               ELSE
                  LET v_movimiento = 1972;
               END IF
            ELIF p_tpo_credito = 26 THEN
               IF (v_subcuenta = 4 OR 
                   v_subcuenta = 8) THEN
                  LET v_movimiento = 1982;
               ELSE
                  LET v_movimiento = 1992;
               END IF
            ELSE
               IF (v_subcuenta = 4 OR 
                   v_subcuenta = 8) THEN
                  IF p_estado = 20 THEN
                     IF p_tpo_uso = 1 THEN 
                        LET v_movimiento = 242;
                     ELSE
                        LET v_movimiento = 542;
                     END IF
                  ELSE
                     LET v_movimiento = 2002;
                  END IF
               ELSE
                  IF p_tpo_uso = 1 THEN 
                     LET v_movimiento = 432;
                  ELSE
                     LET v_movimiento = 552;
                  END IF
               END IF
            END IF

            IF p_uso_anualidad  = 0 THEN
                EXIT FOREACH;
            END IF

            IF p_uso_anualidad < v_pesos_subcta THEN   ----el saldo deudor es menor al saldo de la subcuenta
               LET v_aivs_subcta  = p_aivs_deudor * (-1);
               LET v_pesos_subcta = p_uso_anualidad * (-1);

               LET p_uso_anualidad = 0;
               LET p_aivs_deudor   = 0;
            ELSE   ----el saldo deudor es mayor al saldo de la subcuenta
               LET v_aivs_subcta  = v_aivs_subcta * (-1);
               LET v_pesos_subcta = v_pesos_subcta * (-1);

               LET p_aivs_deudor   = p_aivs_deudor + v_aivs_subcta;
               LET p_uso_anualidad = p_uso_anualidad + v_pesos_subcta;
            END IF

            LET v_f_registro = TODAY;
            LET v_h_registro = CURRENT;

            LET v_aivs_deudor  = v_aivs_deudor  + v_aivs_subcta;
            LET v_pesos_deudor = v_pesos_deudor + v_pesos_subcta;

            IF v_aivs_subcta < 0 THEN   ---liquidaci�n deudor
               INSERT INTO cre_ag_preliquida
               VALUES ( v_f_liquida,
                        p_id_derechohabiente,
                        v_subcuenta,
                        v_fondo_inv,
                        v_movimiento,
                        p_folio_liq,
                        p_id_cre_uso_garantia,
                        v_aivs_subcta,
                        v_pesos_subcta,
                        v_f_valor,
                        v_f_registro,
                        v_h_registro,
                        v_origen);

               IF p_estado = 320 THEN
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

         ----preliquidaci�n deudor
         IF v_aivs_deudor < 0 THEN
            IF p_tpo_uso = 1 THEN 
               LET v_movimiento = 242;
            ELSE
               LET v_movimiento = 542;
            END IF

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_uso_garantia,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_id_cre_uso_garantia,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_agr
            VALUES ( p_id_cre_uso_garantia,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_id_cre_uso_garantia,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);
         END IF
      END IF
   END IF

   RETURN v_status;

   --Finaliza la funci�n de preliquidaci�n de uso de anualidad
END FUNCTION;


