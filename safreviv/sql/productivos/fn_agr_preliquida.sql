






CREATE FUNCTION "safreviv".fn_agr_preliquida(p_folio_liq          DECIMAL(9,0),
                                  p_folio_acr          DECIMAL(9,0),
                                  p_sdo_deudor         DECIMAL(12,2),
                                  p_id_cre_acreditado  DECIMAL(9,0),
                                  p_id_derechohabiente DECIMAL(9,0),
                                  p_valor_fondo        DECIMAL(19,14),
                                  p_tpo_trabajador     CHAR(1),
                                  p_nss                CHAR(11),
                                  p_f_liq              DATE)

RETURNING SMALLINT

   ----variables para saldo global
   DEFINE v_aivs_sdo        DECIMAL(12,2);
   DEFINE v_pesos_sdo       DECIMAL(12,2);
   DEFINE v_aivs_deudor     DECIMAL(12,2);
   DEFINE v_pesos_deudor    DECIMAL(12,2);
   DEFINE v_aivs_am         DECIMAL(12,2);
   DEFINE v_pesos_am        DECIMAL(12,2);
   DEFINE v_aivs_cc         DECIMAL(12,2);
   DEFINE v_pesos_cc        DECIMAL(12,2);
   DEFINE p_aivs_deudor     DECIMAL(12,2);

   ----variables para saldo por subcuenta
   DEFINE v_aivs_subcta     DECIMAL(12,2);
   DEFINE v_pesos_subcta    DECIMAL(12,2);

   ----variables para montos de amortizaciones por subcuenta
   DEFINE v_aivs_am_subcta  DECIMAL(12,2);
   DEFINE v_pesos_am_subcta DECIMAL(12,2);

   ----variables para asignación de valores
   ----a movimientos, fondo,
   ----subcuentas y tipo de trabajador
   DEFINE v_movimiento      SMALLINT;
   DEFINE v_mvto_amort      SMALLINT;
   DEFINE v_mvto_cargo      SMALLINT;
   DEFINE v_fondo_inv       SMALLINT;
   DEFINE v_subcuenta       SMALLINT;
   DEFINE v_prelacion       SMALLINT;
   DEFINE v_origen          CHAR(20);
   DEFINE v_proceso         SMALLINT;

   DEFINE v_f_liquida       DATE;
   DEFINE v_f_registro      DATE;
   DEFINE v_h_registro      DATETIME HOUR TO SECOND;
   DEFINE v_f_valor         DATE;

   ----variable para status de ejecución
   DEFINE v_status          SMALLINT;
   DEFINE v_ax_status       SMALLINT;
   DEFINE v_fc_status       SMALLINT;

   ----variables para registro en control contable
   DEFINE v_tpo_trabajador  CHAR(1);
   DEFINE v_tpo_deudor      SMALLINT;
   DEFINE v_tpo_origina     SMALLINT;

   ----variables para verificación de fortalecimiento del crédito
   DEFINE v_pesos_afc       DECIMAL(12,2);
   DEFINE v_aivs_afc        DECIMAL(12,2);
   DEFINE v_pesos_afc_cargo DECIMAL(12,2);
   DEFINE v_acc_afc_cargo   DECIMAL(12,2);
   DEFINE v_subcta_afc      SMALLINT;
   DEFINE v_subcta_abn      SMALLINT;
   DEFINE v_fondo_afc       SMALLINT;
   DEFINE v_mv_cargo_afc    SMALLINT;
   DEFINE v_mv_abono_afc    SMALLINT;

   ----variables para registro en proceso de devolución de saldos excedentes
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_num_credito           DECIMAL(10,0);
   DEFINE v_tpo_transferencia     CHAR(2);
   DEFINE v_origen_devolucion     CHAR(2);
   DEFINE v_f_movimiento          DATE;
   DEFINE v_aivs97                DECIMAL(12,2);
   DEFINE v_pesos97               DECIMAL(12,2);
   DEFINE v_edo_procesar          SMALLINT;
   DEFINE v_estado                SMALLINT;
   DEFINE v_lote                  SMALLINT;
   DEFINE v_f_proceso             DATE;

   --Variables para la ejecucion de la funcion preliquida voluntarias riss
   DEFINE v_estado_volriss       SMALLINT;
   DEFINE v_error_volriss        SMALLINT;

   --VARIABLES DESMARCA LIQUIDACIÓN NO PROCEDENTE
   DEFINE v_marca_entra           SMALLINT;
   DEFINE v_estado_marca          SMALLINT;
   DEFINE v_marca_causa           SMALLINT;
   DEFINE v_usuario               CHAR(20);
   DEFINE v_proceso_cod           SMALLINT;
   DEFINE v_ax_cod_error          SMALLINT;

   DEFINE v_folio_referencia      DECIMAL(10,0);
   DEFINE v_mov_deudor            SMALLINT;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_agr_preliquida.trace';
   --SET DEBUG FILE TO '/safreviv_int/archivos/fn_agr_preliquida.trace';
   --TRACE ON;

   LET v_aivs_sdo        = 0;
   LET v_pesos_sdo       = 0;
   LET v_aivs_am         = 0;
   LET v_pesos_am        = 0;
   LET v_aivs_cc         = 0;
   LET v_pesos_cc        = 0;
   LET v_aivs_subcta     = 0;
   LET v_pesos_subcta    = 0;
   LET v_aivs_am_subcta  = 0;
   LET v_pesos_am_subcta = 0;
   LET v_aivs_deudor     = 0;
   LET v_pesos_deudor    = 0;
   LET p_aivs_deudor     = 0;
   LET v_status          = 0;
   LET v_tpo_deudor      = -1;

   LET v_pesos_afc       = 0;
   LET v_pesos_afc_cargo = 0;
   LET v_acc_afc_cargo   = 0;
   LET v_subcta_afc      = 49;
   LET v_fondo_afc       = 10;
   LET v_mv_cargo_afc    = 302;
   LET v_mv_abono_afc    = 201;
   LET v_mvto_cargo      = 262;
   LET v_fondo_inv       = 11;
   LET v_f_liquida       = p_f_liq;
   LET v_f_valor         = p_f_liq;
   LET v_origen          = "TRANSF ANUALIDAD";
   LET v_proceso         = 312;

   LET v_tpo_origina     = 4;

   LET v_estado_marca    = 30;
   LET v_marca_causa     = 0;
   LET v_marca_entra     = 225;
   LET v_proceso_cod     = 312;
   LET v_mov_deudor      = 181;

   LET v_folio_referencia = 1;
   LET v_f_proceso        = TODAY;

   ---Valor aivs al día de la preliquidación
   IF p_valor_fondo IS NULL THEN
      LET v_status = 1;
   ELSE
      IF p_sdo_deudor > 0 THEN
         LET p_aivs_deudor = ROUND((p_sdo_deudor / p_valor_fondo),2);

         IF EXISTS(
            SELECT id_cre_acreditado
              FROM cre_saldo_deudor
             WHERE id_cre_acreditado = p_id_cre_acreditado
               AND movimiento        = 181) THEN

            ----Actualización de montos en deudor
            UPDATE cre_saldo_deudor
               SET monto_pesos  = p_sdo_deudor,
                   monto_aivs   = p_aivs_deudor,
                   f_movimiento = v_f_liquida,
                   f_proceso    = v_f_liquida
             WHERE id_cre_acreditado = p_id_cre_acreditado
               AND folio_referencia  = p_folio_acr
               AND movimiento        = v_mov_deudor;
         ELSE
            SELECT folio_archivo,
                   f_proceso
              INTO v_folio_referencia,
                   v_f_proceso
              FROM cre_acreditado c,
                   cre_ctr_archivo r
             WHERE c.id_cre_acreditado  = p_id_cre_acreditado
               AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo;

            IF v_folio_referencia IS NULL THEN
               LET v_folio_referencia = 1;
            END IF

            IF v_f_proceso IS NULL THEN
               LET v_f_proceso = TODAY;
            END IF

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_acreditado,
                     v_folio_referencia, 
                     v_f_proceso,
                     v_mov_deudor,
                     p_id_cre_acreditado,
                     p_aivs_deudor,
                     p_sdo_deudor,
                     v_f_proceso );
         END IF
      ELSE
         LET p_sdo_deudor  = 0;
         LET p_aivs_deudor = 0;
      END IF

    {
      EXECUTE FUNCTION fn_cre_preliq_volriss(p_folio_liq         ,
                                             p_id_cre_acreditado ,
                                             p_id_derechohabiente,
                                             p_valor_fondo       ,
                                             p_tpo_trabajador    ,
                                             v_proceso           )
                                        INTO v_estado_volriss,v_error_volriss;

      ----Verificación de aportaciones para fortalecimiento del crédito
      EXECUTE FUNCTION fn_cre_fort_cred(p_folio_liq,
                                        p_id_cre_acreditado,
                                        p_id_derechohabiente,
                                        p_valor_fondo,
                                        p_tpo_trabajador,
                                        v_tpo_origina)
                                   INTO v_fc_status;
    }

      ----verificación movimientos liquidados y de fortalecimiento
      ----del crédito para conformar saldo
      DELETE
        FROM safre_tmp:tmp_cta_mov_ag
       WHERE id_derechohabiente = p_id_derechohabiente;

      INSERT INTO safre_tmp:tmp_cta_mov_ag
      SELECT mv.*
        FROM cta_movimiento mv
       WHERE mv.id_derechohabiente = p_id_derechohabiente
         AND mv.subcuenta IN(4,8,42,44)
         AND mv.fondo_inversion = 11;

      INSERT INTO safre_tmp:tmp_cta_mov_ag
      SELECT mvp.*
        FROM cre_ag_preliquida mvp
       WHERE mvp.id_derechohabiente = p_id_derechohabiente
         AND mvp.subcuenta IN(4,44)
         AND mvp.movimiento = 201;

      INSERT INTO safre_tmp:tmp_cta_mov_ag
      SELECT mvp.*
        FROM cre_ag_preliquida mvp
       WHERE mvp.id_derechohabiente = p_id_derechohabiente
         AND mvp.subcuenta IN(4,55)
         AND mvp.movimiento IN (891,1582);

      ----Obtención saldo de la cuenta individual
      SELECT SUM(monto_acciones), ROUND((sum(monto_acciones*p_valor_fondo)),2)
        INTO v_aivs_sdo, v_pesos_sdo
        FROM safre_tmp:tmp_cta_mov_ag
       WHERE id_derechohabiente = p_id_derechohabiente
         AND subcuenta IN(4,8,42,44);

      IF v_aivs_sdo IS NULL OR v_aivs_sdo <= 0 THEN
         LET v_aivs_sdo  = 0;
         LET v_pesos_sdo = 0;

         IF v_pesos_afc <= 0 THEN
            IF p_sdo_deudor > 0 THEN  ---Hay saldo deudor
               LET v_aivs_cc  = p_aivs_deudor * (-1);
               LET v_pesos_cc = p_sdo_deudor * (-1);

               INSERT INTO cre_saldo_deudor
               VALUES ( p_id_cre_acreditado,
                        p_folio_liq,
                        v_f_liquida,
                        v_mvto_cargo,
                        p_id_cre_acreditado,
                        v_aivs_cc,
                        v_pesos_cc,
                        v_f_liquida);

               INSERT INTO safre_tmp:tmp_cre_saldo_deudor_agr
               VALUES ( p_id_cre_acreditado,
                        p_folio_liq,
                        v_f_liquida,
                        v_mvto_cargo,
                        p_id_cre_acreditado,
                        v_aivs_cc,
                        v_pesos_cc,
                        v_f_liquida);

               LET v_tpo_deudor = 2;
            ELSE  ---No hay saldo deudor
               LET v_tpo_deudor = 0;
            END IF
         END IF

         FOREACH
            SELECT s.usuario_marca
              INTO v_usuario
              FROM sfr_marca_activa s
             WHERE s.marca              = v_marca_entra
               AND s.id_derechohabiente = p_id_derechohabiente
               AND s.n_referencia       = p_id_cre_acreditado

            -- se invoca la función de desmarca
            EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derechohabiente,
                                                v_marca_entra,
                                                p_id_cre_acreditado,
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
              FROM safre_tmp:tmp_cta_mov_ag m, cat_subcuenta c
             WHERE m.id_derechohabiente = p_id_derechohabiente
               AND m.subcuenta IN(4,8,42,44)
               AND m.subcuenta = c.subcuenta
            GROUP BY m.subcuenta, c.prelacion
            ORDER BY c.prelacion

            IF v_aivs_subcta IS NULL OR v_aivs_subcta <= 0 THEN
               LET v_aivs_sdo  = 0;
               LET v_pesos_sdo = 0;
            ELSE
               LET v_pesos_subcta = ROUND((v_aivs_subcta * p_valor_fondo),2);
            END IF

            IF v_subcuenta = 4 OR v_subcuenta = 8 THEN
               LET v_movimiento = 492;
               LET v_mvto_amort = 82;
            ELSE
               LET v_movimiento = 152;
               LET v_mvto_amort = 162;
            END IF

            IF p_sdo_deudor = 0 AND (v_subcuenta = 8 OR v_subcuenta = 42) THEN   ----no hay saldo deudor
               LET v_aivs_subcta  = v_aivs_subcta * (-1);
               LET v_pesos_subcta = v_pesos_subcta * (-1);

               LET v_aivs_am  = v_aivs_am + v_aivs_subcta;
               LET v_pesos_am = v_pesos_am + v_pesos_subcta;

               LET v_f_registro = TODAY;
               LET v_h_registro = CURRENT;
               LET v_origen     = "REMANENTE ANUALIDAD";

               INSERT INTO cre_ag_preliquida
               VALUES ( v_f_liquida,
                        p_id_derechohabiente,
                        v_subcuenta,
                        v_fondo_inv,
                        v_mvto_amort,
                        p_folio_liq,
                        p_id_cre_acreditado,
                        v_aivs_subcta,
                        v_pesos_subcta,
                        v_f_valor,
                        v_f_registro,
                        v_h_registro,
                        v_origen);

               LET v_tpo_deudor = 1;
            ELSE
               IF p_sdo_deudor < v_pesos_subcta THEN   ----el saldo deudor es menor al saldo de la subcuenta
                  IF v_subcuenta = 8 OR v_subcuenta = 42 THEN
                     LET v_aivs_am_subcta  = (v_aivs_subcta - p_aivs_deudor) * (-1);
                     LET v_pesos_am_subcta = (v_pesos_subcta - p_sdo_deudor) * (-1);

                     LET v_aivs_am  = v_aivs_am + v_aivs_am_subcta;
                     LET v_pesos_am = v_pesos_am + v_pesos_am_subcta;
                  END IF

                  LET v_aivs_subcta  = p_aivs_deudor * (-1);
                  LET v_pesos_subcta = p_sdo_deudor * (-1);

                  LET p_sdo_deudor  = 0;
                  LET p_aivs_deudor = 0;
               ELSE   ----el saldo deudor es mayor al saldo de la subcuenta
                  LET v_aivs_subcta  = v_aivs_subcta * (-1);
                  LET v_pesos_subcta = v_pesos_subcta * (-1);

                  LET p_aivs_deudor = p_aivs_deudor + v_aivs_subcta;
                  LET p_sdo_deudor  = p_sdo_deudor + v_pesos_subcta;
               END IF

               LET v_f_registro = TODAY;
               LET v_h_registro = CURRENT;

               LET v_aivs_deudor  = v_aivs_deudor + v_aivs_subcta;
               LET v_pesos_deudor = v_pesos_deudor + v_pesos_subcta;
               LET v_origen       = "TRANSF ANUALIDAD";

               IF v_aivs_subcta < 0 THEN   ---liquidación deudor
                  INSERT INTO cre_ag_preliquida
                  VALUES ( v_f_liquida,
                           p_id_derechohabiente,
                           v_subcuenta,
                           v_fondo_inv,
                           v_movimiento,
                           p_folio_liq,
                           p_id_cre_acreditado,
                           v_aivs_subcta,
                           v_pesos_subcta,
                           v_f_valor,
                           v_f_registro,
                           v_h_registro,
                           v_origen);
               END IF

               IF v_aivs_am_subcta < 0 AND
                  (v_subcuenta = 8 OR v_subcuenta = 42) THEN   ---liquidación amortización
                  LET v_origen     = "REMANENTE ANUALIDAD";

                  INSERT INTO cre_ag_preliquida
                  VALUES ( v_f_liquida,
                           p_id_derechohabiente,
                           v_subcuenta,
                           v_fondo_inv,
                           v_mvto_amort,
                           p_folio_liq,
                           p_id_cre_acreditado,
                           v_aivs_am_subcta,
                           v_pesos_am_subcta,
                           v_f_valor,
                           v_f_registro,
                           v_h_registro,
                           v_origen);
               END IF
            END IF
         END FOREACH;

         IF p_sdo_deudor > 0 THEN
            LET v_aivs_cc  = p_aivs_deudor * (-1);
            LET v_pesos_cc = p_sdo_deudor * (-1);

            LET v_tpo_deudor = 5;
         END IF

         ----preliquidación deudor
         IF v_aivs_deudor < 0 THEN
            LET v_movimiento = 492;

            IF v_pesos_deudor = 0 THEN
               SELECT SUM(tp.monto_pesos)
                 INTO v_pesos_deudor
                 FROM cre_ag_preliquida tp
                WHERE tp.folio_liquida      = p_folio_liq
                  AND tp.id_derechohabiente = p_id_derechohabiente
                  AND tp.movimiento IN(492,152);
            END IF

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_id_cre_acreditado,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_agr
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_movimiento,
                     p_id_cre_acreditado,
                     v_aivs_deudor,
                     v_pesos_deudor,
                     v_f_liquida);
         END IF

         ----preliquidación deudor amortización
         IF v_aivs_am < 0 THEN
            LET v_mvto_amort = 82;

            IF v_tpo_deudor = -1 THEN
               LET v_tpo_deudor = 4;
            END IF

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_mvto_amort,
                     p_id_cre_acreditado,
                     v_aivs_am,
                     v_pesos_am,
                     v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_agr
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_mvto_amort,
                     p_id_cre_acreditado,
                     v_aivs_am,
                     v_pesos_am,
                     v_f_liquida);
         END IF

         ----preliquidación deudor cargo a credito
         IF v_aivs_cc < 0 THEN
            LET v_tpo_deudor = 5;

            INSERT INTO cre_saldo_deudor
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_mvto_cargo,
                     p_id_cre_acreditado,
                     v_aivs_cc,
                     v_pesos_cc,
                     v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_agr
            VALUES ( p_id_cre_acreditado,
                     p_folio_liq,
                     v_f_liquida,
                     v_mvto_cargo,
                     p_id_cre_acreditado,
                     v_aivs_cc,
                     v_pesos_cc,
                     v_f_liquida);
         END IF

         IF v_tpo_deudor = -1 THEN
            LET v_tpo_deudor = 3;
         END IF
      END IF

      INSERT INTO cre_ctr_contable
      VALUES (p_id_cre_acreditado,
              v_tpo_deudor,
              p_tpo_trabajador,
              p_folio_liq,
              p_folio_acr );

     {
      ----ejecución función para verificación de saldo en fondo72
      EXECUTE FUNCTION safre_viv:fn_cre_preliquida_fondo72(p_folio_liq,
                                                           p_id_cre_acreditado,
                                                           p_nss,
                                                           v_tpo_origina)
                                                      INTO v_ax_status;
     }
   END IF

   RETURN v_status;

   --Finaliza la función de preliquidación de anualidades garantizadas
END FUNCTION;


