






CREATE FUNCTION "safreviv".fn_acr_preliquida_rem(p_folio_liq DECIMAL(9,0),
                                      p_folio_acr DECIMAL(9,0),
                                      p_id_cre_acreditado DECIMAL(9,0),
                                      p_id_derechohabiente DECIMAL(9,0),
                                      p_valor_fondo DECIMAL(19,14),
                                      p_tpo_trabajador CHAR(1),
                                      p_f_liq DATE)

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

   DEFINE v_f_liquida       DATE;
   DEFINE v_f_registro      DATE;
   DEFINE v_h_registro      DATETIME HOUR TO SECOND;
   DEFINE v_f_valor         DATE;

   ----variable para status de ejecución
   DEFINE v_status          SMALLINT;
   DEFINE v_bnd             SMALLINT;
   DEFINE v_ax_status       SMALLINT;
   DEFINE v_fc_status       SMALLINT;
   DEFINE v_tpo_origina     SMALLINT;

   ----variables para registro en control contable
   DEFINE v_tpo_trabajador  CHAR(1);
   DEFINE v_tpo_deudor      SMALLINT;

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

   -- Varibales para la ejecucion de preliquidacion voluntaria riss
   DEFINE v_estado_volriss        SMALLINT;
   DEFINE v_error_volriss         SMALLINT;
   DEFINE v_proceso               SMALLINT;

   --SET DEBUG FILE TO 'preliquidaAcrRem.trace';
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
   LET v_bnd             = 0;
   LET p_aivs_deudor     = 0;
   LET v_status          = 0;
   LET v_tpo_deudor      = -1;

   LET v_mvto_cargo      = 252;
   LET v_fondo_inv       = 11;
   LET v_f_liquida       = p_f_liq;
   LET v_f_valor         = p_f_liq;
   LET v_origen          = "REMANENTE TRANS ACRED";

   LET v_tpo_origina     = 1;

   LET v_estado_volriss = 0;
   LET v_error_volriss  = 0;
   LET v_proceso        = 212;

   ---Valor aivs al día de la preliquidación
   IF p_valor_fondo IS NULL THEN
      LET v_status = 1;
   ELSE
{
      -- Se realiza la ejecucion de la función que preliquida la subcuenta 55
      EXECUTE FUNCTION fn_cre_preqliq_volriss(p_folio_liq         ,
                                              p_id_cre_acreditado ,
                                              p_id_derechohabiente,
                                              p_valor_fondo       ,
                                              p_tpo_trabajador    ,
                                              v_proceso           )
                                         INTO v_estado_volriss,v_error_volriss;
}

      DELETE
        FROM safre_tmp:tmp_cta_mov_acr
       WHERE id_derechohabiente = p_id_derechohabiente;

      INSERT INTO safre_tmp:tmp_cta_mov_acr
      SELECT mv.*
        FROM cta_movimiento mv
       WHERE mv.id_derechohabiente = p_id_derechohabiente
         AND mv.subcuenta IN(4,8,42,44);

{
      INSERT INTO safre_tmp:tmp_cta_mov_acr
      SELECT mvp.*
        FROM cre_ta_preliquida mvp
       WHERE mvp.id_derechohabiente = p_id_derechohabiente
         AND mvp.subcuenta IN(4,55)
         AND mvp.movimiento IN (881,1572);
}

      ----Obtención saldo de la cuenta individual
      SELECT SUM(monto_acciones), ROUND((sum(monto_acciones*p_valor_fondo)),2)
        INTO v_aivs_sdo, v_pesos_sdo
        FROM safre_tmp:tmp_cta_mov_acr
       WHERE id_derechohabiente = p_id_derechohabiente
         AND subcuenta IN(4,8,42,44);

      IF v_aivs_sdo IS NULL OR v_aivs_sdo <= 0 THEN
         LET v_aivs_sdo  = 0;
         LET v_pesos_sdo = 0;
      ELSE  ----Obtención saldo por subcuenta
         FOREACH
            SELECT m.subcuenta, c.prelacion, SUM(m.monto_acciones)
              INTO v_subcuenta, v_prelacion, v_aivs_subcta
              FROM safre_tmp:tmp_cta_mov_acr m, cat_subcuenta c
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
               LET v_mvto_amort = 572;
            ELSE
               LET v_mvto_amort = 582;
            END IF

            IF v_aivs_subcta > 0 THEN   ----hay saldo remanente
               LET v_aivs_subcta  = v_aivs_subcta * (-1);
               LET v_pesos_subcta = v_pesos_subcta * (-1);

               LET v_f_registro = TODAY;
               LET v_h_registro = CURRENT;

               INSERT INTO cre_ta_preliquida
               VALUES (v_f_liquida,
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

               LET v_aivs_am = v_aivs_am + v_aivs_subcta;
               LET v_pesos_am = v_pesos_am + v_pesos_subcta;

               LET v_aivs_subcta  = 0;
               LET v_pesos_subcta = 0;
            END IF
         END FOREACH;

         -- se valida los pesos obtenidos
         IF v_pesos_am IS NULL THEN
            LET v_pesos_am = 0;
         END IF

         -- preliquidación amortización
         IF v_aivs_am < 0 THEN
            LET v_mvto_amort = 572;

            INSERT INTO cre_saldo_deudor
            VALUES (p_id_cre_acreditado,
                    p_folio_liq,
                    v_f_liquida,
                    v_mvto_amort,
                    p_id_cre_acreditado,
                    v_aivs_am,
                    v_pesos_am,
                    v_f_liquida);

            INSERT INTO safre_tmp:tmp_cre_saldo_deudor_acr
            VALUES (p_id_cre_acreditado,
                    p_folio_liq,
                    v_f_liquida,
                    v_mvto_amort,
                    p_id_cre_acreditado,
                    v_aivs_am,
                    v_pesos_am,
                    v_f_liquida);
         END IF
      END IF
   END IF

   RETURN v_status;

   --Finaliza la función de preliquidación de remanentes de transferencia de acreditados
END FUNCTION;


