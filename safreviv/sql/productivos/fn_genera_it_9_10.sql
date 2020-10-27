






CREATE FUNCTION "safreviv".fn_genera_it_9_10()
RETURNING INTEGER

   DEFINE v_tot_registros           INTEGER;
   DEFINE v_aivs92                  DECIMAL(12,2);
   DEFINE v_aivs97                  DECIMAL(12,2);

   DEFINE v_tpo                     CHAR(1);
   DEFINE v_nss                     CHAR(11);
   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_tpo_orig                SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_folio_liquida           DECIMAL(9,0);
   DEFINE v_movimiento              SMALLINT;
   DEFINE v_f_saldo                 DATE;
   DEFINE v_id_referencia           DECIMAL(9,0);
   DEFINE v_ax_excep_error          SMALLINT;
   DEFINE v_reintentos              SMALLINT;
   DEFINE v_causal                  CHAR(3);
   DEFINE v_importe                 DECIMAL(12,2);

   ON EXCEPTION SET v_ax_excep_error
      -- Devolverá el código de error que ocasione la excepción
      RETURN v_ax_excep_error;
   END EXCEPTION

   SET DEBUG FILE TO '/safreviv_int/archivos/genArh910.trace';
   TRACE ON;

   LET v_tot_registros = 1;
   LET v_aivs92        = 0;
   LET v_aivs97        = 0;

   LET v_tpo                = "";
   LET v_nss                = "";
   LET v_id_derechohabiente = "";
   LET v_id_cre_acreditado  = "";
   LET v_tpo_orig           = "";
   LET v_f_proceso          = "";
   LET v_periodo_pago       = "";
   LET v_folio_liquida      = "";
   LET v_f_saldo            = "";
   LET v_movimiento         = "";
   LET v_id_referencia      = "";
   LET v_reintentos         = "";
   LET v_causal             = "";
   LET v_importe            = "";

   FOREACH
      SELECT *
        INTO v_tpo               ,
             v_nss               ,
             v_id_derechohabiente,
             v_id_cre_acreditado ,
             v_tpo_orig          ,
             v_f_proceso         ,
             v_periodo_pago
        FROM safre_tmp:reg_it9_10
      ORDER BY tpo

      IF v_tpo = "A" OR
         v_tpo = "E" OR
         v_tpo = "U" OR
         v_tpo = "W" OR
         v_tpo = "C" THEN

         IF v_tpo = "A" OR
            v_tpo = "E" THEN
            IF v_tpo_orig = 1 THEN
               LET v_movimiento = 472;
            ELSE
               LET v_movimiento = 492;
            END IF
         END IF

         IF v_tpo = "U" OR
            v_tpo = "W" THEN
            LET v_movimiento = 242;
         END IF

         IF v_tpo = "C" THEN
            LET v_movimiento = 62;
         END IF

         IF v_tpo = "A" OR
            v_tpo = "E" THEN
            SELECT folio_liquida
              INTO v_folio_liquida
              FROM cre_acreditado
             WHERE id_cre_acreditado = v_id_cre_acreditado;
         ELSE
            SELECT folio_liquida, periodo_pago, importe_v97
              INTO v_folio_liquida, v_periodo_pago, v_importe
              FROM cre_uso_garantia
             WHERE id_cre_uso_garantia = v_id_cre_acreditado;
         END IF

         IF v_tpo = "C" THEN
            LET v_id_referencia = v_periodo_pago;
         ELSE
            LET v_id_referencia = v_id_cre_acreditado;
         END IF

         IF v_folio_liquida IS NULL OR v_folio_liquida = "" THEN
            LET v_folio_liquida = 0;
         END IF

         SELECT f_liquida, SUM(monto_acciones)
           INTO v_f_saldo, v_aivs92
           FROM safre_tmp:tmp_reg_adelanto
          WHERE id_derechohabiente = v_id_derechohabiente
            AND id_referencia      = v_id_referencia
            AND folio_liquida      = v_folio_liquida
            AND subcuenta          = 8
         GROUP BY 1;

         IF v_f_saldo IS NULL OR v_f_saldo = "" THEN
            LET v_f_saldo = v_f_proceso;
         END IF

         IF v_aivs92 IS NULL OR v_aivs92 = "" THEN
            LET v_aivs92 = 0;
         END IF

         SELECT f_liquida, SUM(monto_acciones)
           INTO v_f_saldo, v_aivs97
           FROM safre_tmp:tmp_reg_adelanto
          WHERE id_derechohabiente = v_id_derechohabiente
            AND id_referencia      = v_id_referencia
            AND folio_liquida      = v_folio_liquida
            AND subcuenta          = 4
         GROUP BY 1;

         IF v_f_saldo IS NULL OR v_f_saldo = "" THEN
            LET v_f_saldo = v_f_proceso;
         END IF

         IF v_aivs97 IS NULL OR v_aivs97 = "" THEN
            LET v_aivs97 = 0;
         END IF

         IF v_tpo = "A" THEN
            SELECT COUNT(*)
              INTO v_reintentos
              FROM cre_his_acreditado
             WHERE id_cre_acreditado = v_id_cre_acreditado
               AND edo_procesar IN(90,100,110);

            IF v_reintentos IS NULL OR v_reintentos = "" THEN
               LET v_reintentos = 0;
            END IF

            FOREACH
               SELECT FIRST 1 diagnostico
                 INTO v_causal
                 FROM cre_his_acreditado
                WHERE id_cre_acreditado = v_id_cre_acreditado
                  AND edo_procesar      IN(90,100,110)
                  AND f_proceso IN(SELECT MAX(f_proceso)
                                     FROM cre_his_acreditado
                                    WHERE id_cre_acreditado = v_id_cre_acreditado
                                      AND edo_procesar IN(90,100,110))
            END FOREACH;
         END IF

         IF v_tpo = "W" OR v_tpo = "C" THEN
            SELECT COUNT(*)
              INTO v_reintentos
              FROM cre_uso_garantia
             WHERE id_derechohabiente  = v_id_derechohabiente
               AND id_cre_uso_garantia > v_id_cre_acreditado
               AND importe_v97         = v_importe
               AND edo_procesar        IN(90,100,110);

            IF v_reintentos IS NULL OR v_reintentos = "" THEN
               LET v_reintentos = 0;
            END IF

            FOREACH
               SELECT FIRST 1 diagnostico
                 INTO v_causal
                 FROM cre_uso_garantia
                WHERE id_derechohabiente  = v_id_derechohabiente
                  AND id_cre_uso_garantia > v_id_cre_acreditado
                  AND edo_procesar        IN(90,100,110)
                  AND importe_v97         = v_importe
                  AND f_proceso IN(SELECT MAX(f_proceso)
                                     FROM cre_uso_garantia
                                    WHERE id_derechohabiente  = v_id_derechohabiente
                                      AND id_cre_uso_garantia > v_id_cre_acreditado
                                      AND edo_procesar        IN(90,100,110)
                                      AND importe_v97         = v_importe)

               IF v_causal IS NULL OR v_causal = "" THEN
                  LET v_causal = "NA";
               END IF
            END FOREACH;
         END IF
      END IF

      IF v_tpo = "N" OR
         v_tpo = "V" THEN
         SELECT SUM(monto_acciones)
           INTO v_aivs92
           FROM cta_movimiento
          WHERE id_derechohabiente = v_id_derechohabiente
            AND subcuenta          = 8
            AND fondo_inversion    = 11;

         IF v_aivs92 IS NULL OR v_aivs92 = "" THEN
            LET v_aivs92 = 0;
         END IF

         SELECT SUM(monto_acciones)
           INTO v_aivs97
           FROM cta_movimiento
          WHERE id_derechohabiente = v_id_derechohabiente
            AND subcuenta          = 4
            AND fondo_inversion    = 11;

         IF v_aivs97 IS NULL OR v_aivs97 = "" THEN
            LET v_aivs97 = 0;
         END IF
      END IF

      IF v_tpo = "C" OR v_tpo = "W" THEN
        LET v_tpo = "A";
      END IF

      IF v_tpo = "U" THEN
        LET v_tpo = "E";
      END IF

      IF v_tpo = "V" THEN
        LET v_tpo = "N";
      END IF

      INSERT INTO safre_tmp:arh_it9_10 (tpo,
                              nss,
                              movimiento,
                              aivs_92,
                              aivs_97,
                              tpo_orig,
                              f_proceso,
                              perido_pago,
                              reintentos,
                              causal)
                      VALUES (v_tpo,
                              v_nss,
                              v_movimiento,
                              v_aivs92,
                              v_aivs97,
                              v_tpo_orig,
                              v_f_saldo,
                              v_periodo_pago,
                              v_reintentos,
                              v_causal);

      LET v_tot_registros = v_tot_registros + 1;

      LET v_aivs92             = 0;
      LET v_aivs97             = 0;
      LET v_tpo                = "";
      LET v_nss                = "";
      LET v_id_derechohabiente = "";
      LET v_id_cre_acreditado  = "";
      LET v_tpo_orig           = "";
      LET v_f_proceso          = "";
      LET v_periodo_pago       = "";
      LET v_folio_liquida      = "";
      LET v_f_saldo            = "";
      LET v_movimiento         = "";
      LET v_id_referencia      = "";
      LET v_reintentos         = "";
      LET v_causal             = "";
      LET v_importe            = "";
   END FOREACH;

   RETURN v_tot_registros;

END FUNCTION;


