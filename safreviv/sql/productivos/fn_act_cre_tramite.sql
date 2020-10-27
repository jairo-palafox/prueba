






CREATE FUNCTION "safreviv".fn_act_cre_tramite()
   RETURNING SMALLINT, SMALLINT, INTEGER

   DEFINE v_ax_diagnostico                 CHAR(4);
   DEFINE v_usuario_marca                  CHAR(20);
   DEFINE v_ax_ususario                    CHAR(20);
   DEFINE v_f_vigencia                     DATE;
   DEFINE v_ax_f_vigencia                  DATE;
   DEFINE v_ax_f_proceso                   DATETIME HOUR TO SECOND;
   DEFINE v_id_cre_tramite                 DECIMAL(9,0);
   DEFINE v_id_derechohabiente             DECIMAL(9,0);
   DEFINE v_num_credito                    DECIMAL(10,0);
   DEFINE v_ax_num_credito                 DECIMAL(10,0);
   DEFINE v_n_referencia                   INTEGER;
   DEFINE v_estado                         SMALLINT;
   DEFINE v_marca                          SMALLINT;
   DEFINE v_proceso_marca                  SMALLINT;
   DEFINE v_ax_estado                      SMALLINT;
   DEFINE v_ax_cod_error                   SMALLINT;
   DEFINE v_estado_marca                   SMALLINT;
   DEFINE v_edo_fin                        SMALLINT;
   DEFINE v_fecha_verifica                 DATE;
   DEFINE v_tot_canc                       INTEGER;
   DEFINE v_id_cre_acreditado              DECIMAL(9,0);

   -- Variables para la inserción en la tabla cta_marca_ws
   DEFINE v_tpo_originacion                CHAR(2);
   DEFINE v_tpo_credito                    SMALLINT;

   -- Variables en caso de error
   DEFINE v_cod_error                      SMALLINT;
   DEFINE v_si_error                       SMALLINT;

   ON EXCEPTION SET v_cod_error
      LET v_si_error = 1;
      RETURN v_si_error, v_cod_error, v_tot_canc;
   END EXCEPTION;

   ---SET DEBUG FILE TO "/safreviv_int/archivos/fn_act_cre_tramite.trace";
   ---TRACE ON;

   -- InicializaciÃ³n de variables
   LET v_marca             = 213;
   LET v_si_error          = 0;
   LET v_cod_error         = 0;
   LET v_tpo_originacion   = "43";
   LET v_tpo_credito       = 10;
   LET v_estado_marca      = 40;
   LET v_ax_ususario       = "infonavit";
   LET v_edo_fin           = 290;
   LET v_fecha_verifica    = TODAY;
   LET v_f_vigencia        = "";
   LET v_tot_canc          = 0;
   LET v_id_cre_acreditado = "";

   ---Registros con fecha de vigencia
   FOREACH
      SELECT t.id_cre_tramite, MAX(h.f_vigencia)
        INTO v_id_cre_tramite, v_f_vigencia
        FROM cre_tramite t, cre_his_tramite h
       WHERE t.estado         = 18
         AND t.id_cre_tramite = h.id_cre_tramite
      GROUP BY 1

      FOREACH
       SELECT FIRST 1 id_derechohabiente
         INTO v_id_derechohabiente 
         FROM cre_tramite
        WHERE id_cre_tramite = v_id_cre_tramite
      END FOREACH;

      LET v_f_vigencia = v_f_vigencia + 60 UNITS DAY;

      IF v_f_vigencia < TODAY THEN
         FOREACH
            SELECT sfr.n_referencia,
                   sfr.usuario_marca,
                   sfr.proceso_marca
              INTO v_n_referencia,
                   v_usuario_marca,
                   v_proceso_marca
              FROM cre_tramite cre,
                   sfr_marca_activa sfr
             WHERE cre.id_cre_tramite     = v_id_cre_tramite
               AND cre.id_derechohabiente = sfr.id_derechohabiente
               AND sfr.marca   = 213
               AND cre.estado  = 18

            -- Se invoca a la función que desmarca la cuenta
            EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                                 v_marca,         -- 213
                                                 v_n_referencia,
                                                 v_estado_marca,   --40
                                                 v_marca,
                                                 v_ax_ususario,
                                                 v_proceso_marca )
                                            INTO v_ax_cod_error;
         END FOREACH;

         -- Se forma la desmarca a Procesar
         IF NOT EXISTS (SELECT id_derechohabiente
                          FROM sfr_marca_activa
                         WHERE id_derechohabiente = v_id_derechohabiente
                           AND marca IN(SELECT UNIQUE marca_inf
                                          FROM cat_tipo_credito)) THEN
            -- Se abre un where marca_inf is not null foreach para que en caso de que tenga mas de un registro, tome solo el primero
            FOREACH
               SELECT FIRST 1 num_credito,
                      f_vigencia,
                      estado,
                      diagnostico,
                      f_proceso
                 INTO v_ax_num_credito,
                      v_ax_f_vigencia,
                      v_ax_estado,
                      v_ax_diagnostico,
                      v_ax_f_proceso
                 FROM cre_his_tramite
                WHERE id_cre_tramite = v_id_cre_tramite
                  AND f_proceso = ( SELECT MIN(f_proceso)
                                      FROM cre_his_tramite
                                     WHERE id_cre_tramite = v_id_cre_tramite )
            END FOREACH;

            IF NOT EXISTS(SELECT id_derechohabiente
                            FROM cta_marca_ws
                           WHERE id_derechohabiente = v_id_derechohabiente
                             AND id_origen in( SELECT MAX(id_cre_acreditado)
                                                 FROM cre_acreditado
                                                WHERE id_derechohabiente = v_id_derechohabiente
                                                  AND estado             = 18
                                                  AND tpo_credito        = 10 )) THEN
               SELECT MAX(id_cre_acreditado)
                 INTO v_id_cre_acreditado
                 FROM cre_acreditado
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND estado             = 18
                  AND tpo_credito        = 10;

               IF v_id_cre_acreditado IS NOT NULL THEN
                  -- estructura cta_marca_ws
                  INSERT INTO cta_marca_ws VALUES(v_id_derechohabiente,
                                                  v_id_cre_acreditado,
                                                  v_tpo_originacion,
                                                  v_tpo_credito,
                                                  234,
                                                  TODAY,
                                                  1,
                                                  "",
                                                  "",
                                                  0,
                                                  v_ax_num_credito,
                                                  TODAY,
                                                  '04',
                                                  1,
                                                  v_ax_ususario   --'infonavit'
                                                  );
               END IF
            END IF
         END IF

         -- Se actualiza el estado de cre_tramite a 290
         UPDATE cre_tramite
            SET estado         = v_edo_fin
          WHERE id_cre_tramite = v_id_cre_tramite;

         UPDATE cre_his_tramite
            SET estado         = v_edo_fin
          WHERE id_cre_tramite = v_id_cre_tramite;

         UPDATE cre_acreditado
            SET estado             = v_edo_fin
          WHERE id_derechohabiente = v_id_derechohabiente
            AND tpo_credito        = 10
            AND estado             = 18;

         LET v_tot_canc = v_tot_canc + 1;
      END IF

      LET v_f_vigencia = "";
      LET v_id_cre_acreditado = "";
   END FOREACH;

   LET v_f_vigencia = "";
   LET v_id_cre_acreditado = "";

   ---Registros sin fecha de vigencia
   FOREACH
      SELECT t.id_cre_tramite, t.id_derechohabiente
        INTO v_id_cre_tramite, v_id_derechohabiente
        FROM cre_tramite t
       WHERE t.estado = 18
         AND t.f_vigencia IS NULL

      FOREACH
         SELECT sfr.n_referencia,
                sfr.usuario_marca,
                sfr.proceso_marca
           INTO v_n_referencia,
                v_usuario_marca,
                v_proceso_marca
           FROM cre_tramite cre,
                sfr_marca_activa sfr
          WHERE cre.id_cre_tramite     = v_id_cre_tramite
            AND cre.id_derechohabiente = sfr.id_derechohabiente
            AND sfr.marca   = 213
            AND cre.estado  = 18

         -- Se invoca a la función que desmarca la cuenta
         EXECUTE FUNCTION fn_desmarca_cuenta( v_id_derechohabiente,
                                              v_marca,         -- 213
                                              v_n_referencia,
                                              v_estado_marca,   --40
                                              v_marca,
                                              v_ax_ususario,
                                              v_proceso_marca )
                                         INTO v_ax_cod_error;
      END FOREACH;

      -- Se forma la desmarca a Procesar
      IF NOT EXISTS (SELECT id_derechohabiente
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = v_id_derechohabiente
                        AND marca IN(SELECT UNIQUE marca_inf
                                       FROM cat_tipo_credito)) THEN
         -- Se abre un where marca_inf is not null foreach para que en caso de que tenga mas de un registro, tome solo el primero
         FOREACH
            SELECT FIRST 1 num_credito,
                   f_vigencia,
                   estado,
                   diagnostico,
                   f_proceso
              INTO v_ax_num_credito,
                   v_ax_f_vigencia,
                   v_ax_estado,
                   v_ax_diagnostico,
                   v_ax_f_proceso
              FROM cre_his_tramite
             WHERE id_cre_tramite = v_id_cre_tramite
               AND f_proceso = ( SELECT MIN(f_proceso)
                                   FROM cre_his_tramite
                                  WHERE id_cre_tramite = v_id_cre_tramite )
         END FOREACH;

         IF NOT EXISTS(SELECT id_derechohabiente
                         FROM cta_marca_ws
                        WHERE id_derechohabiente = v_id_derechohabiente
                          AND id_origen in( SELECT MAX(id_cre_acreditado)
                                              FROM cre_acreditado
                                             WHERE id_derechohabiente = v_id_derechohabiente
                                               AND estado             = 18
                                               AND tpo_credito        = 10 )) THEN
            SELECT MAX(id_cre_acreditado)
              INTO v_id_cre_acreditado
              FROM cre_acreditado
             WHERE id_derechohabiente = v_id_derechohabiente
               AND estado             = 18
               AND tpo_credito        = 10;

            IF v_id_cre_acreditado IS NOT NULL THEN
               -- estructura cta_marca_ws
               INSERT INTO cta_marca_ws VALUES(v_id_derechohabiente,
                                               v_id_cre_acreditado,
                                               v_tpo_originacion,
                                               v_tpo_credito,
                                               234,
                                               TODAY,
                                               1,
                                               "",
                                               "",
                                               0,
                                               v_ax_num_credito,
                                               TODAY,
                                               '04',
                                               1,
                                               v_ax_ususario   --'infonavit'
                                               );
            END IF
         END IF
      END IF

      -- Se actualiza el estado de cre_tramite a 290
      UPDATE cre_tramite
         SET estado         = v_edo_fin
       WHERE id_cre_tramite = v_id_cre_tramite;

      UPDATE cre_his_tramite
         SET estado         = v_edo_fin
       WHERE id_cre_tramite = v_id_cre_tramite;

      UPDATE cre_acreditado
         SET estado             = v_edo_fin
       WHERE id_derechohabiente = v_id_derechohabiente
         AND tpo_credito        = 10
         AND estado             = 18;

      LET v_tot_canc = v_tot_canc + 1;

      LET v_f_vigencia = "";
      LET v_id_cre_acreditado = "";
   END FOREACH;

   RETURN v_si_error, v_cod_error, v_tot_canc;

END FUNCTION;


