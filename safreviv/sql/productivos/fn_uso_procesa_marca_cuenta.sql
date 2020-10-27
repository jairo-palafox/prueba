






CREATE FUNCTION "safreviv".fn_uso_procesa_marca_cuenta(p_v_usuario            CHAR(20),
                                            p_d_folio              DECIMAL(9),
                                            p_d_id_cre_ctr_archivo DECIMAL(9),
                                            p_si_proceso_cod       SMALLINT)
   RETURNING SMALLINT;

   -- REGISTRO de cre acreditado
   DEFINE cre_id_cre_uso_garantia   DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_tpo_transferencia     CHAR(2);
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_edo_procesar          SMALLINT;

   -- Variables auxiliares
   DEFINE v_ax_existe_marca         SMALLINT; -- indica si ya existe la marca
   DEFINE v_ax_fecha_causa          DATE; -- fecha causa
   DEFINE v_ax_marca_inf            SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc            SMALLINT; -- marca procesar
   DEFINE v_ax_id_credito           SMALLINT; -- identificador del crédito
   DEFINE v_ax_cod_error            SMALLINT; -- contiene código error (retorno de la función externa)
   DEFINE v_ax_excep_error          SMALLINT; -- contiene código error (ocurrido en el proceso)
   DEFINE v_ax_codigo_rechazo       SMALLINT; -- código de rechazo
   DEFINE v_ax_estado_marca         SMALLINT; -- estado marca
   DEFINE v_ax_marca_activa         SMALLINT; -- marca activa
   DEFINE v_ax_marca_causa          SMALLINT; -- marca causa
   DEFINE v_ax_marca_entra          SMALLINT; -- marca entra
   DEFINE v_ax_sts_marcaje          SMALLINT; -- estatus de retorno de la función
   DEFINE v_result_conv             SMALLINT; -- resultado rechazo por convivencia
   DEFINE v_id_ocg_solicitud_ug     DECIMAL(9,0); -- idendificador de registro en ocg_ug
   DEFINE v_inconsistencia          SMALLINT; -- inconsistencia de registro en ocg_ug

   ON EXCEPTION SET v_ax_excep_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_excep_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/usoProcMarca.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_excep_error      = 0;
   LET v_ax_fecha_causa      = "";
   LET v_ax_codigo_rechazo   = 0;
   LET v_ax_estado_marca     = 0;
   LET v_ax_marca_causa      = NULL;
   LET v_result_conv         = 0;
   LET v_id_ocg_solicitud_ug = "";
   LET v_inconsistencia      = "";

   FOREACH
      -- se consultan los registros de cre acreditado
      SELECT id_cre_uso_garantia,
             id_derechohabiente,
             tpo_transferencia,
             num_credito,
             edo_procesar
        INTO cre_id_cre_uso_garantia,
             cre_id_derechohabiente,
             cre_tpo_transferencia,
             cre_num_credito,
             cre_edo_procesar
        FROM cre_uso_garantia
       WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_archivo
         AND estado = 10

      -- verifica si el registro corresponde a Anualidades Garantizadas (tpo transf = 43)
      IF cre_tpo_transferencia = "43" THEN
         LET v_ax_marca_entra = 225; -- uso de la garantía

         IF EXISTS ( SELECT id_derechohabiente
                       FROM sfr_marca_activa
                      WHERE id_derechohabiente = cre_id_derechohabiente
                        AND marca IN(SELECT marca_inf
                                       FROM cat_tipo_credito
                                      WHERE tpo_originacion = 4) ) THEN

            -- se indica que si existe el derechohabiente en la tabla
            LET v_ax_existe_marca = 1;
         ELSE
            -- se indica que no existe el derechohabiente en la tabla
            LET v_ax_existe_marca = 0;
         END IF

         -- se verifica si existe la marca
         IF v_ax_existe_marca = 1 THEN
            -- Para los registros solo infonavit no genera la marca 225
            -- solo actualiza en cre_uso_garantia a 20 (Vigente marcada)
            IF (cre_edo_procesar = 7) THEN
               UPDATE cre_uso_garantia
                  SET estado = 20
                WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
            ELSE
               -- se ejecuta la función de marcaje
               EXECUTE FUNCTION fn_marca_cuenta(cre_id_derechohabiente,
                                                v_ax_marca_entra,
                                                cre_id_cre_uso_garantia,
                                                p_d_folio,
                                                v_ax_estado_marca,
                                                v_ax_codigo_rechazo,
                                                v_ax_marca_causa,
                                                v_ax_fecha_causa,
                                                p_v_usuario,
                                                p_si_proceso_cod)
                                          INTO  v_ax_sts_marcaje;

               -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
               IF v_ax_sts_marcaje = 0 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE cre_uso_garantia
                     SET estado = 20
                   WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
               ELSE
                  -- si el estado de retorno de marcaje es nulo se asigna cero
                  IF v_ax_sts_marcaje IS NULL THEN
                     LET v_ax_sts_marcaje = 0;
                  ELIF v_ax_sts_marcaje < 0 THEN
                     FOREACH
                        SELECT FIRST 1 c.marca_activa,
                               c.rch_cod
                          INTO v_ax_marca_activa,
                               v_ax_sts_marcaje
                          FROM sfr_convivencia c,
                               sfr_marca_activa a
                         WHERE a.id_derechohabiente = cre_id_derechohabiente
                           AND a.marca = c.marca_activa
                           AND c.marca_entra = v_ax_marca_entra
                           AND c.rch_cod > 0
                         ORDER BY a.f_inicio DESC
                     END FOREACH;
                     --LET v_ax_sts_marcaje = v_ax_sts_marcaje * -1;
                  END IF

                  -- en caso de ser mayor a cero
                  IF v_ax_sts_marcaje > 999 THEN
                     LET v_ax_sts_marcaje = 150;
                  END IF

                  -- se actualiza el estado en cre uso garantia
                  UPDATE cre_uso_garantia
                     SET estado = 150,
                         diagnostico = v_ax_sts_marcaje
                   WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
               END IF
            END IF
         ELSE
            -- se actualiza el estado en cre uso garantia
            UPDATE cre_uso_garantia
               SET estado = 150,
                   diagnostico = 13
             WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
         END IF
      ELSE
         LET v_ax_marca_entra = 223; -- uso de la anualidad
         LET v_inconsistencia = "";

         IF EXISTS (
            SELECT id_derechohabiente
              FROM sfr_marca_activa
             WHERE id_derechohabiente = cre_id_derechohabiente
               AND marca = 202) THEN

            -- Para los registros solo infonavit no genera la marca 223
            -- solo actualiza en cre_uso_garantia a 20 (Vigente marcada)
            IF (cre_edo_procesar = 7) THEN
               UPDATE cre_uso_garantia
                  SET estado = 20
                WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
            ELSE
               -- se indica que si existe el derechohabiente en la tabla y se ejecuta la función de marcaje
               EXECUTE FUNCTION fn_marca_cuenta(cre_id_derechohabiente,
                                                v_ax_marca_entra,
                                                cre_id_cre_uso_garantia,
                                                p_d_folio,
                                                v_ax_estado_marca,
                                                v_ax_codigo_rechazo,
                                                v_ax_marca_causa,
                                                v_ax_fecha_causa,
                                                p_v_usuario,
                                                p_si_proceso_cod)
                                           INTO v_ax_sts_marcaje;

               -- si el marcaje fue procedente se ejecuta el procedure que inserta en cta marca ws
               IF v_ax_sts_marcaje = 0 THEN
                  -- se actualiza el estado en cre acreditado y cre his acreditado
                  UPDATE cre_uso_garantia
                     SET estado = 20
                   WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;
               ELSE
                  -- si el estao de retorno de marcaje es nulo se asigna cero
                  IF v_ax_sts_marcaje IS NULL THEN
                     LET v_ax_sts_marcaje = 0;
                  ELIF v_ax_sts_marcaje < 0 THEN
                     FOREACH
                        SELECT FIRST 1 c.marca_activa,
                               c.rch_cod
                          INTO v_ax_marca_activa,
                               v_ax_sts_marcaje
                          FROM sfr_convivencia c,
                               sfr_marca_activa a
                         WHERE a.id_derechohabiente = cre_id_derechohabiente
                           AND a.marca = c.marca_activa
                           AND c.marca_entra = v_ax_marca_entra
                           AND c.rch_cod > 0
                         ORDER BY a.f_inicio desc
                     END FOREACH;
                     --LET v_ax_sts_marcaje = v_ax_sts_marcaje * -1;
                  END IF

                  -- en caso de ser mayor a cero
                  IF v_ax_sts_marcaje > 999 THEN
                     LET v_ax_sts_marcaje = 150;
                     LET v_inconsistencia = 14;
                  ELIF EXISTS (SELECT rch_cod
                                 FROM cat_rch_marca
                                WHERE rch_cod = v_ax_sts_marcaje
                                  AND rch_cod IN(SELECT UNIQUE marca_inf
                                                  FROM cat_tipo_credito
                                                 WHERE marca_inf IS NOT NULL)) THEN
                     LET v_inconsistencia = 14;
                  ELSE
                     LET v_inconsistencia = 63;
                  END IF

                  -- se actualiza el estado en cre uso garantia
                  UPDATE cre_uso_garantia
                     SET estado = 150,
                         diagnostico = v_ax_sts_marcaje
                   WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;

                  SELECT id_referencia_ocg
                    INTO v_id_ocg_solicitud_ug
                    FROM ocg_transaccion_cre
                   WHERE id_referencia_cre = cre_id_cre_uso_garantia
                     AND subproceso        = 3
                     AND f_proceso         = TODAY;

                  EXECUTE FUNCTION fn_ins_inconsistencia(v_id_ocg_solicitud_ug,
                                                        v_inconsistencia)
                                                   INTO v_result_conv;
               END IF
            END IF
         ELSE
            -- se actualiza el estado en cre uso garantia
            UPDATE cre_uso_garantia
               SET estado = 150,
                   diagnostico = 13
             WHERE id_cre_uso_garantia = cre_id_cre_uso_garantia;

            SELECT id_referencia_ocg
              INTO v_id_ocg_solicitud_ug
              FROM ocg_transaccion_cre
             WHERE id_referencia_cre = cre_id_cre_uso_garantia
               AND subproceso        = 3
               AND f_proceso         = TODAY;

            LET v_inconsistencia = 43;

            EXECUTE FUNCTION fn_ins_inconsistencia(v_id_ocg_solicitud_ug,
                                                  v_inconsistencia)
                                             INTO v_result_conv;
         END IF
      END IF
   END FOREACH

   RETURN v_ax_excep_error;

END FUNCTION;


