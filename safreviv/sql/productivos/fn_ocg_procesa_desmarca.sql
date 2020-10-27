






CREATE FUNCTION "safreviv".fn_ocg_procesa_desmarca(p_id_cre_acreditado DECIMAL(9),
                                    p_sol_desmarca_prc SMALLINT,
                                    p_usuario CHAR(20))

   RETURNING SMALLINT;

   -- REGISTRO de cre acreditado
   DEFINE cre_id_cre_acreditado      DECIMAL(9,0);
   DEFINE cre_id_derechohabiente     DECIMAL(9,0);
   DEFINE cre_tpo_credito            SMALLINT;
   DEFINE cre_num_credito            DECIMAL(10,0);
   DEFINE cre_f_culmina              DATE;
   DEFINE cre_estado                 SMALLINT;
   DEFINE cre_edo_procesar           SMALLINT;
   DEFINE cre_tpo_originacion        SMALLINT;
   DEFINE cre_folio                  DECIMAL(9,0);

   -- REGISTRO de cta credito
   DEFINE cta_id_derechohabiente     DECIMAL(9,0);
   DEFINE cta_proceso_cod            SMALLINT;
   DEFINE cta_tpo_credito            SMALLINT;
   DEFINE cta_num_credito            DECIMAL(10,0);
   DEFINE cta_f_credito              DATE;

   -- REGISTRO de cta his credito
   DEFINE cta_his_id_derechohabiente DECIMAL(9,0);
   DEFINE cta_his_proceso_cod        SMALLINT;
   DEFINE cta_his_tpo_credito        SMALLINT;
   DEFINE cta_his_num_credito        DECIMAL(10,0);
   DEFINE cta_his_f_credito          DATE;
   DEFINE cta_his_estado             SMALLINT;
   DEFINE cta_his_f_actualiza        DATE;

   -- REGISTRO de cta marca ws
   DEFINE ws_id_derechohabiente      DECIMAL(9,0);
   DEFINE ws_id_origen               DECIMAL(9,0);
   DEFINE ws_modulo_cod              CHAR(3);
   DEFINE ws_tpo_credito             SMALLINT;
   DEFINE ws_marca                   SMALLINT;
   DEFINE ws_f_solicita              DATE;
   DEFINE ws_intento                 SMALLINT;
   DEFINE ws_cod_result_op           SMALLINT;
   DEFINE ws_diagnostico             SMALLINT;
   DEFINE ws_situacion               SMALLINT;
   DEFINE ws_num_credito             DECIMAL(10,0);
   DEFINE ws_f_infonavit             DATE;
   DEFINE ws_marca_procesar          CHAR(2);
   DEFINE ws_folio_archivo           DECIMAL(9,0);
   DEFINE ws_usuario                 CHAR(20);

   -- parametros de la función de desmarca
   DEFINE des_id_derechohabiente     DECIMAL(9,0);
   DEFINE des_marca_entra            SMALLINT;
   DEFINE des_n_referencia           INTEGER;
   DEFINE des_estado_marca           SMALLINT;
   DEFINE des_marca_causa            SMALLINT;
   DEFINE des_usuario                CHAR(20);

   -- Variables auxiliares
   DEFINE v_ax_estado                SMALLINT; -- estatus del registro
   DEFINE v_ax_edo_procesar          SMALLINT; -- estado Procesar
   DEFINE v_ax_marca_inf             SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc             SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_transferencia     CHAR(2);  -- tipo de transferencia
   DEFINE v_ax_cod_error             SMALLINT; -- contiene código error (retorno de la función externa)
   DEFINE v_ax_excep_error           SMALLINT; -- contiene código error (ocurrido en el proceso)
   DEFINE v_proceso_cod              SMALLINT;
   DEFINE v_folio_sin_conciliar      DECIMAL(9,0);  --Folio del registro que aun no se concilia con procesar

   ON EXCEPTION SET v_ax_excep_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_excep_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_int/archivos/ProcDesmarcaOCG.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_excep_error      = 0;
   LET v_ax_estado           = 170;
   LET v_ax_edo_procesar     = 210;
   LET cre_id_cre_acreditado = p_id_cre_acreditado;
   LET v_proceso_cod         = 3913;
   LET cre_folio             = 1;

   -- se obtienen los datos de tmp desmarca para el archivo en proceso
   FOREACH
      SELECT c.id_cre_acreditado,
             c.id_derechohabiente,
             c.tpo_credito,
             c.num_credito,
             c.f_culmina,
             c.estado,
             c.edo_procesar,
             c.tpo_originacion,
             r.folio_archivo
        INTO cre_id_cre_acreditado,
             cre_id_derechohabiente,
             cre_tpo_credito,
             cre_num_credito,
             cre_f_culmina,
             cre_estado,
             cre_edo_procesar,
             cre_tpo_originacion,
             cre_folio
        FROM cre_acreditado c,
       OUTER cre_ctr_archivo r
       WHERE c.id_cre_acreditado  = cre_id_cre_acreditado
         AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo

      IF cre_folio IS NULL THEN
         LET cre_folio = 1;
      END IF

      -- si la fecha de culminación del crédito es la fecha de hoy
      LET cre_f_culmina = TODAY;

      -- se actualiza el id credito del registro de afi derechohabiente a 0-Sin crédito
      UPDATE afi_derechohabiente
         SET id_credito = 0,
             f_credito  = cre_f_culmina 
       WHERE id_derechohabiente = cre_id_derechohabiente;

      -- se inicializa la variable del derechohabiente
      LET cta_id_derechohabiente = NULL;

      -- se lee el registro de cta_credito para el derechohabiente en proceso
      FOREACH
         SELECT id_derechohabiente,
                proceso_cod,
                tpo_credito,
                num_credito,
                f_credito
           INTO cta_id_derechohabiente,
                cta_proceso_cod,
                cta_tpo_credito,
                cta_num_credito,
                cta_f_credito
           FROM cta_credito
          WHERE id_derechohabiente = cre_id_derechohabiente
            AND tpo_credito        = cre_tpo_credito
            AND num_credito        = cre_num_credito

         -- verifica si se encontró registro en cta credito
         IF cta_id_derechohabiente IS NOT NULL THEN
            -- se comprueba si existe el registro en cta histórico
            IF NOT EXISTS (
               SELECT id_derechohabiente
                 FROM cta_his_credito
                WHERE id_derechohabiente = cre_id_derechohabiente
                  AND tpo_credito        = cta_tpo_credito
                  AND num_credito        = cta_num_credito) THEN

               IF cta_num_credito = "" THEN
                  LET cta_num_credito = 0;
               END IF

               -- No existe. Se asignan los valores en el registro cta_historico
               LET cta_his_id_derechohabiente = cta_id_derechohabiente;
               LET cta_his_proceso_cod        = cta_proceso_cod;
               LET cta_his_tpo_credito        = cta_tpo_credito;
               LET cta_his_num_credito        = cta_num_credito;
               LET cta_his_f_credito          = cta_f_credito;
               LET cta_his_estado             = 2;
               LET cta_his_f_actualiza        = TODAY;

               -- se inserta el registro en cta histórico
               INSERT INTO cta_his_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito,
                           estado,
                           f_actualiza)
                   VALUES (cta_his_id_derechohabiente,
                           cta_his_proceso_cod,
                           cta_his_tpo_credito,
                           cta_his_num_credito,
                           cta_his_f_credito,
                           cta_his_estado,
                           cta_his_f_actualiza);
            END IF

            -- se elimina el registro de cta credito
            DELETE
              FROM cta_credito
             WHERE id_derechohabiente = cre_id_derechohabiente
               AND tpo_credito        = cre_tpo_credito
               AND num_credito        = cre_num_credito;
         END IF
      END FOREACH

      -- se obtiene las marcas y el tipo de transferencia para el tipo de crédito en proceso
      SELECT marca_inf, marca_prc, DECODE(id_proceso,201,"03",1201, "16", 301,"43")
        INTO v_ax_marca_inf, v_ax_marca_prc, v_ax_tpo_transferencia
        FROM cat_tipo_credito
       WHERE tpo_credito     = cre_tpo_credito
         AND tpo_originacion = cre_tpo_originacion;

      IF v_ax_marca_inf IS NULL OR v_ax_marca_prc IS NULL OR v_ax_tpo_transferencia IS NULL THEN
         SELECT FIRST 1 marca_inf, marca_prc, DECODE(id_proceso,201,"03",1201, "16", 301,"43")
          INTO v_ax_marca_inf, v_ax_marca_prc, v_ax_tpo_transferencia
          FROM cat_tipo_credito
         WHERE tpo_credito = cre_tpo_credito;
      END IF

      -- se asignan los valores para la función de desmarca
      LET des_id_derechohabiente = cre_id_derechohabiente;
      LET des_marca_entra        = v_ax_marca_inf;
      LET des_n_referencia       = cre_id_cre_acreditado;
      LET des_estado_marca       = 0;
      LET des_marca_causa        = 0;
      LET des_usuario            = p_usuario;

      -- se invoca la función de desmarca
      EXECUTE FUNCTION fn_desmarca_cuenta(des_id_derechohabiente,
                                          des_marca_entra,
                                          des_n_referencia,
                                          des_estado_marca,
                                          des_marca_causa,
                                          des_usuario,
                                          v_proceso_cod)
                                     INTO v_ax_cod_error;

       IF v_ax_tpo_transferencia = "" THEN
          LET v_ax_tpo_transferencia = "03";
       END IF

      -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
      IF v_ax_tpo_transferencia = "03" THEN
         -- corresponde a Transferencia de Acreditados
         LET ws_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
      ELIF v_ax_tpo_transferencia = "16" THEN
         -- corresponde a Anualidades Garantizadas
         LET ws_marca_procesar = "02"; -- 'grt' => 02 (Créditos en Garantía)
      ELSE
         -- corresponde a Anualidades Garantizadas
         LET ws_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
      END IF

      IF p_sol_desmarca_prc THEN
         -- se valida si no existe el el id_derechohabiente en proceso en la tabla del WS
         IF EXISTS (
            SELECT id_derechohabiente
              FROM cta_marca_ws
             WHERE id_derechohabiente = cre_id_derechohabiente) THEN

               -- Ya existe el derechohabiente en la tabla de WS. Se elimina
               DELETE
                 FROM cta_marca_ws
                WHERE id_derechohabiente = cre_id_derechohabiente;
         END IF

         IF cre_tpo_credito <> 19 AND
            cre_tpo_credito <> 17 THEN
            --Se valida la conciliacion de saldos con procesar en la tabla de acreditados
            SELECT folio_liquida
            INTO v_folio_sin_conciliar
            FROM cre_acreditado
            WHERE id_cre_acreditado = cre_id_cre_acreditado
            AND edo_procesar >= 120;

            IF v_folio_sin_conciliar IS NOT NULL THEN
               --Se valida en la tabala de uso de la garantia
               IF cre_tpo_originacion = 4 THEN
                  SELECT FIRST 1 folio_liquida
                    INTO v_folio_sin_conciliar
                    FROM cre_uso_garantia
                   WHERE id_derechohabiente = cre_id_derechohabiente
                     AND estado IN(20,140)
                     AND edo_procesar IN(10,70,80,85)
                     AND tpo_transferencia IN("43","48");
               END IF

               IF v_folio_sin_conciliar IS NULL THEN
                  --No se encontró ningún registro pendiente por conciliar, se procede a desmarcar
                  -- se asignan los valores del registro a insertar en la tabla de WebService
                  LET ws_id_derechohabiente = cre_id_derechohabiente;
                  LET ws_id_origen          = cre_id_cre_acreditado;
                  LET ws_modulo_cod         = v_ax_tpo_transferencia;
                  LET ws_tpo_credito        = cre_tpo_credito;
                  LET ws_marca              = v_ax_marca_prc;
                  LET ws_f_solicita         = TODAY;
                  LET ws_intento            = 1;
                  LET ws_cod_result_op      = NULL;
                  LET ws_diagnostico        = NULL;
                  LET ws_situacion          = 0;
                  LET ws_num_credito        = cre_num_credito;
                  LET ws_f_infonavit        = cre_f_culmina;
                  LET ws_folio_archivo      = cre_folio;
                  LET ws_usuario            = p_usuario;

                  -- se inserta el registro en la tabla del WebService
                  INSERT INTO cta_marca_ws (
                        id_derechohabiente,
                        id_origen,
                        modulo_cod,
                        tpo_credito,
                        marca,
                        f_solicita,
                        intento,
                        cod_result_op,
                        diagnostico,
                        situacion,
                        num_credito,
                        f_infonavit,
                        marca_procesar,
                        folio_archivo,
                        usuario)
                    VALUES (ws_id_derechohabiente,
                        ws_id_origen,
                        ws_modulo_cod,
                        ws_tpo_credito,
                        ws_marca,
                        ws_f_solicita,
                        ws_intento,
                        ws_cod_result_op,
                        ws_diagnostico,
                        ws_situacion,
                        ws_num_credito,
                        ws_f_infonavit,
                        ws_marca_procesar,
                        ws_folio_archivo,
                        ws_usuario);

                  LET v_ax_edo_procesar = 160;
               ELSE
                  --Se actualiza el estado a rechazado para que se intente desmarcar en otro momento
                  LET v_ax_estado = 280;
               END IF
            ELSE
               --Se actualiza el estado a rechazado para que se intente desmarcar en otro momento
               LET v_ax_estado = 280;
            END IF
         END IF
      END IF

      -- se actualiza el registro en proceso al estado que corresponda - Desmarca Infonavit o rechazo
      UPDATE cre_acreditado
         SET estado       = v_ax_estado,
             edo_procesar = v_ax_edo_procesar
       WHERE id_cre_acreditado = cre_id_cre_acreditado;
   END FOREACH;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cta_his_credito;

   RETURN v_ax_excep_error;

END FUNCTION;


