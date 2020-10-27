






CREATE FUNCTION "safreviv".fn_reversa_acr_arch_entrada(p_d_folio            DECIMAL(9,0),
                                            p_c_op_arch_ent      VARCHAR(5),
                                            p_id_cre_ctr_archivo VARCHAR(18))
   RETURNING SMALLINT;
   -- REGISTRO de cta credito
   DEFINE cta_id_derechohabiente  DECIMAL(9,0);
   DEFINE cta_proceso_cod         SMALLINT;
   DEFINE cta_tpo_credito         SMALLINT;
   DEFINE cta_num_credito         DECIMAL(10,0);
   DEFINE cta_f_credito           DATE;
   -- REGISTRO de cta his credito
   DEFINE ctah_id_derechohabiente DECIMAL(9,0);
   DEFINE ctah_proceso_cod        SMALLINT;
   DEFINE ctah_tpo_credito        SMALLINT;
   DEFINE ctah_num_credito        DECIMAL(10,0);
   DEFINE ctah_f_credito          DATE;
   DEFINE ctah_estado             SMALLINT;
   DEFINE ctah_f_actualiza        DATE;
   -- Variables auxiliares
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo    VARCHAR(18);
   DEFINE v_d_num_credito         DECIMAL(10,0);
   DEFINE v_ax_estado             SMALLINT;
   DEFINE v_ax_estado_act         SMALLINT;
   DEFINE v_ax_lote               SMALLINT;
   DEFINE v_ax_f_lote             DATE;
   DEFINE v_ax_f_proceso          DATE;
   DEFINE v_ax_tpo_credito        SMALLINT;
   DEFINE v_ax_marca_inf          SMALLINT;
   DEFINE v_ax_marca_prc          SMALLINT;
   DEFINE v_ax_edo_rev_mto        SMALLINT;
   DEFINE v_ax_tot_rev_mto        DECIMAL(9,0);
   DEFINE v_ax_sql_error          SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_ax_sql_error
      -- Imprime el codigo de error
      RETURN v_ax_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrReversoArchEnt.trace';
   --TRACE ON;

   -- se asume que no ocurrirá ningún error
   LET v_ax_sql_error = 0;

   -- se verifica si el proceso a reversar se trata de Recurrente (opt1)
   IF p_c_op_arch_ent = "opt1" THEN
      -- se obtiene la información del archivo
      SELECT lote, f_lote, f_proceso
        INTO v_ax_lote, v_ax_f_lote, v_ax_f_proceso
        FROM  cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se procesan los registros de his acreditado para el folio dado
      FOREACH
      SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito
        INTO v_ax_id_cre_acreditado, v_ax_id_derechohabiente, v_ax_tpo_credito, v_d_num_credito
        FROM  cre_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE
           FROM  cta_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_origen          = v_ax_id_cre_acreditado
            AND modulo_cod         = "03" -- ACR
            AND tpo_credito        = v_ax_tpo_credito;

         -- se obtiene la marca infonavit y procesar para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf, marca_prc
            INTO v_ax_marca_inf, v_ax_marca_prc
            FROM  cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE  sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- eliminan los registros cta credito
         DELETE
           FROM cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND proceso_cod IN(201,301)
            AND tpo_credito = v_ax_tpo_credito
            AND num_credito = v_d_num_credito;

         -- se verifica si existe el registro en cta credito
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM  cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
            -- se procesan los registros de cta historico
            FOREACH
            SELECT FIRST 1 id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO ctah_id_derechohabiente, ctah_proceso_cod, ctah_tpo_credito, ctah_num_credito, ctah_f_credito
              FROM cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_credito        = v_ax_tpo_credito
               AND num_credito        = v_d_num_credito

               -- se inserta el registro en cta credito
               INSERT INTO  cta_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito)
                   VALUES (ctah_id_derechohabiente,
                           ctah_proceso_cod,
                           ctah_tpo_credito,
                           ctah_num_credito,
                           ctah_f_credito);

               -- se elimina el registro de la tabla cta historicos
               DELETE
                 FROM  cta_his_credito
                WHERE id_derechohabiente = ctah_id_derechohabiente
                  AND tpo_credito = ctah_tpo_credito
                  AND num_credito = ctah_num_credito;
            END FOREACH
         END IF

         -- se eliminan los registros de cre acreditado
         DELETE
           FROM cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se actualiza el registro de afi_derechohabiente
         UPDATE  afi_derechohabiente
            SET id_credito = 0,
                f_credito = NULL
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de his acreditado
      DELETE
        FROM  cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de sin originación
      DELETE
        FROM  cre_sin_originacion
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de cre saldo deudor
      DELETE
        FROM  cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;
{
      -- se ejecuta la función que reversa los registros de mandatos
      EXECUTE FUNCTION  fn_mdt_rev_inserta_inst(v_ax_lote, v_ax_f_lote)
      INTO v_ax_edo_rev_mto, v_ax_tot_rev_mto;

      -- verifica si se pudo reversar mantados
      IF v_ax_edo_rev_mto = 1 THEN
         RETURN -1;
      END IF;
}
   END IF;

   -- se verifica si el proceso a reversar se trata de Rechazos (opt2), Saldos (opt3),
   -- Devueltas (opt4) o No Atendidas (opt5)
   IF p_c_op_arch_ent = "opt2" OR p_c_op_arch_ent = "opt3" OR
      p_c_op_arch_ent = "opt4" OR p_c_op_arch_ent = "opt5" THEN
      FOREACH
         -- se procesan los registros de his acreditado para el folio dado
         SELECT UNIQUE id_cre_acreditado
           INTO v_ax_id_cre_acreditado
           FROM  cre_his_acreditado
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- se actualiza el edo_procesar de cre acreditado a 60-MARCADA PROCESAR
         UPDATE  cre_acreditado
            SET edo_procesar = 55
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- eliminan los registros de his acreditado en proceso
         DELETE
           FROM  cre_his_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado            
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- de tratarse del reverso de Saldos Transferidos se reversa la tabla temporal
         IF p_c_op_arch_ent = "opt3" THEN
            -- se obtiene el id derechohabiente de la tabla maestro
            SELECT id_derechohabiente
              INTO v_ax_id_derechohabiente
              FROM  cre_acreditado
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

            DELETE
              FROM safre_tmp:tmp_cre_confirmado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND modulo_cod = "acr";
         END IF

         -- se obtiene el derechohabiente para el id acreditado en proceso
         SELECT id_derechohabiente, tpo_credito
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito
           FROM  cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca para el reverso
         FOREACH
          SELECT FIRST 1 marca_prc
            INTO v_ax_marca_prc
            FROM  cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- se ejecuta la funcion de reverso de marca (Porcesar)
         EXECUTE PROCEDURE  sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_prc,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);
      END FOREACH;

      -- de tratarse del reverso de No Atendidas se reversa la parte de Uso de Anualidad
      IF p_c_op_arch_ent = "opt5" THEN
         FOREACH
         SELECT id_derechohabiente
           INTO v_ax_id_derechohabiente
           FROM  cre_uso_garantia
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
            -- se actualiza el estado procesar del registro maestro
            UPDATE  cre_uso_garantia
               SET edo_procesar = 80
             WHERE estado >= 140
               AND edo_procesar = 70
               AND id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_transferencia = '43'
               AND id_cre_ctr_archivo IN (
                   SELECT id_cre_ctr_archivo
                     FROM  cre_ctr_archivo
         	          WHERE operacion NOT IN (1,6,9,14));

            -- se elimina registro de uso de garantia para el id del archivo y el derechohabiente en proceso
            DELETE
              FROM  cre_uso_garantia
             WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
               AND id_derechohabiente = v_ax_id_derechohabiente;
         END FOREACH
      END IF
   END IF

   -- se verifica si el proceso a reversar se trata de Desmarca (opt6)
   IF p_c_op_arch_ent = "opt6" THEN
      -- se procesan los registros de his acreditado para el id del archivo
      FOREACH
       SELECT id_cre_acreditado, estado
         INTO v_ax_id_cre_acreditado, v_ax_estado
         FROM  cre_his_acreditado
        WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- en caso de el estado ser 140 o 25 se asume que se refiere a una reactivación
         IF v_ax_estado = 25 OR v_ax_estado = 140 THEN
            LET v_ax_estado_act = 170;

            -- se actualiza el edo_procesar de la tabla maestro
            UPDATE  cre_acreditado
               SET estado = v_ax_estado_act,
                   edo_procesar = 120
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
         ELSE
            LET v_ax_estado_act = 140;

            -- se actualiza el edo_procesar de la tabla maestro
            UPDATE  cre_acreditado
               SET estado = v_ax_estado_act
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
         END IF

         -- se obtiene la información de la tabla maestro
         SELECT id_derechohabiente, tpo_credito, id_cre_ctr_archivo, num_credito
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito, v_id_cre_ctr_archivo, v_d_num_credito
           FROM  cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca (infonavit) para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf
            INTO v_ax_marca_inf
            FROM  cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- se valida el estado obtenido
         IF v_ax_estado = 160 OR v_ax_estado = 170 THEN
            -- se ejecuta función de reversa desmarca
            EXECUTE PROCEDURE  sp_reversa_desmarca(v_ax_id_derechohabiente,
                                                            v_ax_marca_inf,
                                                            v_ax_id_cre_acreditado,
                                                            p_d_folio);

            -- se inicializa variable
            LET ctah_id_derechohabiente = NULL;

            -- se leen el registro de cta credito
            FOREACH
            SELECT id_derechohabiente,
                   proceso_cod,
                   tpo_credito,
                   num_credito,
                   f_credito
              INTO ctah_id_derechohabiente,
                   ctah_proceso_cod,
                   ctah_tpo_credito,
                   ctah_num_credito,
                   ctah_f_credito
              FROM  cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND num_credito = v_d_num_credito

               -- verifica si se encontró registro en cta credito
               IF ctah_id_derechohabiente IS NOT NULL THEN
                  IF NOT EXISTS (
                  SELECT id_derechohabiente
                    FROM  cta_credito
                   WHERE id_derechohabiente = v_ax_id_derechohabiente
                     AND num_credito = v_d_num_credito) THEN
                     -- se asignan los valores en el registro cta his credito
                     LET cta_id_derechohabiente = ctah_id_derechohabiente;
                     LET cta_proceso_cod        = ctah_proceso_cod;
                     LET cta_tpo_credito        = ctah_tpo_credito;
                     LET cta_num_credito        = ctah_num_credito;
                     LET cta_f_credito          = ctah_f_credito;

                     -- se inserta el registro en cta his credito
                     INSERT INTO cta_credito (
                                 id_derechohabiente,
                                 proceso_cod,
                                 tpo_credito,
                                 num_credito,
                                 f_credito)
                         VALUES (cta_id_derechohabiente,
                                 cta_proceso_cod,
                                 cta_tpo_credito,
                                 cta_num_credito,
                                 cta_f_credito);
                  END IF

                  -- se elimina el registro de cta credito
                  DELETE
                    FROM  cta_his_credito
                   WHERE id_derechohabiente = v_ax_id_derechohabiente;
               END IF
            END FOREACH

            -- eliminan los registros his marca ws para el derechohabiente en proceso
            DELETE
              FROM  cta_marca_ws
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_credito = v_ax_tpo_credito
               AND situacion = 0;
         ELIF v_ax_estado = 25 OR v_ax_estado = 140 THEN
            -- se inicializa la variable
            LET cta_id_derechohabiente = NULL;

            -- Se refiere a una Reactivación. Se reversa la parte de cta credito
            SELECT id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO cta_id_derechohabiente, cta_proceso_cod, cta_tpo_credito, cta_num_credito, cta_f_credito
              FROM  cta_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_credito = v_ax_tpo_credito
               AND num_credito = v_d_num_credito;

            -- se valida que exista registro en cta credito
            IF cta_id_derechohabiente IS NOT NULL THEN
               LET ctah_id_derechohabiente = cta_id_derechohabiente;
               LET ctah_proceso_cod        = cta_proceso_cod;
               LET ctah_tpo_credito        = cta_tpo_credito;
               LET ctah_num_credito        = cta_num_credito;
               LET ctah_f_credito          = cta_f_credito;
               LET ctah_estado             = 4;
               LET ctah_f_actualiza        = TODAY;

               -- se inserta el registro
               INSERT INTO  cta_his_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito,
                           estado,
                           f_actualiza)
                   VALUES (ctah_id_derechohabiente,
                           ctah_proceso_cod,
                           ctah_tpo_credito,
                           ctah_num_credito,
                           ctah_f_credito,
                           ctah_estado,
                           ctah_f_actualiza);

               -- se elimina el registro de cta credito
               DELETE
                 FROM  cta_credito
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_credito = v_ax_tpo_credito
                  AND num_credito = v_d_num_credito;
            END IF

            -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
            EXECUTE PROCEDURE  sp_reversa_marca(v_ax_id_derechohabiente,
                                                         v_ax_marca_inf,
                                                         v_ax_id_cre_acreditado,
                                                         p_d_folio);
         ELSE
            CONTINUE FOREACH;
         END IF
      END FOREACH

      ---------------------------------------
      -- Se reversa Recurrente en Desmarca --
      ---------------------------------------
      -- se obtiene la información del archivo
      SELECT lote, f_lote, f_proceso
        INTO v_ax_lote, v_ax_f_lote, v_ax_f_proceso
        FROM  cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se procesan los registros de his acreditado para el folio dado
      FOREACH
       SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito
         INTO v_ax_id_cre_acreditado, v_ax_id_derechohabiente, v_ax_tpo_credito, v_d_num_credito
         FROM  cre_acreditado
        WHERE estado = 20
          AND id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE
           FROM  cta_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_origen          = v_ax_id_cre_acreditado
            AND modulo_cod         = "03" -- ACR
            AND tpo_credito        = v_ax_tpo_credito;

         -- se obtiene la marca infonavit y procesar para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf, marca_prc
            INTO v_ax_marca_inf, v_ax_marca_prc
            FROM  cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE  sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- eliminan los registros cta credito
         DELETE
           FROM cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND proceso_cod IN(201,301)
            AND tpo_credito = v_ax_tpo_credito
            AND num_credito = v_d_num_credito;

         -- se verifica si existe el registro en cta credito
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM  cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
            -- se procesan los registros de cta historico
            FOREACH
            SELECT FIRST 1 id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO ctah_id_derechohabiente, ctah_proceso_cod, ctah_tpo_credito, ctah_num_credito, ctah_f_credito
              FROM cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_credito        = v_ax_tpo_credito
               AND num_credito        = v_d_num_credito

               -- se inserta el registro en cta credito
               INSERT INTO cta_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito)
                   VALUES (ctah_id_derechohabiente,
                           ctah_proceso_cod,
                           ctah_tpo_credito,
                           ctah_num_credito,
                           ctah_f_credito);

               -- se elimina el registro de la tabla cta historicos
               DELETE
                 FROM cta_his_credito
                WHERE id_derechohabiente = ctah_id_derechohabiente
                  AND tpo_credito = ctah_tpo_credito
                  AND num_credito = ctah_num_credito;
            END FOREACH
         END IF

         -- se eliminan los registros de cre acreditado
         DELETE
           FROM cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se actualiza el registro de afi_derechohabiente
         UPDATE afi_derechohabiente
            SET id_credito = 0,
                f_credito = NULL
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de his acreditado
      DELETE
        FROM  cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de sin originación
      DELETE
        FROM  cre_sin_originacion
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de cre saldo deudor
      DELETE
        FROM  cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;
   END IF

   -- se eliminan los registros de rch acreditado
   DELETE
     FROM  cre_rch_acreditado
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- se elimina el registro de control de archivos para el folio en proceso
   DELETE
     FROM  cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE  cta_credito;

   RETURN v_ax_sql_error;
END FUNCTION
;


