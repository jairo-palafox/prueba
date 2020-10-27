






CREATE FUNCTION "safreviv".fn_agr_reversa_arch_entrada(p_d_folio            DECIMAL(9,0),
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
   DEFINE v_ax_tpo_transferencia  CHAR(2);
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
   --SET DEBUG FILE TO '/safreviv_int/agr/rescate/acrReversoArchEnt.trace';
   --TRACE ON;

   -- se asume que no ocurrirá ningún error
   LET v_ax_sql_error = 0;
   LET v_ax_tpo_transferencia = "43"; -- Anualidades Garantizadas

   -- se verifica si el proceso a reversar se trata de Recurrente (opt1)
   IF p_c_op_arch_ent = "opt1" THEN
      -- se obtiene la información del archivo
      SELECT lote, f_lote, f_proceso
        INTO v_ax_lote, v_ax_f_lote, v_ax_f_proceso
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      FOREACH
         -- se procesan los registros de his acreditado para el folio dado
         SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito
           INTO v_ax_id_cre_acreditado, v_ax_id_derechohabiente, v_ax_tpo_credito, v_d_num_credito
           FROM safre_viv:cre_acreditado
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE
           FROM safre_viv:cta_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_origen          = v_ax_id_cre_acreditado
            AND modulo_cod         = "43" -- AGR
            AND tpo_credito        = v_ax_tpo_credito;

         -- se obtiene la marca infonavit y procesar para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf, marca_prc
            INTO v_ax_marca_inf, v_ax_marca_prc
            FROM safre_viv:cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- eliminan los registros cta credito
         DELETE
           FROM safre_viv:cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND proceso_cod = 301
            AND tpo_credito = v_ax_tpo_credito
            AND num_credito = v_d_num_credito
            AND f_credito = v_ax_f_proceso;

         -- se verifica si existe el registro en cta credito
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM safre_viv:cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
            -- se procesan los registros de cta historico
            FOREACH
            SELECT FIRST 1 id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO ctah_id_derechohabiente, ctah_proceso_cod, ctah_tpo_credito, ctah_num_credito, ctah_f_credito
              FROM safre_viv:cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND f_actualiza = v_ax_f_proceso
               -- se inserta el registro en cta credito
               INSERT INTO safre_viv:cta_credito (
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
                 FROM safre_viv:cta_his_credito
                WHERE id_derechohabiente = ctah_id_derechohabiente
                  AND num_credito = ctah_num_credito
                  AND proceso_cod = ctah_proceso_cod
                  AND tpo_credito = ctah_tpo_credito
                  AND f_actualiza = v_ax_f_proceso;
            END FOREACH
         END IF

         -- se eliminan los registros de cre acreditado
         DELETE
           FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se actualiza el registro de afi_derechohabiente
         UPDATE safre_viv:afi_derechohabiente
            SET id_credito = 0,
                f_credito = NULL
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de his acreditado
      DELETE
        FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de rch acreditado
      DELETE
        FROM safre_viv:cre_rch_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de sin originación
      DELETE
        FROM safre_viv:cre_sin_originacion
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de cre saldo deudor
      DELETE
        FROM safre_viv:cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;
{
      -- se ejecuta la función que reversa los registros de mandatos
      EXECUTE FUNCTION safre_viv:fn_mdt_rev_inserta_inst(v_ax_lote, v_ax_f_lote)
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
           FROM safre_viv:cre_his_acreditado
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- se actualiza el edo_procesar de cre acreditado a 60-MARCADA PROCESAR
         UPDATE safre_viv:cre_acreditado
            SET edo_procesar = 55
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- eliminan los registros de his acreditado en proceso
         DELETE
           FROM safre_viv:cre_his_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado            
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- de tratarse del reverso de Saldos Transferidos se reversa la tabla temporal
         IF p_c_op_arch_ent = "opt3" THEN
            -- se obtiene el id derechohabiente de la tabla maestro
            SELECT id_derechohabiente
              INTO v_ax_id_derechohabiente
              FROM safre_viv:cre_acreditado
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

            DELETE
              FROM safre_tmp:tmp_cre_confirmado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND modulo_cod = "agr";
         END IF

         -- se obtiene el derechohabiente para el id acreditado en proceso
         SELECT id_derechohabiente, tpo_credito
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca para el reverso
         FOREACH
          SELECT FIRST 1 marca_prc
            INTO v_ax_marca_prc
            FROM safre_viv:cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- se ejecuta la funcion de reverso de marca (Porcesar)
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_prc,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);
      END FOREACH;

      FOREACH
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM safre_viv:cre_uso_garantia
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
         -- se actualiza el estado procesar del registro maestro
         UPDATE safre_viv:cre_uso_garantia
            SET edo_procesar = 80
          WHERE estado >= 140
            AND edo_procesar = 70
            AND id_cre_ctr_archivo IN (
                SELECT id_cre_ctr_archivo
                  FROM safre_viv:cre_ctr_archivo
                 WHERE operacion NOT IN (1,6,9,14))
            AND id_derechohabiente = v_ax_id_derechohabiente
            AND tpo_transferencia = '43';

         -- eliminan los registros de his acreditado en proceso
         DELETE
           FROM safre_viv:cre_uso_garantia
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
            AND id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH

      -- de tratarse del reverso de Saldos Transferidos se reversa la tabla de rechazos
      IF p_c_op_arch_ent = "opt3" THEN
         DELETE
           FROM safre_viv:cre_rch_acreditado
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;
      END IF
   END IF

   -- se verifica si el proceso a reversar se trata de Desmarca (opt6)
   IF p_c_op_arch_ent = "opt6" THEN
      -- se procesan los registros de his acreditado para el id del archivo
      FOREACH
       SELECT id_cre_acreditado, estado
         INTO v_ax_id_cre_acreditado, v_ax_estado
         FROM safre_viv:cre_his_acreditado
        WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- en caso de el estado ser 140 o 25 se asume que se refiere a una reactivación
         IF v_ax_estado = 25 OR v_ax_estado = 140 THEN
            LET v_ax_estado_act = 170;

            -- se actualiza el edo_procesar de la tabla maestro
            UPDATE safre_viv:cre_acreditado
               SET estado = v_ax_estado_act,
                   edo_procesar = 120
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
         ELSE
            LET v_ax_estado_act = 140;

            -- se actualiza el edo_procesar de la tabla maestro
            UPDATE safre_viv:cre_acreditado
               SET estado = v_ax_estado_act
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
         END IF

         -- se obtiene la información de la tabla maestro
         SELECT id_derechohabiente, tpo_credito, id_cre_ctr_archivo, num_credito
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito, v_id_cre_ctr_archivo, v_d_num_credito
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca (infonavit) para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf
            INTO v_ax_marca_inf
            FROM safre_viv:cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- se valida el estado obtenido
         IF v_ax_estado = 160 OR v_ax_estado = 170 THEN
            -- se ejecuta función de reversa desmarca
            EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(v_ax_id_derechohabiente,
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
              FROM safre_viv:cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND num_credito = v_d_num_credito
               -- verifica si se encontró registro en cta credito
               IF ctah_id_derechohabiente IS NOT NULL THEN
                  IF NOT EXISTS (
                  SELECT id_derechohabiente
                    FROM safre_viv:cta_credito
                   WHERE id_derechohabiente = v_ax_id_derechohabiente
                     AND num_credito = v_d_num_credito) THEN
                     -- se asignan los valores en el registro cta his credito
                     LET cta_id_derechohabiente = ctah_id_derechohabiente;
                     LET cta_proceso_cod        = ctah_proceso_cod;
                     LET cta_tpo_credito        = ctah_tpo_credito;
                     LET cta_num_credito        = ctah_num_credito;
                     LET cta_f_credito          = ctah_f_credito;

                     -- se inserta el registro en cta his credito
                     INSERT INTO safre_viv:cta_credito (
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
                    FROM safre_viv:cta_his_credito
                   WHERE id_derechohabiente = v_ax_id_derechohabiente;
               END IF
            END FOREACH

            -- eliminan los registros cta marca ws para el derechohabiente en proceso
            DELETE
              FROM safre_viv:cta_marca_ws
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_credito = v_ax_tpo_credito
               AND situacion = 0;
         ELIF v_ax_estado = 25 OR v_ax_estado = 140 THEN
            -- se inicializa la variable
            LET cta_id_derechohabiente = NULL;

            -- Se refiere a una Reactivación. Se reversa la parte de cta credito
            SELECT id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO cta_id_derechohabiente, cta_proceso_cod, cta_tpo_credito, cta_num_credito, cta_f_credito
              FROM safre_viv:cta_credito
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
               INSERT INTO safre_viv:cta_his_credito (
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
                 FROM safre_viv:cta_credito
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_credito = v_ax_tpo_credito
                  AND num_credito = v_d_num_credito;
            END IF

            -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
            EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
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
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se procesan los registros de his acreditado para el folio dado
      FOREACH
       SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito
         INTO v_ax_id_cre_acreditado, v_ax_id_derechohabiente, v_ax_tpo_credito, v_d_num_credito
         FROM safre_viv:cre_acreditado
        WHERE estado = 20
          AND id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE
           FROM safre_viv:cta_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_origen          = v_ax_id_cre_acreditado
            AND modulo_cod         = "43" -- AGR
            AND tpo_credito        = v_ax_tpo_credito;

         -- se obtiene la marca infonavit y procesar para el reverso
         FOREACH
          SELECT FIRST 1 marca_inf, marca_prc
            INTO v_ax_marca_inf, v_ax_marca_prc
            FROM safre_viv:cat_tipo_credito
           WHERE tpo_credito = v_ax_tpo_credito
           ORDER BY f_actualiza DESC
         END FOREACH;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- eliminan los registros cta credito
         DELETE
           FROM safre_viv:cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND proceso_cod = 301
            AND tpo_credito = v_ax_tpo_credito
            AND num_credito = v_d_num_credito
            AND f_credito = v_ax_f_proceso;

         -- se verifica si existe el registro en cta credito
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM safre_viv:cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
            -- se procesan los registros de cta historico
            FOREACH
            SELECT FIRST 1 id_derechohabiente, proceso_cod, tpo_credito, num_credito, f_credito
              INTO ctah_id_derechohabiente, ctah_proceso_cod, ctah_tpo_credito, ctah_num_credito, ctah_f_credito
              FROM safre_viv:cta_his_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND f_actualiza = v_ax_f_proceso
               -- se inserta el registro en cta credito
               INSERT INTO safre_viv:cta_credito (
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
                 FROM safre_viv:cta_his_credito
                WHERE id_derechohabiente = ctah_id_derechohabiente
                  AND num_credito = ctah_num_credito
                  AND proceso_cod = ctah_proceso_cod
                  AND tpo_credito = ctah_tpo_credito
                  AND f_actualiza = v_ax_f_proceso;
            END FOREACH
         END IF

         -- se eliminan los registros de cre acreditado
         DELETE
           FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se actualiza el registro de afi_derechohabiente
         UPDATE safre_viv:afi_derechohabiente
            SET id_credito = 0,
                f_credito = NULL
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de his acreditado
      DELETE
        FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de rch acreditado
      DELETE
        FROM safre_viv:cre_rch_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de sin originación
      DELETE
        FROM safre_viv:cre_sin_originacion
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de cre saldo deudor
      DELETE
        FROM safre_viv:cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;
   END IF

   -- se verifica si el proceso a reversar se trata de Uso Anualidad o Garantía (opt11)
   IF p_c_op_arch_ent = "opt11" THEN
      -- se asigna la marca a reversar
      LET v_ax_marca_inf = 225;

      -- se procesan los registros de uso garantía en proceso
      FOREACH
      SELECT id_derechohabiente, id_cre_uso_garantia
        INTO v_ax_id_derechohabiente, v_ax_id_cre_acreditado
        FROM safre_viv:cre_uso_garantia
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
         AND tpo_transferencia = v_ax_tpo_transferencia

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- eliminan los registros de his acreditado en proceso
         DELETE
           FROM safre_viv:cre_uso_garantia
          WHERE id_cre_uso_garantia = v_ax_id_cre_acreditado
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo
            AND tpo_transferencia = v_ax_tpo_transferencia;
      END FOREACH
   END IF

   -- se verifica si el proceso a reversar se trata de Solicitud de Marca/Desmarca (opt12)
   IF p_c_op_arch_ent = "opt12" THEN
      -- se procesan los registros marcados
      FOREACH
      SELECT id_cre_acreditado
        INTO v_ax_id_cre_acreditado
        FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
         AND edo_procesar = 60 -- Marcas aceptadas

         -- se obtiene la marca procesar y demás información
         SELECT id_derechohabiente
           INTO v_ax_id_derechohabiente
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca procesar y demás información
         SELECT marca
           INTO v_ax_marca_prc
           FROM sfr_marca_activa
          WHERE folio = p_d_folio
            AND id_derechohabiente = v_ax_id_derechohabiente;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_prc,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);

         -- se elimina el registro de la tabla de cta_credito, si es que fue insertado en este proceso
         UPDATE safre_viv:cre_acreditado
            SET edo_procesar = 10
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado
            AND edo_procesar = 60
            AND id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH

      -- se procesan los registros desmarcados
      FOREACH
      SELECT id_cre_acreditado
        INTO v_ax_id_cre_acreditado
        FROM safre_viv:cre_his_acreditado
       WHERE edo_procesar = 210 -- Desmarcas aceptadas
         AND id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- se obtiene la marca procesar y demás información
         SELECT id_derechohabiente
           INTO v_ax_id_derechohabiente
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se obtiene la marca procesar y demás información
         SELECT marca
           INTO v_ax_marca_prc
           FROM sfr_marca_activa
          WHERE folio = p_d_folio
            AND id_derechohabiente = v_ax_id_derechohabiente;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(v_ax_id_derechohabiente,
                                                         v_ax_marca_prc,
                                                         v_ax_id_cre_acreditado,
                                                         p_d_folio);
      END FOREACH
      
      -- se eliminan los registros de la tabla de historicos
      DELETE
        FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de la tabla de historicos
      DELETE
        FROM safre_viv:cre_rch_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;
   END IF

   -- se elimina el registro de control de archivos para el folio en proceso
   DELETE
     FROM safre_viv:cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:cta_credito;

   RETURN v_ax_sql_error;
END FUNCTION
;


