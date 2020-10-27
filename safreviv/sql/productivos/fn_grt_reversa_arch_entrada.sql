






CREATE FUNCTION "safreviv".fn_grt_reversa_arch_entrada(p_d_folio            DECIMAL(9,0),
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
   -- Variables auxiliares
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo    VARCHAR(18);
   DEFINE v_d_num_credito         DECIMAL(10,0);
   DEFINE v_ax_folio_archivo      DECIMAL(9,0);
   DEFINE v_ax_estado             SMALLINT;
   DEFINE v_ax_lote               SMALLINT;
   DEFINE v_ax_f_lote             DATE;
   DEFINE v_ax_f_proceso          DATE;
   DEFINE v_ax_periodo_pago       CHAR(6);
   DEFINE v_ax_tpo_credito        SMALLINT;
   DEFINE v_ax_marca_inf          SMALLINT;
   DEFINE v_ax_marca_prc          SMALLINT;
   DEFINE v_ax_edo_rev_mto        SMALLINT;
   DEFINE v_ax_tot_rev_mto        DECIMAL(9,0);
   DEFINE v_ax_sql_error          SMALLINT;
   DEFINE v_folio_liq             DECIMAL(9,0);

   -- Captura el error sql
   ON EXCEPTION SET v_ax_sql_error
      -- Imprime el codigo de error
      RETURN v_ax_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtReversoArchEnt.trace';
   --TRACE ON;

   -- se asume que no ocurrirá ningún error
   LET v_ax_sql_error = 0;

   -- se verifica si el proceso a reversar se trata de Recurrente (opt1)
   IF p_c_op_arch_ent = "opt1" THEN
      SELECT lote, f_lote
        INTO v_ax_lote, v_ax_f_lote
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      SELECT UNIQUE(folio_liquida)
       INTO v_folio_liq
       FROM cre_sg_preliquida;

      DELETE
        FROM cta_movimiento
      WHERE folio_liquida = v_folio_liq;

         -- se procesan los registros de his acreditado para el folio dado
      FOREACH
       SELECT id_cre_acreditado, id_derechohabiente, tpo_credito
         INTO v_ax_id_cre_acreditado, v_ax_id_derechohabiente, v_ax_tpo_credito
         FROM safre_viv:cre_acreditado
        WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE FROM safre_viv:cta_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_origen          = v_ax_id_cre_acreditado
            AND modulo_cod         = "16" -- GRT
            AND tpo_credito        = v_ax_tpo_credito;

         -- se obtiene la marca para el reverso
         SELECT FIRST 1 marca_inf
           INTO v_ax_marca_inf
           FROM safre_viv:cat_tipo_credito
          WHERE tpo_credito = v_ax_tpo_credito;

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_inf,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);
{
         -- 12/07/2012 12:32:24 p.m. Este reverso se ejecutará en saldo transferidos
         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                      v_ax_marca_prc,
                                                      v_ax_id_cre_acreditado,
                                                      p_d_folio);
}
{
         -- eliminan los registros marca activa para el derechohabiente en proceso
         DELETE FROM safre_viv:sfr_marca_activa
          WHERE id_derechohabiente = v_ax_id_derechohabiente;

         -- eliminan los registros marca historica para el derechohabiente en proceso
         DELETE FROM safre_viv:sfr_marca_historica
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
}
{ Ya no se inserta registro en cta_credito en este proceso, se hace en saldos transf.
         -- eliminan los registros cta credito
         DELETE FROM safre_viv:cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
            --AND proceso_cod = ;
}
         -- se eliminan los registros de cre acreditado
         DELETE FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se actualiza el registro de afi_derechohabiente
         UPDATE safre_viv:afi_derechohabiente
            SET id_credito = 0,
                f_credito = NULL
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de his acreditado
      DELETE FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de sin originación
      DELETE FROM safre_viv:cre_sin_originacion
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

      -- se eliminan los registros de cre saldo deudor
      DELETE FROM safre_viv:cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;

      -------------------------------------------
      ----- DEVOLUCIÓN DE SALDOS EXCEDENTES -----
      -------------------------------------------
      -- se eliminan los registros de historicos
      DELETE
        FROM safre_viv:dse_his_devolucion
       WHERE folio = p_d_folio;

      -- se eliminan el registro en proceso los regsitros de la tabla maestro
      DELETE
        FROM safre_viv:dse_agrupa_devolucion
       WHERE folio_liquida = p_d_folio;

      -- se elimina la información de la tabla de control de archivo DSE
      DELETE
        FROM safre_viv:dse_ctr_archivo
       WHERE folio = p_d_folio;
   END IF;

   -- se verifica si el proceso a reversar se trata de Rechazos (opt2), Saldos (opt3),
   -- Devueltas (opt4) o No Atendidas (opt5)
   IF p_c_op_arch_ent = "opt2" OR p_c_op_arch_ent = "opt3" OR
      p_c_op_arch_ent = "opt4" OR p_c_op_arch_ent = "opt5" THEN
      SELECT f_proceso
        INTO v_ax_f_proceso
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- se procesan los registros de his acreditado para el folio dado
      FOREACH
       SELECT UNIQUE id_cre_acreditado
         INTO v_ax_id_cre_acreditado
         FROM safre_viv:cre_his_acreditado
        WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- se actualiza el edo_procesar de cre acreditado a 60-MARCADA PROCESAR
         UPDATE safre_viv:cre_acreditado
            SET edo_procesar = 55
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- eliminan los registros de his acreditado en proceso
         DELETE FROM safre_viv:cre_his_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado
            AND id_cre_ctr_archivo = p_id_cre_ctr_archivo;

         -- verifica si se trata de Saldos (opt3)
         IF p_c_op_arch_ent = "opt3" THEN
            -- se obtiene el derechohabiente para el id acreditado en proceso
            SELECT id_derechohabiente, tpo_credito, num_credito
              INTO v_ax_id_derechohabiente, v_ax_tpo_credito, v_d_num_credito
              FROM safre_viv:cre_acreditado
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

            -- se obtiene la marca para el reverso
            SELECT FIRST 1 marca_prc
              INTO v_ax_marca_prc
              FROM safre_viv:cat_tipo_credito
             WHERE tpo_credito = v_ax_tpo_credito;

            -- se ejecuta la funcion de reverso de marca (Porcesar)
            EXECUTE PROCEDURE safre_viv:sp_reversa_marca(v_ax_id_derechohabiente,
                                                         v_ax_marca_prc,
                                                         v_ax_id_cre_acreditado,
                                                         p_d_folio);

            -- se elimina el registro de la tabla de cta_credito, si es que fue insertado en este proceso
            DELETE FROM safre_viv:cta_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND proceso_cod = 1205
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
         END IF

         -- de tratarse del reverso de No Atendidas se reversa la parte de Uso de Anualidad
         IF p_c_op_arch_ent = "opt5" THEN
            FOREACH
             SELECT id_derechohabiente, periodo_pago
               INTO v_ax_id_derechohabiente, v_ax_periodo_pago
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
                  AND tpo_transferencia = '18'
                  AND periodo_pago = v_ax_periodo_pago;

               -- se elimina registro de uso de garantia para el id del archivo y el derechohabiente en proceso
               DELETE FROM safre_viv:cre_uso_garantia
                WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
                  AND id_derechohabiente = v_ax_id_derechohabiente;
            END FOREACH
         END IF
      END FOREACH;
   END IF

   -- se verifica si el proceso a reversar se trata de Desmarca (opt6)
   IF p_c_op_arch_ent = "opt6" THEN
         -- se procesan los registros de his acreditado para el id del archivo
      FOREACH
       SELECT id_cre_acreditado, estado
         INTO v_ax_id_cre_acreditado, v_ax_estado
         FROM safre_viv:cre_his_acreditado
        WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- se actualiza el edo_procesar de la tabla maestro
         UPDATE safre_viv:cre_acreditado
            SET estado = 20
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         -- se valida el estado obtenido
         IF v_ax_estado <> 160 AND v_ax_estado <> 170 THEN
            CONTINUE FOREACH;
         END IF

         SELECT id_derechohabiente, tpo_credito, id_cre_ctr_archivo, num_credito
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito, v_id_cre_ctr_archivo, v_d_num_credito
           FROM safre_viv:cre_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;

         SELECT folio_archivo
           INTO v_ax_folio_archivo
           FROM cre_ctr_archivo
          WHERE id_cre_ctr_archivo = v_id_cre_ctr_archivo;

         -- se obtiene la marca para el reverso
         SELECT FIRST 1 marca_inf
           INTO v_ax_marca_inf
           FROM safre_viv:cat_tipo_credito
          WHERE tpo_credito = v_ax_tpo_credito;
{
         -- ejecuta función de reversa desmarca
         EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(v_ax_id_derechohabiente,
                                                         v_ax_marca_inf,
                                                         v_ax_id_cre_acreditado,
                                                         v_ax_folio_archivo);
}
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
{
         -- eliminan los registros his marca ws para el derechohabiente en proceso
         DELETE FROM safre_viv:cta_his_marca_ws
          WHERE id_derechohabiente = v_ax_id_derechohabiente;
}
      END FOREACH

      -- se actualiza el edo_procesar de his acreditado
      DELETE FROM safre_viv:cre_his_acreditado
       WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;
   END IF;

   -- se eliminan los registros de rch acreditado
   DELETE
     FROM safre_viv:cre_rch_acreditado
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- se elimina el registro de control de archivos para el folio en proceso
   DELETE
     FROM safre_viv:cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cta_credito;

   RETURN v_ax_sql_error;
END FUNCTION
;


