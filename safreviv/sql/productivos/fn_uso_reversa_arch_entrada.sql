






CREATE FUNCTION "safreviv".fn_uso_reversa_arch_entrada(p_d_folio            DECIMAL(9,0),
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
   -- Registro de uso de garantia
   DEFINE uso_id_cre_uso_garantia DECIMAL(9,0);
   DEFINE uso_id_derechohabiente  DECIMAL(9,0);
   -- Variables auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_ax_lote               SMALLINT;
   DEFINE v_ax_f_lote             DATE;
   DEFINE v_ax_periodo_pago       CHAR(6);
   DEFINE v_ax_marca_entra        SMALLINT;
   DEFINE v_ax_edo_rev_mto        SMALLINT;
   DEFINE v_ax_tot_rev_mto        DECIMAL(9,0);
   DEFINE v_ax_sql_error          SMALLINT;

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

      FOREACH
         -- se procesan los registros de his acreditado para el folio dado
         SELECT id_cre_uso_garantia, id_derechohabiente
           INTO uso_id_cre_uso_garantia, uso_id_derechohabiente
           FROM safre_viv:cre_uso_garantia
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo

         -- eliminan los registros marca ws para el derechohabiente en proceso
         DELETE
           FROM safre_viv:cta_marca_ws
          WHERE id_derechohabiente = uso_id_derechohabiente;

         -- se asigna la marca correspondiente
         LET v_ax_marca_entra = 223; -- uso de la anualidad

         -- ejecuta función de reversa marca considera las tablas de sfr_marca_activa y sfr_marca_historica
         EXECUTE PROCEDURE safre_viv:sp_reversa_marca(uso_id_derechohabiente,
                                                      v_ax_marca_entra,
                                                      uso_id_cre_uso_garantia,
                                                      p_d_folio);
         -- eliminan los registros cta credito
         DELETE
           FROM safre_viv:cta_credito
          WHERE id_derechohabiente = uso_id_derechohabiente;

         -- se eliminan los registros de cre uso garantia
         DELETE
           FROM safre_viv:cre_uso_garantia
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
            AND id_derechohabiente = uso_id_derechohabiente;
      END FOREACH;

      -- se eliminan los registros de cre saldo deudor
      DELETE FROM safre_viv:cre_saldo_deudor
       WHERE folio_referencia = p_d_folio;
   END IF;

   -- se verifica si el proceso a reversar se trata de Rechazos (opt2), Saldos (opt3),
   -- Devueltas (opt4) o No Atendidas (opt5)
   IF p_c_op_arch_ent = "opt2" OR p_c_op_arch_ent = "opt3" OR
      p_c_op_arch_ent = "opt4" OR p_c_op_arch_ent = "opt5" THEN
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

         -- eliminan los registros de cre uso garantia
         DELETE
           FROM safre_viv:cre_uso_garantia
          WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
            AND id_derechohabiente = v_ax_id_derechohabiente;
      END FOREACH
   END IF

   -- se eliminan los registros rechazados
   DELETE
     FROM safre_viv:cre_rch_acreditado
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- se elimina el registro de control de archivos para el folio en proceso
   DELETE
     FROM safre_viv:cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   RETURN v_ax_sql_error;
END FUNCTION;


