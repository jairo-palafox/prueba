






CREATE FUNCTION "safreviv".fn_cambia_folio_liq_cre_uso()
   RETURNING INTEGER, INTEGER, VARCHAR(255)
   -- Registro de uso de garantia
   DEFINE uso_id_derechohabiente  DECIMAL(9,0);
   DEFINE uso_id_cre_uso_garantia DECIMAL(9,0);
   DEFINE uso_folio_liquida       DECIMAL(9,0);
   -- Variables auxiliares
   DEFINE v_ax_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE v_ax_folio_archivo      DECIMAL(9,0);
   DEFINE v_ax_folio_liquida      DECIMAL(9,0);
   DEFINE v_ax_cont_registros     INTEGER;
   DEFINE v_ax_id_proceso         SMALLINT;
   DEFINE v_ax_tpo_transferencia  CHAR(2);
   -- control de excepciones 
   DEFINE v_ax_error              INTEGER; -- codigo de error SQL
   DEFINE v_error_isam            INTEGER; -- codigo de error ISAM
   DEFINE v_mensaje               VARCHAR(255); -- mensaje de error

   ON EXCEPTION SET v_ax_error, v_error_isam, v_mensaje
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_ax_error, v_error_isam, v_mensaje;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrCtrArch_devol.trace';
   --TRACE ON;

   -- se incializan variables
   LET v_ax_id_proceso = 301; --1202;
   LET v_ax_tpo_transferencia = 43; --"18";
   LET v_ax_error = 0;
   LET v_error_isam = 0;
   LET v_mensaje = "El proceso de rechazos termino satisfactoriamente";

   -- se procesan los registros de rechazos de acreditado
   FOREACH
   SELECT id_cre_ctr_archivo, folio_archivo
   INTO v_ax_id_cre_ctr_archivo, v_ax_folio_archivo
   FROM safre_viv:cre_ctr_archivo
   WHERE id_proceso = v_ax_id_proceso
   AND operacion IN (1,6,14)
   AND id_cre_ctr_archivo IN (572, 590)
   --AND id_cre_ctr_archivo IN (574, 395)
   ORDER BY 2,1
      -- se verifica que existan registros de cre_uso_garantia
      SELECT COUNT(*)
      INTO v_ax_cont_registros
      FROM safre_viv:cre_uso_garantia
      WHERE id_cre_ctr_archivo = v_ax_id_cre_ctr_archivo
      AND edo_procesar in (70,75,80,85)
      AND tpo_transferencia = v_ax_tpo_transferencia
      AND folio_liquida = v_ax_folio_archivo;

      IF v_ax_cont_registros = 0 THEN
         CONTINUE FOREACH;
      END IF

      FOREACH
      SELECT id_derechohabiente, id_cre_uso_garantia, folio_liquida
      INTO uso_id_derechohabiente, uso_id_cre_uso_garantia, uso_folio_liquida
      FROM safre_viv:cre_uso_garantia
      WHERE id_cre_ctr_archivo = v_ax_id_cre_ctr_archivo
      AND edo_procesar in (70,75,80,85)
      AND tpo_transferencia = v_ax_tpo_transferencia
      AND folio_liquida = v_ax_folio_archivo
         -- se validan los folios
         IF uso_folio_liquida <> v_ax_folio_archivo THEN
            CONTINUE FOREACH;
         END IF

         -- se verifica cuantos numeros de folios se encontraron
         SELECT COUNT(UNIQUE folio_liquida)
         INTO v_ax_cont_registros
         FROM safre_viv:cre_uso_garantia
         WHERE id_cre_ctr_archivo <> v_ax_id_cre_ctr_archivo
         AND id_derechohabiente = uso_id_derechohabiente
         AND id_cre_ctr_archivo IN (
            SELECT id_cre_ctr_archivo
            FROM safre_viv:cre_ctr_archivo
            WHERE operacion NOT IN (1,6,9,14)
            AND id_proceso = v_ax_id_proceso)
         AND tpo_transferencia = v_ax_tpo_transferencia
         AND folio_liquida <> 0
         AND folio_liquida IS NOT NULL;

         -- se valida el contador de registro
         IF v_ax_cont_registros = 0 THEN
            CONTINUE FOREACH;
         END IF

         -- se obtiene el folio de liquidación
         FOREACH
         SELECT FIRST 1 folio_liquida
         INTO v_ax_folio_liquida
         FROM safre_viv:cre_uso_garantia
         WHERE id_cre_ctr_archivo <> v_ax_id_cre_ctr_archivo
         AND id_derechohabiente = uso_id_derechohabiente
         AND id_cre_ctr_archivo IN (
            SELECT id_cre_ctr_archivo
            FROM safre_viv:cre_ctr_archivo
            WHERE operacion NOT IN (1,6,9,14)
            AND id_proceso = v_ax_id_proceso)
         AND tpo_transferencia = v_ax_tpo_transferencia
         AND folio_liquida <> 0
         AND folio_liquida IS NOT NULL
         ORDER BY folio_liquida DESC
         END FOREACH;

         -- se actualiza el folio de liquidación
         UPDATE safre_viv:cre_uso_garantia
         SET folio_liquida = v_ax_folio_liquida
         WHERE id_derechohabiente = uso_id_derechohabiente
         AND id_cre_uso_garantia = uso_id_cre_uso_garantia
         AND folio_liquida = uso_folio_liquida
         AND id_cre_ctr_archivo = v_ax_id_cre_ctr_archivo
         AND edo_procesar in (70,75,80,85)
         AND tpo_transferencia = v_ax_tpo_transferencia;
      END FOREACH
   END FOREACH

   RETURN v_ax_error, v_error_isam, v_mensaje;
END FUNCTION;


