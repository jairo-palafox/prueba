






CREATE FUNCTION "safreviv".fn_reversa_preliquidacion_acr(p_folio DECIMAL(9,0),
                                              p_i_tpo_originacion SMALLINT)
   RETURNING SMALLINT;
   DEFINE v_sql_error             SMALLINT;
   DEFINE v_id_cre_acreditado     DECIMAL(9,0);
   DEFINE v_estado                SMALLINT;
   DEFINE v_edo_nvo               SMALLINT;
   DEFINE v_f_proceso             DATE;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO 'reverso_preliquidacion_acr.trace';

   -- se inicializa el codigo de error
   LET v_sql_error = 0;

   -------------------------------------------
   ------ TRANSFERENCIA DE ACREDITADOS -------
   -------------------------------------------
   -- se procesan los registros de la tabla maestro
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT UNIQUE id_cre_acreditado, estado
        INTO v_id_cre_acreditado, v_estado
        FROM safre_viv:cre_acreditado
       WHERE folio_liquida = p_folio
         AND estado IN (130,135,138,137)
         AND tpo_originacion = p_i_tpo_originacion

      -- se valida el estado obenido
      IF v_estado = 130 THEN
         LET v_estado = 20;
      ELIF v_estado = 138 THEN
         LET v_estado = 18;
      ELIF v_estado = 137 THEN
         LET v_estado = 270;
      ELSE
         LET v_estado = 25;
      END IF

      -- se actualizan los registros de historicos
      FOREACH
         SELECT FIRST 1 f_proceso
           INTO v_f_proceso
           FROM cre_his_acreditado
          WHERE id_cre_acreditado = v_id_cre_acreditado
            AND estado = v_estado
      END FOREACH

      UPDATE cre_his_acreditado
         SET estado = v_edo_nvo
       WHERE id_cre_acreditado = v_id_cre_acreditado
         AND estado = v_estado
         AND f_proceso = v-f_proceso;

      -- se actualizan los regsitros de la tabla maestro
      UPDATE safre_viv:cre_acreditado
         SET estado = v_estado
       WHERE id_cre_acreditado = v_id_cre_acreditado;
   END FOREACH;

   -- se eliminan los registros de la tabla de preliquidación
   DELETE
     FROM cre_ta_preliquida
    WHERE folio_liquida = p_folio;

   -- se eliminan los registros de la tabla de preliquidación
   DELETE
     FROM cre_saldo_deudor
    WHERE folio_referencia = p_folio;

   -- se elimina el folio de la tabla principal
   DELETE
     FROM glo_folio
    WHERE folio = p_folio;

   -------------------------------------------
   ----- DEVOLUCIÓN DE SALDOS EXCEDENTES -----
   -------------------------------------------
   -- se eliminan los registros de historicos
   DELETE
     FROM dse_his_devolucion
    WHERE folio = p_folio;

   -- se eliminan el registro en proceso los regsitros de la tabla maestro
   DELETE
     FROM dse_agrupa_devolucion
    WHERE folio_liquida = p_folio;

   -- se elimina la información de la tabla de control de archivo DSE
   DELETE
     FROM dse_ctr_archivo
    WHERE folio = p_folio;

   RETURN v_sql_error;

END FUNCTION
;


