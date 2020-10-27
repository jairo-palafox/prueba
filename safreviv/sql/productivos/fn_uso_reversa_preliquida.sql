






CREATE FUNCTION "safreviv".fn_uso_reversa_preliquida(p_folio DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_sql_error          SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtReversoPreliq.trace';
   --TRACE ON;

   -- se inicializa el codigo de error
   LET v_sql_error = 0;

   -- se eliminan los registros de la tabla de preliquidación
   DELETE
     FROM safre_viv:cre_saldo_deudor
    WHERE folio_referencia = p_folio;
{
   -- se procesan los registros de la tabla maestro
   FOREACH
      -- se procesan los registros de his acreditado para el folio dado
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM safre_viv:cre_ug_preliquida
       WHERE folio_liquida = p_folio

      -- se actualizan los regsitros de la tabla maestro
      UPDATE safre_viv:cre_uso_garantia
         SET estado = 20
       WHERE tpo_transferencia = 18
         AND estado = 130
         AND id_derechohabiente = v_id_derechohabiente;

      -- se eliminan los registros de la tabla de preliquidación
      DELETE
        FROM safre_viv:cre_ug_preliquida
       WHERE folio_liquida = p_folio
         AND id_derechohabiente = v_id_derechohabiente;
   END FOREACH;
}
   -- se actualizan los regsitros de la tabla maestro
   UPDATE safre_viv:cre_uso_garantia
      SET estado = 20
    WHERE tpo_transferencia IN ('18','48')
      AND estado = 130
      AND folio_liquida = p_folio;

   -- se eliminan los registros de la tabla de preliquidación
   DELETE
     FROM safre_viv:cre_ug_preliquida
    WHERE folio_liquida = p_folio;

   -- se elimina el folio de la tabla principal
   DELETE
     FROM glo_folio
    WHERE folio = p_folio;

   RETURN v_sql_error;
END FUNCTION;


