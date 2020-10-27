






CREATE FUNCTION "safreviv".fn_reversa_preliquidacion_dse(p_folio DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_sql_error             SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- regresa el código de error que ocasionó la excepción
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO 'reverso_preliquidacion_dse.trace';

   -- se inicializan variable
   LET v_sql_error = 0;

   FOREACH
   SELECT id_dse_grp_devolucion
     INTO v_id_dse_grp_devolucion
     FROM safre_viv:dse_agrupa_devolucion
    WHERE folio_liquida = p_folio
      AND estado = 130

      UPDATE safre_viv:dse_his_devolucion
         SET estado = 20
       WHERE estado = 130
         AND id_dse_grp_devolucion = v_id_dse_grp_devolucion;

      UPDATE safre_viv:dse_agrupa_devolucion
         SET estado = 20
       WHERE estado = 130
         AND folio_liquida = p_folio
         AND id_dse_grp_devolucion = v_id_dse_grp_devolucion;

      UPDATE safre_viv:dse_devolucion
         SET estado = 15
       WHERE estado = 130
         AND id_derechohabiente IN (
             SELECT id_derechohabiente
             FROM safre_viv:dse_agrupa_devolucion
             WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion);
   END FOREACH

   -- se elimina el folio de la tabla principal
   DELETE
     FROM glo_folio
    WHERE folio = p_folio;

   RETURN v_sql_error;
END FUNCTION;


