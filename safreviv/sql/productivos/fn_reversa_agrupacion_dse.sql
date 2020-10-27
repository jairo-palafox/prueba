






CREATE FUNCTION "safreviv".fn_reversa_agrupacion_dse(p_folio DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_sql_error             SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Devolvera el codigo de error que ocasione la excepcion      
      RETURN v_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseAcrReversaAgrupDSE.trace';
   --TRACE ON;

   -- se inializan variables
   LET v_sql_error = 0;

   FOREACH
   SELECT id_dse_grp_devolucion
     INTO v_id_dse_grp_devolucion
     FROM safre_viv:dse_his_devolucion
    WHERE folio = p_folio

      -- se elimina el registro en la tabla de his devolución
      DELETE
        FROM safre_viv:dse_agrupa_devolucion
       WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion
         AND edo_procesar IN (5, 10, 20)
         AND estado = 20;

      -- se elimina el registro en la tabla historica
      DELETE 
        FROM safre_viv:dse_his_devolucion
       WHERE folio = p_folio
         AND id_dse_grp_devolucion = v_id_dse_grp_devolucion
         AND estado = 20
         AND edo_procesar IN (5, 10, 20);
   END FOREACH

   -- se actualiza dse devolucion a estado 10 - Integrado
   UPDATE safre_viv:dse_devolucion
      SET estado = 10, -- Integrado
          folio = 0
    WHERE folio = p_folio
      AND estado = 15; -- Agrupado

   -- En caso de terminar correctamente, devolvera 0
   RETURN v_sql_error;
END FUNCTION;


