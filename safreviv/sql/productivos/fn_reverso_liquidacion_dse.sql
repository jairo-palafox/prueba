






CREATE FUNCTION  "safreviv".fn_reverso_liquidacion_dse(p_folio  DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_error                 SMALLINT;

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_error;
   END EXCEPTION

   -- se inicia el error en 0 para indicar que por default no ocurrio un error
   LET v_error = 0;

   DELETE 
     FROM safre_viv:cta_movimiento
    WHERE folio_liquida = p_folio;

   FOREACH
   SELECT id_dse_grp_devolucion
     INTO v_id_dse_grp_devolucion
     FROM dse_agrupa_devolucion
    WHERE folio_liquida = p_folio
      AND estado = 140

      UPDATE safre_viv:dse_devolucion
         SET estado = 130
       WHERE estado = 140
         AND id_derechohabiente IN (
             SELECT id_derechohabiente
             FROM safre_viv:dse_agrupa_devolucion
             WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion
               AND folio_liquida = p_folio
               AND estado = 140);
     
      -- se actualiza el estado de la tabla de agrupación
      UPDATE dse_agrupa_devolucion
         SET estado = 130
       WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion
         AND folio_liquida = p_folio
         AND estado = 140;

      -- se actualiza el estado de la tabla de historicos
      UPDATE dse_his_devolucion
         SET estado = 130
       WHERE id_dse_grp_devolucion = v_id_dse_grp_devolucion
         AND estado = 140;
   END FOREACH

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar nuevamente
   -- la función general de liquidación
   UPDATE safre_viv:glo_folio
      SET status = 1
    WHERE folio = p_folio;

   -- En caso de terminar correctamente, devolvera 0
   RETURN v_error;
END FUNCTION;


