






CREATE FUNCTION  "safreviv".fn_reverso_liquidacion_fondo72_dse(p_folio  DECIMAL(9,0))
   RETURNING SMALLINT;
   DEFINE v_error SMALLINT;

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_error;
   END EXCEPTION

   -- se inicia el error en 0 para indicar que por default no ocurrio un error
   LET v_error = 0;

   UPDATE dse_restitucion_fondo72
      SET estado = 10
    WHERE estado = 140
      AND folio = p_folio;

   DELETE
     FROM safre_viv:cta_fondo72
    WHERE folio_liquida = p_folio;

   -- se actualiza el status de glo folio, para que el folio lo pueda tomar nuevamente
   -- la función general de liquidación
   DELETE
     FROM safre_viv:glo_folio
    WHERE folio = p_folio;

   -- En caso de terminar correctamente, devolvera 0
   RETURN v_error;
END FUNCTION;


