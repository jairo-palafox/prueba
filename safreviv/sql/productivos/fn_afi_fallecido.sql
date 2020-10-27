






CREATE FUNCTION "safreviv".fn_afi_fallecido(p_id_derechohabiente DECIMAL(9,0), p_valida SMALLINT, p_usuario CHAR(20))

   RETURNING SMALLINT;

   DEFINE v_resultado               SMALLINT;
   DEFINE v_f_actualiza             DATE;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      LET v_resultado = 0;

      RETURN v_resultado;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dhFallecido.trace';
   --TRACE ON;

   LET v_f_actualiza = TODAY;

   IF NOT EXISTS (SELECT id_derechohabiente
                    FROM afi_fallecido
                   WHERE id_derechohabiente = p_id_derechohabiente) THEN

      INSERT INTO afi_fallecido
      VALUES(p_id_derechohabiente,
             p_valida,
             p_usuario,
             v_f_actualiza);

      LET v_resultado = p_valida;
   ELSE
      SELECT estado
        INTO v_resultado
        FROM afi_fallecido
       WHERE id_derechohabiente = p_id_derechohabiente;

      IF v_resultado <> p_valida THEN
         UPDATE afi_fallecido
            SET estado             = p_valida,
                f_actualiza        = v_f_actualiza,
                usuario            = p_usuario
          WHERE id_derechohabiente = p_id_derechohabiente;

          LET v_resultado = p_valida;
      END IF
   END IF

   UPDATE STATISTICS FOR TABLE afi_fallecido;

   RETURN v_resultado;

END FUNCTION;


