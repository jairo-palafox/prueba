






CREATE FUNCTION "safreviv".fn_reverso_unificacion(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT, p_usuario_cod  VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100)

	DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

	LET v_resultado = 1;
	LET v_mensaje = "Reverso de negocio finalizado";

   RETURN v_resultado, v_mensaje;
END FUNCTION;


