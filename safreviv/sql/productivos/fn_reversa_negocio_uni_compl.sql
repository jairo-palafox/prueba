






CREATE FUNCTION "safreviv".fn_reversa_negocio_uni_compl(p_folio DECIMAL(9,0), p_proceso_cod SMALLINT, p_usuario_cod  VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100)

	DEFINE v_resultado            SMALLINT;
  DEFINE v_mensaje              VARCHAR(100);

   UPDATE glo_folio
   SET    status = -1
   WHERE  folio = p_folio;

	LET v_resultado = 0;
	LET v_mensaje = "El reverso de negocio se ejecuto correctamente";

   RETURN v_resultado, v_mensaje;
END FUNCTION;


