






CREATE FUNCTION "safreviv".fn_aop_reverso(p_folio DECIMAL(9,0), p_usuario_cod  VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_mensaje       			VARCHAR(100);
   DEFINE v_status               SMALLINT;
	DEFINE  v_error_isam      		INTEGER ;

	DEFINE v_resultado               SMALLINT;
	DEFINE v_estado                  SMALLINT;

	-- en caso de excepcion
   ON EXCEPTION SET v_status, v_error_isam, v_mensaje
               
      RETURN v_status , v_mensaje;
   END EXCEPTION 

   SET PDQPRIORITY HIGH;

	LET v_status = 0;
	LET v_mensaje = "El reverso del folio " || p_folio || " se ejecuto correctamente";

	--Se busca el estado del reverso
	SELECT cve_estado 
	INTO v_estado
	FROM aop_ctr_ajuste 
	WHERE folio = p_folio;

	IF (v_estado IN (2)) THEN
		--Se reversa la poliza contable generada
		EXECUTE FUNCTION fn_cnt_rev_rop(p_folio, 0) INTO v_resultado;		--Primero se valida que se pueda ejecutar el reverso
		IF (v_resultado <> 0) THEN
			LET v_status = v_resultado;
			LET v_mensaje = "la poliza del folio " || p_folio || " ya no puede ser reversada";
			RETURN v_status, v_mensaje;
		END IF

		EXECUTE FUNCTION fn_cnt_rev_rop(p_folio, 1) INTO v_resultado;		--Se ejecuta el reverso
		IF (v_resultado <> 0) THEN
			LET v_status = v_resultado;
			LET v_mensaje = "No fue posible reversar la poliza del folio " || p_folio || " por lo que el reverso no se ejecutara...";
			RETURN v_status, v_mensaje;
		END IF
	END IF

   --Se eliminan las cifras calculadas
   DELETE FROM  aop_cifras_ajuste WHERE folio = p_folio;

	--Se eliminan todos datos de preliquidacion
   DROP TABLE IF EXISTS aop_preliquida CASCADE ;

	--Se eliminan los datos liquidados si es que existen
	DELETE FROM cta_movimiento WHERE folio_liquida = p_folio;
	DELETE FROM cta_decreto WHERE folio_liquida = p_folio;
	DELETE FROM cta_fondo72 WHERE folio_liquida = p_folio;

   --Cambiamos el estado del folio
	UPDATE glo_folio SET status = -1 where folio = p_folio;

	--Se actualiza el estado de control
	UPDATE aop_ctr_ajuste SET cve_estado = 5 
	WHERE tpo_ajuste = 1 AND folio = p_folio;

   --Actualizamos el estado de la operacion y el proceso
	UPDATE bat_ctr_operacion SET estado_cod = 10
	WHERE proceso_cod = 2501 AND opera_cod IN (1, 2) AND folio = p_folio;

	UPDATE bat_ctr_proceso SET estado_cod = 10
	WHERE proceso_cod = 2501 AND folio = p_folio;

   SET PDQPRIORITY DEFAULT;
	
   RETURN v_status, v_mensaje;

END FUNCTION;


