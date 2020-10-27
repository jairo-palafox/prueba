






CREATE FUNCTION "safreviv".fn_preliquida_reverso_operativo(p_folio DECIMAL(9,0), p_folio_ajuste DECIMAL(9,0), p_usuario VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100);

	DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

	DEFINE v_folio_previo 			DECIMAL(9,0);

	DEFINE v_id_derechohabiente	DECIMAL(9,0);
	DEFINE v_subcuenta				SMALLINT;
	DEFINE v_fondo_inversion		SMALLINT;
	DEFINE v_tipo						SMALLINT;
	DEFINE v_movimiento				SMALLINT;
   DEFINE v_movimiento_72			SMALLINT;
	DEFINE v_monto_acciones			DECIMAL(16,6);
	DEFINE v_monto_pesos				DECIMAL(12,2);
	DEFINE v_f_valor					DATE;
	DEFINE v_modulo_cod				CHAR(3);
	DEFINE v_proceso_cod				SMALLINT;
	DEFINE v_h_registro         	DATETIME HOUR TO SECOND;


	LET v_resultado = 0;
	LET v_mensaje = "La preliquidacion de reverso operativo se ejecuto correctamente";

	--Primero se obtiene el modulo al que pertenece el folio a ajustar
	SELECT fol.proceso_cod, cat.modulo_cod
	INTO v_proceso_cod, v_modulo_cod
	FROM glo_folio fol 
	INNER JOIN cat_proceso cat ON cat.proceso_cod = fol.proceso_cod 
	WHERE fol.folio = p_folio_ajuste;
	
	IF (v_proceso_cod IS NULL) THEN
		LET v_resultado = 2;
		LET v_mensaje = "ERROR: El folio " || p_folio_ajuste || " no existe en el sistema";

		--Se registra la ejecucion del proceso
		INSERT INTO aop_ctr_ajuste VALUES (	p_folio,							--folio
														v_proceso_cod,					--proceso_cod
														TODAY,							--f_proceso
														p_folio_ajuste,				--folio_ajustado
														1,									--tpo_ajuste	'1' Reverso Operativo
														9,									--cve_estado	'9' Reverso Operativo Rechazado
														v_resultado,					--cve_rechazo	'2' Folio no existe en el sistema
														p_usuario						--usuario
													 );
		RETURN v_resultado, v_mensaje;
	END IF

	DROP TABLE IF EXISTS aop_preliquida CASCADE ;
	CREATE TABLE aop_preliquida
	(
		f_liquida 				DATE,
		id_derechohabiente 	DECIMAL(9,0),
		subcuenta 				SMALLINT,
		fondo_inversion 		SMALLINT,
		movimiento 				SMALLINT,
		folio_liquida 			DECIMAL (9,0),
		id_referencia 			DECIMAL(9,0),
		monto_acciones 		DECIMAL(16,6),
		monto_pesos 			DECIMAL(12,2),
		f_valor 					DATE,
		f_registro 				DATE,
		h_registro 				DATETIME HOUR TO SECOND,
		origen 					CHAR(20)
	)fragment by round robin in aop_1_dbs, aop_2_dbs  
	 extent size 16 next size 16 lock mode page;

	--Se valida que el folio no alla tenido un reverso operativo previo
	SELECT
		folio
	INTO
		v_folio_previo
	FROM aop_ctr_ajuste
	WHERE cve_estado NOT IN (5,9)
	AND folio_ajustado = p_folio_ajuste;

	IF (v_folio_previo IS NOT NULL) THEN
		LET v_resultado = 1;
		LET v_mensaje = "ERROR: El folio " || p_folio_ajuste || " presenta un reverso operativo previo con folio " || v_folio_previo;

		--Se registra la ejecucion del proceso
		INSERT INTO aop_ctr_ajuste VALUES (	p_folio,							--folio
														v_proceso_cod,					--proceso_cod
														TODAY,							--f_proceso
														p_folio_ajuste,				--folio_ajustado
														1,									--tpo_ajuste	'1' Significa Reverso Operativo
														9,									--cve_estado	'9' Significa Reverso Operativo Rechazado
														v_resultado,					--cve_rechazo	'1' Folio previamente ajustado
														p_usuario						--usuario
													 );
		RETURN v_resultado, v_mensaje;
	END IF

	--Se valida que el folio tenga almenos un movimiento liquidado
	SELECT FIRST 1
		f_liquida
	INTO
		v_f_valor
	FROM cta_movimiento
	where folio_liquida = p_folio_ajuste;

	IF (v_f_valor IS NULL) THEN
		SELECT FIRST 1
			f_liquida
		INTO
			v_f_valor
		FROM cta_fondo72
		where folio_liquida = p_folio_ajuste;

		IF (v_f_valor IS NULL) THEN
			SELECT FIRST 1
				f_liquida
			INTO
				v_f_valor
			FROM cta_decreto
			where folio_liquida = p_folio_ajuste;
			
			IF (v_f_valor IS NULL) THEN
				LET v_resultado = 3;
				LET v_mensaje = "ERROR: El folio " || p_folio_ajuste || " no tiene movimientos liquidados";

				--Se registra la ejecucion del proceso
				INSERT INTO aop_ctr_ajuste VALUES (	p_folio,							--folio
																v_proceso_cod,					--proceso_cod
																TODAY,							--f_proceso
																p_folio_ajuste,				--folio_ajustado
																1,									--tpo_ajuste	'1' Significa Reverso Operativo
																9,									--cve_estado	'9' Significa Reverso Operativo Rechazado
																v_resultado,					--cve_rechazo	'3' Folio sin movimientos liquidados
																p_usuario						--usuario
															 );
				RETURN v_resultado, v_mensaje;
			END IF
		END IF
	END IF

	--Se valida que la fecha de liquidacion del folio sea menos al dia de hoy
	IF (TODAY <= v_f_valor) THEN
		LET v_resultado = 4;
		LET v_mensaje = "ERROR: El folio " || p_folio_ajuste || " aun no genera la poliza contable";

		--Se registra la ejecucion del proceso
		INSERT INTO aop_ctr_ajuste VALUES (	p_folio,							--folio
														v_proceso_cod,					--proceso_cod
														TODAY,							--f_proceso
														p_folio_ajuste,				--folio_ajustado
														1,									--tpo_ajuste	'1' Significa Reverso Operativo
														9,									--cve_estado	'9' Significa Reverso Operativo Rechazado
														v_resultado,					--cve_rechazo	'4' Folio sin movimientos liquidados
														p_usuario						--usuario
													 );
		RETURN v_resultado, v_mensaje;
	END IF

	LET v_h_registro = CURRENT HOUR TO SECOND;

	--Se recupera la informacion de los movimientos liquidados por el folio
	FOREACH
		SELECT
			mov.id_derechohabiente,
			mov.subcuenta,
			mov.fondo_inversion,
			cat.tipo,
			mov.monto_acciones * -1,
			mov.monto_pesos * -1,
			mov.f_valor
		INTO
			v_id_derechohabiente,
			v_subcuenta,
			v_fondo_inversion,
			v_tipo,
			v_monto_acciones,
			v_monto_pesos,
			v_f_valor
		FROM cta_movimiento mov
		INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento
		WHERE folio_liquida = p_folio_ajuste

		IF (v_tipo = 1) THEN
			LET v_movimiento = 1212;			--Cargo
		ELSE
			LET v_movimiento = 521;				--Abono
		END IF

		INSERT INTO aop_preliquida  VALUES (TODAY,									--f_liquida
														v_id_derechohabiente,				--id_derechohabiente
														v_subcuenta,							--subcuenta
														v_fondo_inversion,					--fondo_inversion
														v_movimiento,							--movimiento
														p_folio,									--folio_liquida
														p_folio_ajuste,						--id_referencia
														v_monto_acciones,						--monto_acciones
														v_monto_pesos,							--monto_pesos
														v_f_valor,								--f_valor
														TODAY,									--f_registro
														v_h_registro,							--h_registro
														'ROP-FOLIO-'||p_folio_ajuste		--origen
														);
	END FOREACH;

	--Se recupera la informacion de los movimientos de decreto por el folio
	FOREACH
		SELECT
			mov.id_decreto,
			mov.subcuenta,
			mov.fondo_inversion,
			cat.tipo,
			mov.monto_acciones * -1,
			mov.monto_pesos * -1,
			mov.f_valor
		INTO
			v_id_derechohabiente,
			v_subcuenta,
			v_fondo_inversion,
			v_tipo,
			v_monto_acciones,
			v_monto_pesos,
			v_f_valor
		FROM cta_decreto mov
		INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento
		WHERE folio_liquida = p_folio_ajuste

		IF (v_tipo = 1) THEN
			LET v_movimiento = 1212;			--Cargo
		ELSE
			LET v_movimiento = 521;				--Abono
		END IF

		INSERT INTO aop_preliquida  VALUES (TODAY,									--f_liquida
														v_id_derechohabiente,				--id_derechohabiente
														v_subcuenta,							--subcuenta
														v_fondo_inversion,					--fondo_inversion
														v_movimiento,							--movimiento
														p_folio,									--folio_liquida
														p_folio_ajuste,						--id_referencia
														v_monto_acciones,						--monto_acciones
														v_monto_pesos,							--monto_pesos
														v_f_valor,								--f_valor
														TODAY,									--f_registro
														v_h_registro,							--h_registro
														'ROP-FOLIO-'||p_folio_ajuste		--origen
														);
	END FOREACH;

   --Se recupera la informacion de fondo anterior para el folio
	FOREACH
		SELECT
			mov.id_afi_fondo72,
			mov.subcuenta,
         mov.movimiento,
			0,
			cat.tipo,
			0,
			mov.importe * -1,
			mov.f_liquida
		INTO
			v_id_derechohabiente,
			v_subcuenta,
         v_movimiento_72,
			v_fondo_inversion,
			v_tipo,
			v_monto_acciones,
			v_monto_pesos,
			v_f_valor
		FROM cta_fondo72 mov
		INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento
		WHERE folio_liquida = p_folio_ajuste

		IF (v_tipo = 1) THEN
         IF (v_movimiento_72 = 601)THEN
            LET v_movimiento = 2022;			--Cargo Reverso de restitucion tanto adicional
         ELSE
            LET v_movimiento = 1212;			--Cargo
         END IF
		ELSE
         IF (v_movimiento_72 = 422) THEN
            LET v_movimiento = 841;				--Abono por tanto adicional
         ELSE
            LET v_movimiento = 521;				--Abono
         END IF
		END IF

		INSERT INTO aop_preliquida  VALUES (TODAY,									--f_liquida
														v_id_derechohabiente,				--id_derechohabiente
														v_subcuenta,							--subcuenta
														v_fondo_inversion,					--fondo_inversion
														v_movimiento,							--movimiento
														p_folio,									--folio_liquida
														p_folio_ajuste,						--id_referencia
														v_monto_acciones,						--monto_acciones
														v_monto_pesos,							--monto_pesos
														v_f_valor,								--f_valor
														TODAY,									--f_registro
														v_h_registro,							--h_registro
														'ROP-FOLIO-'||p_folio_ajuste		--origen
														);
	END FOREACH;

	create index ix_aop_preliquida on aop_preliquida
   (folio_liquida) using btree  in aop_ix_dbs;

	UPDATE STATISTICS FOR table aop_preliquida;

	UPDATE glo_folio
   SET    status = 1
	WHERE  folio = p_folio;

	--Se registra la ejecucion del proceso
		INSERT INTO aop_ctr_ajuste VALUES (	p_folio,							--folio
														v_proceso_cod,					--proceso_cod
														TODAY,							--f_proceso
														p_folio_ajuste,				--folio_ajustado
														1,									--tpo_ajuste	'1' Significa Reverso Operativo
														1,									--cve_estado	'1' Significa Reverso Operativo Preliquidado
														NULL,								--cve_rechazo	NULL folo aceptado
														p_usuario						--usuario
													 );

		INSERT INTO aop_cifras_ajuste
		SELECT
			folio_liquida,
			fondo_inversion,
			subcuenta,
			SUM(monto_pesos),
			SUM(monto_acciones),
			COUNT(*)
		FROM aop_preliquida
		WHERE folio_liquida = p_folio
		GROUP BY 1,2,3;
	
   RETURN v_resultado, v_mensaje;
END FUNCTION;


