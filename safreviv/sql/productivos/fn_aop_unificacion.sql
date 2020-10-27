






CREATE FUNCTION "safreviv".fn_aop_unificacion()
RETURNING SMALLINT, VARCHAR(100);

	DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

	DEFINE v_folio_uni   	      DECIMAL(9,0);
   DEFINE v_folio_ajuste         DECIMAL(9,0);

	DEFINE v_id_derechohabiente	DECIMAL(9,0);
	DEFINE v_subcuenta				SMALLINT;
	DEFINE v_fondo_inversion		SMALLINT;
	DEFINE v_tipo						SMALLINT;
	DEFINE v_movimiento				SMALLINT;
	DEFINE v_monto_acciones			DECIMAL(16,6);
	DEFINE v_monto_pesos				DECIMAL(12,2);
	DEFINE v_f_valor					DATE;
	DEFINE v_modulo_cod				CHAR(3);
	DEFINE v_proceso_cod				SMALLINT;
	DEFINE v_h_registro         	DATETIME HOUR TO SECOND;

	LET v_resultado = 0;
	LET v_mensaje = "La preliquidacion de reverso operativo se ejecuto correctamente";

   DROP TABLE IF EXISTS aop_preliquida_uni CASCADE ;
	CREATE TABLE aop_preliquida_uni
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
	) in cat_dbs  
	 extent size 16 next size 16 lock mode page;
   
   --Se generan los contrarios para cada folio
   FOREACH
      SELECT folio
      INTO v_folio_uni
      FROM tmp_folio_uni

      --Se genera el folio de Ajuste Operativo
      CALL fn_genera_folio(2501,1,'safreviv') RETURNING v_folio_ajuste;
      LET v_h_registro = CURRENT HOUR TO SECOND;
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
         WHERE folio_liquida = v_folio_uni

         IF (v_tipo = 1) THEN
            LET v_movimiento = 1212;			--Cargo
         ELSE
            LET v_movimiento = 521;				--Abono
         END IF

         INSERT INTO aop_preliquida_uni VALUES (TODAY,									--f_liquida
                                             v_id_derechohabiente,				--id_derechohabiente
                                             v_subcuenta,							--subcuenta
                                             v_fondo_inversion,					--fondo_inversion
                                             v_movimiento,							--movimiento
                                             v_folio_ajuste,						--folio_liquida
                                             v_folio_uni,						   --id_referencia
                                             v_monto_acciones,						--monto_acciones
                                             v_monto_pesos,							--monto_pesos
                                             v_f_valor,								--f_valor
                                             TODAY,									--f_registro
                                             v_h_registro,							--h_registro
                                             'ROP-FOLIO-'||v_folio_uni  		--origen
                                             );
      END FOREACH;
   END FOREACH;

   INSERT INTO cta_movimiento SELECT * FROM aop_preliquida_uni;
   
   RETURN v_resultado, v_mensaje;
END FUNCTION;


