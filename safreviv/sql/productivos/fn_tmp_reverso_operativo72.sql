






CREATE FUNCTION "safreviv".fn_tmp_reverso_operativo72(p_folio DECIMAL(9,0), p_folio_ajuste DECIMAL(9,0), p_usuario VARCHAR(30))
RETURNING SMALLINT, VARCHAR(100);

	DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);


	DEFINE v_id_afi_fondo72	      DECIMAL(9,0);
	DEFINE v_subcuenta				SMALLINT;
	DEFINE v_tipo						SMALLINT;
	DEFINE v_movimiento				SMALLINT;
   DEFINE v_movimiento_72			SMALLINT;
	DEFINE v_monto_pesos				DECIMAL(12,2);


	LET v_resultado = 0;
	LET v_mensaje = "La preliquidacion de reverso operativo se ejecuto correctamente";

   --Se recupera la informacion de fondo anterior para el folio
	FOREACH
		SELECT
			mov.id_afi_fondo72,
			mov.subcuenta,
         mov.movimiento,
			cat.tipo,
			mov.importe * -1
		INTO
			v_id_afi_fondo72,
			v_subcuenta,
         v_movimiento_72,
			v_tipo,
			v_monto_pesos
		FROM cta_fondo72 mov
		INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento
		WHERE folio_liquida = p_folio_ajuste

		IF (v_tipo = 1) THEN
			LET v_movimiento = 1212;			--Cargo
		ELSE
         IF (v_movimiento_72 = 422) THEN
            LET v_movimiento = 841;				--Abono por tanto adicional
         ELSE
            LET v_movimiento = 521;				--Abono
         END IF
		END IF

		INSERT INTO safre_tmp:tmp_reverso_op72  VALUES (v_id_afi_fondo72,				      --id_afi_fondo72
                                                      MDY(12,23,2014),                 --f_liquida
                                                      v_subcuenta,							--subcuenta
                                                      v_movimiento,							--movimiento
                                                      p_folio,									--folio_liquida
                                                      p_folio_ajuste,						--id_referencia
                                                      v_monto_pesos,							--importe
                                                      ' ',                             --estado_pago
                                                      MDY(12,23,2014),					   --f_registro
                                                      '15:23:30',							   --h_registro
                                                      'ROP-FOLIO-'||p_folio_ajuste		--origen
                                                      );
	END FOREACH;
	
   RETURN v_resultado, v_mensaje;
END FUNCTION;


