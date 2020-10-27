






CREATE FUNCTION "safreviv".fn_valida_formato_periodo_pago(p_periodo CHAR(06))

RETURNING CHAR(06),SMALLINT;

	--Define variables
	DEFINE v_periodo_valido CHAR(06);
	DEFINE v_bimestre CHAR (2);
	DEFINE v_ano CHAR (4);
	DEFINE v_ano_int SMALLINT;
	DEFINE v_estado SMALLINT;

	--Inicializa variables
	LET v_periodo_valido = ""; 
	LET v_bimestre = "";
	LET v_ano = "";
	LET v_estado = 0; --Si no hay error, será cero
	LET v_ano_int = YEAR (TODAY);
	LET v_bimestre = p_periodo[5,6];
	LET v_ano = p_periodo[1,4];
	
	--Valida que el año del periodo de pago no sea mayor a la fecha actual
	IF v_ano > v_ano_int THEN
		LET v_estado = 1; --Error, verifique el año del periodo de pago
	ELSE	
		--Valida el mes
		IF v_bimestre = "01" OR v_bimestre = "02" OR v_bimestre = "03" OR v_bimestre = "04" OR v_bimestre = "05" OR v_bimestre = "06" THEN 
			LET v_bimestre = v_bimestre; 
		ELSE 
			LET v_estado = 2; --Error, verifique el mes del periodo de pago		
		END IF;
	END IF; 

	LET v_periodo_valido = v_ano||v_bimestre;

	RETURN v_periodo_valido,v_estado;
END FUNCTION;


