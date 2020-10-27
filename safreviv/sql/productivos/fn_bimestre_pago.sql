






CREATE PROCEDURE "safreviv".fn_bimestre_pago(p_periodo CHAR(06))

RETURNING CHAR(06);

	--Define variables
	DEFINE v_periodo_bimestral CHAR(06);
	DEFINE v_bimestre CHAR (2);
	DEFINE v_ano CHAR (4);


	--Inicializa variables
	LET v_periodo_bimestral = "";
	LET v_bimestre = "";
	LET v_ano = "";

	LET v_bimestre = p_periodo[5,6];
	LET v_ano = p_periodo[1,4];

		IF v_bimestre = "01" THEN 
			LET v_bimestre = "01"; 
		END IF;
		IF v_bimestre = "02" THEN 
			LET v_bimestre = "01"; 
		END IF;
		IF v_bimestre = "03" THEN 
			LET v_bimestre = "02"; 
		END IF;
		IF v_bimestre = "04" THEN 
			LET v_bimestre = "02"; 
		END IF;
		IF v_bimestre = "05" THEN 
			LET v_bimestre = "03"; 
		END IF;
		IF v_bimestre = "06" THEN 
			LET v_bimestre = "03"; 
		END IF;
		IF v_bimestre = "07" THEN 
			LET v_bimestre = "04"; 
		END IF;
		IF v_bimestre = "08" THEN 
			LET v_bimestre = "04"; 
		END IF;
		IF v_bimestre = "09" THEN 
			LET v_bimestre = "05"; 
		END IF;
		IF v_bimestre = "10" THEN 
			LET v_bimestre = "05"; 
		END IF;
		IF v_bimestre = "11" THEN 
			LET v_bimestre = "06"; 
		END IF;
		IF v_bimestre = "12" THEN 
			LET v_bimestre = "06"; 
		END IF;

	LET v_periodo_bimestral = v_ano||v_bimestre;

	RETURN v_periodo_bimestral;
END PROCEDURE;


