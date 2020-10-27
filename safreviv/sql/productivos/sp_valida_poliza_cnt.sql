






CREATE PROCEDURE "safreviv".sp_valida_poliza_cnt(p_folio_liquida     DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                     p_cod_proceso        SMALLINT)      --C�digo Proceso
RETURNING SMALLINT;


	--Definici�n de variables
	DEFINE v_bnd_proceso         SMALLINT;  --Estatus del proceso
	DEFINE v_tot_registros       INTEGER;  	--Total de registros
	--DEFINE v_folio_liquida       DECIMAL(9,0);  	--Total de registros

	--Inicializaci�n de Variables
	LET v_bnd_proceso 	= 0; --Estado correcto - La p�liza no se ha generado para este proceso
	LET v_tot_registros = 0;
	--LET v_folio_liquida = 0;

	--Verifica que exista el folio de liquidaci�n
	SELECT count (folio_liquida)
	INTO v_tot_registros
	FROM cnt_transaccion 
	WHERE folio_liquida = p_folio_liquida;
	
	IF v_tot_registros <> 0 THEN 			

				--Valida si existen registros contables cuando la p�liza a�n no ha sido generada
				SELECT COUNT(*)
				INTO   v_tot_registros
				FROM   safre_viv:cnt_transaccion
				WHERE  cod_proceso   = p_cod_proceso
				AND    folio_liquida = p_folio_liquida
				AND    estado        = 10; --Registrado - La p�liza a�n no se ha generado
				IF DBINFO('sqlca.sqlerrd2') = 0 THEN
					   LET v_tot_registros = 0;
				END IF
				
				
				IF v_tot_registros <> 0 THEN
					LET v_bnd_proceso = 0; -- La p�liza no se ha generado a�n
				ELSE
					LET v_bnd_proceso = 1; -- ERROR:La p�liza contable ya fue generada
				END IF 
				
	ELSE 
		LET v_bnd_proceso = 0; -- La p�liza no se ha generado a�n
	END IF 

	RETURN v_bnd_proceso;

END PROCEDURE ;


