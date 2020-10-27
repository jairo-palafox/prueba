






CREATE FUNCTION "safreviv".fn_cbd_prepara_negativos(p_fcorte DATE)
RETURNING SMALLINT;

	DEFINE v_resultado         SMALLINT;

	DEFINE v_nss					CHAR(11);
	DEFINE v_sdo_92				DECIMAL(22,6);
	DEFINE v_sdo_97				DECIMAL(22,6);
	DEFINE v_sdo_92_inf			DECIMAL(22,6);
	DEFINE v_sdo_97_inf			DECIMAL(22,6);

	SET PDQPRIORITY HIGH;

	DROP TABLE IF EXISTS cbd_saldos_negativos;
   CREATE TABLE cbd_saldos_negativos
     (
       nss CHAR(11) NOT NULL,
       acciones_92 decimal(22,6),
       acciones_97 decimal(22,6),
		 acciones_92_inf decimal(22,6),
		 acciones_97_inf decimal(22,6),
		 fcorte date
	 )FRAGMENT BY ROUND ROBIN IN bdnsv_1_dbs,bdnsv_2_dbs,bdnsv_3_dbs,bdnsv_4_dbs;
     --) IN cbd_3_dbs;

	DROP TABLE IF EXISTS tmp_saldos;
   SELECT id_derechohabiente,
          subcuenta,
          p_fcorte v_fcorte,
          SUM(monto_acciones) monto_acciones
     FROM cta_movimiento
    WHERE subcuenta IN (4,8,42,44)
	 AND fondo_inversion = 11
	 AND f_liquida <= p_fcorte
    GROUP BY id_derechohabiente,
             subcuenta
    INTO TEMP tmp_saldos;
    UPDATE STATISTICS FOR TABLE tmp_saldos;

	DROP TABLE IF EXISTS tmp_saldo_negativo;
	SELECT tmp.id_derechohabiente,
          tmp.subcuenta,
			 tmp.monto_acciones
	FROM tmp_saldos tmp
	WHERE tmp.monto_acciones < 0
	INTO TEMP tmp_saldo_negativo;
	UPDATE STATISTICS FOR TABLE tmp_saldo_negativo;

	FOREACH
		SELECT DISTINCT
			afi.nss,
			sdo92.monto_acciones,
			sdo97.monto_acciones,
			sdo92_inf.monto_acciones,
			sdo97_inf.monto_acciones
		INTO
			v_nss,
			v_sdo_92,
			v_sdo_97,
			v_sdo_92_inf,
			v_sdo_97_inf
		FROM tmp_saldo_negativo sdo
		INNER JOIN afi_derechohabiente afi ON afi.id_derechohabiente = sdo.id_derechohabiente
		LEFT OUTER JOIN(
			SELECT id_derechohabiente, monto_acciones FROM tmp_saldo_negativo WHERE subcuenta = 8 
		) sdo92 ON sdo92.id_derechohabiente = sdo.id_derechohabiente
		LEFT OUTER JOIN(
			SELECT id_derechohabiente, monto_acciones FROM tmp_saldo_negativo WHERE subcuenta = 4 
		) sdo97 ON sdo97.id_derechohabiente = sdo.id_derechohabiente
		LEFT OUTER JOIN(
			SELECT id_derechohabiente, monto_acciones FROM tmp_saldo_negativo WHERE subcuenta = 42 
		) sdo92_inf ON sdo92_inf.id_derechohabiente = sdo.id_derechohabiente
		LEFT OUTER JOIN(
			SELECT id_derechohabiente, monto_acciones FROM tmp_saldo_negativo WHERE subcuenta = 44 
		) sdo97_inf ON sdo97_inf.id_derechohabiente = sdo.id_derechohabiente

		INSERT INTO cbd_saldos_negativos VALUES(	v_nss,
																v_sdo_92,
																v_sdo_97,
																v_sdo_92_inf,
																v_sdo_97_inf,
																p_fcorte);
																
	END FOREACH;

	UPDATE cbd_saldos_negativos SET acciones_92 = 0.000000 WHERE acciones_92 IS NULL;
	UPDATE cbd_saldos_negativos SET acciones_97 = 0.000000 WHERE acciones_97 IS NULL;
	UPDATE cbd_saldos_negativos SET acciones_92_inf = 0.000000 WHERE acciones_92_inf IS NULL;
	UPDATE cbd_saldos_negativos SET acciones_97_inf = 0.000000 WHERE acciones_97_inf IS NULL;

	DROP TABLE IF EXISTS tmp_saldos;
	DROP TABLE IF EXISTS tmp_saldo_negativo;

	LET v_resultado = 0;
	
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado;
END FUNCTION;


