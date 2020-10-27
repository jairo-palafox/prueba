






CREATE PROCEDURE "safreviv".sp_rev_dis_avances_pago_ajuste(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT ,CHAR(70);


	DEFINE v_id_derechohabiente DECIMAL(9,0); -- Id derechohabiente segun nss
	DEFINE v_periodo_pago 		VARCHAR(06); 
	DEFINE v_num_credito 		CHAR(10); -- Número de credito
	DEFINE v_nrp 				CHAR(11); -- Numero de registro patronal
	DEFINE v_status         	SMALLINT;
	DEFINE sql_err          	INTEGER ;
	DEFINE isam_err         	INTEGER ;
	DEFINE error_info       	CHAR(70);
	DEFINE  v_char           	CHAR(30);
	DEFINE v_bnd_proceso        SMALLINT;       --Estatus del proceso
	DEFINE v_monto_amortizacion  DECIMAL(12,2); -- Monto de la amortización
	DEFINE v_monto_dif_amo		DECIMAL(12,2);
	
	ON EXCEPTION
	    SET sql_err, isam_err, error_info
			 LET v_status = sql_err;
	    RETURN  v_status ,isam_err , error_info;  
	END EXCEPTION
	
	LET v_bnd_proceso = 0; --Estado correcto
	LET v_id_derechohabiente = 0;
	LET v_num_credito = "";
	LET v_periodo_pago = "";
	LET v_nrp = "";
	LET v_monto_amortizacion = 0.00;
	LET v_monto_dif_amo = 0.00;
	
	FOREACH
	
		SELECT id_derechohabiente, num_credito, periodo_pago, nrp, monto_amortizacion, monto_dif_amo
		INTO v_id_derechohabiente, v_num_credito, v_periodo_pago, v_nrp, v_monto_amortizacion, v_monto_dif_amo
		FROM dis_det_avance_pago
		WHERE folio = p_folio
		
		
			UPDATE dis_det_avance_pago
			SET monto_amortizacion = v_monto_amortizacion,
				monto_dif_amo = v_monto_dif_amo,
				estado = 30
			WHERE id_derechohabiente = v_id_derechohabiente
			AND num_credito = v_num_credito
			AND periodo_pago = v_periodo_pago
			AND nrp = v_nrp
			AND estado IN (30,85);
	
	
	END FOREACH;
	
	--Elimina registros de dis_det_avance_pago
	DELETE FROM safre_viv:dis_det_avance_pago
	WHERE folio = p_folio;
	
	--Elimina registros de dis_rch
	DELETE FROM safre_viv:dis_rch_avance_pago
	WHERE folio = p_folio;
	
	--Elimina registros de cta_movimiento
	DELETE FROM safre_viv:cta_movimiento
	WHERE folio_liquida = p_folio;

	UPDATE statistics FOR TABLE dis_det_avance_pago;

	LET v_char = "Reverso de ajustes correcto";
	RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


