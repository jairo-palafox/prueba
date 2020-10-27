






CREATE PROCEDURE "safreviv".sp_rev_dis_avances_pago_dif(p_folio DECIMAL(9,0))

RETURNING SMALLINT, SMALLINT ,CHAR(70);


	DEFINE v_id_derechohabiente DECIMAL(9,0); -- Id derechohabiente segun nss
	DEFINE v_periodo_pago 		VARCHAR(06); 
	DEFINE v_num_credito 		CHAR(10); -- Número de credito
	DEFINE v_nrp 				CHAR(11); -- Numero de registro patronal
	DEFINE v_status         	SMALLINT;
	DEFINE sql_err          	INTEGER ;
	DEFINE isam_err         	INTEGER ;
	DEFINE error_info       	CHAR(70);
	DEFINE  v_char           	CHAR(20);
	DEFINE v_bnd_proceso        SMALLINT;       --Estatus del proceso
	
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
	
	FOREACH
	
		--obtenemos datos de dse_devolucion
		SELECT id_derechohabiente, num_credito, periodo_pago
		INTO v_id_derechohabiente,v_num_credito,v_periodo_pago
		FROM safre_viv:dse_devolucion
		WHERE folio_referencia = p_folio
				
		--obtenemos el nrp de dis_det_avance_pago
		SELECT 	nrp 
		INTO 	v_nrp
		FROM 	safre_viv:dis_det_avance_pago
		WHERE 	id_derechohabiente = v_id_derechohabiente
		AND		num_credito = v_num_credito
		AND 	periodo_pago = v_periodo_pago;

		--Actualizamos estados de los registros afectados
		UPDATE  safre_viv:dis_det_avance_pago
		SET 	estado = 50 
		WHERE 	id_derechohabiente = v_id_derechohabiente
		AND 	num_credito = v_num_credito
		AND 	periodo_pago = v_periodo_pago
		AND 	estado = 300; --Para rechazo de diferencia de pagos
	
	END FOREACH;
	
	--Elimina registros de dse_devolucion
	DELETE FROM safre_viv:dse_devolucion
	WHERE folio_referencia = p_folio;
	

	UPDATE statistics FOR TABLE dis_det_avance_pago;

	LET v_char = "Reverso correcto";
	RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


