






CREATE PROCEDURE "safreviv".sp_dis_avances_pago_ajuste(p_folio DECIMAL(10,0),p_usuario CHAR(30))

RETURNING SMALLINT, SMALLINT, CHAR(70);

	DEFINE v_nss                 CHAR(11); -- No. de seguro social para obtener id_derechohabiente
	DEFINE v_id_derechohabiente  DECIMAL(9,0); -- Id derechohabiente segun nss
	DEFINE v_num_credito         CHAR(10); -- Número de credito
	DEFINE v_periodo_pago        CHAR(06); -- Periodo de pago
	DEFINE v_nrp                 CHAR(11); -- Número de registro patronal
	DEFINE v_estado              SMALLINT; -- Tipo de estado de rechazo
	DEFINE v_tot_amortizacion    DECIMAL(22,2); -- Total de amortización para ajuste
	DEFINE v_tpo_avance          SMALLINT; 
	DEFINE v_tpo_registro        CHAR(01); -- Tipo de registro
	DEFINE v_f_pago              DATE; -- Fecha de pago
	DEFINE v_monto_aportacion    DECIMAL(12,2); -- Monto de la aportacion
	DEFINE v_id_det_avance_pago  DECIMAL(9,0); --Valor del id de avance de pago
	DEFINE v_folio_avance		 DECIMAL(10,0); --Folio de avance de pagos
	DEFINE v_monto_amortizacion  DECIMAL(12,2); -- Monto de la amortización
	DEFINE v_dif_apo             DECIMAL(22,2); -- Monto de la aportacion
	DEFINE v_dif_amo             DECIMAL(22,2); -- Monto de la amortización
	DEFINE v_f_presentacion      DATE; -- Fecha de presentacion
	DEFINE v_hora_proceso        DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
	DEFINE v_origen              VARCHAR(20); 
	
	DEFINE sql_err              INTEGER ;
	DEFINE isam_err             INTEGER ;
	DEFINE error_info           CHAR(70);
	DEFINE v_char               VARCHAR(30);
	DEFINE v_status             SMALLINT;

	ON EXCEPTION
	   SET sql_err, isam_err, error_info
			 LET v_status = sql_err;
			 RETURN  v_status ,isam_err , error_info;
	END EXCEPTION
	
	LET v_nss                  = "";
	LET v_id_derechohabiente   = 0;
	LET v_num_credito          = "";
	LET v_periodo_pago         = "";
	LET v_nrp                  = "";
	LET v_estado               = 0;
	LET v_tot_amortizacion     = 0.00;
	LET v_tpo_avance           = 1814; --Valor del tipo de avance para rechazo en el ajuste
	LET v_tpo_registro         = "2";
	LET v_f_pago               = TODAY;
	LET v_monto_aportacion     = 0.00;
	LET v_id_det_avance_pago   = 0;
	LET v_folio_avance		   = 0;
	LET v_monto_amortizacion   = 0.00;
	LET v_dif_apo              = 0.00;
    LET v_dif_amo              = 0.00;
	LET v_f_presentacion       = TODAY;
	LET v_hora_proceso         = CURRENT HOUR TO SECOND;
	LET v_origen               = "Ajus Avance";
	LET v_char				   = "";


	FOREACH 
		--Consulta para obtener datos de temporales
		SELECT nss, num_credito, periodo_pago, nrp, amortizacion
		INTO   v_nss, v_num_credito, v_periodo_pago, v_nrp, v_tot_amortizacion
		FROM   safre_tmp:tmp_dis_ajuste_ava_pag0

	   SELECT id_derechohabiente
	   INTO v_id_derechohabiente
	   FROM afi_derechohabiente
	   WHERE nss = v_nss;
	   
		IF (v_id_derechohabiente IS NULL) OR (v_id_derechohabiente = 0) THEN
			--#Rechazo por no existir en el maestro de derechohabientes
			LET v_estado = 82;
			LET v_id_derechohabiente = 999999999; --Se agrega este número cuando no existe registro
		ELSE
			IF EXISTS( 
					SELECT id_dis_det_avance_pago
					FROM dis_det_avance_pago
					WHERE id_derechohabiente = v_id_derechohabiente
					AND num_credito = v_num_credito
					AND periodo_pago = v_periodo_pago
					AND nrp = v_nrp

                   ) THEN --Verifica el estado del registro con la misma llave
				   
				   
						SELECT monto_aportacion, monto_amortizacion, estado
						INTO v_monto_aportacion, v_monto_amortizacion, v_estado
						FROM dis_det_avance_pago
						WHERE id_derechohabiente = v_id_derechohabiente
						AND num_credito = v_num_credito
						AND periodo_pago = v_periodo_pago
						AND nrp = v_nrp;
					   
					   IF v_estado <> 30 THEN
							LET v_estado = 84;
					   END IF;
					   
		    ELSE --Si el registro no existe con la llave de búsqueda le asignamos al registro el estado 83
				LET v_estado = 83;
			END IF 
		
		END IF;


		--Se crea registro en tabla de rechazo dependiendo del estado
		IF v_estado = 82 OR v_estado = 83 OR v_estado = 84 THEN
			
			LET v_tpo_avance = 1814;
			
			INSERT INTO safre_viv:dis_rch_avance_pago
			VALUES (seq_dis_rch_avance.NEXTVAL,
					p_folio,
					v_tpo_avance, --1814
					v_tpo_registro, --2
					v_nss,
					v_id_derechohabiente,
					v_num_credito,
					v_periodo_pago,
					v_f_pago,
					v_nrp,
					v_monto_aportacion,
					v_tot_amortizacion,
					v_f_presentacion,
					v_estado);
					
			CONTINUE FOREACH;
		END IF;
			
		 --Si el registro no es rechazado
		
		IF v_estado = 30 AND v_monto_amortizacion > 0 THEN 
		
				--Se extrae la información del registro
				SELECT id_dis_det_avance_pago, folio, tpo_avance, tpo_registro, f_pago, 
					   monto_aportacion, monto_amortizacion, monto_dif_apo, monto_dif_amo, f_presentacion, estado
				INTO v_id_det_avance_pago, v_folio_avance, v_tpo_avance, v_tpo_registro, v_f_pago,
					 v_monto_aportacion, v_monto_amortizacion, v_dif_apo, v_dif_amo, v_f_presentacion, v_estado
				FROM dis_det_avance_pago
				WHERE id_derechohabiente = v_id_derechohabiente
				AND num_credito = v_num_credito
				AND periodo_pago = v_periodo_pago
				AND nrp = v_nrp
				AND estado = 30;
				
				--Se actualiza registro anterior con la amortización que viene en el archivo de ajuste
				UPDATE safre_viv:dis_det_avance_pago
				SET monto_amortizacion = v_tot_amortizacion,
					monto_dif_amo = v_tot_amortizacion
				WHERE id_derechohabiente = v_id_derechohabiente
				AND num_credito = v_num_credito
				AND periodo_pago = v_periodo_pago
				AND nrp = v_nrp
				AND estado = 30;
				
				IF v_monto_aportacion = 0 THEN --Se cierra el avance por montos = 0
				
					UPDATE dis_det_avance_pago
					SET estado = 85
					WHERE id_derechohabiente = v_id_derechohabiente
					AND num_credito = v_num_credito
					AND periodo_pago = v_periodo_pago
					AND nrp = v_nrp
					AND estado = 30;
					
				END IF
				
				
				--Se crea un registro con el estado 80
				INSERT INTO safre_viv:dis_det_avance_pago
				VALUES (seq_dis_avance.NEXTVAL,
					   p_folio, --verificar
					   v_periodo_pago,
					   v_id_derechohabiente,
					   1814,
					   v_tpo_registro,
					   v_num_credito,
					   v_f_pago,
					   v_nrp,
					   v_monto_aportacion,
					   v_monto_amortizacion,
					   v_dif_apo,
					   v_dif_amo,
					   v_f_presentacion,
					   80);
					   
				LET v_origen = "Ajus Avance";
				LET v_origen = v_origen||'-'||v_periodo_pago;
				
				--Se ingresan 2 registros a la tabla cta_movimiento
				--Abono ajuste avance abierto
				INSERT INTO safre_viv:cta_movimiento
				VALUES (TODAY,
						v_id_derechohabiente,
						41,
						0,
						471,
						p_folio, --Folio ajuste avance abierto
						seq_dis_avance.CURRVAL,
						0,
						v_monto_amortizacion,
						v_f_pago,
						TODAY,
						v_hora_proceso,
						v_origen);
						
				--Cargo ajuste avance abierto
				INSERT INTO safre_viv:cta_movimiento
				VALUES (TODAY,
						v_id_derechohabiente,
						41,
						0,
						1062,
						p_folio, --Folio ajuste avance abierto
						seq_dis_avance.CURRVAL,
						0,
						v_monto_amortizacion*(-1),
						v_f_pago,
						TODAY,
						v_hora_proceso,
						v_origen);
			END IF

	END FOREACH;

--#####################################################################################

   UPDATE statistics FOR TABLE dis_det_avance_pago;
   LET v_char = "Ajuste de Avances Correcto";
   RETURN 0, 0, v_char;

END PROCEDURE;


