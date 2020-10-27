






CREATE PROCEDURE "safreviv".sp_dis_avances_pago_dif(p_folio DECIMAL(9,0),p_usuario CHAR(30))

--Última modificación 15062016
--Declaración de variables
  DEFINE v_tpo_registro 		   CHAR(01); -- Tipo de registro
  DEFINE v_num_credito 		           CHAR(10); -- Número de credito
  DEFINE v_nss 				   CHAR(11); -- No. de seguro social para obtener id_derechohabiente
  DEFINE v_periodo_pago 		   VARCHAR(06); 
  DEFINE v_f_pago 			   DATE; -- Fecha de pago
  DEFINE v_nrp 				   CHAR(11); -- Numero de registro patronal
  DEFINE v_monto_aportacion 	           DECIMAL(12,2); -- Monto de la aportacion
  DEFINE v_monto_amortizacion              DECIMAL(12,2); -- Monto de la amortización
  DEFINE v_entidad_recaudadora             SMALLINT;
  DEFINE v_folio_sua 			   SMALLINT;
  DEFINE v_reg_detalle 		           SMALLINT;
  DEFINE v_monto_apo_avance 	           DECIMAL(12,2); -- Monto de la aportación del avance
  DEFINE v_bnd_transaccion                 SMALLINT;
  DEFINE v_status                          SMALLINT;
  DEFINE error_info                        CHAR(70);
  DEFINE v_id_derechohabiente              DECIMAL(9,0); -- Id derechohabiente segun nss
  DEFINE v_precio_fec_hoy                  DECIMAL(19,14); --Precio de fondo del día
  DEFINE p_fecha_01mm			   DATE;
  DEFINE p_proceso_cod_reg_pago            SMALLINT;
  DEFINE v_edo_compensa_apo 	           SMALLINT;
  DEFINE v_precio_f_pag                    DECIMAL(19,14); --Precio de fecha de pago
  DEFINE v_f_presentacion 	           DATE;
  DEFINE v_estado 			   SMALLINT;
  DEFINE v_tpo_patron                      CHAR(02);       --Tipo de patron
  DEFINE v_tot_registros_3	           INTEGER;
  DEFINE v_tot_detalles_archivo            INTEGER;
  DEFINE v_localiza_trabajador             CHAR(1);

  LET v_tpo_registro         = "2"; --por el catálogo de layout
  LET v_num_credito          = "";
  LET v_nss                  = "";
  LET v_f_pago               = TODAY;
  LET v_periodo_pago         = "";
  LET v_nrp                  = "";
  LET v_monto_aportacion     = 0.00;
  LET v_monto_amortizacion   = 0.00;
  LET v_entidad_recaudadora  = 1812; --1812 para rechazo de diferencias
  LET v_folio_sua            = 0;
  LET v_reg_detalle          = 0;
  LET v_monto_apo_avance     = 0.00;
  LET v_bnd_transaccion      = 0;
  LET v_id_derechohabiente   = 0;
  LET v_precio_fec_hoy       = 0.00;
  LET p_fecha_01mm           = TODAY;
  LET p_proceso_cod_reg_pago = 1401;
  LET v_edo_compensa_apo     = 0;
  LET v_precio_f_pag         = 0.00;
  LET v_f_presentacion       = TODAY;
  LET v_estado               = 0;
  LET v_tpo_patron           = "";
  LET v_tot_registros_3      = 0;
  LET v_tot_detalles_archivo = 0;
  LET v_localiza_trabajador  = 1;
 
  --Obtiene total de registros del registro tipo 3 para validación
  SELECT tot_registros
  INTO   v_tot_registros_3
  FROM   safre_tmp:tmp_dis_dif_ava_pag3;
   
  --Consulta total de registros de detalle del archivo
  SELECT COUNT(*)
  INTO v_tot_detalles_archivo
  FROM safre_tmp:tmp_dis_dif_ava_pag2;

  --### Se agrega validación para rechazo por diferencia en total de registros detalle vs sumario ###--
  IF v_tot_detalles_archivo <> v_tot_registros_3 THEN
     LET v_estado = 28; 
  END IF

  --##############	   AGREGADO      ###############
  --Si hay rechazo por diferencia de totales detalle vs sumario
  IF v_estado = 28 THEN
     FOREACH
       SELECT tpo_registro, nss, num_credito, periodo_pago,
              fecha_pago, nrp, monto_aportacion, monto_amortizacion
       INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
              v_f_pago,v_nrp, v_monto_aportacion, v_monto_amortizacion
       FROM   safre_tmp:tmp_dis_dif_ava_pag2
			
       ----#División campos de importes
       LET v_monto_aportacion   = v_monto_aportacion/100 ;
       LET v_monto_amortizacion = v_monto_amortizacion/100 ;

       --#Obtenemos id_derechohabiente según número seguro social
       SELECT id_derechohabiente
       INTO   v_id_derechohabiente
       FROM   safre_viv:afi_derechohabiente
       WHERE  nss = v_nss;

       --#Inserción en la tabla rechazos de avance de pago
       --TRACE 'Estado '||v_estado;
       --TRACE 'Derechohabiente '||v_id_derechohabiente;
       --TRACE 'Tabla >>dis_rch_avance_pago';
       INSERT INTO safre_viv:dis_rch_avance_pago
            VALUES (seq_dis_rch_avance.NEXTVAL,
                    p_folio,
                    v_tpo_avance,
                    v_tpo_registro,
                    v_nss,
                    v_id_derechohabiente,
                    v_num_credito,
                    v_periodo_pago,
                    v_f_pago,
                    v_nrp,
                    v_monto_aportacion,
                    v_monto_amortizacion,
                    v_f_presentacion,
                    v_estado);
     END FOREACH;
  END IF
	
  --#Obtener fecha de presentación
  SELECT f_presentacion
  INTO   v_f_presentacion
  FROM   safre_tmp:tmp_dis_dif_ava_pag0
  WHERE  tpo_registro = 0;

  --Identificar precio de Acción del día
  SELECT precio_fondo 
  INTO   v_precio_fec_hoy
  FROM   safre_viv:glo_valor_fondo
  WHERE  fondo       = 11
  AND    f_valuacion = TODAY;

  IF v_estado <> 28 THEN
     FOREACH
       SELECT tpo_registro, nss, num_credito, periodo_pago,
	      fecha_pago, entidad_recaudadora, nrp, monto_aportacion, monto_amortizacion, 
	      folio_sua
       INTO   v_tpo_registro, v_nss, v_num_credito, v_periodo_pago,
	      v_f_pago, v_entidad_recaudadora, v_nrp, v_monto_aportacion, v_monto_amortizacion,
	      v_folio_sua
       FROM   safre_tmp:tmp_dis_dif_ava_pag2
	
       ----#División de importes
       LET v_monto_aportacion   = v_monto_aportacion/100;
       LET v_monto_amortizacion = v_monto_amortizacion/100;
				
       --Validamos que exista el registro en dis_det_avance_pago
       SELECT 	id_derechohabiente, COUNT(*)
       INTO	v_id_derechohabiente,v_reg_detalle
       FROM 	safre_viv:dis_det_avance_pago
       WHERE 	nrp          = v_nrp
       AND 	num_credito  = v_num_credito
       AND 	periodo_pago = v_periodo_pago
       AND 	estado       = 50
       GROUP BY id_derechohabiente; --Estado de Avance de pagos Cubierto
					
       IF v_reg_detalle = 1 THEN
					
	  --Si el registro está en dis_det_avance, ahora se valida que el estado de la aportación sea menor
	  SELECT edo_compensa_apo --validar
	  INTO   v_edo_compensa_apo
	  FROM 	 safre_viv:dis_compensa_avance --el edo_compensa_avance = 1
          WHERE  nrp          = v_nrp
          AND 	 num_credito  = v_num_credito
          AND 	 periodo_pago = v_periodo_pago; 
						
	  --validamos Si el estado es ( 2 Y 5 PAGO MAYOR AL AVANCE)
	  IF v_edo_compensa_apo = 2 OR
	     v_edo_compensa_apo = 5 THEN 					
							
	     --Obtenemos el tipo de patrón
	     SELECT tpo_patron,f_pago
	     INTO   v_tpo_patron, v_f_pago
	     FROM   safre_viv:cta_his_pagos
	     WHERE  id_derechohabiente = v_id_derechohabiente
	     AND    nrp                = v_nrp
	     AND    periodo_pago       = v_periodo_pago; 
											
	     --Identificar precio de Acción del día
	     SELECT precio_fondo 
	     INTO   v_precio_f_pag
	     FROM   safre_viv:glo_valor_fondo
	     WHERE  fondo       = 11
	     AND    f_valuacion = v_f_pago;
							
             --Insertamos en dse_devolucion para crédito tradicional
             EXECUTE PROCEDURE safre_viv:sp_dis_Transaccion4(v_id_derechohabiente,
                                                             v_num_credito,
                                                             v_periodo_pago,
                                                             v_f_pago, 
                                                             v_monto_aportacion, --el que nos envían
                                                             v_precio_f_pag,
                                                             v_precio_fec_hoy,
                                                             p_fecha_01mm,
                                                             p_proceso_cod_reg_pago, --1401
                                                             p_folio,
                                                             v_tpo_patron,
                                                             v_localiza_trabajador, --de cta_his_pagos
                                                             0)
                          INTO v_bnd_transaccion, v_status,error_info;

	     LET v_estado = 300;
							 
	     --Actualizamos dis_det_avance para indicar que se realizó el rechazo por diferencias correctamente
	     UPDATE safre_viv:dis_det_avance_pago
	     SET    estado       = v_estado --Para rechazo de diferencia de pagos
	     WHERE  nrp          = v_nrp
	     AND    num_credito  = v_num_credito
	     AND    periodo_pago = v_periodo_pago
	     AND    estado       = 50;

          ELSE

	     LET v_estado = 302;
						
	     --Rechazamos el registro por no contar con estado de la aportación menor al avance y actualizamos dis_det_avance
	     {UPDATE safre_viv:dis_det_avance_pago
	     SET    estado       = v_estado --Para rechazo de diferencia de pagos
	     WHERE  nrp          = v_nrp
	     AND    num_credito  = v_num_credito
	     AND    periodo_pago = v_periodo_pago
	     AND    estado       = 50;}
							
             INSERT INTO safre_viv:dis_rch_avance_pago
                  VALUES (seq_dis_rch_avance.NEXTVAL,
                          p_folio,
                          v_entidad_recaudadora,
                          v_tpo_registro,
                          v_nss,
                          v_id_derechohabiente,
                          v_num_credito,
                          v_periodo_pago,
                          v_f_pago,
                          v_nrp,
                          v_monto_aportacion,
                          v_monto_amortizacion,
                          v_f_presentacion,
                          v_estado);
	  END IF;
       ELSE 
	  --Si no existe el registro para aplicarle el rechazo, insertamos en dis_rch_avance_pago 
	  LET v_estado = 301;
						
	  INSERT INTO safre_viv:dis_rch_avance_pago
	       VALUES (seq_dis_rch_avance.NEXTVAL,
                       p_folio,
                       v_entidad_recaudadora,
                       v_tpo_registro,
                       v_nss,
	               v_id_derechohabiente,
                       v_num_credito,
                       v_periodo_pago,
	               v_f_pago,
                       v_nrp,
                       v_monto_aportacion,
                       v_monto_amortizacion,
	               v_f_presentacion,
                       v_estado);
       END IF;
     END FOREACH; 
  END IF;

  UPDATE statistics FOR TABLE dis_det_avance_pago;

END PROCEDURE;


