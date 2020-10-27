






CREATE PROCEDURE "safreviv".sp_dis_transaccion10(p_id_derechohabiente DECIMAL(9,0), 
                                     p_imp_ap_pat     DECIMAL(12,2),
                                     p_imp_am_cre     DECIMAL(12,2),
                                     p_folio_disp     DECIMAL(10,0), 
                                     p_id_referencia  DECIMAL(9,0), 
                                     p_precio_fec_hoy DECIMAL(19,14),
                                     p_nrp            CHAR(11),
                                     p_periodo_pago   CHAR(6),
                                     p_folio_sua      DECIMAL(6),
                                     p_num_crd_ifv    CHAR(10),
                                     p_proceso_cod    SMALLINT,
                                     p_tpo_patron     CHAR(2),   
                                     p_fecha_pago_val DATE,
									 p_origen_dispersion SMALLINT,
									 p_tipo_trabajador SMALLINT)      
									 
RETURNING SMALLINT, SMALLINT ,CHAR(70);

                                     
DEFINE v_f_liquida       DATE;
DEFINE v_subcuenta       SMALLINT;
DEFINE v_fondo_inversion SMALLINT;
DEFINE v_movimiento      SMALLINT;
DEFINE v_monto_acciones  DECIMAL(12,2);
DEFINE v_monto_pesos     DECIMAL(22,2);
DEFINE v_f_valor         DATE;
DEFINE v_f_registro      DATE;
DEFINE v_h_registro      DATETIME HOUR TO SECOND;
DEFINE v_subcuenta_1     SMALLINT;
DEFINE v_movimiento_1    SMALLINT;
DEFINE v_monto_acciones1 DECIMAL(12,2);
DEFINE v_monto_pesos_1   DECIMAL(22,2);
DEFINE v_origen          CHAR(20);
DEFINE v_proceso_cod_RP  SMALLINT;
DEFINE v_id_ref_seq      DECIMAL (9,0);  --Secuencia de interfaz
DEFINE  v_status         SMALLINT;
DEFINE  sql_err          INTEGER ;
DEFINE  isam_err         INTEGER ;
DEFINE  error_info       CHAR(70);
DEFINE  v_char           CHAR(20);
DEFINE v_bnd_proceso     SMALLINT;       --Estatus del proceso

ON EXCEPTION
   SET sql_err, isam_err, error_info
         LET v_status = sql_err;
         RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

LET v_f_liquida       = TODAY;
LET v_fondo_inversion = 11;
LET v_f_valor         = TODAY;
LET v_f_registro      = TODAY;
LET v_h_registro      = CURRENT HOUR TO SECOND;
LET v_origen          = "Dis Cartera-"||p_periodo_pago;
LET v_subcuenta_1     = 0;
LET v_movimiento_1    = 0;
LET v_monto_acciones1 = 0.00;
LET v_monto_pesos_1   = 0.00;
LET v_subcuenta       = 0;
LET v_movimiento      = 0;
LET v_monto_acciones  = 0.00;
LET v_monto_pesos     = 0.00;
LET v_proceso_cod_RP  = 1401;
LET v_id_ref_seq      = 0;
LET v_bnd_proceso     = 0; --Estado correcto


	IF (p_origen_dispersion = 1 OR p_origen_dispersion = 2 OR p_origen_dispersion = 4) AND p_tipo_trabajador = 1 THEN

		LET v_origen = "Dis DEV-"||p_periodo_pago;
		
		--Se asigna movimiento
		LET v_movimiento = 602; --Cargo devolución pago liquidado
	END IF;
	
	IF p_origen_dispersion = 3 AND p_tipo_trabajador = 1 THEN

		LET v_origen = "Dis AVA-"||p_periodo_pago;
		
		--Se asigna movimiento
		LET v_movimiento = 562;
		
	END IF;


	IF p_origen_dispersion = 5 THEN

		--LET v_origen = "Dis AVA-"||p_periodo_pago;

		IF p_imp_ap_pat > 0 THEN
		--Se asigna movimiento
		LET v_movimiento = 612;
		
		END IF;
		
		IF p_imp_am_cre > 0 THEN
		--Se asigna movimiento
		LET v_movimiento = 622;
		
		END IF;
	END IF;

   --Obtiene referencia para conculiar de la secuencia de interfaz
   LET v_id_ref_seq = seq_dis_interface_hs.NEXTVAL;
   
   --Si importe de aportacion > 0 asigna a SUBCUENTA
   IF p_imp_ap_pat > 0 THEN
		---Se asigna movimiento
      
	  
      LET v_monto_acciones = (p_imp_ap_pat / p_precio_fec_hoy) * -1;    
      LET v_monto_pesos    = p_imp_ap_pat * -1;
    
      --Si el proceso_cod de pagos es (Registro de Pagos LQINFO) (1)
      IF p_proceso_cod = v_proceso_cod_RP THEN
         IF p_tpo_patron = '99' THEN
            LET v_subcuenta = 44;
         ELSE 
            LET v_subcuenta = 4;
         END IF 
      ELSE 
      --Si el proceso_cod de pagos es (Registro de Pagos Sólo Infonavit) (3)
         LET v_subcuenta = 44;   
      END IF
	  
	  IF p_periodo_pago <= '200505' THEN
           LET v_movimiento = 1000;
      END IF
      
      INSERT INTO safre_viv:dis_preliquida             
      VALUES(v_f_liquida, p_id_derechohabiente, v_subcuenta, 
             v_fondo_inversion, v_movimiento, p_folio_disp, 
             v_id_ref_seq, v_monto_acciones, v_monto_pesos,
             v_f_valor, v_f_registro, v_h_registro, v_origen);     
          
   END IF 
  
   --Si amortizacion > 0 asigna a SUBCUENTA
   IF p_imp_am_cre > 0 THEN

   --Identificar precio de Acción del día
   SELECT precio_fondo INTO p_precio_fec_hoy
   FROM safre_viv:glo_valor_fondo
   WHERE fondo = 10
     AND f_valuacion = TODAY;
    
      LET v_fondo_inversion = 10;

      LET v_monto_pesos_1 = (p_imp_am_cre * p_precio_fec_hoy ) * -1;

      LET v_monto_acciones1 = v_monto_pesos_1;
	  
	  
      --Si el proceso_cod de pagos es (Registro de Pagos LQINFO) (1)
      IF p_proceso_cod = v_proceso_cod_RP THEN 
         IF p_tpo_patron = '99' THEN
            LET v_subcuenta_1 = 43;
         ELSE 
            LET v_subcuenta_1 = 41;
         END IF 
      ELSE 
      --Si el proceso_cod de pagos es (Registro de Pagos Sólo Infonavit) (3)
         LET v_subcuenta_1 = 43;
      END IF

         IF p_periodo_pago <= '200505' THEN
            LET v_movimiento = 562;
         END IF

         INSERT INTO safre_viv:dis_preliquida
            VALUES(v_f_liquida, p_id_derechohabiente, v_subcuenta_1, 
                   v_fondo_inversion, v_movimiento, p_folio_disp, 
                   v_id_ref_seq, v_monto_acciones1, v_monto_pesos_1,
                   v_f_valor, v_f_registro, v_h_registro, v_origen);            
      
   END IF

   LET v_char = "Terminada transacción 10 correctamente";
      RETURN v_bnd_proceso , 0 , v_char;

   --TRACE 'Termina SPL3';
             
END PROCEDURE;


