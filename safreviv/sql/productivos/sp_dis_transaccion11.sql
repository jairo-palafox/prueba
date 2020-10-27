






CREATE PROCEDURE "safreviv".sp_dis_transaccion11(p_id_derechohabiente  DECIMAL(9,0), 
                                      p_imp_ap_pat          DECIMAL(12,2),
                                      p_monto_pesos	        DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                      p_imp_am_cre          DECIMAL(12,2),
                                      p_folio_disp          DECIMAL(10,0), 
                                      p_id_referencia       DECIMAL(9,0), 
                                      p_precio_fec_hoy      DECIMAL(19,14),
                                      p_nrp                 CHAR(11),
                                      p_periodo_pago        CHAR(6),
                                      p_folio_sua           DECIMAL(6),
                                      p_num_crd_ifv         CHAR(10),
                                      p_proceso_cod         SMALLINT,
                                      p_tpo_patron          CHAR(2),    --Tipo Patron, viene de NRP[1,2]
                                      p_fecha_pago_val      DATE,	   --Fecha de pago
                                      p_tipo_trabajador     SMALLINT,
                                      p_localiza_trabajador CHAR(1),
                                      p_aiv_ap_pat          DECIMAL(18,6)) --Valor de AIVS 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 16032016
--Declaración de variables
DEFINE v_f_liquida           DATE;
DEFINE v_subcuenta           SMALLINT;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_monto_acciones      DECIMAL(12,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_subcuenta_1         SMALLINT;
DEFINE v_movimiento_1        SMALLINT;
DEFINE v_monto_acciones1     DECIMAL(12,2);
DEFINE v_monto_pesos_1       DECIMAL(22,2);
DEFINE v_origen              CHAR(20);
DEFINE v_proceso_cod_RP      SMALLINT;
--DEFINE v_id_ref_seq          DECIMAL (9,0);  --Secuencia de interfaz
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN v_status ,isam_err , error_info;  
END EXCEPTION

LET v_f_liquida       = TODAY;
LET v_fondo_inversion = 11;
LET v_f_valor         = TODAY;
LET v_f_registro      = TODAY;
LET v_h_registro      = CURRENT HOUR TO SECOND;
LET v_origen          = "Dis-Pag-TRM-"||p_periodo_pago; --Verificar
LET v_subcuenta_1     = 0;
LET v_movimiento_1    = 0;
LET v_monto_acciones1 = 0.00;
LET v_monto_pesos_1   = 0.00;
LET v_subcuenta       = 0;
LET v_movimiento      = 0;
LET v_monto_acciones  = 0.00;
LET v_monto_pesos     = 0.00;
LET v_proceso_cod_RP  = 1401;
--LET v_id_ref_seq      = 0;
LET v_bnd_proceso     = 0; --Estado correcto

--Obtiene referencia de la secuencia de interfaz
--LET v_id_ref_seq      = seq_dis_interface_hs.NEXTVAL;
--LET v_movimiento_1    = 481; --481 - Abono Dispersión Entregada por TRM
LET v_movimiento      = 1202; --1202 - Cargo Dispersión Entregada por TRM

  --Si importe de aportacion > 0 asigna a SUBCUENTA
  --IF p_imp_ap_pat > 0 THEN
  IF p_aiv_ap_pat > 0 THEN
     LET v_monto_pesos    = p_monto_pesos * -1;
     LET v_monto_acciones = p_aiv_ap_pat * -1;
     	
     IF p_localiza_trabajador = 3 THEN
        IF p_tpo_patron = '99' THEN
	       LET v_subcuenta = 44;
        ELSE 
	       LET v_subcuenta = 4;
        END IF; 
     ELSE
        LET v_subcuenta = 4;
     END IF 
		 
     INSERT INTO dis_preliquida             
          VALUES(v_f_liquida, 
                 p_id_derechohabiente, 
                 v_subcuenta, 
                 v_fondo_inversion, 
                 v_movimiento, 
                 p_folio_disp, 
                 p_id_referencia, 
                 v_monto_acciones, 
                 v_monto_pesos,
                 v_f_valor, 
                 v_f_registro, 
                 v_h_registro, 
                 v_origen);

     {INSERT INTO dis_preliquida             
          VALUES(v_f_liquida, 
                 p_id_derechohabiente, 
                 v_subcuenta, 
                 v_fondo_inversion, 
                 v_movimiento_1, 
                 p_folio_disp, 
                 p_id_referencia, 
                 v_monto_acciones *-1, 
                 v_monto_pesos *-1,
                 v_f_valor, 
                 v_f_registro, 
                 v_h_registro, 
                 v_origen);}
  END IF 
  
  --Si amortizacion > 0 asigna a SUBCUENTA
  IF p_imp_am_cre > 0 THEN
     LET p_precio_fec_hoy  = 1;
     LET v_fondo_inversion = 10;
	  
     --Divide el importe de la amortizacion entre el precio del dia 
     LET v_monto_pesos_1   = (p_imp_am_cre * p_precio_fec_hoy ) * -1;
     LET v_monto_acciones1 = v_monto_pesos_1;

     IF p_localiza_trabajador = 3 THEN
        IF p_tpo_patron = '99' THEN
	       LET v_subcuenta_1 = 43;
        ELSE 
	       LET v_subcuenta_1 = 41;
        END IF 
     ELSE
        LET v_subcuenta_1 = 41;
     END IF

     INSERT INTO dis_preliquida
          VALUES(v_f_liquida, 
                 p_id_derechohabiente, 
                 v_subcuenta_1, 
                 v_fondo_inversion, 
                 v_movimiento, 
                 p_folio_disp, 
                 p_id_referencia, 
                 v_monto_acciones1, 
                 v_monto_pesos_1,
                 v_f_valor, 
                 v_f_registro, 
                 v_h_registro, 
                 v_origen);             

     {INSERT INTO dis_preliquida
	      VALUES(v_f_liquida, 
		         p_id_derechohabiente, 
		         v_subcuenta_1, 
		         v_fondo_inversion, 
		         v_movimiento_1, 
		         p_folio_disp, 
		         p_id_referencia, 
		         v_monto_acciones1 *-1, 
		         v_monto_pesos_1 *-1,
		         v_f_valor, 
		         v_f_registro, 
		         v_h_registro, 
		         v_origen);}
  END IF

  LET v_char = "Terminada transacción TRM correctamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


