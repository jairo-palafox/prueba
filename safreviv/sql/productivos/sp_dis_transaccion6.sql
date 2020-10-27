






CREATE PROCEDURE "safreviv".sp_dis_transaccion6(p_id_derechohabiente  DECIMAL(9,0), 
                                     p_imp_ap_pat          DECIMAL(12,2),
                                     p_imp_am_cre          DECIMAL(12,2),
                                     p_folio_disp          DECIMAL(10,0), 
                                     p_id_referencia       DECIMAL(9,0), 
                                     p_precio_fec_hoy      DECIMAL(19,14),
                                     p_proceso_cod         SMALLINT,       --Proceso operacion
                                     p_tpo_patron          CHAR(2),
				     p_localiza_trabajador CHAR(1))
									 
RETURNING SMALLINT, SMALLINT ,CHAR(70);
                                     
DEFINE v_f_liquida          DATE;
DEFINE v_subcuenta          SMALLINT;
DEFINE v_fondo_inversion    SMALLINT;
DEFINE v_movimiento         SMALLINT;
DEFINE v_monto_acciones_ap  DECIMAL(12,2);
DEFINE v_monto_pesos_ap     DECIMAL(22,2);
DEFINE v_f_valor            DATE;
DEFINE v_f_registro         DATE;
DEFINE v_h_registro         DATETIME HOUR TO SECOND;
DEFINE v_subcuenta_1        SMALLINT;
DEFINE v_movimiento_1       SMALLINT;
DEFINE v_monto_acciones1    DECIMAL(12,2);
DEFINE v_monto_pesos_1      DECIMAL(22,2);
DEFINE v_origen             CHAR(20);
--DEFINE v_nrp1               CHAR(02);
DEFINE v_bnd_52             DECIMAL (10,0);
DEFINE v_proceso_cod_RP     SMALLINT;

DEFINE  v_status         SMALLINT;
DEFINE  sql_err          INTEGER ;
DEFINE  isam_err         INTEGER ;
DEFINE  error_info       CHAR(70);
DEFINE  v_char           CHAR(20);
DEFINE  v_bnd_transaccion SMALLINT;
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
LET v_origen          = "Dispersión P-HS";
--LET v_nrp1            = "";
LET v_bnd_52          = 0;
LET v_monto_acciones_ap = 0.00;
LET v_monto_pesos_ap    = 0.00;
LET v_monto_acciones1   = 0.00;
LET v_monto_pesos_1     = 0.00;
LET v_subcuenta         = 0;
LET v_subcuenta_1       = 0;
LET v_movimiento        = 0;
LET v_movimiento_1      = 0;
LET v_proceso_cod_RP    = 1401;
LET v_bnd_transaccion = 0;
LET v_bnd_proceso     = 0; --Estado correcto


   --SET DEBUG FILE TO '/ds/safreviv_int/BD/Transaccion6.--TRACE';
   --TRACE 'Derechoghabiente '||p_id_derechohabiente;
   --TRACE 'Aportaciones     '||p_imp_ap_pat;
   --TRACE 'Amortizaciones   '||p_imp_am_cre;
   --TRACE 'Folio Dispersion '||p_folio_disp;
   --TRACE 'Referencia       '||p_id_referencia;
   --TRACE 'Precio fec hoy   '||p_precio_fec_hoy;
   --TRACE 'Proceso Cos pag  '||p_proceso_cod;
   --TRACE 'Tipo Trabajadr   '||p_tpo_patron;

   --Se obtienen las primeras dos posiciones del NRP
   --LET v_nrp1 = p_nrp[1,2];
   ----TRACE 'NRP Dos Pocisiones '||v_nrp1;

   --TRACE 'Aportaciones > 0 '||p_imp_ap_pat;
   --Si importe de aportacion > 0 asigna a SUBCUENTA 
   IF p_imp_ap_pat > 0 THEN
      --Si el proceso_cod de pagos es (Registro de Pagos LQINFO) (1)
      --Valor Cargo por Pago a Crédito Tradicional
      LET v_movimiento = 42;


      LET v_monto_acciones_ap = (p_imp_ap_pat / p_precio_fec_hoy )* -1; 
      --LET v_monto_pesos_ap = (v_monto_acciones_ap * p_precio_fec_hoy);
      LET v_monto_pesos_ap    = p_imp_ap_pat * -1;
      
/*      IF v_monto_acciones_ap < 0 THEN 
         --Divide el importe de aportaciones entre el precio del dia 
         LET v_monto_acciones_ap = (v_monto_acciones_ap * -1);
      END IF 
      IF v_monto_pesos_ap < 0 THEN 
         --Multiplica el importe de aportaciones por el precio del dia 
         LET v_monto_pesos_ap = (v_monto_pesos_ap * -1);
      END IF */
      
      --TRACE 'Proceso Cod REG PAG '||p_proceso_cod;
      IF p_proceso_cod = v_proceso_cod_RP THEN
		IF p_localiza_trabajador = 3 THEN
			 IF p_tpo_patron = '99' THEN
				LET v_subcuenta = 44;
			 ELSE 
				LET v_subcuenta = 4;
			 END IF 
		ELSE
			
		END IF 
      ELSE 
      --Si el proceso_cod de pagos es (Registro de Pagos Sólo Infonavit) (3)
         LET v_subcuenta = 44;   
      END IF
      --TRACE 'Inserta Dis preliquida ';
      --TRACE 'F liquia  '||v_f_liquida;
      --TRACE 'Derechoh  '||p_id_derechohabiente;
      --TRACE 'Subcuenta '||v_subcuenta;
      --TRACE 'FONDO     '||v_fondo_inversion;
      --TRACE 'Movimeinto '||v_movimiento;
      --TRACE 'Folio disp '||p_folio_disp;
      --TRACE 'referencia '||p_id_referencia;
      --TRACE 'Acciones   '||v_monto_acciones_ap;
      --TRACE 'Pesos      '||v_monto_pesos_ap;
      --TRACE 'F valor    '||v_f_valor;
      --TRACE 'f Registro '||v_f_registro;
      --TRACE 'H registro '||v_h_registro;
      --TRACE 'Origen     '||v_origen;
      INSERT INTO safre_viv:dis_preliquida
      VALUES(v_f_liquida, p_id_derechohabiente, v_subcuenta, 
             v_fondo_inversion, v_movimiento, p_folio_disp, 
             p_id_referencia, v_monto_acciones_ap, v_monto_pesos_ap,
             v_f_valor, v_f_registro, v_h_registro, v_origen);
      --TRACE 'Termina de insertar ';
   END IF 

   --TRACE 'Amortizacion > 0 '||p_imp_am_cre;
   --Si amortizacion > 0 asigna a SUBCUENTA 
   IF p_imp_am_cre > 0 THEN

         --Identificar precio de Acción del día
   SELECT precio_fondo INTO p_precio_fec_hoy
   FROM safre_viv:glo_valor_fondo
   WHERE fondo = 10
     AND f_valuacion = TODAY;

      --Si el proceso_cod de pagos es (Registro de Pagos LQINFO) (1)
      --Valor Cargo por amortización a Crédito Tradicional
      LET  v_movimiento_1 = 52;
      LET v_fondo_inversion = 10;
      --Divide el importe de la amortizacion entre el precio del dia 
      --LET v_monto_acciones1 = (p_imp_am_cre / p_precio_fec_hoy ) * -1;
      LET v_monto_pesos_1 = (p_imp_am_cre * p_precio_fec_hoy)* -1;

      LET v_monto_acciones1 = v_monto_pesos_1;
      --TRACE 'Proceso Cod REG PAG '||p_proceso_cod;
      IF p_proceso_cod = v_proceso_cod_RP THEN
	  
		IF p_localiza_trabajador =  3 THEN
			 IF p_tpo_patron = '99' THEN
				LET v_subcuenta_1 = 43;
			 ELSE 
				LET v_subcuenta_1 = 41;
			 END IF
		ELSE
			LET v_subcuenta_1 = 41;
		END IF 
         --TRACE 'v_subcuenta_1 a '||v_subcuenta_1;
      ELSE 
      --Si el proceso_cod de pagos es (Registro de Pagos Sólo Infonavit) (43)
         LET v_subcuenta_1 = 43;
      END IF
      --TRACE 'v_subcuenta_1 b '||v_subcuenta_1;

      --TRACE 'Fecha liquida    '||v_f_liquida;
      --TRACE 'derechohabiente  '||p_id_derechohabiente;
      --TRACE 'Suncuenta        '||v_subcuenta_1;
      --TRACE 'Folio duapersion '||p_folio_disp;

      SELECT COUNT(*)
        INTO v_bnd_52
      FROM safre_viv:dis_preliquida
      WHERE f_liquida  = v_f_liquida
        AND id_derechohabiente = p_id_derechohabiente
        AND subcuenta = v_subcuenta_1
        AND fondo_inversion = 0
        AND movimiento = 52
        AND folio_liquida = p_folio_disp
        AND id_referencia = p_id_referencia;

      --TRACE 'Inserta Dis preliquida con Movimiento 52 '||v_bnd_52;

      IF v_bnd_52 = 0 OR v_bnd_52 IS NULL THEN
         --TRACE 'Inserta Dis preliquida del 52';
         --TRACE 'F liquia  '||v_f_liquida;
         --TRACE 'Derechoh  '||p_id_derechohabiente;
         --TRACE 'Subcuenta '||v_subcuenta_1;
         --TRACE 'FONDO     '||0;
         --TRACE 'Movimeinto '||v_movimiento_1;
         --TRACE 'Folio disp '||p_folio_disp;
         --TRACE 'referencia '||p_id_referencia;
         --TRACE 'Acciones   '||v_monto_acciones1;
         --TRACE 'Pesos      '||v_monto_pesos_1;
         --TRACE 'F valor    '||v_f_valor;
         --TRACE 'f Registro '||v_f_registro;
         --TRACE 'H registro '||v_h_registro;
         --TRACE 'Origen     '||v_origen;
         INSERT INTO safre_viv:dis_preliquida
            VALUES(v_f_liquida, p_id_derechohabiente, v_subcuenta_1,
                   v_fondo_inversion, v_movimiento_1, p_folio_disp,p_id_referencia,
                   v_monto_acciones1, v_monto_pesos_1,
                   v_f_valor, v_f_registro, v_h_registro, v_origen);
         --TRACE 'Termina de insertar ';
      END IF
   END IF
   
   
   LET v_char = "Terminada transacción 6 correctamente";
   RETURN v_bnd_proceso , 0 , v_char;
         --TRACE 'Termina el stored 6';
END PROCEDURE;


