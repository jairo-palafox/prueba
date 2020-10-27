






CREATE PROCEDURE "safreviv".sp_dis_transaccion1(p_id_derechohabiente  DECIMAL(9,0),   --id_derechohabiente
                                     p_num_crd_ifv         DECIMAL(10,0),  --Numero de credito
                                     p_periodo_pago        CHAR(6),        --Periodo de pago
                                     p_fecha_pago_val      DATE,           --Fecha de pago
                                     p_imp_ap_pat          DECIMAL(12,2),  --Importe de aportaciones
				     p_monto_pesos	   DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                     p_precio_fec_pag      DECIMAL(19,14), --Precio del fondo de la fecha de pago
                                     p_precio_fec_hoy      DECIMAL(19,14), --Precio del fondo del día
                                     p_fecha_01mm          DATE,           --Fecha de movimiento
                                     p_proceso_cod         SMALLINT,       --Proceso operacion
                                     p_folio_referencia    DECIMAL(9,0),
                                     p_tpo_patron          CHAR(02),       --Tipo Patron, viene de NRP[1,2]
				     p_localiza_trabajador CHAR(1),        --Viene Folio Dispersion
				     p_aiv_ap_pat          DECIMAL(18,6))  --Valor de AIVS
									 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 21062013
--Declaración de variables
DEFINE v_tpo_transferencia   CHAR(2);
DEFINE v_origen_devolucion   CHAR(2);
DEFINE v_aivs_aportacion     DECIMAL(16,6);
DEFINE v_nss_separacion      CHAR(11);
DEFINE v_estado              SMALLINT;
DEFINE v_folio 		     DECIMAL(9,0);
DEFINe v_modulo_cod	     CHAR(3);
DEFINE v_subcuenta           SMALLINT;
DEFINE v_monto_aivs          DECIMAL(22,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_proceso_cod_RP      SMALLINT;

DEFINE v_f_liquida           DATE;
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_id_ref_seq          DECIMAL(9,0); --Secuencia de interfaz

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;     --Estatus del proceso
DEFINE v_monto_acciones      DECIMAL(12,2);
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);
DEFINE v_movimiento          SMALLINT;
--pesos al dia
ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

LET v_tpo_transferencia = 19;    --Asigna crédito 43Bis
LET v_origen_devolucion = 02;    --Asigna valor SAR97
LET v_aivs_aportacion   = 0.00;
LET v_nss_separacion    = "";
LET v_estado            = 10;    --Asigna estado reenviada
LET v_folio             = 0;
LET v_subcuenta         = 0; 
LET v_monto_aivs        = 0.00;
LET v_monto_pesos       = 0.00;
LET v_proceso_cod_RP    = 1401;
LET v_bnd_transaccion   = 0;
LET v_bnd_proceso       = 0;     --Estado correcto
LET v_f_liquida         = TODAY;
LET v_f_valor           = TODAY;
LET v_f_registro        = TODAY;
{LET v_f_liquida         = '11162012';
LET v_f_valor           = '11162012';
LET v_f_registro        = '11162012';}
LET v_fondo_inversion   = 11;
LET v_id_ref_seq        = 0;
LET v_monto_acciones    = 0.00;
LET v_h_registro        = CURRENT HOUR TO SECOND;
LET v_origen            = "DIS DEV-"||p_periodo_pago;
LET v_movimiento        = 602;
LET v_modulo_cod        = "dis";

  --IF v_monto_aivs > 0 THEN
     --LET v_monto_pesos = (v_monto_aivs * p_precio_fec_hoy);
  --END IF

  IF p_proceso_cod = 101 OR
     p_proceso_cod = 102 OR 
     p_proceso_cod = 103 OR 
     p_proceso_cod = 107 OR 
     p_proceso_cod = 110 THEN
     LET v_movimiento = 912;
  END IF

  --Se agrega validación para trabajador = 3 --
  IF p_localiza_trabajador = 3 THEN
     IF p_tpo_patron = '99' THEN
        LET v_subcuenta = 44;
     ELSE 
        LET v_subcuenta = 4;
     END IF
  ELSE
     LET v_subcuenta = 4;
  END IF

  --Se calcula el monto_aivs
  IF p_imp_ap_pat > 0 THEN
     --LET v_monto_aivs      = (p_imp_ap_pat / p_precio_fec_pag);
     --LET v_aivs_aportacion = (p_imp_ap_pat / p_precio_fec_pag);

     --LET v_monto_acciones  = (p_imp_ap_pat / p_precio_fec_hoy )* -1;
     --LET v_monto_pesos     = p_imp_ap_pat * -1;
     LET v_monto_pesos 	   = p_monto_pesos;
     LET v_monto_acciones  = p_aiv_ap_pat;
	 
     --LET v_monto_pesos     = (p_aiv_ap_pat * p_precio_fec_hoy);

     LET v_id_ref_seq      = seq_dse_devolucion.NEXTVAL;

     INSERT INTO dse_devolucion
     VALUES (v_id_ref_seq, 
             v_folio,
	     v_modulo_cod,
             p_id_derechohabiente, 
             p_num_crd_ifv, 
             v_tpo_transferencia,
             v_origen_devolucion, 
             p_fecha_pago_val, 
             p_fecha_01mm,
             p_periodo_pago, 
             p_folio_referencia, 
             v_subcuenta, 
             v_monto_acciones, 
             v_monto_pesos,
    	     v_monto_pesos, 
             --p_imp_ap_pat,  
             --p_imp_ap_pat, 
             v_monto_acciones, 
             v_nss_separacion, 
             v_estado);

     INSERT INTO dis_preliquida
     VALUES(v_f_liquida, 
            p_id_derechohabiente, 
            v_subcuenta,
            v_fondo_inversion, 
            v_movimiento, 
            p_folio_referencia,
            v_id_ref_seq, 
            v_monto_acciones * -1, 
            v_monto_pesos * -1,
            v_f_valor, 
            v_f_registro, 
            v_h_registro, 
            v_origen);
  END IF  
       
  LET v_char = "Terminada transacción 1 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;
	   
END PROCEDURE;


