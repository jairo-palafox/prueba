






CREATE PROCEDURE "safreviv".sp_dis_transaccion20(p_id_derechohabiente  DECIMAL(9,0),   --ID Derechohabiente
                                      p_imp_ap_pat          DECIMAL(12,2),  --Importe de aportación patronal
                                      p_monto_pesos         DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                      p_imp_am_cre          DECIMAL(12,2),  --Importe de amortización del crédito                                     
                                      p_folio_disp          DECIMAL(10,0),  --Folio de Dispersión
                                      p_id_referencia       DECIMAL(9,0),   --ID referencia para enlazar folio de dispersión con folio de pagos
                                      p_precio_fec_hoy      DECIMAL(19,14), --Fecio de fecha de pago del día 
                                      p_folio_sua           DECIMAL(9,0),   --Folio SUA
                                      p_periodo_pago        CHAR(6),        --Periodo de Pago
                                      p_proceso_cod         SMALLINT,       --Proceso operacion
                                      p_tpo_patron          CHAR(2),        --Tipo Patron, viene de NRP[1,2]
                                      p_fecha_pago_val      DATE,           --Fecha de pago
                                      p_nrp                 CHAR(11),       --Numero NRP
                                      p_num_cred_inf        DECIMAL(10,0),
                                      p_localiza_trabajador CHAR(1),
                                      p_aiv_ap_pat          DECIMAL(18,6),  --Valor de AIVS 
                                      p_tpo_credito         SMALLINT)

RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 25042019
--Declaración de variables
DEFINE v_f_liquida           DATE;
DEFINE v_subcuenta           SMALLINT;
DEFINE v_subcuenta1          SMALLINT;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_movimiento1         SMALLINT;
DEFINE v_monto_acciones      DECIMAL(12,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);
DEFINE v_proceso_cod_RP      SMALLINT;
DEFINE v_ind_liquidacion     SMALLINT;      --Indice de liquidación con valor cero por omisión

DEFINE v_bnd_proceso         SMALLINT;      --Estatus del proceso
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER;
DEFINE isam_err              INTEGER;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_id_ref_seq          DECIMAL(9,0);

DEFINE v_tpo_credito         SMALLINT; 
DEFINE v_cve_ent_financiera  SMALLINT; 
DEFINE v_num_ctr_int_ef      CHAR(18); 
DEFINE v_concepto            SMALLINT; 
DEFINE v_id_ctr_transaccion  DECIMAL(9,0); 
DEFINE v_estado	             SMALLINT;
DEFINE v_folio_transaccion   DECIMAL(9,0);
DEFINE v_f_transaccion       DATE;
DEFINE v_folio_factura       DECIMAL(9,0);
DEFINE v_f_factura           DATE;

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
LET v_origen          = "Dis EF-"||p_periodo_pago;
LET v_subcuenta       = 0;
LET v_movimiento      = 0;
LET v_monto_acciones  = 0.00;
LET v_monto_pesos     = 0.00;
LET v_movimiento1     = 0;
LET v_subcuenta1      = 0;
LET v_proceso_cod_RP  = 1401;
LET v_ind_liquidacion = 1;
LET v_bnd_transaccion = 0;
LET v_bnd_proceso     = 0; --Estado correcto
LET v_id_ref_seq      = 0;

  --Si la aportación es mayor a 0
  IF p_imp_ap_pat > 0 THEN
     --LET v_id_ref_seq     = seq_dis_interface_hs.NEXTVAL;
     LET v_movimiento     = 72;

     LET v_monto_pesos    = p_monto_pesos * -1;
     LET v_monto_acciones = p_aiv_ap_pat * -1;

     --Se agrega validación para NRP 99 --
     --IF p_nrp[1,2] = '99' THEN
     --   LET v_subcuenta = 44;
     --ELSE 
        LET v_subcuenta = 4;
     --END IF

     INSERT INTO dis_ap_preliquida VALUES(v_f_liquida, 
                                          p_id_derechohabiente, 
                                          v_subcuenta, 
                                          v_fondo_inversion, 
                                          v_movimiento, 
                                          p_folio_disp, 
                                          seq_dis_interface_ef.NEXTVAL, 
                                          v_monto_acciones, 
                                          v_monto_pesos,
                                          v_f_valor, 
                                          v_f_registro, 
                                          v_h_registro, 
                                          v_origen);

     INSERT INTO dis_interface_ef VALUES(seq_dis_interface_ef.CURRVAL,
                                         p_id_derechohabiente,
                                         p_folio_sua,
                                         p_periodo_pago,
                                         p_fecha_pago_val,
                                         p_nrp,
                                         v_ind_liquidacion,
                                         p_folio_disp,
                                         p_num_cred_inf,
                                         p_monto_pesos,
                                         p_aiv_ap_pat);
                                         --p_imp_ap_pat);

     --Migración Aportaciones Subsecuentes a SACI (Créditos 43 BIS)
     --LET v_tpo_credito        = p_tpo_credito;
     LET v_tpo_credito        = 2;
     LET v_cve_ent_financiera = 0;
     LET v_num_ctr_int_ef     = ""; 
     LET v_concepto           = 0;
     LET v_id_ctr_transaccion = 0.00; 
     LET v_estado             = 10;  --Registrado
     LET v_f_liquida          = TODAY;
     LET v_folio_transaccion  = 0.00;
     LET v_f_transaccion      = "";
     LET v_folio_factura      = 0.00;
     LET v_f_factura          = "";

     INSERT INTO dis_ctr_aps_tns VALUES(seq_dis_interface_ef.CURRVAL,
                                        p_id_derechohabiente,
                                        p_folio_sua,
                                        p_periodo_pago,
                                        p_fecha_pago_val,
                                        p_nrp,
                                        v_ind_liquidacion,
                                        p_folio_disp,
                                        v_f_liquida,
                                        p_num_cred_inf,
                                        p_monto_pesos,
                                        p_aiv_ap_pat,
                                        v_tpo_credito,
                                        v_cve_ent_financiera, 
                                        v_num_ctr_int_ef, 
                                        v_concepto,
                                        v_id_ctr_transaccion, 
                                        v_folio_transaccion,
                                        v_f_transaccion,
                                        v_folio_factura,
                                        v_f_factura,
                                        v_estado);
  END IF

  --###########POR REGLA DE NEGOCIO A LOS 43BIS NO SE LES INSERTA AMORTIZACION

  LET v_char = "Terminada transacción 20 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


