






CREATE PROCEDURE "safreviv".sp_dis_transaccion9(p_id_derechohabiente DECIMAL(9,0), 
                                     p_imp_ap_pat     DECIMAL(12,2),
                                     p_imp_am_cre     DECIMAL(12,2),
                                     p_folio_disp     DECIMAL(10,0),  
                                     p_id_referencia  DECIMAL(9,0), 
                                     p_precio_fec_hoy DECIMAL(19,14),
                                     p_nrp            CHAR(11),
                                     p_periodo_pago   CHAR(6),
                                     p_folio_sua      DECIMAL(6),
                                     p_num_crd_ifv    CHAR(10),
                                     p_folio_reg_pag  DECIMAL(10,0),
                                     p_proceso_cod    SMALLINT,
                                     p_tpo_patron     CHAR(2),        --Tipo Patron, viene de NRP[1,2]
                                     p_fecha_pago_val DATE)
									 
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
DEFINE v_origen          CHAR(20);
--DEFINE v_nrp1            CHAR(02);
DEFINE v_bnd_52          SMALLINT;
DEFINE v_proceso_cod_RP  SMALLINT;

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
LET v_fondo_inversion = 0;
LET v_f_valor         = TODAY;
LET v_f_registro      = TODAY;
LET v_h_registro      = CURRENT HOUR TO SECOND;
LET v_origen          = "Dispersión AP-HS";      
LET v_movimiento        = 0;
--LET v_nrp1              = "";
LET v_subcuenta         = 0;
LET v_monto_acciones    = 0.00;
LET v_monto_pesos       = 0.00;
LET v_bnd_52            = 0;
LET v_proceso_cod_RP    = 1401;
LET v_bnd_transaccion = 0;
LET v_bnd_proceso     = 0; --Estado correcto
         

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/Transaccion9.TRACE';
   --TRACE 'Derechohabiente '||p_id_derechohabiente;
   --TRACE 'Aportaciones    '||p_imp_ap_pat;
   --TRACE 'Amortizaciones  '||p_imp_am_cre;
   --TRACE 'Folo dispersion '||p_folio_disp;
   --TRACE 'Referencia      '||p_id_referencia;
   --TRACE 'Precio del dia  '||p_precio_fec_hoy;
   --TRACE 'NRP             '||p_nrp;
   --TRACE 'Periodo de Pago '||p_periodo_pago;
   --TRACE 'Folio Sua       '||p_folio_sua;
   --TRACE 'Credito Infona  '||p_num_crd_ifv;
   --TRACE 'Folio de Pago   '||p_folio_reg_pag;
   --TRACE 'ProcesoCodigoRP '||p_proceso_cod;

   --Se obtienen las primeras dos posiciones del NRP      
   --LET v_nrp1 = p_nrp[1,2];

   --Si amortizacion > 0 asigna a SUBCUENTA
   --TRACE 'Si amortizacion > 0';
   IF p_imp_am_cre > 0 THEN

      --Identificar precio de Acción del día
   SELECT precio_fondo INTO p_precio_fec_hoy
   FROM safre_viv:glo_valor_fondo
   WHERE fondo = 10
     AND f_valuacion = TODAY;
     
      LET v_movimiento = 52;
      LET v_fondo_inversion = 10;

      LET v_monto_acciones = (p_imp_am_cre / p_precio_fec_hoy ) * -1;
      LET v_monto_pesos = (v_monto_acciones * p_precio_fec_hoy )* -1;
      
      --Si el proceso_cod de pagos es (Registro de Pagos LQINFO) (1)
      IF p_proceso_cod = v_proceso_cod_RP THEN 
         IF p_tpo_patron = '99' THEN
            LET v_subcuenta = 43;
         ELSE 
            LET v_subcuenta = 41;
         END IF 
      ELSE 
      --Si el proceso_cod de pagos es (Registro de Pagos Sólo Infonavit) (3)
         LET v_subcuenta = 43;
      END IF   
   END IF
  
   IF v_movimiento = 52 THEN 
      --TRACE 'Inserta en dis_interface_hs';
      --TRACE 'Folio Dispersión '||p_folio_disp;
      --TRACE 'NRP              '||p_nrp;
      --TRACE 'Periodo Pago     '||p_periodo_pago;
      --TRACE 'Derechohabiente  '||p_id_derechohabiente;
      --TRACE 'Folio SUA        '||p_folio_sua;
      --TRACE 'Credito Infonav  '||p_num_crd_ifv;
      --TRACE 'Fondo Inversión  '||0;
      --TRACE 'Amortización     '||p_imp_am_cre;
      INSERT INTO safre_viv:dis_interface_hs
      VALUES(seq_dis_interface_hs.CURRVAL,
             p_id_derechohabiente,
             p_folio_sua,
             p_periodo_pago,
             p_fecha_pago_val,
             p_nrp,
             p_folio_disp,
             p_num_crd_ifv,
             p_imp_ap_pat,
             p_imp_am_cre);
   END IF 


   LET v_char = "Terminada transacción 9 correctamente";
   RETURN v_bnd_proceso , 0 , v_char;
   --TRACE 'Termina SPL 5';

END PROCEDURE;


