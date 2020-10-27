






CREATE PROCEDURE "safreviv".sp_dis_transaccion13(p_id_derechohabiente  DECIMAL(9,0), 
                                      p_imp_ap_pat          DECIMAL(12,2),
                                      p_imp_am_cre          DECIMAL(12,2),
                                      p_folio_disp          DECIMAL(10,0),  
                                      p_id_referencia       DECIMAL(9,0), 
                                      p_precio_fec_hoy      DECIMAL(19,14),
                                      p_nrp                 CHAR(11),
                                      p_periodo_pago        CHAR(6),
                                      p_folio_sua           DECIMAL(6),
                                      p_num_crd_ifv         CHAR(10),
                                      p_folio_reg_pag       DECIMAL(10,0),
                                      p_proceso_cod         SMALLINT,
                                      p_tpo_patron          CHAR(2),       --Tipo Patron, viene de NRP[1,2]
                                      p_fecha_pago_val      DATE,
                                      p_tipo_trabajador     SMALLINT,
                                      p_localiza_trabajador CHAR(1),
                                      p_aiv_ap_pat          DECIMAL(18,6)) --Valor de AIVS
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 25042019
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
DEFINE v_origen              CHAR(20);
DEFINE v_bnd_52              DECIMAL (10,0);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_tipo_hs			 SMALLINT;       --Identifica el tipo de HS

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

LET v_f_liquida       = TODAY;
--LET v_f_liquida       = '11162012';
LET v_fondo_inversion = 0;
LET v_f_valor         = TODAY;
LET v_f_registro      = TODAY;
LET v_h_registro      = CURRENT HOUR TO SECOND;
LET v_origen          = "Dis Cartera-"||p_periodo_pago;      
LET v_movimiento      = 1322;
LET v_subcuenta       = 0;
LET v_monto_acciones  = 0.00;
LET v_monto_pesos     = 0.00;
LET v_bnd_52          = 0;
LET v_bnd_transaccion = 0;
LET v_bnd_proceso     = 0; --Estado correcto
LET v_tipo_hs		  = 1;
         
  IF p_imp_am_cre > 0 THEN
     LET p_precio_fec_hoy  = 1;     
     LET v_fondo_inversion = 10; 

     LET v_monto_acciones  = (p_imp_am_cre / p_precio_fec_hoy ) * -1;
     LET v_monto_pesos     = v_monto_acciones;

     --## Verificar 
     IF p_localiza_trabajador = 3 THEN
        IF p_tpo_patron = '99' THEN
           LET v_subcuenta = 43;
        ELSE 
           LET v_subcuenta = 41;
        END IF 
     ELSE
        LET v_subcuenta = 41;
     END IF 

     INSERT INTO dis_preliquida VALUES(v_f_liquida, 
                                       p_id_derechohabiente, 
                                       v_subcuenta, 
                                       v_fondo_inversion, 
                                       v_movimiento, 
                                       p_folio_disp, 
                                       seq_dis_interface_hs.NEXTVAL, 
                                       v_monto_acciones, 
                                       v_monto_pesos,
                                       v_f_valor, 
                                       v_f_registro, 
                                       v_h_registro, 
                                       v_origen);   

     INSERT INTO dis_pre_interface_hs VALUES(seq_dis_interface_hs.CURRVAL,
                                             p_id_derechohabiente,
                                             p_folio_sua,
                                             p_periodo_pago,
                                             p_fecha_pago_val,
                                             p_nrp,
                                             p_folio_disp,
                                             p_num_crd_ifv,
                                             p_imp_ap_pat,
                                             p_imp_am_cre,
                                             p_aiv_ap_pat,
                                             v_tipo_hs);
  END IF

  LET v_char = "Terminada transacción 13 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


