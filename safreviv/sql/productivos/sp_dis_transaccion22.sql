






CREATE PROCEDURE "safreviv".sp_dis_transaccion22(p_id_derechohabiente  DECIMAL(9,0), 
                                      p_num_crd_ifv         DECIMAL(10,0), 
                                      p_periodo_pago        CHAR(6),
                                      p_fecha_pago_val      DATE,
                                      p_imp_ap_pat          DECIMAL(12,2),
                                      p_monto_pesos         DECIMAL(22,2), --Importe de las aportaciones por el precio del día
                                      p_precio_fec_pag      DECIMAL(19,14),
                                      p_precio_fec_hoy      DECIMAL(19,14),
                                      p_fecha_01mm          DATE, 
                                      p_proceso_cod         SMALLINT,      --Proceso operacion
                                      p_folio_referencia    DECIMAL(9,0),  --Viene Folio Dispersion
                                      p_tpo_patron          CHAR(2),       --Tipo Patron, viene de NRP[1,2]
                                      p_localiza_trabajador CHAR(1),
                                      p_aiv_ap_pat          DECIMAL(18,6), --Valor de AIVS 
                                      p_tipo_credito        SMALLINT)
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 06062017
--Declaración de variables
DEFINE v_tpo_transferencia   CHAR(2);      
DEFINE v_origen_devolucion   CHAR(2);      
DEFINE v_aivs_aportacion     DECIMAL(12,2);
DEFINE v_nss_separacion      CHAR(11);     
DEFINE v_estado              SMALLINT; 
DEFINE v_folio               DECIMAL(9,0); 
DEFINE v_modulo_cod          CHAR(3);   
DEFINE v_subcuenta           SMALLINT;
DEFINE v_monto_aivs          DECIMAL(22,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_proceso_cod_RP      SMALLINT;

DEFINE v_f_liquida           DATE;
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_id_ref_seq          DECIMAL(9,0);  --Secuencia de interfaz

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER;
DEFINE isam_err              INTEGER;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;      --Estatus del proceso
DEFINE v_monto_acciones      DECIMAL(12,2);
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);
DEFINE v_movimiento          SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;  
END EXCEPTION

LET v_aivs_aportacion   = 0.00;
LET v_nss_separacion    = "";
LET v_folio             = 0;
LET v_subcuenta         = 0; 
LET v_monto_aivs        = 0.00;
LET v_monto_pesos       = 0.00;
LET v_bnd_transaccion   = 0;
LET v_bnd_proceso       = 0; --Estado correcto
LET v_f_liquida         = TODAY;
LET v_f_valor           = TODAY;
LET v_f_registro        = TODAY;
LET v_fondo_inversion   = 11;  
LET v_id_ref_seq        = 0;
LET v_monto_acciones    = 0.00;
LET v_h_registro        = CURRENT HOUR TO SECOND;
			
LET v_tpo_transferencia = 15; --Transferencia de Acreditados
LET v_origen_devolucion = 03; --Aportaciones 5%
LET v_estado            = 10; --Asigna estado reenviada
LET v_proceso_cod_RP    = 1401;
LET v_modulo_cod        = "dis";
LET v_origen            = "Dis DEV-"||p_periodo_pago;
LET v_movimiento        = 1812; --Cargo devolución créditos mejoravit
		
  --###################################--
  -- ACLARATORIO --
  IF p_proceso_cod = 101 OR
     p_proceso_cod = 102 OR 
     p_proceso_cod = 103 OR 
     p_proceso_cod = 107 OR 
     p_proceso_cod = 110 THEN
     LET v_movimiento = 1822;
  END IF
   
  IF p_localiza_trabajador = 3 THEN
     IF p_tpo_patron = '99' THEN
        LET v_subcuenta = 44;
     ELSE 
        LET v_subcuenta = 4;
     END IF
  ELSE 
     LET v_subcuenta = 4;
  END IF 

  --Si aportacion es mayor a precio en fecha de pago
  IF p_imp_ap_pat > 0 THEN
     LET v_monto_pesos     = p_monto_pesos;
     LET v_monto_acciones  = p_aiv_ap_pat;
		
     --Obtiene referencia para conciliar de la secuencia de interfaz
     LET v_id_ref_seq = seq_dse_devolucion.NEXTVAL;
	  
     INSERT INTO dse_devolucion VALUES (v_id_ref_seq, 
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
                                        v_monto_acciones, 
                                        v_nss_separacion, 
                                        v_estado);
				 
     INSERT INTO dis_preliquida VALUES (v_f_liquida, 
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

  LET v_char = "Terminada transacción 22 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;
  
END PROCEDURE;


