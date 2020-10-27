






CREATE PROCEDURE "safreviv".sp_dis_transaccion3(p_id_derechohabiente  DECIMAL(9,0), 
                                     p_imp_ap_pat          DECIMAL(12,2),
                                     p_monto_pesos         DECIMAL(22,2), --Importe de las aortaciones por el precio del día
                                     p_imp_am_cre          DECIMAL(12,2),
                                     p_folio_disp          DECIMAL(10,0), 
                                     p_id_referencia       DECIMAL(9,0), 
                                     p_precio_fec_hoy      DECIMAL(19,14),
                                     p_nrp                 CHAR(11),
                                     p_periodo_pago        CHAR(6),
                                     p_folio_sua           DECIMAL(6),
                                     p_num_crd_ifv         CHAR(10),
                                     p_proceso_cod         SMALLINT,
                                     p_tpo_patron          CHAR(2),       --Tipo Patron, viene de NRP[1,2]
                                     p_fecha_pago_val      DATE,	      --Fecha de pago
                                     p_tipo_trabajador     SMALLINT,
                                     p_localiza_trabajador CHAR(1),
                                     p_aiv_ap_pat          DECIMAL(18,6), --Valor de AIVS
                                     p_tpo_credito         SMALLINT) 
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
DEFINE v_subcuenta_1         SMALLINT;
DEFINE v_movimiento_1        SMALLINT;
DEFINE v_monto_acciones1     DECIMAL(12,2);
DEFINE v_monto_pesos_1       DECIMAL(22,2);
DEFINE v_origen              CHAR(20);
DEFINE v_proceso_cod_RP      SMALLINT;
DEFINE v_id_ref_seq          DECIMAL (9,0);  --Secuencia de interfaz
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_tipo_hs             SMALLINT;       --Define el tipo de HS

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
  LET v_tipo_hs         = 0;

  --Obtiene referencia para conculiar de la secuencia de interfaz
  LET v_id_ref_seq      = seq_dis_interface_hs.NEXTVAL;
   
  --Si importe de aportacion > 0 asigna a SUBCUENTA
  IF p_imp_ap_pat > 0 THEN
     LET v_movimiento     = 42;

     --Crédito Mejoravit+
     IF p_tpo_credito = 23 THEN
        LET v_movimiento = 1752;
     END IF
	 
     --LET v_monto_acciones = (p_imp_ap_pat / p_precio_fec_hoy) * -1;    
     LET v_monto_pesos    = p_monto_pesos * -1;
     LET v_monto_acciones = p_aiv_ap_pat  * -1;
     --LET v_monto_pesos    = (p_aiv_ap_pat * p_precio_fec_hoy);

     IF p_proceso_cod = 101 OR
        p_proceso_cod = 102 OR 
        p_proceso_cod = 103 OR 
        p_proceso_cod = 107 OR 
        p_proceso_cod = 110 THEN
        LET v_movimiento = 872;

        --Crédito Mejoravit+
        IF p_tpo_credito = 23 THEN
           LET v_movimiento = 1772;
        END IF
     END IF
     	
     IF p_localiza_trabajador = 3 THEN
        IF p_tpo_patron = '99' THEN
           LET v_subcuenta = 44;
        ELSE 
           LET v_subcuenta = 4;
        END IF; 
     ELSE
        LET v_subcuenta = 4;
     END IF 
      
     --Verifica bimestre pago
     IF p_periodo_pago <= '200505' THEN
        LET v_movimiento = 632;

        IF p_proceso_cod = 101 OR
           p_proceso_cod = 102 OR 
           p_proceso_cod = 103 OR 
           p_proceso_cod = 107 OR 
           p_proceso_cod = 110 THEN
           LET v_movimiento = 942;
        END IF

        IF p_tipo_trabajador = 0 THEN
           LET v_movimiento = 642; --

           IF p_proceso_cod = 101 OR
              p_proceso_cod = 102 OR 
              p_proceso_cod = 103 OR 
              p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              LET v_movimiento = 952;
           END IF
        END IF
     END IF

     -- Noviembre 2014
     -- PRODINF-434 (PROINFXVII-29)
     -- NRP's 99
     -- Proceso: 1403 - REG PAGOS SOLO INFONAVIT (PAU) 
     IF p_proceso_cod = 1403  THEN
        LET v_subcuenta  = 44;
        LET v_movimiento = 1492;
     END IF
     --------------------------------------------------
		 
     INSERT INTO dis_preliquida VALUES(v_f_liquida, 
                                       p_id_derechohabiente, 
                                       v_subcuenta, 
                                       v_fondo_inversion, 
                                       v_movimiento, 
                                       p_folio_disp, 
                                       v_id_ref_seq, 
                                       v_monto_acciones, 
                                       v_monto_pesos,
                                       v_f_valor, 
                                       v_f_registro, 
                                       v_h_registro, 
                                       v_origen);     
  END IF 
  
  --Si amortizacion > 0 asigna a SUBCUENTA
  IF p_imp_am_cre > 0 THEN
     --Identificar precio de Acción del día
     {SELECT precio_fondo 
     INTO   p_precio_fec_hoy
     FROM   glo_valor_fondo
     WHERE  fondo       = 10
     AND    f_valuacion = TODAY;}
     --AND    f_valuacion = '11162012';
     
     LET p_precio_fec_hoy  = 1;
     LET v_movimiento_1    = 52;
     LET v_fondo_inversion = 10;

     --Crédito Mejoravit+
     IF p_tpo_credito = 23 THEN
        LET v_movimiento_1 = 1762;
     END IF
	  
     --Divide el importe de la amortizacion entre el precio del dia 
     LET v_monto_pesos_1   = (p_imp_am_cre * p_precio_fec_hoy ) * -1;
     LET v_monto_acciones1 = v_monto_pesos_1;

     IF p_proceso_cod = 101 OR
        p_proceso_cod = 102 OR 
        p_proceso_cod = 103 OR 
        p_proceso_cod = 107 OR
        p_proceso_cod = 110 THEN 
        LET v_movimiento_1 = 882;

        --Crédito Mejoravit+
        IF p_tpo_credito = 23 THEN
           LET v_movimiento_1 = 1782;
        END IF
     END IF
	  
     IF p_localiza_trabajador = 3 THEN
        IF p_tpo_patron = '99' THEN
           LET v_subcuenta_1 = 43;
        ELSE 
           LET v_subcuenta_1 = 41;
        END IF 
     ELSE
        LET v_subcuenta_1 = 41;
     END IF

     --Verifica bimestre pago
     IF p_periodo_pago <= '200505' THEN
        LET v_movimiento_1 = 562;

        IF p_proceso_cod = 101 OR
           p_proceso_cod = 102 OR 
           p_proceso_cod = 103 OR 
           p_proceso_cod = 107 OR 
           p_proceso_cod = 110 THEN
           LET v_movimiento_1 = 902;
        END IF

        IF p_tipo_trabajador = 0 THEN
           LET v_movimiento_1 = 652;
           
           IF p_proceso_cod = 101 OR
              p_proceso_cod = 102 OR 
              p_proceso_cod = 103 OR 
              p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              LET v_movimiento_1 = 962;
           END IF
        END IF;
     END IF;

     INSERT INTO dis_preliquida VALUES(v_f_liquida, 
                                       p_id_derechohabiente, 
                                       v_subcuenta_1, 
                                       v_fondo_inversion, 
                                       v_movimiento_1, 
                                       p_folio_disp, 
                                       v_id_ref_seq, 
                                       v_monto_acciones1, 
                                       v_monto_pesos_1,
                                       v_f_valor, 
                                       v_f_registro, 
                                       v_h_registro, 
                                       v_origen);                     
  END IF

  INSERT INTO dis_pre_interface_hs VALUES(v_id_ref_seq,
                                          p_id_derechohabiente,
                                          p_folio_sua,
                                          p_periodo_pago,
                                          p_fecha_pago_val,
                                          p_nrp,
                                          p_folio_disp,
                                          p_num_crd_ifv,
                                          --p_imp_ap_pat,
                                          p_monto_pesos,
                                          p_imp_am_cre,
                                          p_aiv_ap_pat,
                                          v_tipo_hs);

  LET v_char = "Terminada transacción 3 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


