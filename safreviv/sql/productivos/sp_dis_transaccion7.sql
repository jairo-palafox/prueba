






CREATE PROCEDURE "safreviv".sp_dis_transaccion7(p_folio_disp          DECIMAL(10,0),
                                     p_folio_reg_pag       DECIMAL(10,0),
                                     p_id_referencia       DECIMAL(9,0),
                                     p_derechohabiente     DECIMAL(9,0),
                                     p_num_credito         CHAR(10),
                                     p_periodo_pago        CHAR(6),
                                     p_f_pago              DATE,
                                     p_nrp                 CHAR(11),
                                     p_id_avance_pago      DECIMAL(9,0),
                                     p_monto_apo_ava       DECIMAL(12,2),
                                     p_monto_amo_ava       DECIMAL(12,2),
                                     p_monto_apo_pag       DECIMAL(12,2),
				     p_monto_pesos	   DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                     p_monto_amo_pag       DECIMAL(12,2),
                                     p_monto_apo_dif       DECIMAL(12,2),
                                     p_monto_amo_dif       DECIMAL(12,2),
                                     p_edo_compensa_apo    SMALLINT,
                                     p_edo_compensa_amo    SMALLINT,
			             p_proceso_cod	   SMALLINT,
			             p_precio_fec_hoy	   DECIMAL(19,14),
			             p_tpo_patron	   CHAR(2),
			             p_localiza_trabajador CHAR(1),
			             p_aiv_ap_pat 	   DECIMAL(18,6)) --Valor de AIVS 
									 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 21062013
--Declaración de variables
DEFINE v_status          SMALLINT;
DEFINE sql_err           INTEGER ;
DEFINE isam_err          INTEGER ;
DEFINE error_info        CHAR(70);
DEFINE v_char            CHAR(20);
DEFINE v_bnd_transaccion SMALLINT;
DEFINE v_bnd_proceso     SMALLINT;       --Estatus del proceso
DEFINE v_proceso_cod_RP  SMALLINT;
DEFINE v_monto_acciones  DECIMAL(12,2);
DEFINE v_monto_pesos     DECIMAL(22,2);
DEFINE v_fondo_inversion SMALLINT;
DEFINE v_subcuenta       SMALLINT;
DEFINE v_movimiento      SMALLINT;
DEFINE v_f_valor         DATE;
DEFINE v_f_registro      DATE;
DEFINE v_h_registro      DATETIME HOUR TO SECOND;
DEFINE v_f_liquida       DATE;
DEFINE v_origen          CHAR(20);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info;
END EXCEPTION

LET v_bnd_transaccion = 0;
LET v_bnd_proceso     = 0; --Estado correcto
LET v_f_liquida       = TODAY;
--LET v_f_liquida       = '11162012';

LET v_origen          = "Dis AVA-"||p_periodo_pago;
LET v_proceso_cod_RP  = 1401;
LET v_monto_acciones  = 0.00;
LET v_monto_pesos     = 0.00;
LET v_fondo_inversion = 11;
LET v_subcuenta       = 0;
LET v_movimiento      = 0;
LET v_f_valor         = TODAY;
LET v_f_registro      = TODAY;
{LET v_f_valor         = '11162012';
LET v_f_registro      = '11162012';}
LET v_h_registro      = CURRENT HOUR TO SECOND;

  --Si aportacion es mayor a precio en fecha de pago
  IF p_monto_apo_pag > 0 THEN

     --LET v_monto_acciones  = (p_monto_apo_pag / p_precio_fec_hoy) * - 1;
     LET v_monto_pesos     = p_monto_pesos * -1;
     LET v_monto_acciones  = p_aiv_ap_pat * -1;
     --LET v_monto_pesos     = (p_aiv_ap_pat * p_precio_fec_hoy);

     LET v_fondo_inversion = 11;

     IF p_localiza_trabajador = 3 THEN
	IF p_tpo_patron = '99' THEN
	   LET v_subcuenta = 44;
	ELSE
	   LET v_subcuenta = 4;
	END IF
     ELSE
        LET v_subcuenta = 4;
     END IF 

     {--Verifica bimestre pago
     IF p_periodo_pago <= '200505' THEN
        -- No debe existir estos tipos de movimientos
         LET v_movimiento = 632; --
     END IF}

     --Verifica bimestre pago
     --IF p_periodo_pago > '200505' THEN
        --Pago igual al avance
        IF p_edo_compensa_apo = 0 THEN
           LET v_movimiento   = 612; --

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              LET v_movimiento = 922;
           END IF
        END IF

        --Pago menor al avance
        IF p_edo_compensa_apo  = 1 THEN
           IF p_monto_apo_dif  > 2 OR p_monto_apo_dif < (-2) THEN
	      LET v_movimiento = 702; --
           ELSE
              LET v_movimiento = 1072;
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_apo_dif  > 2 OR p_monto_apo_dif < (-2) THEN
                 LET v_movimiento = 972;
              ELSE
                 LET v_movimiento = 1132;
              END IF
           END IF
        END IF

        --Pago mayot al avance
        IF p_edo_compensa_apo = 2 THEN
           IF p_monto_apo_dif > 2 OR p_monto_apo_dif < (-2) THEN --< (-2)
	      LET v_movimiento   = 722; --
           ELSE
              LET v_movimiento   = 1092;
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_apo_dif > 2 OR p_monto_apo_dif < (-2) THEN
                 LET v_movimiento = 992;
              ELSE
                 LET v_movimiento = 1152;
              END IF
           END IF
        END IF
     --END IF

     INSERT INTO dis_preliquida
          VALUES (v_f_liquida, 
                  p_derechohabiente, 
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
  END IF

  --Si amortización es mayor a 0
  IF p_monto_amo_pag > 0 THEN

     --Identificar precio de Acción del día
     {SELECT precio_fondo
     INTO   p_precio_fec_hoy
     FROM   glo_valor_fondo
     WHERE  fondo       = 10
     AND    f_valuacion = TODAY;}
     --AND    f_valuacion = '11162012';

     LET p_precio_fec_hoy  = 1;
     LET v_fondo_inversion = 10;

     --Divide el importe de la amortizacion entre el precio del dia
     LET v_monto_pesos     = (p_monto_amo_pag * p_precio_fec_hoy ) * -1;
     LET v_monto_acciones  = v_monto_pesos;

     IF p_localiza_trabajador = 3 THEN
	IF p_tpo_patron = '99' THEN
	   LET v_subcuenta = 43;
	ELSE
	   LET v_subcuenta = 41;
	END IF
     ELSE
	LET v_subcuenta = 41;
     END IF 

     --Verifica bimestre pago
     IF p_periodo_pago <= '200505' THEN

        --Pago igual al avance
        IF p_edo_compensa_amo = 3 THEN
	   LET v_movimiento   = 742; --

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              LET v_movimiento = 1012;
           END IF
        END IF

        --Pago menor al avance
        IF p_edo_compensa_amo = 4 THEN
           IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
	      LET v_movimiento   = 762; --
           ELSE
              LET v_movimiento   = 1112;
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
                 LET v_movimiento = 1022;
              ELSE
                 LET v_movimiento = 1172;
              END IF
           END IF
        END IF

        --Pago mayot al avance
        IF p_edo_compensa_amo = 5 THEN
           IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2)THEN
	      LET v_movimiento = 772; --
           ELSE
              LET v_movimiento = 1122; 
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
                 LET v_movimiento = 1032;
              ELSE
                 LET v_movimiento = 1182;
              END IF
           END IF
        END IF
     END IF

     --Verifica bimestre pago
     IF p_periodo_pago > '200505' THEN

        --Pago igual al avance
        IF p_edo_compensa_amo = 0 THEN
	   LET v_movimiento   = 622; --

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              LET v_movimiento = 932;
           END IF
        END IF

        --Pago menor al avance
        IF p_edo_compensa_amo = 1 THEN
           IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
	      LET v_movimiento = 712; --
           ELSE
              LET v_movimiento = 1082;
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
                 LET v_movimiento = 982;
              ELSE
                 LET v_movimiento = 1142;
              END IF
           END IF
        END IF

        --Pago mayot al avance
        IF p_edo_compensa_amo = 2 THEN
           IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
	      LET v_movimiento = 732; --
           ELSE
              LET v_movimiento = 1102; 
           END IF

           IF p_proceso_cod = 101 OR
	      p_proceso_cod = 102 OR 
	      p_proceso_cod = 103 OR 
	      p_proceso_cod = 107 OR 
              p_proceso_cod = 110 THEN
              IF p_monto_amo_dif > 2 OR p_monto_amo_dif < (-2) THEN
                 LET v_movimiento = 1002;
              ELSE
                 LET v_movimiento = 1162;
              END IF
           END IF
        END IF
     END IF

     INSERT INTO dis_preliquida
          VALUES(v_f_liquida, 
                 p_derechohabiente, 
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
  END IF

  INSERT INTO dis_compensa_avance
       VALUES (p_folio_disp, --
               p_folio_reg_pag,
               p_id_referencia,
               p_derechohabiente, --
               p_num_credito,
               p_periodo_pago,
               p_f_pago,
               p_nrp,
               p_id_avance_pago,
               p_monto_apo_ava,
               p_monto_amo_ava,
               --p_monto_apo_pag,
	       p_monto_pesos,
               p_monto_amo_pag,
               p_edo_compensa_apo,
               p_edo_compensa_amo);

  --Avances de Pago Cubierto
  {UPDATE dis_det_avance_pago
     SET estado        = 50,
         monto_dif_apo = p_monto_apo_dif,
         monto_dif_amo = p_monto_amo_dif
  WHERE  id_dis_det_avance_pago = p_id_avance_pago;}

  UPDATE dis_det_avance_pago
     SET estado                 = 50,
         monto_dif_apo          = p_monto_apo_dif,
         monto_dif_amo          = p_monto_amo_dif
   WHERE id_dis_det_avance_pago = p_id_avance_pago 
     AND id_derechohabiente     = p_derechohabiente
     AND periodo_pago           = p_periodo_pago
     AND nrp                    = p_nrp
     AND tpo_avance             = 181
     AND estado                 = 30;

  LET v_char = "Terminada transacción 7 correctamente";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


