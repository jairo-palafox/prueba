






CREATE PROCEDURE "safreviv".fn_dis_preliq_can_par_ap(p_fecha_01mm           DATE,          --Fecha de movimiento, proceso
                                          p_usuario              CHAR(20),      --Usuario del proceso
                                          p_folio_disp           DECIMAL(9,0),  --Folio de dispersión
                                          p_pid                  DECIMAL(9,0),  --Pid genrado para el proceso
                                          p_proceso_cod          SMALLINT,      --Proceso dispersión preliquidación
                                          p_opera_cod            SMALLINT,      --Operacion preliquidacion
                                          p_folio_reg_pag        DECIMAL(9,0),  --Folio de registro de pagos
                                          p_proceso_cod_reg_pago SMALLINT)      --Codigo proceso de registro de pagos
--RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)
RETURNING SMALLINT;

--Última modificación 13112018
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(22,2);  --Importe aportaciones patronales -- Se cambia la variable ap_pat por v_monto_pesos (pesos convertidos)
DEFINE v_imp_ap_pat          DECIMAL(22,2);  --Importe aportaciones patronales -- Son los pesos no convertidos
DEFINE v_imp_am_cre          DECIMAL(22,2);  --Importe amortizaciones de credito
DEFINE v_status              INTEGER;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_id_derechohabiente  DECIMAL(9,0);  --Derechohabiente de cuenta credito
DEFINE v_nrp                 CHAR(11);      --Registro patronal
DEFINE v_periodo_pago        CHAR(6);       --Periodo de pago
DEFINE v_subcuenta           SMALLINT;      --Subcuenta para amortización y aportación
DEFINE v_fdo_inversion       SMALLINT;      --Fondo de inversión
DEFINE v_f_pago              DATE;          --Fecha de pago
DEFINE v_hora_proceso        DATETIME HOUR TO SECOND; -- Hora en que se realizo el proceso
DEFINE v_origen              VARCHAR(20);
DEFINE v_id_dis_det_ava_pag  DECIMAL(9,0);
DEFINE v_movimiento1         SMALLINT;      --Tipo movimiento 431 
DEFINE v_movimiento2         SMALLINT;      --Tipo movimiento 522
DEFINE v_hoy                 DATE;
DEFINE v_monto_cero          SMALLINT;
DEFINE v_imp_ap_pat_neg      DECIMAL(22,2);  --Importe aportaciones patronales -- Son los pesos no convertidos
DEFINE v_imp_am_cre_neg      DECIMAL(22,2);  --Importe amortizaciones de credito

{ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN v_status, isam_err, error_info, v_id_derechohabiente;
END EXCEPTION}

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_imp_ap_pat           = 0.00;
LET v_imp_am_cre           = 0.00;
LET v_bnd_transaccion      = 0;
LET v_id_derechohabiente   = 0;
LET v_nrp                  = "";
LET v_periodo_pago         = "";
LET v_subcuenta            = 0;
LET v_fdo_inversion        = 0;
LET v_f_pago               = TODAY;
LET v_hora_proceso         = CURRENT HOUR TO SECOND;
LET v_origen               = "";   --NRP y periodo de pago
LET v_id_dis_det_ava_pag   = 0;
LET v_movimiento1          = 431;
LET v_movimiento2          = 522;
LET v_hoy                  = TODAY;
LET v_monto_cero           = 0;
LET v_imp_ap_pat_neg       = 0;
LET v_imp_am_cre_neg       = 0;

--SET DEBUG FILE TO '/safreviv/dis/sql/fn_dis_preliq_can_par_ap.TRACE';
--SET DEBUG FILE TO '/home/safreviv/fn_dis_preliq_can_par_ap.TRACE';
--TRACE ON;

  CREATE TABLE dis_canc_pav_preliquida(f_liquida          DATE NOT NULL,
                                       id_derechohabiente DECIMAL(9,0) NOT NULL,
                                       subcuenta          SMALLINT NOT NULL,
                                       fondo_inversion    SMALLINT NOT NULL,
                                       movimiento         SMALLINT NOT NULL,
                                       folio_liquida      DECIMAL(9,0) NOT NULL,
                                       id_referencia      DECIMAL(9,0) NOT NULL,
                                       monto_acciones     DECIMAL(22,2),
                                       monto_pesos        DECIMAL(22,2),
                                       f_valor            DATE,
                                       f_registro         DATE,
                                       h_registro         DATETIME HOUR TO SECOND,
                                       origen             CHAR(20))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SET PDQPRIORITY HIGH;

  --Leer los registros aceptados de la tabla dis_canc_par_ava_pag
  --Identifica precio de fondo de Fecha de Pago patronal
  FOREACH
    SELECT cpa.id_derechohabiente,
           cpa.monto_aportacion,
           cpa.monto_amortizacion,
           cpa.nrp,
           cpa.periodo_pago,
           cpa.id_dis_det_avance_pago
    INTO   v_id_derechohabiente,
           v_imp_ap_pat,
           v_imp_am_cre,
           v_nrp,
           v_periodo_pago,
           v_id_dis_det_ava_pag
    FROM   dis_canc_par_ava_pag cpa
    WHERE  cpa.folio  = p_folio_disp
    AND    cpa.estado = 1

    IF v_imp_ap_pat IS NULL THEN
       LET v_imp_ap_pat = 0;
    END IF

    IF v_imp_am_cre IS NULL THEN
       LET v_imp_am_cre = 0;
    END IF

    --Si las aportaciones y amortizaciones son menores o
    --iguales a cero no se dispersa el registro
    IF (v_imp_ap_pat <= 0.00 AND v_imp_am_cre <= 0.00) THEN
       CONTINUE FOREACH;
    END IF

    LET v_origen = v_nrp||'-'||v_periodo_pago;

    IF v_imp_ap_pat > 0.00 THEN
       LET v_subcuenta = 4;

       --Insertamos en cat_movimiento con monto de aportación
       --ABONO CANCELACIÓN PARCIAL AVANCES PAGO
       INSERT INTO dis_canc_pav_preliquida {(f_liquida,
                                            id_derechohabiente,
                                            subcuenta,
                                            fondo_inversion,
                                            movimiento,
                                            folio_liquida,
                                            id_referencia,
                                            monto_acciones,
                                            monto_pesos,
                                            f_valor,
                                            f_registro,
                                            h_registro,
                                            origen)}
                                   VALUES  (v_hoy,
                                            v_id_derechohabiente,
                                            v_subcuenta,
                                            v_fdo_inversion,
                                            v_movimiento1,
                                            p_folio_disp,
                                            v_id_dis_det_ava_pag,
                                            v_monto_cero,
                                            v_imp_ap_pat,
                                            v_f_pago,
                                            v_hoy,
                                            v_hora_proceso,
                                            v_origen);

       --CARGO CANCELACIÓN PARCIAL AVANCES PAGO
       LET v_imp_ap_pat_neg = v_imp_ap_pat * -1;
       
       INSERT INTO dis_canc_pav_preliquida {(f_liquida,
                                            id_derechohabiente,
                                            subcuenta,
                                            fondo_inversion,
                                            movimiento,
                                            folio_liquida,
                                            id_referencia,
                                            monto_acciones,
                                            monto_pesos,
                                            f_valor,
                                            f_registro,
                                            h_registro,
                                            origen)}
                                    VALUES (v_hoy,
                                            v_id_derechohabiente,
                                            v_subcuenta,
                                            v_fdo_inversion,
                                            v_movimiento2,
                                            p_folio_disp,
                                            v_id_dis_det_ava_pag,
                                            v_monto_cero,
                                            v_imp_ap_pat_neg,
                                            v_f_pago,
                                            v_hoy,
                                            v_hora_proceso,
                                            v_origen);
    END IF

    IF v_imp_am_cre > 0.00 THEN
       LET v_subcuenta = 41;

       --Insertamos en cat_movimiento con monto de amortización
       --ABONO CANCELACIÓN PARCIAL AVANCES PAGO
       INSERT INTO dis_canc_pav_preliquida {(f_liquida,
                                            id_derechohabiente,
                                            subcuenta,
                                            fondo_inversion,
                                            movimiento,
                                            folio_liquida,
                                            id_referencia,
                                            monto_acciones,
                                            monto_pesos,
                                            f_valor,
                                            f_registro,
                                            h_registro,
                                            origen)}
                                    VALUES (v_hoy,
                                            v_id_derechohabiente,
                                            v_subcuenta,
                                            v_fdo_inversion,
                                            v_movimiento1,
                                            p_folio_disp,
                                            v_id_dis_det_ava_pag,
                                            v_monto_cero,
                                            v_imp_am_cre,
                                            v_f_pago,
                                            v_hoy,
                                            v_hora_proceso,
                                            v_origen);

       --CARGO CANCELACIÓN PARCIAL AVANCES PAGO
       LET v_imp_am_cre_neg = v_imp_am_cre * -1;
       
       INSERT INTO dis_canc_pav_preliquida {(f_liquida,
                                            id_derechohabiente,
                                            subcuenta,
                                            fondo_inversion,
                                            movimiento,
                                            folio_liquida,
                                            id_referencia,
                                            monto_acciones,
                                            monto_pesos,
                                            f_valor,
                                            f_registro,
                                            h_registro,
                                            origen)}
                                    VALUES (v_hoy,
                                            v_id_derechohabiente,
                                            v_subcuenta,
                                            v_fdo_inversion,
                                            v_movimiento2,
                                            p_folio_disp,
                                            v_id_dis_det_ava_pag,
                                            v_monto_cero,
                                            v_imp_am_cre_neg,
                                            v_f_pago,
                                            v_hoy,
                                            v_hora_proceso,
                                            v_origen);
    END IF
  END FOREACH --dis_canc_par_ava_pag
  
  CREATE INDEX xdis_canc_pav_preliquida2 ON dis_canc_pav_preliquida
  (folio_liquida) IN dis_ix_dbs;

  UPDATE STATISTICS FOR TABLE dis_canc_pav_preliquida;
  
  --TRACE 'Finaliza fn_dis_preliq_can_par_ap con valor '||v_bnd_proceso;
  --LET error_info = "  Preliquidación finalizo correctamente";
  --RETURN v_bnd_transaccion , v_status , error_info, '';
  RETURN v_bnd_proceso;

END PROCEDURE;


