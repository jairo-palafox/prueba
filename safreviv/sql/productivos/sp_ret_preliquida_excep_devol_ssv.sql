






CREATE PROCEDURE "safreviv".sp_ret_preliquida_excep_devol_ssv(p_fol_liq    DECIMAL(10,0),
                                                   v_proceso_cod      SMALLINT,
                                                   v_opera_cod        SMALLINT,
                                                   v_usuario_cod      VARCHAR(20),
                                                   v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

DEFINE  v_mov_cargo SMALLINT;
DEFINE  v_mov_abono_721 SMALLINT;
DEFINE  v_mov_cargo_672 SMALLINT;
DEFINE  v_mov_abono_ex SMALLINT;

DEFINE  v_origen CHAR(20);
DEFINE  v_sub_teso SMALLINT;
DEFINE  v_sub_viv97 SMALLINT;
DEFINE  v_sub_viv92 SMALLINT;
DEFINE  v_fch_valuacion DATE;
DEFINE  v_pre_f DECIMAL(9,6);
DEFINE  v_sal_teso DECIMAL(12,2);
DEFINE  v_sal_teso_p DECIMAL(12,2);
DEFINE  v_sal_aivs_97 DECIMAL(18,6);
DEFINE  v_sal_aivs_92 DECIMAL(18,6);
DEFINE  v_sal_pesos_97 DECIMAL(12,2);
DEFINE  v_sal_pesos_92 DECIMAL(12,2);

DEFINE v_suma_672_97 DECIMAL(18,6);
DEFINE v_suma_721_97 DECIMAL(18,6);
DEFINE v_suma_pesos_672_97 DECIMAL(12,2);
DEFINE v_suma_pesos_721_97 DECIMAL(12,2);
DEFINE v_suma_672_92 DECIMAL(18,6);
DEFINE v_suma_721_92 DECIMAL(18,6);
DEFINE v_suma_pesos_672_92 DECIMAL(12,2);
DEFINE v_suma_pesos_721_92 DECIMAL(12,2);
DEFINE v_imp_saldo DECIMAL(12,2);


DEFINE v_m_c_672_viv92 DECIMAL(12,2);
DEFINE v_m_c_672_viv97 DECIMAL(12,2);

DEFINE v_m_a_721_viv92 DECIMAL(12,2);
DEFINE v_m_a_721_viv97 DECIMAL(12,2);

DEFINE  v_rem DECIMAL(12,2);
DEFINE  v_aivs_rem DECIMAL(18,6);
DEFINE  v_resultado_exec INTEGER;
DEFINE  v_marca_excep_ssv SMALLINT;

DEFINE v_id_der DECIMAL(9,0) ;
DEFINE v_id_sol DECIMAL(9,0) ;
DEFINE registro_ssv_nss CHAR(11)     ;
DEFINE registro_ssv_importe DECIMAL(13,2);

DEFINE v_error_fecha_contable INTEGER;
DEFINE v_error_sin_precio INTEGER;
DEFINE v_bnd_preli SMALLINT;
DEFINE v_movtos_4 SMALLINT;
DEFINE v_movtos_8 SMALLINT;
--DEFINE v_tipo_solicitud CHAR(4);
DEFINE v_sig_a SMALLINT;
DEFINE v_sig_c SMALLINT;
DEFINE v_fecha_pago DATE;
DEFINE v_fecha_vence DATE;
DEFINE hoy DATE;
-- Control de Excepciones
DEFINE v_si_resultado SMALLINT;
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt VARCHAR(250);
DEFINE v_c_msj VARCHAR(250);
-- se configura el retorno de los valores
ON EXCEPTION SET sql_err, isam_err, err_txt 
   LET v_si_resultado = sql_err;
   RETURN v_si_resultado, isam_err, err_txt, v_id_sol;
END EXCEPTION

-- se actualiza el folio a preliquidado
UPDATE glo_folio
SET    status =  1
WHERE  folio  = p_fol_liq;
-- actualiza folio en la operacion y proceso
UPDATE bat_ctr_operacion
SET    folio  = p_fol_liq
WHERE  pid    = v_pid
AND    proceso_cod  = v_proceso_cod
AND    opera_cod    = v_opera_cod;
-- se inician las variables para marca
LET v_bnd_preli = 0;
LET v_id_der = 0 ;
LET v_id_sol = 0;
LET v_mov_cargo = 1802; -- CARGO A LA SSV VIA SIAFF
LET v_origen = "";
LET v_sub_teso = 47; -- TESOFE
LET v_sub_viv97 = 4;
LET v_sub_viv92 = 8;
LET v_c_msj = "El proceso de preliquidación finalizó correctamente.";
LET isam_err = 0;
LET v_si_resultado = 0;
LET v_fch_valuacion = NULL;
LET v_pre_f = 0.000000;
LET v_sal_teso = 0.00;
LET v_sal_teso_p = 0.00;
LET v_sal_aivs_97 = 0.000000;
LET v_sal_aivs_92 = 0.000000;
LET v_sal_pesos_97 = 0.00;
LET v_sal_pesos_92 = 0.00;
LET v_rem = 0.00;
LET v_aivs_rem = 0.000000;

LET v_marca_excep_ssv = 820;
LET v_error_fecha_contable = 13;
LET v_error_sin_precio = 14;
LET v_sig_a = 1;
LET v_sig_c = -1;
LET v_imp_saldo = 0;
LET v_suma_672_97 = 0;
LET v_suma_721_97 = 0;
LET v_suma_pesos_672_97 = 0;
LET v_suma_pesos_721_97 = 0;
LET v_suma_672_92 = 0;
LET v_suma_721_92 = 0;
LET v_suma_pesos_672_92 = 0;
LET v_suma_pesos_721_92 = 0;

LET v_m_a_721_viv92 = 0;
LET v_m_a_721_viv97 = 0;
LET v_m_c_672_viv92 = 0;
LET v_m_c_672_viv97 = 0;

LET v_mov_cargo = 2052;
LET v_mov_abono_721 = 721;
LET v_mov_cargo_672 = 672;
LET v_mov_abono_ex = 1811;

LET hoy = TODAY;   
--SET DEBUG FILE TO "/home/rperez/Infonavit/log/fn_ret_preliquida_ssv.log";
--trace on;
-- busca registros en estatus de capturado
FOREACH 
  SELECT id_solicitud,nss,importe
  INTO   v_id_sol,registro_ssv_nss,registro_ssv_importe
  FROM  ret_excep_devol_ssv
  WHERE estado_solicitud = 15 -- aceptados
  AND   folio = p_fol_liq
  -- BUsca el id_derechohabiente para su inegracion en la tabla de preliquidacion
  LET v_id_der = 0;
  LET v_movtos_4 = 0;
  LET v_movtos_8 = 0;
  
  SELECT id_derechohabiente 
  INTO   v_id_der
  FROM   afi_derechohabiente
  WHERE  nss = registro_ssv_nss;
  -- se inserta el monto en la tabla de preliquidacion
  IF v_id_der IS NULL THEN
    -- no se podra insertar en movimientos si no tiene el id_derechohabiente
    UPDATE ret_excep_devol_ssv
    SET    estado_solicitud = 100,
           cod_rechazo = 7
    WHERE  nss = registro_ssv_nss
    AND    id_solicitud = v_id_sol
    AND    folio = p_fol_liq
    AND    estado_solicitud = 15;
  ELSE
    EXECUTE FUNCTION fn_cal_habil_siguiente(hoy) INTO v_fecha_pago; 
    LET v_fecha_vence = v_fecha_pago + 90;
--    EXECUTE FUNCTION fn_cal_habil_siguiente(hoy+90) INTO v_fecha_vence; 
    LET v_fch_valuacion = MDY(MONTH(v_fecha_pago),1,YEAR(v_fecha_pago));
    IF v_fch_valuacion IS NOT NULL THEN 
      -- Busca el precio de valuacion
      SELECT precio_fondo
      INTO   v_pre_f
      FROM   glo_valor_fondo
      WHERE  f_valuacion = v_fch_valuacion
      AND    fondo = 11;
      IF v_pre_f IS NOT NULL THEN 
        -- SALDO TESOFE
        EXECUTE FUNCTION fn_saldo_dia(registro_ssv_nss,v_id_der,v_sub_teso,hoy)
        INTO v_resultado_exec, v_sal_teso, v_sal_teso_p;
        -- SALDO VIV 97
        EXECUTE FUNCTION fn_saldo_dia(registro_ssv_nss,v_id_der,v_sub_viv97,hoy)
        INTO v_resultado_exec, v_sal_aivs_97, v_sal_pesos_97;
        LET v_sal_pesos_97 = ROUND((v_sal_aivs_97 * v_pre_f),2);
        -- SALDO VIV 92
        EXECUTE FUNCTION fn_saldo_dia(registro_ssv_nss,v_id_der,v_sub_viv92,hoy)
        INTO v_resultado_exec, v_sal_aivs_92, v_sal_pesos_92;
        LET v_sal_pesos_92 = ROUND((v_sal_aivs_92 * v_pre_f),2);
        
        --- Obtienen la suma de los movimientos 672 y 721 por subcuenta
        SELECT NVL(SUM(monto_acciones),0)
        INTO   v_suma_672_97
        FROM   ret_excep_movtos
        WHERE  id_solicitud = v_id_sol
        AND    movimiento = 672
        AND    subcuenta = 4;
        LET    v_suma_pesos_672_97 = ROUND((v_suma_672_97 * v_pre_f),2);
        SELECT NVL(SUM(monto_acciones),0)
        INTO   v_suma_721_97
        FROM   ret_excep_movtos
        WHERE  id_solicitud = v_id_sol
        AND    movimiento = 721
        AND    subcuenta = 4;
        LET    v_suma_pesos_721_97 = ROUND((v_suma_721_97 * v_pre_f),2);
        SELECT NVL(SUM(monto_acciones),0)
        INTO   v_suma_672_92
        FROM   ret_excep_movtos
        WHERE  id_solicitud = v_id_sol
        AND    movimiento = 672
        AND    subcuenta = 8;
        LET    v_suma_pesos_672_92 = ROUND((v_suma_672_92 * v_pre_f),2);
        SELECT NVL(SUM(monto_acciones),0)
        INTO   v_suma_721_92
        FROM   ret_excep_movtos
        WHERE  id_solicitud = v_id_sol
        AND    movimiento = 721
        AND    subcuenta = 8;
        LET    v_suma_pesos_721_92 = ROUND((v_suma_721_92 * v_pre_f),2);
         
        --- SE DETERMINA SI HAY DIFERENCIA
        LET v_imp_saldo = (v_sal_teso + v_sal_pesos_97 + v_sal_pesos_92) - 
                          ((v_suma_pesos_672_97 + v_suma_pesos_721_97) + (v_suma_pesos_672_92 + v_suma_pesos_721_92)); 
        IF (v_imp_saldo >= registro_ssv_importe) AND  (registro_ssv_importe > 0) THEN 
          LET v_origen = "SIN DIF"; 
          IF (v_suma_pesos_672_92 + v_suma_pesos_721_92) <> 0 THEN
            IF  (v_suma_pesos_672_92 + v_suma_pesos_721_92) < 0 THEN 
              LET v_rem =  ((v_suma_pesos_672_92 + v_suma_pesos_721_92) * (-1)); --- Se multiplica por -1 para determinar el mov, cargo o abono
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_a_721_viv92 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_abono_721,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,v_rem *  v_sig_a,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            ELSE 
              LET v_rem =  (v_suma_pesos_672_92 + v_suma_pesos_721_92); 
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_c_672_viv92 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo_672,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF 
          END IF     
          IF (v_suma_pesos_672_97 + v_suma_pesos_721_97) <> 0 THEN
            IF  (v_suma_pesos_672_97 + v_suma_pesos_721_97) < 0 THEN 
              LET v_rem =  ((v_suma_pesos_672_97 + v_suma_pesos_721_97) * (-1)); --- Se multiplica por -1 ya que se debe hace un abono
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_a_721_viv97 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_721,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,v_rem *  v_sig_a,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            ELSE 
              LET v_rem =  (v_suma_pesos_672_97 + v_suma_pesos_721_97); 
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_c_672_viv97 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo_672,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF
          END IF  
          LET v_rem = registro_ssv_importe;
          IF v_sal_teso_p > 0 THEN
            IF v_sal_teso_p > registro_ssv_importe THEN
              LET v_rem = registro_ssv_importe;
            ELSE 
              LET v_rem = v_sal_teso_p;
            END IF
            INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_teso,10,v_mov_cargo,
               p_fol_liq,v_id_sol,v_rem *  v_sig_c,v_rem *  v_sig_c,
               hoy,hoy,CURRENT HOUR TO SECOND,v_origen);
            LET v_rem = registro_ssv_importe - v_rem;
          END IF 
          IF v_rem > 0 THEN 
            IF (v_sal_pesos_92 = 0 AND v_m_a_721_viv92 > 0) OR 
               (v_sal_pesos_92 > 0 AND v_m_a_721_viv92 = 0 AND v_m_c_672_viv92 = 0) OR 
               (v_sal_pesos_92 > 0 AND v_m_a_721_viv92 > 0) THEN
              IF ROUND((v_sal_pesos_92 + v_m_a_721_viv92),2) > v_rem THEN 
                LET v_aivs_rem = ROUND((v_rem / v_pre_f),2);
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_sal_pesos_92 + v_m_a_721_viv92) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_sal_pesos_92 + v_m_a_721_viv92) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - ROUND((v_sal_pesos_92 + v_m_a_721_viv92),2);
              END IF 
            END IF
            IF v_sal_pesos_92 > 0 AND v_m_c_672_viv92 > 0 THEN 
              IF ROUND((v_sal_pesos_92 + v_m_c_672_viv92),2) > v_rem THEN 
                LET v_aivs_rem = ROUND((v_rem / v_pre_f),2);
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_sal_pesos_92 + v_m_c_672_viv92) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_sal_pesos_92 + v_m_c_672_viv92) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - ROUND((v_sal_pesos_92 + v_m_c_672_viv92),2);
              END IF 
            END IF
            IF v_sal_pesos_92 = 0 AND v_m_c_672_viv92 > 0 THEN 
              IF ROUND((v_m_c_672_viv92),2) > v_rem THEN 
                LET v_aivs_rem = ROUND((v_rem / v_pre_f),2);
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_m_c_672_viv92) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_m_c_672_viv92) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - ROUND((v_m_c_672_viv92),2);
              END IF 
            END IF
               
            IF (v_sal_pesos_97 = 0 AND v_m_a_721_viv97 > 0) OR 
               (v_sal_pesos_97 > 0 AND v_m_a_721_viv97 = 0 AND v_m_c_672_viv97 = 0) OR 
               (v_sal_pesos_97 > 0 AND v_m_a_721_viv97 > 0) THEN
              IF v_rem > 0 THEN 
                IF ROUND((v_sal_pesos_97 + v_m_a_721_viv97),2) > v_rem THEN 
                  LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
                  INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                    p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_rem) *  v_sig_c,
                    v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                  LET v_rem = 0;
                ELSE 
                  LET v_aivs_rem = (ROUND(((v_sal_pesos_97 + v_m_a_721_viv97) / v_pre_f),2)); 
                  INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                     p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_sal_pesos_97 + v_m_a_721_viv97) *  v_sig_c,
                     v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                     LET v_rem = v_rem - (v_sal_pesos_97 + v_m_a_721_viv97);
                END IF 
              ELSE 
                IF v_m_a_721_viv97 > 0 THEN 
                  LET v_aivs_rem = (ROUND(((v_m_a_721_viv97) / v_pre_f),2)); 
                  INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                     p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_m_a_721_viv97) *  v_sig_c,
                     v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                END IF 
              END IF 
            END IF
            IF v_sal_pesos_97 > 0 AND v_m_c_672_viv97 > 0 THEN 
              IF ROUND((v_sal_pesos_97 + v_m_c_672_viv97),2) > v_rem THEN 
                LET v_aivs_rem = ROUND((v_rem / v_pre_f),2);
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_sal_pesos_97 + v_m_c_672_viv97) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_sal_pesos_97 + v_m_c_672_viv97) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - ROUND((v_sal_pesos_97 + v_m_c_672_viv97),2);
              END IF 
            END IF
            IF v_sal_pesos_97 = 0 AND v_m_c_672_viv97 > 0 THEN 
              IF ROUND((v_m_c_672_viv97),2) > v_rem THEN 
                LET v_aivs_rem = ROUND((v_rem / v_pre_f),2);
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_m_c_672_viv97) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_m_c_672_viv97) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - ROUND((v_m_c_672_viv97),2);
              END IF 
            END IF
            
          END IF  
        ELSE    ----- SI EL IMPORTE SOLICITADO ES DIFERENTE A LA SUMA DE LOS SALDOS DE LAS VIVIENDAS
          -- REVISA SI TIENE CAJA EN VIVIENDAS
          LET v_movtos_4 = 0;
          LET v_movtos_8 = 0;
          EXECUTE FUNCTION fn_cuenta_movimientos(
                   v_id_der, '', 4) 
          INTO v_movtos_4;
          EXECUTE FUNCTION fn_cuenta_movimientos(
                   v_id_der, '', 8) 
          INTO v_movtos_8;
          LET v_origen = "CON DIF"; 
          IF (v_suma_pesos_672_92 + v_suma_pesos_721_92) <> 0 THEN
            IF  (v_suma_pesos_672_92 + v_suma_pesos_721_92) < 0 THEN 
              LET v_rem =  ((v_suma_pesos_672_92 + v_suma_pesos_721_92) * (-1)); --- Se multiplica por -1 para determinar el mov, cargo o abono
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_a_721_viv92 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_abono_721,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,v_rem *  v_sig_a,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            ELSE 
              LET v_rem =  (v_suma_pesos_672_92 + v_suma_pesos_721_92); 
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_c_672_viv92 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo_672,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF 
          END IF     
          IF (v_suma_pesos_672_97 + v_suma_pesos_721_97) <> 0 THEN
            IF  (v_suma_pesos_672_97 + v_suma_pesos_721_97) < 0 THEN 
              LET v_rem =  ((v_suma_pesos_672_97 + v_suma_pesos_721_97) * (-1)); --- Se multiplica por -1 ya que se debe hace un abono
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_a_721_viv97 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_721,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,v_rem *  v_sig_a,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            ELSE 
              LET v_rem =  (v_suma_pesos_672_97 + v_suma_pesos_721_97); 
              LET v_aivs_rem = (ROUND((v_rem / v_pre_f),2));
              LET v_m_c_672_viv97 = v_rem; 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo_672,
                p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF
          END IF  
          IF registro_ssv_importe = 0 THEN 
            LET registro_ssv_importe = v_imp_saldo;
            LET v_rem = v_imp_saldo;
          ELSE 
            LET v_rem = registro_ssv_importe;
          END IF 
          IF v_sal_teso_p > 0 THEN
            IF v_sal_teso_p > registro_ssv_importe THEN
              LET v_rem = registro_ssv_importe;
            ELSE 
              LET v_rem = v_sal_teso_p;
            END IF
            INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_teso,10,v_mov_cargo,
               p_fol_liq,v_id_sol,v_rem *  v_sig_c,v_rem *  v_sig_c,
               hoy,hoy,CURRENT HOUR TO SECOND,v_origen);
            LET v_rem = registro_ssv_importe - v_rem;
          END IF 
          IF v_rem > 0 THEN --- Para validar Vivienda 92
            IF (v_sal_pesos_92 = 0 AND v_m_a_721_viv92 > 0) OR 
               (v_sal_pesos_92 > 0 AND v_m_a_721_viv92 = 0 AND v_m_c_672_viv92 = 0) OR 
               (v_sal_pesos_92 > 0 AND v_m_a_721_viv92 > 0) THEN
              IF  ROUND(((v_sal_pesos_92 + v_m_a_721_viv92)),2) > v_rem THEN 
                LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_rem) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = 0;
              ELSE 
                LET v_aivs_rem = (ROUND(((v_sal_pesos_92 + v_m_a_721_viv92) / v_pre_f),2)); 
                INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_cargo,
                   p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,(v_sal_pesos_92 + v_m_a_721_viv92) *  v_sig_c,
                   v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
                LET v_rem = v_rem - (v_sal_pesos_92 + v_m_a_721_viv92);
              END IF 
            END IF   
            IF (v_sal_pesos_92 = 0 AND v_m_c_672_viv92 > 0) THEN 
              LET v_aivs_rem = (ROUND(((v_m_c_672_viv92) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv92,11,v_mov_abono_ex,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,v_m_c_672_viv92 *  v_sig_a,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF  
          END IF   
          IF v_rem > 0 THEN --- Para validar Vivienda 97 y si todavia hay un remanente que aplicar
            IF (v_sal_pesos_97 = 0 AND v_m_a_721_viv97 > 0) THEN 
              LET v_aivs_rem = (ROUND(((v_rem - v_m_a_721_viv97) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem - v_m_a_721_viv97) *  v_sig_a,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF 
            IF (v_sal_pesos_97 > 0 AND v_m_a_721_viv97 = 0 AND v_m_c_672_viv97 = 0) THEN
              IF  (v_rem - v_sal_pesos_97) > 0 THEN 
                 LET v_aivs_rem = (ROUND(((v_rem - v_sal_pesos_97) / v_pre_f),2)); 
                 INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                    p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem - v_sal_pesos_97) *  v_sig_a,
                    v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              END IF 
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF 
            IF (v_sal_pesos_97 > 0 AND v_m_a_721_viv97 > 0) THEN
              IF  (v_rem - v_sal_pesos_97 - v_m_a_721_viv97) > 0 THEN 
                 LET v_aivs_rem = (ROUND(((v_rem - v_sal_pesos_97 - v_m_a_721_viv97) / v_pre_f),2)); 
                 INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                    p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem - v_sal_pesos_97 - v_m_a_721_viv97) *  v_sig_a,
                    v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              END IF 
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF
            IF (v_sal_pesos_97 = 0 AND v_m_c_672_viv97 > 0) THEN 
              LET v_aivs_rem = (ROUND(((v_rem + v_m_c_672_viv97) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem + v_m_c_672_viv97) *  v_sig_a,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF 
            IF (v_sal_pesos_97 > 0 AND v_m_c_672_viv97 > 0) THEN
              IF  (v_rem - v_sal_pesos_97 + v_m_c_672_viv97) > 0 THEN 
                 LET v_aivs_rem = (ROUND(((v_rem - v_sal_pesos_97 + v_m_c_672_viv97) / v_pre_f),2)); 
                 INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                    p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem - v_sal_pesos_97 + v_m_c_672_viv97) *  v_sig_a,
                    v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              END IF 
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF
            IF (v_sal_pesos_97 = 0  AND v_m_c_672_viv97 = 0 AND v_m_a_721_viv97 = 0) THEN 
              LET v_aivs_rem = (ROUND(((v_rem) / v_pre_f),2)); 
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_abono_ex,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_a,(v_rem) *  v_sig_a,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
              INSERT INTO ret_preliquida VALUES (hoy,v_id_der,v_sub_viv97,11,v_mov_cargo,
                 p_fol_liq,v_id_sol,v_aivs_rem *  v_sig_c,v_rem *  v_sig_c,
                 v_fch_valuacion,hoy,CURRENT HOUR TO SECOND,v_origen);
            END IF
          END IF  
        END IF ---- COMPARACION IMPORTES
        -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
        LET v_bnd_preli = 1;
        -- se actualica la solicitud a estado liquidado y se le asigna el folio
        IF registro_ssv_importe = 0 THEN 
           LET v_rem = v_imp_saldo;
        ELSE 
           LET v_rem = registro_ssv_importe;
        END IF 

        SELECT SUM(monto_pesos)
        INTO   v_rem
        FROM   ret_preliquida
        WHERE  folio_liquida      = p_fol_liq
        AND    id_derechohabiente = v_id_der
        AND    id_referencia      = v_id_sol
        AND    movimiento         = v_mov_cargo;

        LET v_rem = v_rem * (-1);
        
        UPDATE ret_excep_devol_ssv
        SET    estado_solicitud   = 50,
               importe_cuenta     = v_rem,
               fch_contabiliza    = v_fecha_pago,
               fch_vencimiento    = v_fecha_vence
        WHERE  nss                = registro_ssv_nss
        AND    id_solicitud       = v_id_sol
        AND    folio              = p_fol_liq
        AND    estado_solicitud   = 15;

        
      ELSE ---- NO HAY PRECIO DE ACCION
        UPDATE ret_excep_devol_ssv
        SET    estado_solicitud   = 100,
               cod_rechazo        = v_error_sin_precio
        WHERE  nss                = registro_ssv_nss
        AND    id_solicitud       = v_id_sol
        AND    folio              = p_fol_liq
        AND    estado_solicitud   = 15;
      END IF 
    ELSE ---- NO HAY FECHA DE VALUACION
      UPDATE ret_excep_devol_ssv
      SET    estado_solicitud   = 100,
             cod_rechazo        = v_error_fecha_contable
      WHERE  nss                = registro_ssv_nss
      AND    id_solicitud       = v_id_sol
      AND    folio              = p_fol_liq
      AND    estado_solicitud   = 15;
    END IF 
  END IF 
  LET v_m_a_721_viv92 = 0;
  LET v_m_a_721_viv97 = 0;
  LET v_m_c_672_viv92 = 0;
  LET v_m_c_672_viv97 = 0;
  LET v_rem = 0;
  LET v_aivs_rem = 0;
  
END FOREACH;

-- si no se preliquidaron registros
IF ( v_bnd_preli = 0 ) THEN
  -- se marca el procesoe en error
  LET v_si_resultado = 1000;
  LET isam_err       = 0;
  LET v_c_msj        = "Error. No se preliquidaron solicitudes para el folio.";
END IF;
--trace off;
-- se actullizan las estadisticas de los registros cargados
UPDATE STATISTICS FOR TABLE ret_preliquida;

-- se devuelve el resultado de la ejecucion
RETURN v_si_resultado, isam_err, v_c_msj, v_id_sol;
END PROCEDURE;


