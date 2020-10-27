






CREATE FUNCTION "safreviv".fn_sep_preliquidar_op28(p_folio DECIMAL(9))
RETURNING INTEGER,
          CHAR(70),
          SMALLINT,
          SMALLINT,
          SMALLINT,
          DECIMAL(22,2),
          DECIMAL(22,2),
          DECIMAL(22,2),
          DECIMAL(22,2);
-------------------------------------------------------------------
-- Se incluyen cambios para los requerimientos
-- PROINFXVII-20. Modificaciones a la regla de validacion saldos
-- PROINFXVII-89. Modificaciones para recibir desmarcas de parejas
--                ya liqudadas y desmarcadas en ProceSar por 
--                Patalla, no se validan saldos solo se opera su
--                desmarca
-------------------------------------------------------------------
DEFINE v_estado_destino  SMALLINT;
DEFINE v_ind             SMALLINT;
DEFINE v_diag            CHAR(3);
DEFINE sql_err           INTEGER ;
DEFINE isam_err          INTEGER ;
DEFINE error_info        CHAR(70);

DEFINE v_cod_marca       INTEGER      ;
DEFINE v_m_diag_marca    CHAR(3)      ;
DEFINE v_tot_parejas     INTEGER      ;
DEFINE v_tot_parejas_exp INTEGER      ;
DEFINE v_tot_parejas_dif INTEGER      ;
DEFINE v_tot_cargo_92    DECIMAL(22,2);
DEFINE v_tot_abono_92    DECIMAL(22,2);
DEFINE v_tot_cargo_97    DECIMAL(22,2);
DEFINE v_tot_abono_97    DECIMAL(22,2);
DEFINE v_resultado       SMALLINT     ;
DEFINE v_d_valor_accion  DECIMAL(28,6);

-- arreglo de cargos/abonos
DEFINE v_p_f_liquida          DATE                   ;
DEFINE v_p_id_derechohabiente DECIMAL(9)             ;
DEFINE v_p_subcuenta          SMALLINT               ;
DEFINE v_p_fondo_inversion    SMALLINT               ;
DEFINE v_p_movimiento         SMALLINT               ;
DEFINE v_p_folio_liquida      DECIMAL(9)             ;
DEFINE v_p_id_referencia      DECIMAL(9)             ;
DEFINE v_p_monto_acciones     DECIMAL(22,2)          ;
DEFINE v_p_monto_pesos        DECIMAL(22,2)          ;
DEFINE v_p_f_valor            DATE                   ;
DEFINE v_p_f_registro         DATE                   ;
DEFINE v_p_h_registro         DATETIME HOUR TO SECOND;
DEFINE v_p_origen             CHAR(20)               ;

-- arreglo de historico preliquidacion
DEFINE v_h_id_his_preliquida_op28    DECIMAL(9,0) ;
DEFINE v_h_id_det_02_op28            DECIMAL(9,0) ;
DEFINE v_h_folio                     DECIMAL(9,0) ;
DEFINE v_h_invadido                  CHAR(11)     ;
DEFINE v_h_sdo_origen_sar92_invadido DECIMAL(22,2);
DEFINE v_h_op28_sar92_invadido       DECIMAL(22,2);
DEFINE v_h_cargo_sar92_invadido      DECIMAL(22,2);
DEFINE v_h_sdo_final_sar92_invadido  DECIMAL(22,2);
DEFINE v_h_dif_sar92_invadido        DECIMAL(22,2);
DEFINE v_h_sdo_origen_viv97_invadido DECIMAL(22,2);
DEFINE v_h_op28_viv97_invadido       DECIMAL(22,2);
DEFINE v_h_cargo_viv97_invadido      DECIMAL(22,2);
DEFINE v_h_sdo_final_viv97_invadido  DECIMAL(22,2);
DEFINE v_h_dif_viv97_invadido        DECIMAL(22,2);
DEFINE v_h_asociado                  CHAR(11)     ;
DEFINE v_h_sdo_origen_sar92_asociado DECIMAL(22,2);
DEFINE v_h_op28_sar92_asociado       DECIMAL(22,2);
DEFINE v_h_abono_sar92_asociado      DECIMAL(22,2);
DEFINE v_h_sdo_final_sar92_asociado  DECIMAL(22,2);
DEFINE v_h_dif_sar92_asociado        DECIMAL(22,2);
DEFINE v_h_sdo_origen_viv97_asociado DECIMAL(22,2);
DEFINE v_h_op28_viv97_asociado       DECIMAL(22,2);
DEFINE v_h_abono_viv97_asociado      DECIMAL(22,2);
DEFINE v_h_sdo_final_viv97_asociado  DECIMAL(22,2);
DEFINE v_h_dif_viv97_asociado        DECIMAL(22,2);
DEFINE v_h_id_expediente             DECIMAL(9,0) ;

DEFINE v_saldo_final_92_inv DECIMAL(22,2);
DEFINE v_saldo_final_97_inv DECIMAL(22,2);
DEFINE v_saldo_final_92_aso DECIMAL(22,2);
DEFINE v_saldo_final_97_aso DECIMAL(22,2);

DEFINE v_r_saldo_saldo_92_inv   DECIMAL(16,6);
DEFINE v_r_saldo_saldo_97_inv   DECIMAL(16,6);
DEFINE v_r_saldo_saldo_92_aso   DECIMAL(16,6);
DEFINE v_r_saldo_saldo_97_aso   DECIMAL(16,6);

DEFINE v_dif_final_92_inv DECIMAL(22,2);
DEFINE v_dif_final_97_inv DECIMAL(22,2);
DEFINE v_dif_final_92_aso DECIMAL(22,2);
DEFINE v_dif_final_97_aso DECIMAL(22,2);

DEFINE v_saldo_actual_92_inf_inv   DECIMAL(16,6);
DEFINE v_saldo_actual_92_inf_inv_p DECIMAL(16,6);
DEFINE v_saldo_actual_97_inf_inv   DECIMAL(16,6);
DEFINE v_saldo_actual_97_inf_inv_p DECIMAL(16,6);
DEFINE v_saldo_actual_92_inf_aso   DECIMAL(16,6);
DEFINE v_saldo_actual_92_inf_aso_p DECIMAL(16,6);
DEFINE v_saldo_actual_97_inf_aso   DECIMAL(16,6);
DEFINE v_saldo_actual_97_inf_aso_p DECIMAL(16,6);

-- arreglo operacion 28
define v_r_preliq_estado_cza             smallint;
define v_r_preliq_id_det_02_op28         decimal(9);
define v_r_preliq_id_derechohabiente_inv decimal(9,0);
define v_r_preliq_id_derechohabiente_aso decimal(9,0);
define v_r_preliq_invadido               char(11);
define v_r_preliq_asociado               char(11);
define v_r_preliq_saldo_viv_92_inv       decimal(16,6);
define v_r_preliq_saldo_viv_97_inv       decimal(16,6);
define v_r_preliq_saldo_viv_92_aso       decimal(16,6);
define v_r_preliq_saldo_viv_97_aso       decimal(16,6);
define v_r_preliq_estado                 smallint;
define v_r_preliq_id_expediente_inv      decimal(9);
define v_r_preliq_id_det_op27            decimal(9);
-- variables tipo de subuenta, para imss o solo infonavit
define v_tpo_subc1_inv       smallint;
define v_tpo_subc2_inv       smallint;
define v_tpo_subc1_aso       smallint;
define v_tpo_subc2_aso       smallint;
define v_tipo_trabajador_inv char(1);
define v_tipo_trabajador_aso char(1);
define v_fecha_1_natural     date;
define v_fecha_actual        date;

DEFINE v_saldo_total_viv97_inf DECIMAL(16,6);
DEFINE v_saldo_total_viv92_inf DECIMAL(16,6);

DEFINE v_saldo_total_viv97_op28 DECIMAL(16,6);
DEFINE v_saldo_total_viv92_op28 DECIMAL(16,6);


DEFINE v_preliquida_97  SMALLINT;
DEFINE v_preliquida_92  SMALLINT;
DEFINE v_folio_desmarca DECIMAL(9,0);
DEFINE v_cod_rechazo    INTEGER;
DEFINE v_existe_marca   SMALLINT;
DEFINE v_ind_conciliar  SMALLINT;
DEFINE v_clasifica_separacion CHAR(1);
DEFINE v_resultado_operacion CHAR(2);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET sql_err, isam_err, error_info
      RETURN sql_err, error_info, 0, 0, 0, 0, 0, 0, 0;
   END EXCEPTION ;

   --Se habilita el LOG del SP
   SET DEBUG FILE TO '/safreviv_int/BD/fn_sep_preliquidar_op28.trace';
   TRACE ON;
   -- Valida estado 15 - rechazos informados
   SELECT NVL(MAX(estado),0)
     INTO v_r_preliq_estado_cza
     FROM sep_cza_op28
    WHERE folio = p_folio;

   IF v_r_preliq_estado_cza <> 15 THEN -- 15 = Rechazos Informados
      --TRACE "Folio invalido, estado: ",v_r_preliq_estado_cza;
      RETURN -999, -- Error predefinido, TMP Validarlo
             'FOLIO INVALIDO ESTADO: '||v_r_preliq_estado_cza,
             0, 0, 0, 0, 0, 0, 0;
   END IF

   LET v_tot_parejas     = 0; LET v_tot_parejas_exp = 0; LET v_tot_parejas_dif = 0;
   LET v_tot_cargo_92    = 0; LET v_tot_abono_92    = 0; LET v_tot_cargo_97    = 0;
   LET v_tot_abono_97    = 0; LET v_p_monto_pesos   = 0;

   SELECT precio_fondo
     INTO v_d_valor_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   LET v_fecha_1_natural = MDY(MONTH(TODAY),"01",YEAR(TODAY));
   LET v_fecha_actual = TODAY;

   UPDATE glo_folio
   SET    status = 1
   WHERE  folio =  p_folio
   AND    proceso_cod = 2202;

   -- se actualiza estado del encabezado (control) primero para facilitar
   -- el reverso en caso de error

   EXECUTE FUNCTION fn_maquinaria('maq_sep_ctr_op28',
                                  20,                    -- (Señal) Preliquidar
                                  v_r_preliq_estado_cza) -- (Estado) Rechazos Informados
                             INTO v_cod_marca,
                                  v_m_diag_marca,
                                  v_estado_destino;

   UPDATE sep_cza_op28
      SET estado = v_estado_destino -- Preliquidado
    WHERE folio = p_folio;

   FOREACH SELECT inv.id_det_02_op28              , inv.id_derechohabiente_invadido,
                  aso.id_derechohabiente_asociado , inv.invadido,
                  aso.asociado                    , inv.saldo_viv_92,
                  inv.saldo_viv_97                , aso.saldo_viv_92,
                  aso.saldo_viv_97                , inv.estado      ,
                  inv.id_det_op27                 , inv.resultado_operacion,
                  inv.clasifica_separacion        , inv.ind_conciliar
             INTO v_r_preliq_id_det_02_op28         , v_r_preliq_id_derechohabiente_inv,
                  v_r_preliq_id_derechohabiente_aso , v_r_preliq_invadido,
                  v_r_preliq_asociado               , v_r_preliq_saldo_viv_92_inv,
                  v_r_preliq_saldo_viv_97_inv       , v_r_preliq_saldo_viv_92_aso,
                  v_r_preliq_saldo_viv_97_aso       , v_r_preliq_estado ,
                  v_r_preliq_id_det_op27            , v_resultado_operacion,
                  v_clasifica_separacion            , v_ind_conciliar
             FROM sep_det_02_op28 inv,
                  sep_det_03_op28 aso
            WHERE inv.id_det_02_op28 = aso.id_det_02_op28
              AND inv.estado         = 10 -- Integrado
              AND aso.asociado <> "00000000000"

              -- Indicador para el proceso de operacion 28
              IF v_ind_conciliar <> 9 AND v_ind_conciliar <> 10 THEN
                 LET v_ind_conciliar = 6; -- incializa como preliquidado sin diferencias en saldos
              ELSE
                 UPDATE sep_det_02_op28
                 SET ind_conciliar = v_ind_conciliar
                 WHERE id_det_02_op28 = v_r_preliq_id_det_02_op28;

                 IF v_ind_conciliar = 9 THEN
                    LET v_tot_parejas = v_tot_parejas + 1;
                 ELSE
                    LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
                 END IF
                 CONTINUE FOREACH;
              END IF;

      -- variables para determinar si se han preliquidan los montos de 97 y 92
      LET v_preliquida_97 = 0; -- inicializa a no preliquidado
      LET v_preliquida_92 = 0; -- inicializa a no preliquidado

      LET v_tipo_trabajador_inv = ' ';
      LET v_tipo_trabajador_aso = ' ';

      SELECT tipo_trabajador
        INTO v_tipo_trabajador_inv
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_r_preliq_id_derechohabiente_inv;
      --TRACE 'NSS INV: '||v_r_preliq_id_derechohabiente_inv;
      --TRACE 'TIPO INV: '||v_tipo_trabajador_inv;

      SELECT tipo_trabajador
        INTO v_tipo_trabajador_aso
        FROM afi_derechohabiente
       WHERE id_derechohabiente = v_r_preliq_id_derechohabiente_aso;
      --TRACE 'NSS ASO: '||v_r_preliq_id_derechohabiente_aso;
      --TRACE 'TIPO ASO: '||v_tipo_trabajador_aso;

      IF(v_tipo_trabajador_inv = "S")THEN
         -- solo infonavit
         LET v_tpo_subc1_inv = 42;
         LET v_tpo_subc2_inv = 44;
      ELSE
         -- imss
         LET v_tpo_subc1_inv = 8;
         LET v_tpo_subc2_inv = 4;
      END IF;

      IF(v_tipo_trabajador_aso = "S")THEN
         -- solo infonavit
         LET v_tpo_subc1_aso = 42;
         LET v_tpo_subc2_aso = 44;
      ELSE
         -- imss
         LET v_tpo_subc1_aso = 8;
         LET v_tpo_subc2_aso = 4;
      END IF;

      -- Valida si debe cambiar las subcuentas si la clasifiación es 1-E, para cuentas no localizadas y pasar saldos a cuenta virtual
      -- v_resultado_operacion = '01' --> ACEPTADA
      -- v_clasifica_separacion = 'E' --> NO LOCALIZADA
      IF( v_resultado_operacion = '01' AND v_clasifica_separacion = 'E')THEN
         -- Subcuentas de separación virtuales
         LET v_tpo_subc1_aso = 62; -- SEP virtual Viv 92
         LET v_tpo_subc2_aso = 61; -- SEP virtual Viv 97
      END IF

      -- Busca relacion con expediente
      SELECT NVL(MAX(id_expediente),0)
        INTO v_r_preliq_id_expediente_inv
        FROM sep_det_02_op27
       WHERE id_derechohabiente_invadido = v_r_preliq_id_derechohabiente_inv
         AND estado IN (30,35) -- ACEPTADA PROCEDENTE, LIGADA A EXPEDIENTE
         AND id_det_02_op27 = v_r_preliq_id_det_op27;

      IF v_r_preliq_id_expediente_inv > 0 THEN
        LET v_tot_parejas_exp = v_tot_parejas_exp + 1;
      END IF;

      SELECT seq_sep_his_preliquida_op28.NEXTVAL
        INTO v_h_id_his_preliquida_op28
        FROM systables
       WHERE tabname = "sep_his_preliquida_op28";

      -----------------------------------------------------------
      -- OBTIENE SALDOS INVADIDO y ASOCIADO 1ER NATURAL Y ACTUAL
      -----------------------------------------------------------

      --
      -- saldo invadido viv92 1er natural
      --
      SELECT SUM(a.monto_acciones)
        INTO v_r_saldo_saldo_92_inv
        FROM cta_movimiento a
       WHERE a.id_derechohabiente = v_r_preliq_id_derechohabiente_inv
         AND a.subcuenta  = v_tpo_subc1_inv
         AND a.f_liquida  <= v_fecha_1_natural
         AND a.movimiento NOT IN (451,83,41,63)  ; -- adelantos de aclaracion

      IF v_r_saldo_saldo_92_inv IS NULL THEN
         LET v_r_saldo_saldo_92_inv = 0;
      END IF

      --
      -- saldo invadido viv92 actual
      --
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_invadido,
                                    v_r_preliq_id_derechohabiente_inv,
                                    v_tpo_subc1_inv,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_92_inf_inv,
                                    v_saldo_actual_92_inf_inv_p;

      --
      -- saldo invadido viv97 1er natural
      --
      SELECT SUM(a.monto_acciones)
        INTO v_r_saldo_saldo_97_inv
        FROM cta_movimiento a
       WHERE a.id_derechohabiente = v_r_preliq_id_derechohabiente_inv
         AND a.subcuenta          = v_tpo_subc2_inv
         AND a.f_liquida   <= v_fecha_1_natural
         AND a.movimiento        not in (41,63,451,83)  ; -- adelantos de aclaracion

      IF v_r_saldo_saldo_97_inv IS NULL THEN
         LET v_r_saldo_saldo_97_inv = 0;
      END IF

      --
      -- saldo invadido viv97 actual
      --
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_invadido,
                                    v_r_preliq_id_derechohabiente_inv,
                                    v_tpo_subc2_inv,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_97_inf_inv,
                                    v_saldo_actual_97_inf_inv_p;

      --
      -- saldo asociado viv92 1er natural
      --
      SELECT SUM(a.monto_acciones)
        INTO v_r_saldo_saldo_92_aso
        FROM cta_movimiento a
       WHERE a.id_derechohabiente = v_r_preliq_id_derechohabiente_aso
         AND a.subcuenta  = v_tpo_subc1_aso
         AND a.f_liquida  <= v_fecha_1_natural
         AND a.movimiento NOT IN (41,63,451,83)  ; -- adelantos de aclaracion

      IF v_r_saldo_saldo_92_aso IS NULL THEN
         LET v_r_saldo_saldo_92_aso = 0;
      END IF

      --
      -- saldo_asociado viv92 actual
      --
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_asociado,
                                    v_r_preliq_id_derechohabiente_aso,
                                    v_tpo_subc1_aso,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_92_inf_aso,
                                    v_saldo_actual_92_inf_aso_p;

      --
      -- saldo asociado viv97 1er natural
      --
      SELECT SUM(a.monto_acciones)
        INTO v_r_saldo_saldo_97_aso
        FROM cta_movimiento a
       WHERE a.id_derechohabiente = v_r_preliq_id_derechohabiente_aso
         AND a.subcuenta  = v_tpo_subc2_aso
         AND a.f_liquida  <= v_fecha_1_natural
         AND a.movimiento NOT IN (41,63,451,83)  ; -- adelantos de aclaracion

      IF v_r_saldo_saldo_97_aso IS NULL THEN
         LET v_r_saldo_saldo_97_aso = 0;
      END IF

      --
      -- saldo asociado viv97 actual
      --
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_asociado,
                                    v_r_preliq_id_derechohabiente_aso,
                                    v_tpo_subc2_aso,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_97_inf_aso,
                                    v_saldo_actual_97_inf_aso_p;

      ------------------------------------------------------
      -- CALCULO DE VALIDACION DE SALDOS Y MONTOS A SEPARAR
      ------------------------------------------------------

      --
      -- Saldos totales al 1er dia natural del mes actual
      --
      LET v_saldo_total_viv97_inf = v_r_saldo_saldo_97_inv + v_r_saldo_saldo_97_aso;
      LET v_saldo_total_viv92_inf = v_r_saldo_saldo_92_inv + v_r_saldo_saldo_92_aso;

      --
      -- Totales infromados con op28
      --
      LET v_saldo_total_viv97_op28 = v_r_preliq_saldo_viv_97_inv + v_r_preliq_saldo_viv_97_aso;
      LET v_saldo_total_viv92_op28 = v_r_preliq_saldo_viv_92_inv + v_r_preliq_saldo_viv_92_aso;

      --
      -- Valida que se pueda hacer operaciones con los saldos al primer dia natural del mes actual
      -- * Valida que los saldos de invadido mas asociado op28 deben ser iguales (contemplando que se esta haciendo la separacion)
      -- a los saldos infonavit de invadido mas asociado
      --
      IF( v_saldo_total_viv97_inf >= v_saldo_total_viv97_op28 AND     -- ELSE 7,  ind_conciliar =  7 
          v_saldo_total_viv92_inf >= v_saldo_total_viv92_op28 )THEN
         -- 
         -- Saldos totales al dia actual
         -- 
         LET v_saldo_total_viv97_inf = v_saldo_actual_97_inf_inv + v_saldo_actual_97_inf_aso;
         LET v_saldo_total_viv92_inf = v_saldo_actual_92_inf_inv + v_saldo_actual_92_inf_aso;

         -- 
         -- Valida la suficiencia de saldos a la fecha actual
         -- * Evita que se pueda sobregirar la cuenta si es que existen cargos despues del dia primero del mes
         -- 
         IF( v_saldo_total_viv97_inf >= v_saldo_total_viv97_op28 AND  -- ELSE 8, ind_conciliar = 8
             v_saldo_total_viv92_inf >= v_saldo_total_viv92_op28 )THEN

            --
            -- REGISTRA MOVIMIENTOS DE INVADIDO VIV 92
            --
            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_inv;
            LET v_p_subcuenta          = v_tpo_subc1_inv;
            LET v_p_fondo_inversion    = 11;
            LET v_p_movimiento         = 382; -- Cargo separacion cuenta
            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            IF v_r_saldo_saldo_92_inv >= v_r_preliq_saldo_viv_92_inv THEN 
               IF v_r_saldo_saldo_92_inv = v_r_preliq_saldo_viv_92_inv THEN --saldo iguales inf y op28

                  LET v_p_monto_acciones     = (v_r_saldo_saldo_92_inv-v_r_preliq_saldo_viv_92_inv)*-1; --CARGO
                  LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                  LET v_saldo_final_92_inv   = v_r_saldo_saldo_92_inv-(v_p_monto_acciones*-1);
                  LET v_dif_final_92_inv     = 0;

               ELIF v_r_saldo_saldo_92_inv > v_r_preliq_saldo_viv_92_inv THEN  --saldo inf mayor a op28
--revisar
                     -- si el saldo total es mayor se comparan saldos del asociado   
                     -- y aplica movimiento de separacion hasta por el saldo del inv en el inf                   
                     IF (v_r_saldo_saldo_92_aso - v_r_preliq_saldo_viv_92_aso + v_r_saldo_saldo_92_inv) >= 0 THEN 

                         LET v_p_monto_acciones     = (v_r_preliq_saldo_viv_92_aso - v_r_saldo_saldo_92_aso)*-1; --CARGO
                         LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                         LET v_saldo_final_92_inv   = v_r_saldo_saldo_92_inv - (v_p_monto_acciones*-1);
                         LET v_dif_final_92_inv     = v_saldo_final_92_inv - v_r_preliq_saldo_viv_92_inv;
                     ELSE 

                     -- saldo en invadido insuficiente
                         LET v_p_monto_acciones     = -1; --CARGO
                         LET v_p_monto_pesos        = 0;
                         LET v_saldo_final_92_inv   = v_r_saldo_saldo_92_inv;
                         LET v_dif_final_92_inv     = v_r_preliq_saldo_viv_92_inv-v_r_saldo_saldo_92_inv;
                         LET v_tot_parejas_dif      = v_tot_parejas_dif + 1;
                         LET v_ind_conciliar        = 4; -- Preliquidado viv 97, con diferencias en viv92
                     END IF
                END IF
            ELSE -- saldo en invadido insuficiente

               LET v_p_monto_acciones     = -1; --CARGO
               LET v_p_monto_pesos        = 0;
               LET v_saldo_final_92_inv   = v_r_saldo_saldo_92_inv;
               LET v_dif_final_92_inv     = v_r_preliq_saldo_viv_92_inv-v_r_saldo_saldo_92_inv;
               LET v_tot_parejas_dif      = v_tot_parejas_dif + 1;
               LET v_ind_conciliar        = 4; -- Preliquidado viv 97, con diferencias en viv92
            END IF

            IF v_ind_conciliar = 4 THEN 
               LET v_h_cargo_sar92_invadido = 0;
            ELSE 
               LET v_h_cargo_sar92_invadido =  v_p_monto_acciones;
            END IF
            --
            -- solo diferentes de 0
            --
            IF v_p_monto_acciones <> 0 AND v_p_monto_acciones <> -1 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         , id_derechohabiente,
                                                subcuenta         , fondo_inversion   ,
                                                movimiento        , folio_liquida     ,
                                                id_referencia     , monto_acciones    ,
                                                monto_pesos       , f_valor           ,
                                                f_registro        , h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida     , v_p_id_derechohabiente,
                                                v_p_subcuenta     , v_p_fondo_inversion   ,
                                                v_p_movimiento    , v_p_folio_liquida     ,
                                                v_p_id_referencia , v_p_monto_acciones    ,
                                                v_p_monto_pesos   , v_p_f_valor           ,
                                                v_p_f_registro    , v_p_h_registro        ,
                                                v_p_origen        );

                LET v_preliquida_92 = 1; -- registro preliquidado
 
            ELIF v_p_monto_acciones = -1 THEN
               LET v_preliquida_92 = 0; -- registro NO preliquidado  v_p_monto_acciones = -1 
                  LET v_p_monto_Acciones = 0;
              ELSE
               LET v_preliquida_92 = 1; -- registro preliquidado v_p_monto_acciones = 0
                  LET v_p_monto_Acciones = 0;
            END IF

            LET v_tot_cargo_92 = v_tot_cargo_92 + v_p_monto_acciones;

            --
            -- REGISTRA MOVIMIENTOS DE INVADIDO VIV 97
            --
            LET v_p_f_liquida          = TODAY;
            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_inv;
            LET v_p_subcuenta          = v_tpo_subc2_inv;
            LET v_p_fondo_inversion    = 11;
            LET v_p_movimiento         = 382; -- Cargo separacion cuenta
            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            IF v_r_saldo_saldo_97_inv >= v_r_preliq_saldo_viv_97_inv THEN
               IF v_r_saldo_saldo_97_inv = v_r_preliq_saldo_viv_97_inv THEN --saldo iguales inf y op28

                  LET v_p_monto_acciones     = (v_r_saldo_saldo_97_inv-v_r_preliq_saldo_viv_97_inv)*-1; --CARGO
                  LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                  LET v_saldo_final_97_inv   = v_r_saldo_saldo_97_inv-(v_p_monto_acciones*-1);
                  LET v_dif_final_97_inv     = 0;

               ELIF v_r_saldo_saldo_97_inv > v_r_preliq_saldo_viv_97_inv THEN  --saldo inf mayor a op28
--revisar
                     -- si el saldo total es mayor se comparan saldos del asociado   
                     -- y aplica movimiento de separacion hasta por el saldo del inv en el inf                   
                     IF (v_r_saldo_saldo_97_aso - v_r_preliq_saldo_viv_97_aso + v_r_saldo_saldo_97_inv) >= 0 THEN 

                        LET v_p_monto_acciones     = (v_r_preliq_saldo_viv_97_aso - v_r_saldo_saldo_97_aso)*-1; --CARGO
                        LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                        LET v_saldo_final_97_inv   = v_r_saldo_saldo_97_inv-(v_p_monto_acciones*-1);
                        LET v_dif_final_97_inv     = v_saldo_final_97_inv - v_r_preliq_saldo_viv_97_inv;
                     ELSE 
                        LET v_p_monto_acciones     = -1; --CARGO
                        LET v_p_monto_pesos        = 0;
                        LET v_saldo_final_97_inv   = v_r_saldo_saldo_97_inv;
                        LET v_dif_final_97_inv     = v_r_preliq_saldo_viv_97_inv-v_r_saldo_saldo_97_inv;
                        LET v_tot_parejas_dif      = v_tot_parejas_dif + 1;
                        LET v_ind_conciliar        = 3; -- Preliquidado viv 92, con diferencias en viv97
                     END IF
               END IF
            ELSE -- saldo en invadido insuficiente
               LET v_p_monto_acciones     = -1; --CARGO
               LET v_p_monto_pesos        = 0;
               LET v_saldo_final_97_inv   = v_r_saldo_saldo_97_inv;
               LET v_dif_final_97_inv     = v_r_preliq_saldo_viv_97_inv-v_r_saldo_saldo_97_inv;
               LET v_tot_parejas_dif      = v_tot_parejas_dif + 1;
               LET v_ind_conciliar        = 3; -- Preliquidado viv 92, con diferencias en viv97
            END IF

            IF v_ind_conciliar = 3 THEN 
               LET v_h_cargo_viv97_invadido =  0;
            ELSE
               LET v_h_cargo_viv97_invadido =  v_p_monto_acciones;
            END IF;

            --
            -- solo diferentes de 0
            --
            IF v_p_monto_acciones <> 0 AND v_p_monto_acciones <> -1 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         , id_derechohabiente,
                                                subcuenta         , fondo_inversion   ,
                                                movimiento        , folio_liquida     ,
                                                id_referencia     , monto_acciones    ,
                                                monto_pesos       , f_valor           ,
                                                f_registro        , h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida     , v_p_id_derechohabiente,
                                                v_p_subcuenta     , v_p_fondo_inversion   ,
                                                v_p_movimiento    , v_p_folio_liquida     ,
                                                v_p_id_referencia , v_p_monto_acciones    ,
                                                v_p_monto_pesos   , v_p_f_valor           ,
                                                v_p_f_registro    , v_p_h_registro        ,
                                                v_p_origen        );

                LET v_preliquida_97 = 1; -- registro preliquidado
           ELIF v_p_monto_acciones = -1 THEN
               LET v_preliquida_97 = 0; -- registro NO preliquidado
                  LET v_p_monto_Acciones = 0;
             ELSE
               LET v_preliquida_97 = 1; -- registro preliquidado
                  LET v_p_monto_Acciones = 0;
           END IF
           LET v_tot_cargo_97 = v_tot_cargo_97 + v_p_monto_acciones;

            --
            -- REGISTRA MOVIMIENTOS DE ASOCIADO VIV 92
            --
            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_aso;
            LET v_p_subcuenta          = v_tpo_subc1_aso;
            LET v_p_fondo_inversion    = 11;
            LET v_p_movimiento         = 381; -- Abono separacion cuenta
            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            IF v_r_saldo_saldo_92_inv >= v_r_preliq_saldo_viv_92_inv THEN

               IF v_r_saldo_saldo_92_inv = v_r_preliq_saldo_viv_92_inv THEN --saldo iguales inf y op28
                  LET v_p_monto_acciones     = (v_r_saldo_saldo_92_inv-v_r_preliq_saldo_viv_92_inv); --Abono
                  LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                  LET v_saldo_final_92_aso   = v_r_saldo_saldo_92_aso-(v_p_monto_acciones*-1);
               ELIF v_r_saldo_saldo_92_inv > v_r_preliq_saldo_viv_92_inv THEN  --saldo inf mayor a op28
                    -- si el saldo total es mayor se comparan saldos del asociado   
                    -- y aplica movimiento de separacion hasta por el saldo del inv en el inf                   
--revisar
                    IF (v_r_saldo_saldo_92_aso - v_r_preliq_saldo_viv_92_aso + v_r_saldo_saldo_92_inv) >= 0 THEN 

                       LET v_p_monto_acciones     = (v_r_preliq_saldo_viv_92_aso - v_r_saldo_saldo_92_aso); --Abono
                       LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                       LET v_saldo_final_92_aso   = v_r_saldo_saldo_92_aso-(v_p_monto_acciones*-1);
                    ELSE 
                       LET v_p_monto_acciones     = 0; --CARGO
                       LET v_p_monto_pesos        = 0;
                       LET v_saldo_final_92_aso   = v_r_saldo_saldo_92_aso;
                    END IF
               END IF 
            ELSE -- saldo en invadido insuficiente
               LET v_p_monto_acciones     = 0; --CARGO
               LET v_p_monto_pesos        = 0;
               LET v_saldo_final_92_aso   = v_r_saldo_saldo_92_aso;
            END IF

            LET v_h_abono_sar92_asociado =  v_p_monto_acciones;

            IF v_r_saldo_saldo_92_aso > v_r_preliq_saldo_viv_92_aso THEN
               LET v_dif_final_92_aso = v_r_preliq_saldo_viv_92_aso - (v_r_saldo_saldo_92_aso+v_p_monto_acciones);
            ELSE
               LET v_dif_final_92_aso = ((v_r_saldo_saldo_92_aso+v_p_monto_acciones) - v_r_preliq_saldo_viv_92_aso)*-1;

            END IF

            IF v_dif_final_92_aso > 0 THEN
               LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
            END IF

            ------------------------
            -- solo diferentes de 0
            ------------------------
            IF v_p_monto_acciones <> 0 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         , id_derechohabiente,
                                                subcuenta         , fondo_inversion   ,
                                                movimiento        , folio_liquida     ,
                                                id_referencia     , monto_acciones    ,
                                                monto_pesos       , f_valor           ,
                                                f_registro        , h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida     , v_p_id_derechohabiente,
                                                v_p_subcuenta     , v_p_fondo_inversion   ,
                                                v_p_movimiento    , v_p_folio_liquida     ,
                                                v_p_id_referencia , v_p_monto_acciones    ,
                                                v_p_monto_pesos   , v_p_f_valor           ,
                                                v_p_f_registro    , v_p_h_registro        ,
                                                v_p_origen        );

            END IF
            LET v_tot_abono_92 = v_tot_abono_92 + v_p_monto_acciones;

            --
            -- REGISTRA MOVIMIENTOS DE ASOCIADO VIV 97
            --
            LET v_p_f_liquida          = TODAY;
            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_aso;
            LET v_p_subcuenta          = v_tpo_subc2_aso;
            LET v_p_fondo_inversion    = 11;
            LET v_p_movimiento         = 381; -- Abono separacion cuenta
            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            IF v_r_saldo_saldo_97_inv >= v_r_preliq_saldo_viv_97_inv THEN
               IF v_r_saldo_saldo_97_inv = v_r_preliq_saldo_viv_97_inv THEN --saldo iguales inf y op28
                  LET v_p_monto_acciones     = (v_r_saldo_saldo_97_inv-v_r_preliq_saldo_viv_97_inv); --Abono
                  LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                  LET v_saldo_final_97_aso   = v_r_saldo_saldo_97_aso-(v_p_monto_acciones*-1);
               ELIF v_r_saldo_saldo_97_inv > v_r_preliq_saldo_viv_97_inv THEN  --saldo inf mayor a op28
                    -- si el saldo total es mayor se comparan saldos del asociado   
                    -- y aplica movimiento de separacion hasta por el saldo del inv en el inf                   
--revisar
                    IF (v_r_saldo_saldo_97_aso - v_r_preliq_saldo_viv_97_aso + v_r_saldo_saldo_97_inv) >= 0 THEN 

                       LET v_p_monto_acciones     = (v_r_preliq_saldo_viv_97_aso - v_r_saldo_saldo_97_aso); --Abono
                       LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
                       LET v_saldo_final_97_aso   = v_r_saldo_saldo_97_aso-(v_p_monto_acciones*-1);
                    ELSE 
                       LET v_p_monto_acciones     = 0; --CARGO
                       LET v_p_monto_pesos        = 0;
                       LET v_saldo_final_97_aso   = v_r_saldo_saldo_97_aso;
                    END IF
               END IF
            ELSE -- saldo en invadido insuficiente
               LET v_p_monto_acciones     = 0; --CARGO
               LET v_p_monto_pesos        = 0;
               LET v_saldo_final_97_aso   = v_r_saldo_saldo_97_aso;
            END IF

            LET v_h_abono_viv97_asociado =  v_p_monto_acciones;

            IF v_r_saldo_saldo_97_aso > v_r_preliq_saldo_viv_97_aso THEN
               LET v_dif_final_97_aso = v_r_preliq_saldo_viv_97_aso - (v_r_saldo_saldo_97_aso+v_p_monto_acciones);
            ELSE
              LET v_dif_final_97_aso = ((v_r_saldo_saldo_97_aso+v_p_monto_acciones) - v_r_preliq_saldo_viv_97_aso)*-1;
            END IF
            IF v_dif_final_97_aso > 0 THEN
               LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
            END IF

            --
            -- solo diferentes de 0
            --
            IF v_p_monto_acciones <> 0 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         , id_derechohabiente,
                                                subcuenta         , fondo_inversion   ,
                                                movimiento        , folio_liquida     ,
                                                id_referencia     , monto_acciones    ,
                                                monto_pesos       , f_valor           ,
                                                f_registro        , h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida     , v_p_id_derechohabiente,
                                                v_p_subcuenta     , v_p_fondo_inversion   ,
                                                v_p_movimiento    , v_p_folio_liquida     ,
                                                v_p_id_referencia , v_p_monto_acciones    ,
                                                v_p_monto_pesos   , v_p_f_valor           ,
                                                v_p_f_registro    , v_p_h_registro        ,
                                                v_p_origen        );
            END IF
            LET v_tot_abono_97 = v_tot_abono_97 + v_p_monto_acciones;

            --LET v_h_id_his_preliquida_op28      = ; secuencia generada previamente
            LET v_h_id_det_02_op28              = v_r_preliq_id_det_02_op28;
            LET v_h_folio                       = p_folio;
            LET v_h_invadido                    = v_r_preliq_invadido;
            LET v_h_sdo_origen_sar92_invadido   = v_r_saldo_saldo_92_inv;
            LET v_h_op28_sar92_invadido         = v_r_preliq_saldo_viv_92_inv;
            --LET v_h_cargo_sar92_invadido        = ;
            LET v_h_sdo_final_sar92_invadido    = v_saldo_final_92_inv;
            LET v_h_dif_sar92_invadido          = v_dif_final_92_inv;
            LET v_h_sdo_origen_viv97_invadido   = v_r_saldo_saldo_97_inv;
            LET v_h_op28_viv97_invadido         = v_r_preliq_saldo_viv_97_inv;
            --LET v_h_cargo_viv97_invadido        = ;
            LET v_h_sdo_final_viv97_invadido    = v_saldo_final_97_inv;
            LET v_h_dif_viv97_invadido          = v_dif_final_97_inv;
            LET v_h_asociado                    = v_r_preliq_asociado;
            LET v_h_sdo_origen_sar92_asociado   = v_r_saldo_saldo_92_aso;
            LET v_h_op28_sar92_asociado         = v_r_preliq_saldo_viv_92_aso;
            --LET v_h_abono_sar92_asociado        = ;
            LET v_h_sdo_final_sar92_asociado    = v_saldo_final_92_aso;
            LET v_h_dif_sar92_asociado          = v_dif_final_92_aso;
            LET v_h_sdo_origen_viv97_asociado   = v_r_saldo_saldo_97_aso;
            LET v_h_op28_viv97_asociado         = v_r_preliq_saldo_viv_97_aso;
            --LET v_h_abono_viv97_asociado        = ;
            LET v_h_sdo_final_viv97_asociado    = v_saldo_final_97_aso;
            LET v_h_dif_viv97_asociado          = v_dif_final_97_aso;
            LET v_h_id_expediente               = v_r_preliq_id_expediente_inv;

            -- si el ind_conciliar es 3 o 4 se actuliza el monto en acciones para evitar que salga en el reporte como diferencia el -1


            INSERT INTO sep_his_preliquida_op28 (id_his_preliquida_op28      , id_det_02_op28              ,
                                                 folio                       , invadido                    ,
                                                 sdo_origen_sar92_invadido   , op28_sar92_invadido         ,
                                                 cargo_sar92_invadido        , sdo_final_sar92_invadido    ,
                                                 dif_sar92_invadido          , sdo_origen_viv97_invadido   ,
                                                 op28_viv97_invadido         , cargo_viv97_invadido        ,
                                                 sdo_final_viv97_invadido    , dif_viv97_invadido          ,
                                                 asociado                    , sdo_origen_sar92_asociado   ,
                                                 op28_sar92_asociado         , abono_sar92_asociado        ,
                                                 sdo_final_sar92_asociado    , dif_sar92_asociado          ,
                                                 sdo_origen_viv97_asociado   , op28_viv97_asociado         ,
                                                 abono_viv97_asociado        , sdo_final_viv97_asociado    ,
                                                 dif_viv97_asociado          , id_expediente               )
                                         VALUES (v_h_id_his_preliquida_op28  , v_h_id_det_02_op28              ,
                                                 v_h_folio                   , v_h_invadido                    ,
                                                 v_h_sdo_origen_sar92_invadido , v_h_op28_sar92_invadido       ,
                                                 v_h_cargo_sar92_invadido      , v_h_sdo_final_sar92_invadido  ,
                                                 v_h_dif_sar92_invadido        , v_h_sdo_origen_viv97_invadido ,
                                                 v_h_op28_viv97_invadido       , v_h_cargo_viv97_invadido      ,
                                                 v_h_sdo_final_viv97_invadido  , v_h_dif_viv97_invadido        ,
                                                 v_h_asociado                  , v_h_sdo_origen_sar92_asociado ,
                                                 v_h_op28_sar92_asociado       , v_h_abono_sar92_asociado      ,
                                                 v_h_sdo_final_sar92_asociado  , v_h_dif_sar92_asociado        ,
                                                 v_h_sdo_origen_viv97_asociado , v_h_op28_viv97_asociado       ,
                                                 v_h_abono_viv97_asociado      , v_h_sdo_final_viv97_asociado  ,
                                                 v_h_dif_viv97_asociado        , v_h_id_expediente             );

            -- si por lo menos se realizo algun movimiento, avanza maquinarian para el registro
            IF(v_preliquida_92 = 1 OR v_preliquida_97 = 1)THEN
               EXECUTE FUNCTION fn_maquinaria('maq_sep_det_op28',
                                              10,                -- (Señal) Preliquidar
                                              v_r_preliq_estado) -- (Estado) Integrado
                                         INTO v_cod_marca,
                                              v_m_diag_marca,
                                              v_estado_destino;

               UPDATE sep_det_02_op28
                  SET estado = v_estado_destino
                WHERE id_det_02_op28 = v_r_preliq_id_det_02_op28;
            ELSE
               LET v_ind_conciliar = 5; -- Preliquidado con diferencias en viv 97 y viv92
            END IF
         ELSE --  8
            LET v_ind_conciliar = 8; -- Saldos diferentes al dia actual de preliquidacion => sdo inf <> sdo op28
            LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
         END IF
      ELSE -- 7
         LET v_ind_conciliar = 7; -- Saldos diferentes al 1er dia natural del mes => sdo inf <> sdo op28
         LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
      END IF

      -- se inserta registro en historico para los que necesitan conciliacion y puedan salir en el reporte
      IF v_ind_conciliar = 7 OR v_ind_conciliar = 8 THEN

            --LET v_h_id_his_preliquida_op28      = ; secuencia generada previamente
            LET v_h_id_det_02_op28              = v_r_preliq_id_det_02_op28;
            LET v_h_folio                       = p_folio;
            LET v_h_invadido                    = v_r_preliq_invadido;
            LET v_h_sdo_origen_sar92_invadido   = v_r_saldo_saldo_92_inv;
            LET v_h_op28_sar92_invadido         = v_r_preliq_saldo_viv_92_inv;

            LET v_h_cargo_sar92_invadido        = 0;
            LET v_h_sdo_final_sar92_invadido    = v_r_saldo_saldo_92_inv;                                --v_saldo_final_92_inv;
            LET v_h_dif_sar92_invadido          = v_r_saldo_saldo_92_inv - v_r_preliq_saldo_viv_92_inv; --v_dif_final_92_inv;
            LET v_h_sdo_origen_viv97_invadido   = v_r_saldo_saldo_97_inv;
            LET v_h_op28_viv97_invadido         = v_r_preliq_saldo_viv_97_inv;

            LET v_h_cargo_viv97_invadido        = 0;
            LET v_h_sdo_final_viv97_invadido    = v_r_saldo_saldo_97_inv;                                 --v_saldo_final_97_inv;
            LET v_h_dif_viv97_invadido          = v_r_saldo_saldo_97_inv - v_r_preliq_saldo_viv_97_inv;   --v_dif_final_97_inv;
            LET v_h_asociado                    = v_r_preliq_asociado;
            LET v_h_sdo_origen_sar92_asociado   = v_r_saldo_saldo_92_aso;
            LET v_h_op28_sar92_asociado         = v_r_preliq_saldo_viv_92_aso;

            LET v_h_abono_sar92_asociado        = 0;
            LET v_h_sdo_final_sar92_asociado    = v_r_saldo_saldo_92_aso;                                 --v_saldo_final_92_aso;
            LET v_h_dif_sar92_asociado          = v_r_saldo_saldo_92_aso - v_r_preliq_saldo_viv_92_aso;   --v_dif_final_92_aso;
            LET v_h_sdo_origen_viv97_asociado   = v_r_saldo_saldo_97_aso;
            LET v_h_op28_viv97_asociado         = v_r_preliq_saldo_viv_97_aso;

            LET v_h_abono_viv97_asociado        = 0;
            LET v_h_sdo_final_viv97_asociado    = v_r_saldo_saldo_97_aso;                                 --v_saldo_final_97_aso;
            LET v_h_dif_viv97_asociado          = v_r_saldo_saldo_97_aso - v_r_preliq_saldo_viv_97_aso;   --v_dif_final_97_aso;
            LET v_h_id_expediente               = v_r_preliq_id_expediente_inv;

            INSERT INTO sep_his_preliquida_op28 (id_his_preliquida_op28      , id_det_02_op28              ,
                                                 folio                       , invadido                    ,
                                                 sdo_origen_sar92_invadido   , op28_sar92_invadido         ,
                                                 cargo_sar92_invadido        , sdo_final_sar92_invadido    ,
                                                 dif_sar92_invadido          , sdo_origen_viv97_invadido   ,
                                                 op28_viv97_invadido         , cargo_viv97_invadido        ,
                                                 sdo_final_viv97_invadido    , dif_viv97_invadido          ,
                                                 asociado                    , sdo_origen_sar92_asociado   ,
                                                 op28_sar92_asociado         , abono_sar92_asociado        ,
                                                 sdo_final_sar92_asociado    , dif_sar92_asociado          ,
                                                 sdo_origen_viv97_asociado   , op28_viv97_asociado         ,
                                                 abono_viv97_asociado        , sdo_final_viv97_asociado    ,
                                                 dif_viv97_asociado          , id_expediente               )
                                         VALUES (v_h_id_his_preliquida_op28  , v_h_id_det_02_op28              ,
                                                 v_h_folio                   , v_h_invadido                    ,
                                                 v_h_sdo_origen_sar92_invadido , v_h_op28_sar92_invadido       ,
                                                 v_h_cargo_sar92_invadido      , v_h_sdo_final_sar92_invadido  ,
                                                 v_h_dif_sar92_invadido        , v_h_sdo_origen_viv97_invadido ,
                                                 v_h_op28_viv97_invadido       , v_h_cargo_viv97_invadido      ,
                                                 v_h_sdo_final_viv97_invadido  , v_h_dif_viv97_invadido        ,
                                                 v_h_asociado                  , v_h_sdo_origen_sar92_asociado ,
                                                 v_h_op28_sar92_asociado       , v_h_abono_sar92_asociado      ,
                                                 v_h_sdo_final_sar92_asociado  , v_h_dif_sar92_asociado        ,
                                                 v_h_sdo_origen_viv97_asociado , v_h_op28_viv97_asociado       ,
                                                 v_h_abono_viv97_asociado      , v_h_sdo_final_viv97_asociado  ,
                                                 v_h_dif_viv97_asociado        , v_h_id_expediente             );

      END IF

      -- Actualiza el indicador de op28 según los saldos, donde:
      -- 3 = preliquidado viv92 con diferencias en viv97
      -- 4 = preliquidado viv97 con diferencias en viv92
      -- 5 = preliquidado con diferencias en viv97 y viv92
      -- 6 = preliquidado sin diferencias en saldos
      -- 7 = no liquidado saldos diferentes al 1er natural
      -- 8 = no liquidado saldos diferentes al dia de preliquidacion

      UPDATE sep_det_02_op28
         SET ind_conciliar = v_ind_conciliar
       WHERE id_det_02_op28 = v_r_preliq_id_det_02_op28;

      LET v_tot_parejas = v_tot_parejas + 1;
   END FOREACH

   UPDATE sep_det_02_op28
   SET    estado = 15
   WHERE  estado = 10 ;

   RETURN 0,
          '',
          v_tot_parejas    ,
          v_tot_parejas_exp,
          v_tot_parejas_dif,
          v_tot_cargo_92   ,
          v_tot_abono_92   ,
          v_tot_cargo_97   ,
          v_tot_abono_97;

END FUNCTION;


