






CREATE FUNCTION "safreviv".fn_sep_preliquidar_op40(p_folio DECIMAL(9))
RETURNING INTEGER,
          CHAR(70),
          SMALLINT,
          SMALLINT,
          SMALLINT,
          DECIMAL(22,2),
          DECIMAL(22,2),
          DECIMAL(22,2),
          DECIMAL(22,2);

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind            SMALLINT;
DEFINE v_diag           CHAR(3);
DEFINE sql_err          INTEGER ;
DEFINE isam_err         INTEGER ;
DEFINE error_info       CHAR(70);
DEFINE v_signo          SMALLINT;

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
DEFINE v_r_preliq_estado_cza             SMALLINT;
DEFINE v_r_preliq_id_det_02_op28         DECIMAL(9);
DEFINE v_r_preliq_id_derechohabiente_inv DECIMAL(9,0);
DEFINE v_r_preliq_id_derechohabiente_aso DECIMAL(9,0);
DEFINE v_r_preliq_invadido               CHAR(11);
DEFINE v_r_preliq_asociado               CHAR(11);
DEFINE v_r_preliq_saldo_viv_92_inv       DECIMAL(16,6);
DEFINE v_r_preliq_saldo_viv_97_inv       DECIMAL(16,6);
DEFINE v_r_preliq_saldo_viv_92_aso       DECIMAL(16,6);
DEFINE v_r_preliq_saldo_viv_97_aso       DECIMAL(16,6);
DEFINE v_r_preliq_estado                 SMALLINT;
DEFINE v_r_preliq_id_expediente_inv      DECIMAL(9);
DEFINE v_r_preliq_id_det_op27            DECIMAL(9);
-- variables tipo de subuenta, para imss o solo infonavit
DEFINE v_tpo_subc1_inv       SMALLINT;
DEFINE v_tpo_subc2_inv       SMALLINT;
DEFINE v_tpo_subc1_aso       SMALLINT;
DEFINE v_tpo_subc2_aso       SMALLINT;
DEFINE v_tipo_trabajador_inv CHAR(1);
DEFINE v_tipo_trabajador_aso CHAR(1);
DEFINE v_fecha_1_natural     DATE;
DEFINE v_fecha_actual        DATE;

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
      RETURN sql_err,
             error_info,
             0,
             0,
             0,
             0,
             0,
             0,
             0;
   END EXCEPTION ;

   --Se habilita el LOG del SP
   SET DEBUG FILE TO '/safreviv_int/BD/fn_sep_preliquidar_op28.trace';
   TRACE ON;
   -- Valida estado 15 - rechazos informados
   SELECT NVL(estado,0)
     INTO v_r_preliq_estado_cza
     FROM sep_cza_op40
    WHERE folio = p_folio;

   IF v_r_preliq_estado_cza <> 15 THEN -- 15 = Rechazos Informados
      --TRACE "Folio invalido, estado: ",v_r_preliq_estado_cza;
      RETURN -999, -- Error predefinido, TMP Validarlo
             'FOLIO INVALIDO ESTADO: '||v_r_preliq_estado_cza,
             0,
             0,
             0,
             0,
             0,
             0,
             0;
   END IF

   LET v_tot_parejas     = 0;
   LET v_tot_parejas_exp = 0;
   LET v_tot_parejas_dif = 0;
   LET v_tot_cargo_92    = 0;
   LET v_tot_abono_92    = 0;
   LET v_tot_cargo_97    = 0;
   LET v_tot_abono_97    = 0;
   LET v_p_monto_pesos   = 0;

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
   AND    proceso_cod = 2237;

   -- se actualiza estado del encabezado (control) primero para facilitar
   -- el reverso en caso de error

   EXECUTE FUNCTION fn_maquinaria('maq_sep_ctr_op28',
                                  20,                    -- (Señal) Preliquidar
                                  v_r_preliq_estado_cza) -- (Estado) Rechazos Informados
                             INTO v_cod_marca,
                                  v_m_diag_marca,
                                  v_estado_destino;

   UPDATE sep_cza_op40
      SET estado = v_estado_destino -- Preliquidado
    WHERE folio = p_folio;

   FOREACH SELECT inv.id_det_02_op40,
   	          inv.id_derechohabiente_invadido,
   	          aso.id_derechohabiente_asociado,
   	          inv.invadido,
   	          aso.asociado,
   	          inv.saldo_viv_92,
                  inv.saldo_viv_97,
   	          aso.saldo_viv_92,
                  aso.saldo_viv_97,
                  inv.estado      ,
                  --inv.id_det_op27,
                  --inv.resultado_operacion,
                  inv.tipo_movimiento,
                  inv.ind_conciliar
   	     INTO v_r_preliq_id_det_02_op28,
   	          v_r_preliq_id_derechohabiente_inv,
   	          v_r_preliq_id_derechohabiente_aso,
   	          v_r_preliq_invadido,
   	          v_r_preliq_asociado,
                  v_r_preliq_saldo_viv_92_inv,
                  v_r_preliq_saldo_viv_97_inv,
                  v_r_preliq_saldo_viv_92_aso,
                  v_r_preliq_saldo_viv_97_aso,
                  v_r_preliq_estado ,
                  --v_r_preliq_id_det_op27,
                  --v_resultado_operacion,
                  v_clasifica_separacion,
                  v_ind_conciliar
   	     FROM sep_det_02_op40 inv,
   	          sep_det_03_op40 aso
   	    WHERE inv.id_det_02_op40 = aso.id_det_02_op40
   	      AND inv.estado = 10 -- Integrado
              AND aso.asociado <> "00000000000"
              AND inv.ind_conciliar = 1  -- solo aceptados



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

      -- imss
         LET v_tpo_subc1_inv = 8;
         LET v_tpo_subc2_inv = 4;
         LET v_tpo_subc1_aso = 8;
         LET v_tpo_subc2_aso = 4;

      -- Valida si debe cambiar las subcuentas si la clasifiación es 1-E, para cuentas no localizadas y pasar saldos a cuenta virtual
      -- v_resultado_operacion = '01' --> ACEPTADA
      -- v_clasifica_separacion = 'E' --> NO LOCALIZADA
      --IF( v_resultado_operacion = '01' AND v_clasifica_separacion = 'E')THEN
         -- Subcuentas de separación virtuales
         --LET v_tpo_subc1_aso = 62; -- SEP virtual Viv 92
         --LET v_tpo_subc2_aso = 61; -- SEP virtual Viv 97
      --END IF


      SELECT seq_sep_his_preliquida_op28.NEXTVAL
        INTO v_h_id_his_preliquida_op28
        FROM systables
       WHERE tabname = "sep_his_preliquida_op28";

      -- obtiene saldo al dia para invadido

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
      -- Saldos viv 92 invadido a la fecha actual
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_invadido,
                                    v_r_preliq_id_derechohabiente_inv,
                                    v_tpo_subc1_inv,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_92_inf_inv,
                                    v_saldo_actual_92_inf_inv_p;

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
      -- Saldos viv 97 invadido a la fecha actual
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_invadido,
                                    v_r_preliq_id_derechohabiente_inv,
                                    v_tpo_subc2_inv,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_97_inf_inv,
                                    v_saldo_actual_97_inf_inv_p;

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
      -- Saldos viv 92 asociado a la fecha actual
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_asociado,
                                    v_r_preliq_id_derechohabiente_aso,
                                    v_tpo_subc1_aso,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_92_inf_aso,
                                    v_saldo_actual_92_inf_aso_p;

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
      -- Saldos viv 97 asociado a la fecha actual
      EXECUTE FUNCTION fn_saldo_dia(v_r_preliq_asociado,
                                    v_r_preliq_id_derechohabiente_aso,
                                    v_tpo_subc2_aso,
                                    TODAY)
                               INTO v_resultado,
                                    v_saldo_actual_97_inf_aso,
                                    v_saldo_actual_97_inf_aso_p;




      --validacion de suficiencia de saldo en nss en que se hace el cargo

      IF  v_clasifica_separacion = 1 THEN    -- cargo en la invadida
          IF v_saldo_actual_97_inf_inv < v_r_preliq_saldo_viv_97_inv THEN
             LET v_ind_conciliar = 6; --saldo insuficiente viv97
          ELIF v_saldo_actual_92_inf_inv < v_r_preliq_saldo_viv_92_inv THEN
             LET v_ind_conciliar = 7; --saldo insuficiente viv92
          END IF
      ELIF v_clasifica_separacion = 2 THEN -- abono en la invadida se valida saldo en asociado
          IF v_saldo_actual_97_inf_aso < v_r_preliq_saldo_viv_97_aso THEN
             LET v_ind_conciliar = 6; --saldo insuficiente viv97
          ELIF v_saldo_actual_92_inf_aso < v_r_preliq_saldo_viv_92_aso THEN
             LET v_ind_conciliar = 7; --saldo insuficiente viv92
          END IF
      END IF


      IF v_ind_conciliar <> 1 THEN

         UPDATE sep_det_02_op40
            SET ind_conciliar  = v_ind_conciliar
          WHERE id_det_02_op40 = v_r_preliq_id_det_02_op28;

          LET v_tot_parejas_dif = v_tot_parejas_dif + 1;
      ELSE
            -----------------------------------
            -- REGISTRA MOVIMIENTOS DE INVADIDO

            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_inv;
            LET v_p_subcuenta          = v_tpo_subc1_inv;
            LET v_p_fondo_inversion    = 11;
            IF v_clasifica_separacion = 1 THEN
               LET v_p_movimiento         = 382; -- Cargo separacion cuenta
               LET v_signo                = -1;
            ELSE
               LET v_p_movimiento         = 381; -- Abono separacion cuenta
               LET v_signo                = 1;
            END IF

            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            LET v_p_monto_acciones     = v_r_preliq_saldo_viv_92_inv * v_signo; --CARGO /ABONO
            LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
            LET v_saldo_final_92_inv   = v_r_saldo_saldo_92_inv + v_p_monto_acciones;
            LET v_dif_final_92_inv     = 0;


            LET v_h_cargo_sar92_invadido =  v_p_monto_acciones;

            ------------------------
            -- solo diferentes de 0
            ------------------------
            IF v_p_monto_acciones <> 0  THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         ,
                                                id_derechohabiente,
                                                subcuenta         ,
                                                fondo_inversion   ,
                                                movimiento        ,
                                                folio_liquida     ,
                                                id_referencia     ,
                                                monto_acciones    ,
                                                monto_pesos       ,
                                                f_valor           ,
                                                f_registro        ,
                                                h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida         ,
                                                v_p_id_derechohabiente,
                                                v_p_subcuenta         ,
                                                v_p_fondo_inversion   ,
                                                v_p_movimiento        ,
                                                v_p_folio_liquida     ,
                                                v_p_id_referencia     ,
                                                v_p_monto_acciones    ,
                                                v_p_monto_pesos       ,
                                                v_p_f_valor           ,
                                                v_p_f_registro        ,
                                                v_p_h_registro        ,
                                                v_p_origen            );

               LET v_preliquida_92 = 1; -- registro preliquidado
          END IF
          IF v_p_movimiento = 382 THEN
             LET v_tot_cargo_92 = v_tot_cargo_92 + v_p_monto_acciones;
          ELSE 
             LET v_tot_abono_92 = v_tot_abono_92 + v_p_monto_acciones;
          END IF

            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_inv;
            LET v_p_subcuenta          = v_tpo_subc2_inv;
            LET v_p_fondo_inversion    = 11;
            IF v_clasifica_separacion = 1 THEN
               LET v_p_movimiento         = 382; -- Cargo separacion cuenta
               LET v_signo                = -1;
            ELSE
               LET v_p_movimiento         = 381; -- Abono separacion cuenta
               LET v_signo                = 1;
            END IF

            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            LET v_p_monto_acciones     = v_r_preliq_saldo_viv_97_inv * v_signo; --CARGO / ABONO
            LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
            LET v_saldo_final_97_inv   = v_r_saldo_saldo_97_inv + v_p_monto_acciones;
            LET v_dif_final_97_inv     = 0;

            LET v_h_cargo_viv97_invadido =  v_p_monto_acciones;

            ------------------------
            -- solo diferentes de 0
            ------------------------

            IF v_p_monto_acciones <> 0  THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         ,
                                                id_derechohabiente,
                                                subcuenta         ,
                                                fondo_inversion   ,
                                                movimiento        ,
                                                folio_liquida     ,
                                                id_referencia     ,
                                                monto_acciones    ,
                                                monto_pesos       ,
                                                f_valor           ,
                                                f_registro        ,
                                                h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida         ,
                                                v_p_id_derechohabiente,
                                                v_p_subcuenta         ,
                                                v_p_fondo_inversion   ,
                                                v_p_movimiento        ,
                                                v_p_folio_liquida     ,
                                                v_p_id_referencia     ,
                                                v_p_monto_acciones    ,
                                                v_p_monto_pesos       ,
                                                v_p_f_valor           ,
                                                v_p_f_registro        ,
                                                v_p_h_registro        ,
                                                v_p_origen            );

                  LET v_preliquida_97 = 1; -- registro preliquidado
             END IF
             IF v_p_movimiento = 382 THEN
                LET v_tot_cargo_97  = v_tot_cargo_97 + v_p_monto_acciones;
             ELSE 
                LET v_tot_abono_97  = v_tot_abono_97 + v_p_monto_acciones;
             END IF 


            --------------------------------------
            -- REGISTRA MOVIMIENTOS DE ASOCIADO
            ---------------------------------------
            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_aso;
            LET v_p_subcuenta          = v_tpo_subc1_aso;
            LET v_p_fondo_inversion    = 11;
            IF v_clasifica_separacion = 1 THEN
               LET v_p_movimiento         = 381; -- Abono separacion cuenta
               LET v_signo                = 1;
            ELSE
               LET v_p_movimiento         = 382; -- Cargo separacion cuenta
               LET v_signo                = -1;
            END IF

            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            LET v_p_monto_acciones     = v_r_preliq_saldo_viv_92_inv * v_signo; --Abono /Cargo
            LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
            LET v_saldo_final_92_aso   = v_r_saldo_saldo_92_aso + v_p_monto_acciones;
            LET v_h_abono_sar92_asociado =  v_p_monto_acciones;
            LET v_dif_final_92_aso     = 0;

            ------------------------
            -- solo diferentes de 0
            ------------------------

            IF v_p_monto_acciones <> 0 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         ,
                                                id_derechohabiente,
                                                subcuenta         ,
                                                fondo_inversion   ,
                                                movimiento        ,
                                                folio_liquida     ,
                                                id_referencia     ,
                                                monto_acciones    ,
                                                monto_pesos       ,
                                                f_valor           ,
                                                f_registro        ,
                                                h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida         ,
                                                v_p_id_derechohabiente,
                                                v_p_subcuenta         ,
                                                v_p_fondo_inversion   ,
                                                v_p_movimiento        ,
                                                v_p_folio_liquida     ,
                                                v_p_id_referencia     ,
                                                v_p_monto_acciones    ,
                                                v_p_monto_pesos       ,
                                                v_p_f_valor           ,
                                                v_p_f_registro        ,
                                                v_p_h_registro        ,
                                                v_p_origen            );

            END IF
            IF v_p_movimiento = 381 THEN
               LET v_tot_abono_92 = v_tot_abono_92 + v_p_monto_acciones;
            ELSE 
               LET v_tot_cargo_92 = v_tot_cargo_92 + v_p_monto_acciones;
            END IF



            LET v_p_f_liquida          = TODAY;
            LET v_p_id_derechohabiente = v_r_preliq_id_derechohabiente_aso;
            LET v_p_subcuenta          = v_tpo_subc2_aso;
            LET v_p_fondo_inversion    = 11;
            IF v_clasifica_separacion = 1 THEN
                LET v_p_movimiento         = 381; -- Abono separacion cuenta
                LET v_signo                = 1;
            ELSE
                LET v_p_movimiento         = 382; -- Cargo separacion cuenta
                LET v_signo                = -1;
            END IF

            LET v_p_folio_liquida      = p_folio;
            LET v_p_id_referencia      = v_h_id_his_preliquida_op28;
            LET v_p_f_valor            = TODAY;
            LET v_p_f_registro         = TODAY;
            LET v_p_h_registro         = CURRENT HOUR TO SECOND;
            LET v_p_origen             = v_r_preliq_id_det_02_op28;

            LET v_p_monto_acciones     = v_r_preliq_saldo_viv_97_inv * v_signo; --Abono /Cargo
            LET v_p_monto_pesos        = v_p_monto_acciones * v_d_valor_accion   ;
            LET v_saldo_final_97_aso   = v_r_saldo_saldo_97_aso + v_p_monto_acciones;

            LET v_h_abono_viv97_asociado =  v_p_monto_acciones;
            LET v_dif_final_97_aso       =  0;

            ---------------------------------------------------------------------
            -- solo diferentes de 0
            ----------------------------------------------------------------------
            IF v_p_monto_acciones <> 0 THEN
               INSERT INTO sep_preliquida_op28 (f_liquida         ,
                                                id_derechohabiente,
                                                subcuenta         ,
                                                fondo_inversion   ,
                                                movimiento        ,
                                                folio_liquida     ,
                                                id_referencia     ,
                                                monto_acciones    ,
                                                monto_pesos       ,
                                                f_valor           ,
                                                f_registro        ,
                                                h_registro        ,
                                                origen            )
                                        VALUES (v_p_f_liquida         ,
                                                v_p_id_derechohabiente,
                                                v_p_subcuenta         ,
                                                v_p_fondo_inversion   ,
                                                v_p_movimiento        ,
                                                v_p_folio_liquida     ,
                                                v_p_id_referencia     ,
                                                v_p_monto_acciones    ,
                                                v_p_monto_pesos       ,
                                                v_p_f_valor           ,
                                                v_p_f_registro        ,
                                                v_p_h_registro        ,
                                                v_p_origen            );
            END IF
            IF v_p_movimiento = 381 THEN
               LET v_tot_abono_97 = v_tot_abono_97 + v_p_monto_acciones;
            ELSE 
               LET v_tot_cargo_97 = v_tot_cargo_97 + v_p_monto_acciones;
            END IF

            --LET v_h_id_his_preliquida_op28      = ; secuencia generada previamente
            LET v_h_id_det_02_op28              = v_r_preliq_id_det_02_op28;
            LET v_h_folio                       = p_folio;
            LET v_h_invadido                    = v_r_preliq_invadido;
            LET v_h_sdo_origen_sar92_invadido   = v_saldo_actual_92_inf_inv;
            LET v_h_op28_sar92_invadido         = v_r_preliq_saldo_viv_92_inv;
            --LET v_h_cargo_sar92_invadido        = ;
            LET v_h_sdo_final_sar92_invadido    = v_saldo_final_92_inv;
            LET v_h_dif_sar92_invadido          = v_dif_final_92_inv;
            LET v_h_sdo_origen_viv97_invadido   = v_saldo_actual_97_inf_inv;
            LET v_h_op28_viv97_invadido         = v_r_preliq_saldo_viv_97_inv;
            --LET v_h_cargo_viv97_invadido        = ;
            LET v_h_sdo_final_viv97_invadido    = v_saldo_final_97_inv;
            LET v_h_dif_viv97_invadido          = v_dif_final_97_inv;
            LET v_h_asociado                    = v_r_preliq_asociado;
            LET v_h_sdo_origen_sar92_asociado   = v_saldo_actual_92_inf_aso;
            LET v_h_op28_sar92_asociado         = v_r_preliq_saldo_viv_92_aso;
            --LET v_h_abono_sar92_asociado        = ;
            LET v_h_sdo_final_sar92_asociado    = v_saldo_final_92_aso;
            LET v_h_dif_sar92_asociado          = v_dif_final_92_aso;
            LET v_h_sdo_origen_viv97_asociado   = v_saldo_actual_97_inf_aso;
            LET v_h_op28_viv97_asociado         = v_r_preliq_saldo_viv_97_aso;
            --LET v_h_abono_viv97_asociado        = ;
            LET v_h_sdo_final_viv97_asociado    = v_saldo_final_97_aso;
            LET v_h_dif_viv97_asociado          = v_dif_final_97_aso;
            LET v_h_id_expediente               = 0;

            INSERT INTO sep_his_preliquida_op28 (id_his_preliquida_op28      ,
                                                 id_det_02_op28              ,
                                                 folio                       ,
                                                 invadido                    ,
                                                 sdo_origen_sar92_invadido   ,
                                                 op28_sar92_invadido         ,
                                                 cargo_sar92_invadido        ,
                                                 sdo_final_sar92_invadido    ,
                                                 dif_sar92_invadido          ,
                                                 sdo_origen_viv97_invadido   ,
                                                 op28_viv97_invadido         ,
                                                 cargo_viv97_invadido        ,
                                                 sdo_final_viv97_invadido    ,
                                                 dif_viv97_invadido          ,
                                                 asociado                    ,
                                                 sdo_origen_sar92_asociado   ,
                                                 op28_sar92_asociado         ,
                                                 abono_sar92_asociado        ,
                                                 sdo_final_sar92_asociado    ,
                                                 dif_sar92_asociado          ,
                                                 sdo_origen_viv97_asociado   ,
                                                 op28_viv97_asociado         ,
                                                 abono_viv97_asociado        ,
                                                 sdo_final_viv97_asociado     ,
                                                 dif_viv97_asociado          ,
                                                 id_expediente               )
                                         VALUES (v_h_id_his_preliquida_op28      ,
                                                 v_h_id_det_02_op28              ,
                                                 v_h_folio                       ,
                                                 v_h_invadido                    ,
                                                 v_h_sdo_origen_sar92_invadido   ,
                                                 v_h_op28_sar92_invadido         ,
                                                 v_h_cargo_sar92_invadido        ,
                                                 v_h_sdo_final_sar92_invadido    ,
                                                 v_h_dif_sar92_invadido          ,
                                                 v_h_sdo_origen_viv97_invadido   ,
                                                 v_h_op28_viv97_invadido         ,
                                                 v_h_cargo_viv97_invadido        ,
                                                 v_h_sdo_final_viv97_invadido    ,
                                                 v_h_dif_viv97_invadido          ,
                                                 v_h_asociado                    ,
                                                 v_h_sdo_origen_sar92_asociado   ,
                                                 v_h_op28_sar92_asociado         ,
                                                 v_h_abono_sar92_asociado        ,
                                                 v_h_sdo_final_sar92_asociado    ,
                                                 v_h_dif_sar92_asociado          ,
                                                 v_h_sdo_origen_viv97_asociado   ,
                                                 v_h_op28_viv97_asociado         ,
                                                 v_h_abono_viv97_asociado        ,
                                                 v_h_sdo_final_viv97_asociado     ,
                                                 v_h_dif_viv97_asociado          ,
                                                 v_h_id_expediente               );

	          -- si por lo menos se realizó algun movimiento, avanza maquinarian para el registro
                  IF(v_preliquida_92 = 1 OR v_preliquida_97 = 1)THEN
                     EXECUTE FUNCTION fn_maquinaria('maq_sep_det_op28',
                                                    10,                -- (Señal) Preliquidar
                                                    v_r_preliq_estado) -- (Estado) Integrado
                                               INTO v_cod_marca,
                                                    v_m_diag_marca,
                                                    v_estado_destino;

                    UPDATE sep_det_02_op40
                       SET estado = v_estado_destino
                     WHERE id_det_02_op40 = v_r_preliq_id_det_02_op28;

                  END IF

                 LET v_tot_parejas = v_tot_parejas + 1;
        END IF
   END FOREACH

   UPDATE sep_det_02_op40
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


