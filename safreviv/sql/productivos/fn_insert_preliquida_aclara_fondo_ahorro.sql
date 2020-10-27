






CREATE FUNCTION "safreviv".fn_insert_preliquida_aclara_fondo_ahorro(v_folio_liquida    DECIMAL(10,0),
                                                         v_proceso_cod      SMALLINT,
                                                         v_opera_cod        SMALLINT,
                                                         v_usuario_cod      VARCHAR(20),
                                                         v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

   DEFINE  v_b_paso                      SMALLINT;
   DEFINE  v_id_afi_fondo72              DECIMAL(9,0);
   DEFINE  v_id_afi_fondo72_temp         DECIMAL(9,0);
   DEFINE  v_id_derechohabiente          DECIMAL(9,0);
   DEFINE  v_id_solicitud                DECIMAL(9,0); 
   DEFINE  v_id_solicitud_cargo          DECIMAL(9,0); 
   DEFINE  v_id_solicitud_abono          DECIMAL(9,0);
   DEFINE  v_id_afi_fondo72_cargo        DECIMAL(9,0);
   DEFINE  v_id_afi_fondo72_abono        DECIMAL(9,0);
   DEFINE  v_tpo_movto                   CHAR(2);
   DEFINE  v_importe_cargo               DECIMAL(14,2);
   DEFINE  v_importe_abono               DECIMAL(14,2);
   DEFINE  v_importe                     DECIMAL(14,2);
   DEFINE  v_movimiento_cargo_ci         SMALLINT;
   DEFINE  v_movimiento_abono_ci         SMALLINT;
   DEFINE  v_movimiento_cargo_uc         SMALLINT;
   DEFINE  v_movimiento_abono_uc         SMALLINT;
   DEFINE  v_movimiento_cargo_fa         SMALLINT;
   DEFINE  v_movimiento_abono_fa         SMALLINT;
   DEFINE  v_signo_cargo_ci              SMALLINT;
   DEFINE  v_signo_abono_ci              SMALLINT;
   DEFINE  v_signo_cargo_uc              SMALLINT;
   DEFINE  v_signo_abono_uc              SMALLINT;
   DEFINE  v_signo_cargo_fa              SMALLINT;
   DEFINE  v_signo_abono_fa              SMALLINT;
   DEFINE  v_valor_mov                   SMALLINT;
   DEFINE  v_origen                      CHAR(20);
   DEFINE  v_subcuenta                   SMALLINT;
   DEFINE  v_conteo                      SMALLINT; -- conteo de coincidencias de solicitante
   DEFINE  v_marca_ci                    SMALLINT;  -- marca para Aclaracion FA Alta 811
   DEFINE  v_marca_uc                    SMALLINT;  -- marca para Aclaracion FA Unificacion 812
   DEFINE  v_marca_fa                    SMALLINT;  -- marca para Aclaracion FA Separacion 813
   DEFINE  v_error_importe               SMALLINT;
   
   DEFINE  v_resultado_consulta          SMALLINT;
                                    
   DEFINE v_i_estado_marca               INTEGER;
   DEFINE v_marca_fondo_ahorro           INTEGER; -- 802 de acuerdo a catalogo
   DEFINE v_bnd_preli                    SMALLINT;
   DEFINE v_nss                          CHAR(12);
   DEFINE r_tanto_adicional              DECIMAL(14,2);
   DEFINE v_estado_solicitud             INTEGER;  
   DEFINE v_estado_solicitud_preliq      INTEGER;  
   
   DEFINE v_bnd_saldo_igual              SMALLINT;
   DEFINE v_bnd_saldo_id_igual           SMALLINT;
   DEFINE v_bnd_saldo_id_diferencia      SMALLINT;

   DEFINE reg_movtos_fa_id_solicitud         DECIMAL(9,0);
   DEFINE reg_movtos_fa_folio                DECIMAL(9,0);
   DEFINE reg_movtos_fa_f_liquida            DATE;
   DEFINE reg_movtos_fa_subcuenta            SMALLINT;
   DEFINE reg_movtos_fa_id_afi_fondo72_cargo DECIMAL(9,0);
   DEFINE reg_movtos_fa_id_afi_fondo72_abono DECIMAL(9,0);
   DEFINE reg_movtos_fa_movimiento_cargo     SMALLINT;
   DEFINE reg_movtos_fa_movimiento_abono     SMALLINT;
   DEFINE reg_movtos_fa_importe_cargo        DECIMAL(14,2);
   DEFINE reg_movtos_fa_importe_abono        DECIMAL(14,2);
   DEFINE reg_movtos_fa_f_registro           DATE;
   DEFINE reg_movtos_fa_h_registro           DATETIME HOUR TO SECOND;
   DEFINE reg_movtos_fa_origen               CHAR(100);
   DEFINE reg_uni_nss                        CHAR(11);
   DEFINE reg_uni_rfc                        CHAR(13);
   DEFINE reg_uni_nombre                     CHAR(40);
   
   -- banderas para controlar de donde se obtendra el recurso economico
   -- para el retiro

   DEFINE v_el_saldo_es_igual            SMALLINT;
   DEFINE v_saldo_entre_varios_afi       SMALLINT;
   DEFINE v_saldo_con_sobregiro          SMALLINT;

   DEFINE p_id_afi_fondo72_temp          DECIMAL(9,0);
   DEFINE p_subcuenta                    SMALLINT;
   DEFINE p_movimiento                   SMALLINT;
   DEFINE p_folio                        DECIMAL(9,0);
   DEFINE p_id_solicitud                 DECIMAL(9,0);
   DEFINE p_pes_viv72                    DECIMAL(14,2);
   DEFINE p_tanto_adicional              DECIMAL(14,2);
   DEFINE p_saldo_diferencia             DECIMAL(14,2);  
   DEFINE v_saldo_viv72_activo           DECIMAL(14,2);
   DEFINE v_tiene_acumulado              SMALLINT;
   DEFINE v_saldo_acumulado              DECIMAL(14,2);
   DEFINE v_reg_saldos                   SMALLINT;
   
   -- Control de Excepciones
   DEFINE v_si_resultado                SMALLINT;
   DEFINE sql_err                       INTEGER;
   DEFINE isam_err                      INTEGER;
   DEFINE err_txt                       VARCHAR(250);
   DEFINE v_c_msj                       VARCHAR(250);

      -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, v_id_solicitud;
   END EXCEPTION

   -- se inician las variables para marca
   LET v_marca_fondo_ahorro          = 802; -- marca para retiro de fondo ahorro
   LET v_origen                      = "";
   LET v_subcuenta                   = 40;
   LET v_estado_solicitud            = 15; -- ACEPTADAS
   LET v_estado_solicitud_preliq     = 50; -- PRELIQUIDADAS
   LET v_marca_ci                    = 811;
   LET v_marca_uc                    = 812;
   LET v_marca_fa                    = 813;
   LET v_movimiento_cargo_ci         = 1412;
   LET v_movimiento_abono_ci         = 241;
   LET v_movimiento_cargo_uc         = 392;
   LET v_movimiento_abono_uc         = 151;
   LET v_movimiento_cargo_fa         = 382;
   LET v_movimiento_abono_fa         = 381;   

   LET v_signo_cargo_ci              = 0;
   LET v_signo_abono_ci              = 0;
   LET v_signo_cargo_uc              = 0;
   LET v_signo_abono_uc              = 0;
   LET v_signo_cargo_fa              = 0;
   LET v_signo_abono_fa              = 0;   


   LET v_i_estado_marca              = 0;
   LET v_bnd_preli                   = 0;
   LET v_b_paso                      = 0;
   LET v_bnd_saldo_igual             = 0;
   LET v_bnd_saldo_id_igual          = 0;
   LET v_bnd_saldo_id_diferencia     = 0;    
   
   LET v_importe                     = 0;
   LET v_saldo_viv72_activo          = 0;
   LET v_id_afi_fondo72              = 0;
   LET v_id_afi_fondo72_temp         = 0;
   LET v_id_derechohabiente          = 0;
   LET v_id_solicitud                = 0;
   LET v_resultado_consulta          = 0;
   
   LET p_id_afi_fondo72_temp         = 0;
   LET p_subcuenta                   = 0;
   LET p_movimiento                  = 0;
   LET p_folio                       = 0;
   LET p_id_solicitud                = 0;
   LET p_pes_viv72                   = 0;
   LET p_tanto_adicional             = 0;
   LET p_saldo_diferencia            = 0;

   -- se inician las banderas del recurso economico   
   LET v_el_saldo_es_igual           = 0;
   LET v_saldo_entre_varios_afi      = 0;
   LET v_saldo_con_sobregiro         = 0;

   -- se asume que el proceso termina bien
   LET v_si_resultado                = 0;
   LET isam_err                      = 0;
   LET v_c_msj                       = 'El proceso finalizó exitosamente.';

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_fondo_ahorro_cont.log';

   -- actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status       = 1 -- preliquidado
   WHERE  folio        = v_folio_liquida
   AND    proceso_cod  = v_proceso_cod
   AND    status       = 0;

   --actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;
   
   -- se obtiene el signo del movimiento de cargo de CI
   SELECT tipo
   INTO   v_signo_cargo_ci
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo_ci ;
   -- se obtiene el signo del movimiento de abono de CI
   SELECT tipo
   INTO   v_signo_abono_ci
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono_ci ;
   -- se obtiene el signo del movimiento de cargo de UC
   SELECT tipo
   INTO   v_signo_cargo_uc
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo_uc ;
   -- se obtiene el signo del movimiento de abono de UC
   SELECT tipo
   INTO   v_signo_abono_uc
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono_uc ;
   -- se obtiene el signo del movimiento de cargo de FA
   SELECT tipo
   INTO   v_signo_cargo_fa
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo_fa ;
   -- se obtiene el signo del movimiento de abono de FA
   SELECT tipo
   INTO   v_signo_abono_fa
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono_fa ;

   CREATE TEMP TABLE tmp_saldo_traspaso (
                                          id_afi_fondo72  DECIMAL( 9,0)
                                         ,saldo           DECIMAL(18,2)
                                         );
   
   -- se leen las solicitudes aprobadas para su preliquidacion
   FOREACH
       SELECT a.id_solicitud, a.id_afi_fondo72_cargo, a.id_solicitud_abono, a.id_afi_fondo72_abono, a.tpo_movto
         INTO v_id_solicitud_cargo             , -- id de la solicitud de cargo
              v_id_afi_fondo72_cargo           , -- id_afi_fondo72_cargo
              v_id_solicitud_abono             , --id solicitud de abono
              v_id_afi_fondo72_abono           , -- id_afi_fondo72_abono
              v_tpo_movto                        -- tipo de movimiento CI, UC, FA
         FROM ret_aclara_accion_fa a, ret_det_aclara_fondo_ahorro b
        WHERE a.folio = v_folio_liquida
          AND a.id_solicitud = b.id_solicitud
          AND b.estado_solicitud = v_estado_solicitud
        ORDER BY 1
       LET v_error_importe = 0;
       IF v_tpo_movto = "CI" OR v_tpo_movto = "UC" THEN
           SELECT SUM(NVL(importe,0))
           INTO   v_importe_cargo
           FROM   afi_fondo72 afi
                  LEFT OUTER JOIN cta_fondo72 cta
                               ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                              AND cta.subcuenta      =  v_subcuenta
                              AND (cta.movimiento    <> 422 AND  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                   cta.movimiento    <> 601)     -- 601 ABONO, RETIRO FONDO 72 NO PAGADO, TANTO 
           WHERE afi.id_afi_fondo72 = v_id_afi_fondo72_cargo;
           -- Busca si tiene saldo acumulado para incluiro en el traspaso
           LET v_tiene_acumulado = 0;
           LET v_saldo_acumulado = 0;
           LET v_reg_saldos      = 0;
           SELECT COUNT(*) 
           INTO   v_tiene_acumulado
           FROM   tmp_saldo_traspaso
           WHERE  id_afi_fondo72 = v_id_afi_fondo72_cargo;
           IF v_tiene_acumulado = 1 THEN
               SELECT saldo
               INTO   v_saldo_acumulado
               FROM   tmp_saldo_traspaso
               WHERE  id_afi_fondo72 = v_id_afi_fondo72_cargo;
           END IF
           LET v_importe_cargo = v_importe_cargo + v_saldo_acumulado;
           ---- Acumula saldos para los registros que tengan varias ocurrencias
           IF v_importe_cargo > 0 THEN 
               SELECT COUNT(*) 
               INTO   v_reg_saldos
               FROM   tmp_saldo_traspaso
               WHERE  id_afi_fondo72 = v_id_afi_fondo72_abono;
               IF v_reg_saldos <> 0 THEN 
                   UPDATE tmp_saldo_traspaso
                   SET    saldo = saldo + v_importe_cargo
                   WHERE  id_afi_fondo72 = v_id_afi_fondo72_abono;
               ELSE 
                   INSERT INTO tmp_saldo_traspaso
                   VALUES (v_id_afi_fondo72_abono, v_importe_cargo);
               END IF
           END IF

           LET reg_movtos_fa_id_solicitud          = v_id_solicitud_cargo;
           LET reg_movtos_fa_folio                 = v_folio_liquida;
           LET reg_movtos_fa_f_liquida             = today;
           LET reg_movtos_fa_subcuenta             = v_subcuenta;
           LET reg_movtos_fa_id_afi_fondo72_cargo  = v_id_afi_fondo72_cargo;
           LET reg_movtos_fa_id_afi_fondo72_abono  = v_id_afi_fondo72_abono;
           IF v_tpo_movto = "CI" THEN 
               LET reg_movtos_fa_movimiento_cargo      = v_movimiento_cargo_ci;
               LET reg_movtos_fa_movimiento_abono      = v_movimiento_abono_ci;
               LET reg_movtos_fa_origen                = "Aclaraciones Fondo Anterior CI";
               LET reg_movtos_fa_importe_cargo         = v_importe_cargo * v_signo_cargo_ci;
               LET reg_movtos_fa_importe_abono         = v_importe_cargo * v_signo_abono_ci;
               
           ELSE
               LET reg_movtos_fa_movimiento_cargo      = v_movimiento_cargo_uc;
               LET reg_movtos_fa_movimiento_abono      = v_movimiento_abono_uc;
               LET reg_movtos_fa_origen                = "Aclaraciones Fondo Anterior UC";
               LET reg_movtos_fa_importe_cargo         = v_importe_cargo * v_signo_cargo_uc;
               LET reg_movtos_fa_importe_abono         = v_importe_cargo * v_signo_abono_uc;
           END IF

           LET reg_movtos_fa_f_registro            = today;
           LET reg_movtos_fa_h_registro            = CURRENT HOUR TO SECOND;
           IF v_tpo_movto = "UC" THEN
               SELECT nss, rfc, nombre
               INTO   reg_uni_nss, reg_uni_rfc, reg_uni_nombre
               FROM   afi_fondo72
               WHERE  id_afi_fondo72 = reg_movtos_fa_id_afi_fondo72_cargo;
               INSERT INTO uni_preunifica_fondo72
                           (
                            folio_preunifica,
                            id_afi_fondo72,
                            nss,
                            rfc,
                            nombre,
                            fecha_liquidacion,
                            folio_liquidacion,
                            movimiento,
                            origen,
                            monto_pesos,
                            tipo_nss,
                            diagnostico
                           )
                    VALUES (
                            reg_movtos_fa_folio,
                            reg_movtos_fa_id_afi_fondo72_cargo,
                            reg_uni_nss,
                            reg_uni_rfc,
                            reg_uni_nombre,
                            today,
                            reg_movtos_fa_folio,
                            0,
                            "ACLARACIONES FONDO DE AHORRO " || reg_movtos_fa_id_solicitud,
                            reg_movtos_fa_importe_abono,
                            2,   -- Unificado
                            3 ---- Se deja como liquidado para que no se incluya en el proceso normal de unificacion
                           );
               SELECT nss, rfc, nombre
               INTO   reg_uni_nss, reg_uni_rfc, reg_uni_nombre
               FROM   afi_fondo72
               WHERE  id_afi_fondo72 = reg_movtos_fa_id_afi_fondo72_abono;
               INSERT INTO uni_preunifica_fondo72
                           (
                            folio_preunifica,
                            id_afi_fondo72,
                            nss,
                            rfc,
                            nombre,
                            fecha_liquidacion,
                            folio_liquidacion,
                            movimiento,
                            origen,
                            monto_pesos,
                            tipo_nss,
                            diagnostico
                           )
                    VALUES (
                            reg_movtos_fa_folio,
                            reg_movtos_fa_id_afi_fondo72_abono,
                            reg_uni_nss,
                            reg_uni_rfc,
                            reg_uni_nombre,
                            today,
                            reg_movtos_fa_folio,
                            0,
                            "ACLARACIONES FONDO DE AHORRO " || reg_movtos_fa_id_solicitud,
                            reg_movtos_fa_importe_abono,
                            1,   -- Unificador
                            3 ---- Se deja como liquidado para que no se incluya en el proceso normal de unificacion
                           );
           END IF

       END IF 
       IF v_tpo_movto = "FA" THEN
           SELECT SUM(NVL(importe,0))
           INTO   v_importe_cargo
           FROM   afi_fondo72 afi
                  LEFT OUTER JOIN cta_fondo72 cta
                               ON afi.id_afi_fondo72 =  cta.id_afi_fondo72
                              AND cta.subcuenta      =  v_subcuenta
                              AND (cta.movimiento    <> 422 AND  -- 422 CARGO RETIRO FONDO 72-92, TANTO ADICIONAL
                                   cta.movimiento    <> 601)     -- 601 ABONO, RETIRO FONDO 72 NO PAGADO, TANTO 
           WHERE afi.id_afi_fondo72 = v_id_afi_fondo72_cargo;
           
           SELECT imp_movto
           INTO   v_importe
           FROM   ret_det_aclara_fondo_ahorro
           WHERE  id_solicitud = v_id_solicitud_cargo;

           IF v_importe > v_importe_cargo THEN
               SELECT id_derechohabiente
               INTO   v_id_derechohabiente
               FROM   afi_fondo72
               WHERE  id_afi_fondo72 = v_id_afi_fondo72_cargo;
               -- Desmarca la cuenta de cargo
               EXECUTE FUNCTION fn_desmarca_cuenta(
                   v_id_derechohabiente
                   ,v_marca_fa
                   ,v_id_solicitud_cargo
                   ,0
                   ,0
                   ,v_usuario_cod
                   ,v_proceso_cod) INTO v_i_estado_marca;

               SELECT id_derechohabiente
               INTO   v_id_derechohabiente
               FROM   afi_fondo72
               WHERE  id_afi_fondo72 = v_id_afi_fondo72_abono;
               -- Se actualiza la solictud con estatus de rechazo y codigo de rechazo 854 por Importe Incorrecto
               UPDATE ret_det_aclara_fondo_ahorro
               SET    estado_solicitud = 100,
                      cod_rechazo      = 854
               WHERE  id_solicitud     = v_id_solicitud_cargo
                  OR  id_solicitud     = v_id_solicitud_abono;                   
               LET v_error_importe = 1;
           ELSE 
               LET v_importe_cargo = v_importe_cargo - v_importe;

               SELECT imp_movto
               INTO   v_importe
               FROM   ret_det_aclara_fondo_ahorro
               WHERE  id_solicitud = v_id_solicitud_abono;

               IF v_importe_cargo <> v_importe THEN
                   SELECT id_derechohabiente
                   INTO   v_id_derechohabiente
                   FROM   afi_fondo72
                   WHERE  id_afi_fondo72 = v_id_afi_fondo72_cargo;
                   -- Desmarca la cuenta de cargo
                   EXECUTE FUNCTION fn_desmarca_cuenta(
                       v_id_derechohabiente
                       ,v_marca_fa
                       ,v_id_solicitud_cargo
                       ,0
                       ,0
                       ,v_usuario_cod
                       ,v_proceso_cod) INTO v_i_estado_marca;

                   SELECT id_derechohabiente
                   INTO   v_id_derechohabiente
                   FROM   afi_fondo72
                   WHERE  id_afi_fondo72 = v_id_afi_fondo72_abono;
                   -- Se actualiza la solictud con estatus de rechazo y codigo de rechazo 853 por Importes Diferentes
                   UPDATE ret_det_aclara_fondo_ahorro
                   SET    estado_solicitud = 100,
                          cod_rechazo      = 853
                   WHERE  id_solicitud     = v_id_solicitud_cargo
                      OR  id_solicitud     = v_id_solicitud_abono;                   
                   LET v_error_importe = 1;
               ELSE 
                   LET reg_movtos_fa_id_solicitud          = v_id_solicitud_cargo;
                   LET reg_movtos_fa_folio                 = v_folio_liquida;
                   LET reg_movtos_fa_f_liquida             = today;
                   LET reg_movtos_fa_subcuenta             = v_subcuenta;
                   LET reg_movtos_fa_id_afi_fondo72_cargo  = v_id_afi_fondo72_cargo;
                   LET reg_movtos_fa_id_afi_fondo72_abono  = v_id_afi_fondo72_abono;
                   LET reg_movtos_fa_movimiento_cargo      = v_movimiento_cargo_fa;
                   LET reg_movtos_fa_movimiento_abono      = v_movimiento_abono_fa;
                   LET reg_movtos_fa_importe_cargo         = v_importe_cargo * v_signo_cargo_fa;
                   LET reg_movtos_fa_importe_abono         = v_importe_cargo * v_signo_abono_fa;
                   LET reg_movtos_fa_f_registro            = today;
                   LET reg_movtos_fa_h_registro            = CURRENT HOUR TO SECOND;
                   LET reg_movtos_fa_origen                = "Aclaraciones Fondo Anterior FA";
                   -- Registra en el historico de separacion
                   EXECUTE FUNCTION sp_sep_inserta_exp_f72(reg_movtos_fa_id_afi_fondo72_cargo,
                                                           reg_movtos_fa_id_afi_fondo72_abono,
                                                           reg_movtos_fa_importe_abono) 
                               INTO sql_err, isam_err, err_txt;                   
               END IF 
           END IF 
       END IF 
       IF v_error_importe = 0 THEN 
           IF reg_movtos_fa_importe_abono > 0 THEN
               --  Inserta el movimiento de cargo
               INSERT INTO ret_preliquida_afa
                           (
                            id_afi_fondo72,
                            f_liquida,
                            subcuenta,
                            movimiento,
                            folio_liquida,
                            id_referencia,
                            importe,
                            f_registro,
                            h_registro,
                            origen
                           )
                    VALUES (
                            reg_movtos_fa_id_afi_fondo72_cargo,
                            reg_movtos_fa_f_liquida,
                            reg_movtos_fa_subcuenta,
                            reg_movtos_fa_movimiento_cargo,
                            reg_movtos_fa_folio,
                            reg_movtos_fa_id_solicitud,
                            reg_movtos_fa_importe_cargo,
                            reg_movtos_fa_f_registro,
                            reg_movtos_fa_h_registro,
                            reg_movtos_fa_origen
                           ); 
               --  Inserta el movimiento de abono
               INSERT INTO ret_preliquida_afa
                           (
                            id_afi_fondo72,
                            f_liquida,
                            subcuenta,
                            movimiento,
                            folio_liquida,
                            id_referencia,
                            importe,
                            f_registro,
                            h_registro,
                            origen
                           )
                    VALUES (
                            reg_movtos_fa_id_afi_fondo72_abono,
                            reg_movtos_fa_f_liquida,
                            reg_movtos_fa_subcuenta,
                            reg_movtos_fa_movimiento_abono,
                            reg_movtos_fa_folio,
                            reg_movtos_fa_id_solicitud,
                            reg_movtos_fa_importe_abono,
                            reg_movtos_fa_f_registro,
                            reg_movtos_fa_h_registro,
                            reg_movtos_fa_origen
                           ); 

           END IF
           UPDATE ret_det_aclara_fondo_ahorro
           SET    estado_solicitud = v_estado_solicitud_preliq
           WHERE  id_solicitud = reg_movtos_fa_id_solicitud;
           IF v_tpo_movto = "FA" THEN
               UPDATE ret_det_aclara_fondo_ahorro
               SET    estado_solicitud = v_estado_solicitud_preliq
               WHERE  id_solicitud = v_id_solicitud_abono;
           END IF 
           LET v_bnd_preli = v_bnd_preli + 1;
       END IF
   END FOREACH; -- para cada solicitud aceptada

   IF ( v_bnd_preli = 0 ) THEN
      -- no se preliquidaron registros
      LET v_si_resultado = 1000;
      LET v_c_msj        = "No se preliquidaron registros. Favor de verificar";
   END IF

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida_afa;
   
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, v_id_solicitud;
END FUNCTION;


