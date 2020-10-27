






CREATE FUNCTION "safreviv".fn_ret_preliquida_ssv(v_folio_liquida    DECIMAL(10,0),
                                                   v_proceso_cod      SMALLINT,
                                                   v_opera_cod        SMALLINT,
                                                   v_usuario_cod      VARCHAR(20),
                                                   v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

DEFINE  v_movimiento_cargo     SMALLINT;
DEFINE  v_valor_mov            SMALLINT;
DEFINE  v_origen               CHAR(20);
DEFINE  v_subcuenta_tesofe     SMALLINT;
DEFINE  v_subcuenta_viv97      SMALLINT;
DEFINE  v_subcuenta_viv92      SMALLINT;
DEFINE  v_fch_valuacion        DATE;
DEFINE  v_precio_fondo         DECIMAL(9,6);
DEFINE  v_saldo_tesofe         DECIMAL(12,2);
DEFINE  v_saldo_tesofe_p       DECIMAL(12,2);
DEFINE  v_saldo_aivs_viv97     DECIMAL(18,6);
DEFINE  v_saldo_aivs_viv92     DECIMAL(18,6);
DEFINE  v_saldo_pesos_viv97    DECIMAL(12,2);
DEFINE  v_saldo_pesos_viv92    DECIMAL(12,2);
DEFINE  v_remanente            DECIMAL(12,2);
DEFINE  v_aivs_remanente       DECIMAL(18,6);
DEFINE  v_resultado_exec       INTEGER;
DEFINE  v_marca_cargo_ssv      SMALLINT;
DEFINE  v_estado_marca         SMALLINT;
DEFINE  v_marca_causa          SMALLINT;
DEFINE  v_i_estado_marca       SMALLINT;
-- para calcular el saldo del trabajador
DEFINE  v_fondo                SMALLINT; -- fondo de inversion
-- registro_ssv
DEFINE registro_ssv_id_derechohabiente        DECIMAL(9,0) ;
DEFINE registro_ssv_id_solicitud              DECIMAL(9,0) ;
DEFINE registro_ssv_folio                     DECIMAL(9,0) ;
DEFINE registro_ssv_estado_solicitud          SMALLINT     ;
DEFINE registro_ssv_cod_rechazo               SMALLINT     ;
DEFINE registro_ssv_nss                       CHAR(11)     ;
DEFINE registro_ssv_importe                   DECIMAL(13,2);
DEFINE registro_ssv_fch_contable              DATE;
DEFINE registro_ssv_grupo                     SMALLINT;
DEFINE registro_ssv_via_pago                  CHAR(8);
DEFINE registro_ssv_tipo_sol                  CHAR(4);
DEFINE v_error_det_nss_no_encontrado          INTEGER;
DEFINE v_error_det_tpo_registro_invalido      INTEGER;
DEFINE v_error_monto_invalido                 INTEGER; -- error 
DEFINE v_error_marca                          INTEGER;
DEFINE v_error_fecha_contable                 INTEGER;
DEFINE v_error_sin_precio                     INTEGER;
DEFINE v_bnd_preli                            SMALLINT;
DEFINE v_movtos_4_8                           SMALLINT;
DEFINE v_movtos_unifica                       SMALLINT;
DEFINE v_marca_tramite                        SMALLINT;
DEFINE v_marca_unifica                        SMALLINT;
DEFINE v_marca_unifica_150_151                SMALLINT;
DEFINE v_marca_retiro                         SMALLINT;
DEFINE v_referencia_unificado                 DECIMAL(10,0);
DEFINE v_id_derechohabiente_unificador        DECIMAL(10,0);
DEFINE v_nss_unificador                       CHAR(11);
DEFINE v_cargo_comp_1_1                       SMALLINT;
DEFINE v_abono_comp_1_1                       SMALLINT;
DEFINE v_cargo_1_2                            SMALLINT;
DEFINE v_abono_comp_1_2                       SMALLINT;
DEFINE v_cargo_comp_1_2                       SMALLINT;
DEFINE v_cargo_1_3                            SMALLINT;
DEFINE v_abono_comp_1_3                       SMALLINT;
DEFINE v_cargo_comp_1_3                       SMALLINT;
DEFINE v_cargo_1_4                            SMALLINT;
DEFINE v_abono_comp_1_4                       SMALLINT;
DEFINE v_cargo_comp_1_4                       SMALLINT;
DEFINE v_cargo_1_5                            SMALLINT;
DEFINE v_abono_comp_1_5                       SMALLINT;
DEFINE v_cargo_comp_1_5                       SMALLINT;
DEFINE v_cargo_1_6                            SMALLINT;
DEFINE v_abono_comp_1_6                       SMALLINT;
DEFINE v_cargo_comp_1_6                       SMALLINT;
DEFINE v_cargo_1_7                            SMALLINT;
DEFINE v_abono_comp_1_7                       SMALLINT;
DEFINE v_cargo_comp_1_7                       SMALLINT;
DEFINE v_tipo_solicitud                       CHAR(4);
DEFINE v_signo_abono                          SMALLINT;
DEFINE v_signo_cargo                          SMALLINT;
DEFINE v_cargo                                SMALLINT;
DEFINE v_abono_comp                           SMALLINT;
DEFINE v_cargo_comp                           SMALLINT;
DEFINE v_monto_acciones                       DECIMAL(18,6);
DEFINE v_monto_pesos                          DECIMAL(18,2);
DEFINE v_dif_retiro                           DECIMAL(18,2);
DEFINE v_movtos_credito                       SMALLINT;
DEFINE v_total_movtos_credito                 SMALLINT;
DEFINE v_query                                CHAR(5000);
DEFINE v_tabla                                CHAR(25);
-- Control de Excepciones
DEFINE v_si_resultado          SMALLINT;
DEFINE sql_err                 INTEGER;
DEFINE isam_err                INTEGER;
DEFINE err_txt                 VARCHAR(250);
DEFINE v_c_msj                 VARCHAR(250);
   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt, registro_ssv_id_solicitud;
   END EXCEPTION

   -- se actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status      =  1
   WHERE  folio       = v_folio_liquida;
   -- actualiza folio en la operacion y proceso
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;
   -- se inician las variables para marca
   LET v_bnd_preli = 0;
   LET registro_ssv_id_derechohabiente = 0 ;
   LET registro_ssv_id_solicitud = 0;
   LET v_movimiento_cargo = 1802; -- CARGO A LA SSV VIA SIAFF
   LET v_origen = "";
   LET v_subcuenta_tesofe = 47; -- TESOFE
   LET v_subcuenta_viv97 = 4;
   LET v_subcuenta_viv92 = 8;
   LET v_c_msj = "El proceso de preliquidación finalizó correctamente.";
   LET isam_err = 0;
   LET v_si_resultado = 0;
   LET v_fch_valuacion = NULL;
   LET v_precio_fondo = 0.000000;
   LET v_saldo_tesofe = 0.00;
   LET v_saldo_tesofe_p = 0.00;
   LET v_saldo_aivs_viv97 = 0.000000;
   LET v_saldo_aivs_viv92 = 0.000000;
   LET v_saldo_pesos_viv97 = 0.00;
   LET v_saldo_pesos_viv92 = 0.00;
   LET v_remanente = 0.00;
   LET v_aivs_remanente = 0.000000;
   LET v_marca_cargo_ssv = 819;
   LET v_estado_marca = 0;
   LET v_marca_causa = 0;
   LET v_i_estado_marca = 0;
   LET v_error_det_nss_no_encontrado = 1;
   LET v_error_det_tpo_registro_invalido = 2;
   LET v_error_monto_invalido = 11;
   LET v_error_marca = 12;
   LET v_error_fecha_contable = 13;
   LET v_error_sin_precio = 14;
   LET v_cargo_comp_1_1 = 1832;
   LET v_abono_comp_1_1 = 1701;
   LET v_cargo_1_2 = 1842;
   LET v_abono_comp_1_2 = 1691;
   LET v_cargo_comp_1_2 = 1852;
   LET v_cargo_1_3 = 1862;
   LET v_abono_comp_1_3 = 1711;
   LET v_cargo_comp_1_3 = 1872;
   LET v_cargo_1_4 = 1882;
   LET v_abono_comp_1_4 = 1721;
   LET v_cargo_comp_1_4 = 1892;
   LET v_cargo_1_5 = 1902;
   LET v_abono_comp_1_5 = 1731;
   LET v_cargo_comp_1_5 = 1912;
   LET v_cargo_1_6 = 1922;
   LET v_abono_comp_1_6 = 1741;
   LET v_cargo_comp_1_6 = 1932;
   LET v_cargo_1_7 = 1942;
   LET v_abono_comp_1_7 = 1751;
   LET v_cargo_comp_1_7 = 1952;
   LET v_tipo_solicitud = "";
   LET v_signo_abono = 1;
   LET v_signo_cargo = -1;
   LET v_cargo = 0;
   LET v_abono_comp = 0;
   LET v_cargo_comp = 0;
   LET v_movtos_credito = 0;
   LET v_total_movtos_credito = 0;
   LET v_monto_acciones = 0.0;
   LET v_monto_pesos = 0.0;
   -- se inician las variables para calculo de saldo 
   LET v_fondo = 10; -- FONDO cuya aiv cuesta 1 peso siempre
   LET v_monto_acciones = 0;

   --SET DEBUG FILE TO "/home/rperez/Infonavit/log/fn_ret_preliquida_ssv.log";
   -- se obtiene el signo del movimiento de retiro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo;
   --trace on;
   -- busca registros en estatus de capturado
   FOREACH 
      SELECT 
         id_solicitud,
         folio,
         estado_solicitud,
         cod_rechazo,
         nss,
         importe,
         fch_contable,
         grupo,
         via_pago,
         tipo_sol
      INTO 
          registro_ssv_id_solicitud,
          registro_ssv_folio,
          registro_ssv_estado_solicitud,
          registro_ssv_cod_rechazo,
          registro_ssv_nss,
          registro_ssv_importe,
          registro_ssv_fch_contable,
          registro_ssv_grupo,
          registro_ssv_via_pago,
          registro_ssv_tipo_sol
      FROM  ret_cargos_ssv_siaff
      WHERE estado_solicitud = 10 -- aceptados
      AND   folio            = v_folio_liquida
      -- BUsca el id_derechohabiente para su inegracion en la tabla de preliquidacion
      LET registro_ssv_id_derechohabiente = 0;
      LET v_movtos_4_8 = 0;
      LET v_movtos_unifica = 0;
      LET v_marca_tramite = 0;
      LET v_marca_retiro = 0;
      LET v_marca_unifica_150_151 = 0;
      LET v_marca_unifica = 0;
      LET v_referencia_unificado = 0;
      LET v_id_derechohabiente_unificador = 0;
      LET v_nss_unificador = "";
      LET v_monto_acciones = 0.0;
      LET v_monto_pesos = 0.0;
      LET v_movtos_credito = 0;
      LET v_total_movtos_credito = 0;
      LET v_cargo = 0;
      LET v_abono_comp = 0;
      LET v_cargo_comp = 0;
      
      IF registro_ssv_tipo_sol IS NULL OR registro_ssv_tipo_sol = "" THEN 
         LET v_tipo_solicitud = "0.0";
      ELSE 
         LET v_tipo_solicitud = registro_ssv_tipo_sol;
      END IF 

      SELECT id_derechohabiente 
      INTO   registro_ssv_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = registro_ssv_nss;
      -- se inserta el monto en la tabla de preliquidacion
      IF registro_ssv_id_derechohabiente IS NULL THEN
         -- no se podra insertar en movimientos si no tiene el id_derechohabiente
         UPDATE ret_cargos_ssv_siaff
         SET    estado_solicitud = 100,
                cod_rechazo = 7
         WHERE  nss = registro_ssv_nss
         AND    id_solicitud = registro_ssv_id_solicitud
         AND    folio = v_folio_liquida
         AND    estado_solicitud = 10;
      ELSE
         -- Busca fecha liquidacion 
         SELECT MONTH(fch_contable) || ",1," ||  YEAR(fch_contable) 
         INTO   v_fch_valuacion
         FROM   ret_cargos_ssv_siaff
         WHERE  id_solicitud = registro_ssv_id_solicitud;
         IF v_fch_valuacion IS NOT NULL THEN 
            -- Busca el precio de valuacion
            SELECT precio_fondo
            INTO   v_precio_fondo
            FROM   glo_valor_fondo
            WHERE  f_valuacion = v_fch_valuacion
            AND    fondo = 11;
            IF v_precio_fondo IS NOT NULL THEN 
               EXECUTE FUNCTION fn_saldo_dia(
                  registro_ssv_nss
                ,registro_ssv_id_derechohabiente
                ,v_subcuenta_tesofe -- marca de disposicion
                ,today)
               INTO v_resultado_exec, v_saldo_tesofe, v_saldo_tesofe_p;
               EXECUTE FUNCTION fn_saldo_dia(
                 registro_ssv_nss
                ,registro_ssv_id_derechohabiente
                ,v_subcuenta_viv97 -- marca de disposicion
                ,today)
               INTO v_resultado_exec, v_saldo_aivs_viv97, v_saldo_pesos_viv97;
               LET v_saldo_pesos_viv97 = ROUND((v_saldo_aivs_viv97 * v_precio_fondo),2);
               EXECUTE FUNCTION fn_saldo_dia(
                 registro_ssv_nss
                ,registro_ssv_id_derechohabiente
                ,v_subcuenta_viv92 -- marca de disposicion
                ,today)
               INTO v_resultado_exec, v_saldo_aivs_viv92, v_saldo_pesos_viv92;
               LET v_saldo_pesos_viv92 = ROUND((v_saldo_aivs_viv92 * v_precio_fondo),2);
               LET v_origen = TRIM(registro_ssv_via_pago) || " " || registro_ssv_grupo; 

               IF registro_ssv_via_pago = "ADAI"  AND 
                  (registro_ssv_importe <= (v_saldo_tesofe_p + v_saldo_pesos_viv97)) THEN 
                  IF v_saldo_tesofe_p > 0 THEN 
                     IF v_saldo_tesofe_p <= registro_ssv_importe THEN
                        INSERT INTO ret_preliquida 
                        VALUES (
                           TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_movimiento_cargo,
                           v_folio_liquida,registro_ssv_id_solicitud,v_saldo_tesofe *  v_valor_mov,v_saldo_tesofe_p *  v_valor_mov,
                           TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                        LET v_remanente =  (registro_ssv_importe - v_saldo_tesofe_p);
                        IF v_remanente > 0 THEN 
                           LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                           INSERT INTO ret_preliquida 
                           VALUES (
                              TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                              v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,v_remanente *  v_valor_mov,
                              TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                        END IF 
                     ELSE 
                        INSERT INTO ret_preliquida 
                        VALUES (
                           TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_movimiento_cargo,
                           v_folio_liquida,registro_ssv_id_solicitud,registro_ssv_importe *  v_valor_mov,registro_ssv_importe *  v_valor_mov,
                           TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                     END IF 
                  ELSE 
                     LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                     INSERT INTO ret_preliquida 
                     VALUES (
                        TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                        v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,registro_ssv_importe *  v_valor_mov,
                        TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                  END IF    
                  UPDATE ret_cargos_ssv_siaff
                  SET    tipo_sol = "1.8"      ----- SIN DIFERENCIAS
                  WHERE  id_solicitud = registro_ssv_id_solicitud;
               ELSE 
                  IF ((registro_ssv_importe > (v_saldo_tesofe_p + v_saldo_pesos_viv97 + v_saldo_pesos_viv92)) OR 
                     (registro_ssv_via_pago = "ADAI" AND (registro_ssv_importe > (v_saldo_tesofe_p + v_saldo_pesos_viv97)))) THEN 
                     SELECT COUNT(*)
                     INTO   v_marca_tramite
                     FROM   sfr_marca_activa
                     WHERE  id_derechohabiente = registro_ssv_id_derechohabiente
                     AND    marca IN (596, 594, 595, 590, 591, 597);     --- Tramite Judicial
                                   
                     SELECT COUNT(*)
                     INTO   v_marca_retiro
                     FROM   sfr_marca_historica
                     WHERE  id_derechohabiente = registro_ssv_id_derechohabiente
                     AND    marca IN (803, 805, 806, 808, 815);          --- Retiros
                                   
                     SELECT COUNT(*)
                     INTO   v_marca_unifica
                     FROM   sfr_marca_activa
                     WHERE  id_derechohabiente = registro_ssv_id_derechohabiente
                     AND    marca IN (150, 151);                         --- Unificación
                     
                     IF v_tipo_solicitud = "1.1" THEN 
                        LET v_tipo_solicitud = "1.1";
                        LET v_abono_comp     = v_abono_comp_1_1;
                        LET v_cargo_comp     = v_cargo_comp_1_1;
                     END IF 

                     IF ((v_saldo_pesos_viv97 = 0 AND     ------   SIN CAJA DE VIVIENDAS
                          v_saldo_pesos_viv92 = 0 AND
                          v_marca_retiro      = 0 AND
                          v_marca_unifica     = 0 AND 
                          v_tipo_solicitud    = "0.0") OR 
                         (registro_ssv_via_pago = "ADAI" AND 
                          (v_saldo_pesos_viv97 = 0 AND     ------   SIN CAJA DE VIVIENDAS
                           v_marca_retiro      = 0 AND
                           v_marca_unifica     = 0 AND 
                           v_tipo_solicitud    = "0.0"))
                        ) THEN 
                        LET v_movtos_4_8 = 0;
                        EXECUTE FUNCTION fn_cuenta_movimientos(
                                 registro_ssv_id_derechohabiente, '', 4) 
                        INTO v_movtos_4_8;
                        IF v_movtos_4_8 = 0 THEN
                           EXECUTE FUNCTION fn_cuenta_movimientos(
                                    registro_ssv_id_derechohabiente, '', 8) 
                           INTO v_movtos_4_8;
                           IF (v_movtos_4_8 = 0 OR 
                              registro_ssv_via_pago = "ADAI") THEN 
                              LET v_tipo_solicitud = "1.2";
                              LET v_cargo          = v_cargo_1_2;
                              LET v_abono_comp     = v_abono_comp_1_2;
                              LET v_cargo_comp     = v_cargo_comp_1_2;
                           END IF 
                        END IF 
                     END IF 

                     IF v_marca_unifica > 0 AND v_tipo_solicitud    = "0.0" THEN 
                        --- SE BUSCA AL UNIFICADOR PARA APLICARLE A ESTE LOS MOVIMIENTOS
                        SELECT marca
                        INTO   v_marca_unifica_150_151
                        FROM   sfr_marca_activa
                        WHERE  id_derechohabiente = registro_ssv_id_derechohabiente
                        AND    marca IN (150, 151);

                        IF v_marca_unifica_150_151 = 150 THEN 
                           SELECT DISTINCT a.id_derechohabiente, a.nss_unificador
                           INTO   v_id_derechohabiente_unificador, v_nss_unificador
                           FROM   uni_det_unificador a,
                                  uni_det_unificado  b
                           WHERE  a.id_unificador = b.id_unificador
                           AND    b.id_derechohabiente = registro_ssv_id_derechohabiente
                           AND    a.estado_familia = 1;
                        ELSE 

                           SELECT a.id_derechohabiente, a.nss
                           INTO   v_id_derechohabiente_unificador, v_nss_unificador
                           FROM   uni_inf_unificador a,
                                  uni_inf_unificado b 
                           WHERE  a.id_inf_unificador = b.id_unificador 
                           AND    b.id_derechohabiente = registro_ssv_id_derechohabiente
                           AND    a.estado_familia = 1;
                        END IF 

                        EXECUTE FUNCTION fn_saldo_dia(
                           v_nss_unificador
                          ,v_id_derechohabiente_unificador
                          ,v_subcuenta_tesofe -- marca de disposicion
                          ,today)
                        INTO v_resultado_exec, v_saldo_tesofe, v_saldo_tesofe_p;
                        EXECUTE FUNCTION fn_saldo_dia(
                           v_nss_unificador
                          ,v_id_derechohabiente_unificador
                          ,v_subcuenta_viv97 -- marca de disposicion
                          ,today)
                        INTO v_resultado_exec, v_saldo_aivs_viv97, v_saldo_pesos_viv97;
                        LET v_saldo_pesos_viv97 = ROUND((v_saldo_aivs_viv97 * v_precio_fondo),2);
                        EXECUTE FUNCTION fn_saldo_dia(
                           v_nss_unificador
                          ,v_id_derechohabiente_unificador
                          ,v_subcuenta_viv92 -- marca de disposicion
                          ,today)
                        INTO v_resultado_exec, v_saldo_aivs_viv92, v_saldo_pesos_viv92;
                        LET v_saldo_pesos_viv92 = ROUND((v_saldo_aivs_viv92 * v_precio_fondo),2);
                        LET registro_ssv_id_derechohabiente = v_id_derechohabiente_unificador;

                        LET v_tipo_solicitud = "1.31";
                        LET v_cargo          = v_cargo_1_3;
                        LET v_abono_comp     = v_abono_comp_1_3;
                        LET v_cargo_comp     = v_cargo_comp_1_3;
                        
                     END IF 
                     IF v_marca_tramite > 0 AND v_tipo_solicitud    = "0.0"  THEN 
                        LET v_tipo_solicitud = "1.4";
                        LET v_cargo          = v_cargo_1_4;
                        LET v_abono_comp     = v_abono_comp_1_4;
                        LET v_cargo_comp     = v_cargo_comp_1_4;
                     END IF

                     IF v_marca_retiro      > 0 AND 
                        --v_saldo_pesos_viv97 = 0 AND
                        --v_saldo_pesos_viv92 = 0 AND 
                        v_tipo_solicitud    = "0.0" THEN 
                        
                        LET v_dif_retiro = registro_ssv_importe - v_saldo_tesofe_p;
                        LET v_aivs_remanente = ROUND((v_dif_retiro / v_precio_fondo),2);     --- Se busca un retiro por esta cantidad de aivs
                        LET v_aivs_remanente = v_aivs_remanente - (v_saldo_aivs_viv97 + v_saldo_aivs_viv92);
                        LET v_query = "";
                        FOREACH 
                           SELECT tabla
                           INTO   v_tabla
                           FROM   cat_tab_movimiento
                           LET v_query = TRIM (v_query) || " SELECT SUM(monto_acciones) m_acciones, SUM(monto_pesos) m_pesos " ||
                                                           " FROM   " || v_tabla || 
                                                           " WHERE  movimiento in (192, 212, 222, 822) " ||
                                                           " AND    id_derechohabiente = " || registro_ssv_id_derechohabiente || 
                                                           " UNION ";
                        END FOREACH
                        LET v_query = TRIM (v_query) || " SELECT SUM(monto_acciones) m_acciones, SUM(monto_pesos) m_pesos " ||
                                                        " FROM   cta_movimiento " ||
                                                        " WHERE  movimiento in (192, 212, 222, 822) " ||
                                                        " AND    id_derechohabiente = " || registro_ssv_id_derechohabiente;
                        PREPARE stmt_tmp FROM v_query;
                        DECLARE cust_cur cursor FOR stmt_tmp;  
                        OPEN cust_cur;
                           WHILE (1 = 1)
                              FETCH cust_cur INTO v_monto_acciones, v_monto_pesos;
                                 IF (SQLCODE != 100) THEN
                                    IF v_monto_acciones = v_aivs_remanente*(-1) THEN 
                                       LET v_tipo_solicitud = "1.5";
                                       LET v_cargo          = v_cargo_1_5;
                                       LET v_abono_comp     = v_abono_comp_1_5;
                                       LET v_cargo_comp     = v_cargo_comp_1_5;
{                                       IF v_saldo_tesofe_p > 0 THEN 
                                          INSERT INTO ret_preliquida 
                                          VALUES (
                                             TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_cargo,
                                             v_folio_liquida,registro_ssv_id_solicitud,v_saldo_tesofe *  v_signo_cargo,v_saldo_tesofe_p *  v_signo_cargo,
                                             TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       END IF 
                                       ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_dif_retiro *  v_signo_abono,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_dif_retiro *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);   }
                                       EXIT WHILE;
                                    END IF
                                     
                                 ELSE 
                                    EXIT WHILE;
                                 END IF 
                           END WHILE
                           CLOSE cust_cur;
                           FREE cust_cur;
                           FREE stmt_tmp;
                     END IF 

                     IF v_tipo_solicitud = "0.0" THEN -----    Se buscan los movimientos de creditos 
                        LET v_total_movtos_credito = 0;
                        EXECUTE FUNCTION fn_cuenta_movimientos(
                                 registro_ssv_id_derechohabiente, 'cre', 0) 
                        INTO v_total_movtos_credito;
                        IF v_total_movtos_credito > 0 THEN 
                           LET v_tipo_solicitud = "1.6";
                           LET v_cargo          = v_cargo_1_6;
                           LET v_abono_comp     = v_abono_comp_1_6;
                           LET v_cargo_comp     = v_cargo_comp_1_6;
                        END IF 
                     END IF 

                     IF v_tipo_solicitud = "0.0" THEN -----    No se encontro en ninguno de los escenarios anteriores
                           LET v_tipo_solicitud = "1.7";
                           LET v_cargo          = v_cargo_1_7;
                           LET v_abono_comp     = v_abono_comp_1_7;
                           LET v_cargo_comp     = v_cargo_comp_1_7;
                     END IF  

---- ESCENARIO 1.1, 1.2, 1.3, 1.4, 1.6 y 1.7 UTILIZAN EL MISMO ALGORITMO DE MOVIMIENTOS
                     --IF v_tipo_solicitud <> "1.5" THEN 
                        IF v_saldo_tesofe_p > 0 THEN 
                           IF v_saldo_tesofe_p <= registro_ssv_importe THEN
                              INSERT INTO ret_preliquida 
                              VALUES (
                                 TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_cargo,
                                 v_folio_liquida,registro_ssv_id_solicitud,v_saldo_tesofe *  v_signo_cargo,v_saldo_tesofe_p *  v_signo_cargo,
                                 TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                              LET v_remanente =  (registro_ssv_importe - v_saldo_tesofe_p);
                              IF v_remanente > 0 THEN 
                                 IF v_saldo_pesos_viv97 > 0 THEN 
                                    IF v_saldo_pesos_viv97 <= v_remanente THEN 
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv97 *  v_signo_cargo,v_saldo_pesos_viv97 *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97));
                                       IF v_remanente > 0 THEN 
                                          IF v_saldo_pesos_viv92 > 0 AND registro_ssv_via_pago <> "ADAI" THEN 
                                             IF v_saldo_pesos_viv92 <= v_remanente THEN
                                                INSERT INTO ret_preliquida 
                                                VALUES (
                                                   TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                                   v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv92 *  v_signo_cargo,v_saldo_pesos_viv92 *  v_signo_cargo,
                                                   TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                                LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97 + v_saldo_pesos_viv92));
                                                IF v_remanente > 0 THEN 
                                                   LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                                   ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                                   INSERT INTO ret_preliquida 
                                                   VALUES (
                                                      TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                                      v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                                      TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                                   ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                                   INSERT INTO ret_preliquida 
                                                   VALUES (
                                                      TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                                      v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                                      TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                                   IF v_tipo_solicitud = "1.31" THEN 
                                                      LET v_tipo_solicitud = "1.32";
                                                   END IF
                                                END IF  
                                             ELSE 
                                                LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                                INSERT INTO ret_preliquida 
                                                VALUES (
                                                   TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                                   v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                                   TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             END IF 
                                          ELSE 
                                             LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                             ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             IF v_tipo_solicitud = "1.31" THEN 
                                                LET v_tipo_solicitud = "1.32";
                                             END IF
                                          END IF 
                                       END IF 
                                    ELSE 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                    END IF             
                                 ELSE 
                                    IF v_saldo_pesos_viv92 > 0 AND registro_ssv_via_pago <> "ADAI" THEN 
                                       IF v_saldo_pesos_viv92 <= v_remanente THEN 
                                          INSERT INTO ret_preliquida 
                                          VALUES (
                                             TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                             v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv92 *  v_signo_cargo,v_saldo_pesos_viv92 *  v_signo_cargo,
                                             TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                          LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97 + v_saldo_pesos_viv92));
                                          IF v_remanente > 0 THEN 
                                             LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                             ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             IF v_tipo_solicitud = "1.31" THEN 
                                                LET v_tipo_solicitud = "1.32";
                                             END IF
                                          END IF 
                                       ELSE 
                                          LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                          INSERT INTO ret_preliquida 
                                          VALUES (
                                             TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                             v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                             TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       END IF 
                                    ELSE 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       IF v_tipo_solicitud = "1.31" THEN 
                                          LET v_tipo_solicitud = "1.32";
                                       END IF
                                    END IF 
                                 END IF 
                              END IF 
                           ELSE 
                              INSERT INTO ret_preliquida 
                              VALUES (
                                 TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_cargo,
                                 v_folio_liquida,registro_ssv_id_solicitud,registro_ssv_importe *  v_signo_cargo,registro_ssv_importe *  v_signo_cargo,
                                 TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                           END IF 
                        ELSE 
                           IF v_saldo_pesos_viv97 > 0 THEN 
                              IF v_saldo_pesos_viv97 <= registro_ssv_importe THEN 
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv97 *  v_signo_cargo,v_saldo_pesos_viv97 *  v_signo_cargo,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                 LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97));
                                 IF v_remanente > 0 THEN 
                                    IF v_saldo_pesos_viv92 > 0 AND registro_ssv_via_pago <> "ADAI" THEN 
                                       IF v_saldo_pesos_viv92 <= v_remanente THEN
                                          INSERT INTO ret_preliquida 
                                          VALUES (
                                             TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                             v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv92 *  v_signo_cargo,v_saldo_pesos_viv92 *  v_signo_cargo,
                                             TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                          LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97 + v_saldo_pesos_viv92));
                                          IF v_remanente > 0 THEN 
                                             LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                             ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                             INSERT INTO ret_preliquida 
                                             VALUES (
                                                TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                                v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                                TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                             IF v_tipo_solicitud = "1.31" THEN 
                                                LET v_tipo_solicitud = "1.32";
                                             END IF
                                          END IF  
                                       ELSE 
                                          LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                          INSERT INTO ret_preliquida 
                                          VALUES (
                                             TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                             v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                             TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       END IF 
                                    ELSE 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       IF v_tipo_solicitud = "1.31" THEN 
                                          LET v_tipo_solicitud = "1.32";
                                       END IF
                                    END IF 
                                 END IF 
                              ELSE 
                                 LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,registro_ssv_importe *  v_signo_cargo,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                              END IF             
                           ELSE 
                              IF v_saldo_pesos_viv92 > 0 AND registro_ssv_via_pago <> "ADAI" THEN 
                                 IF v_saldo_pesos_viv92 <= registro_ssv_importe THEN 
                                    INSERT INTO ret_preliquida 
                                    VALUES (
                                       TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                       v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv92 *  v_signo_cargo,v_saldo_pesos_viv92 *  v_signo_cargo,
                                       TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                    LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97 + v_saldo_pesos_viv92));
                                    IF v_remanente > 0 THEN 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,v_remanente *  v_signo_abono,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,v_remanente *  v_signo_cargo,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                       IF v_tipo_solicitud = "1.31" THEN 
                                          LET v_tipo_solicitud = "1.32";
                                       END IF
                                    END IF 
                                 ELSE 
                                    LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                                    INSERT INTO ret_preliquida 
                                    VALUES (
                                       TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_cargo,
                                       v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,registro_ssv_importe *  v_signo_cargo,
                                       TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                 END IF 
                              ELSE 
                                 LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                                 ----- Abono a la subcuenta de vivienda 97 por definicion de la regla
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_abono_comp,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_abono,registro_ssv_importe *  v_signo_abono,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                 ----- Cargo a la subcuenta de vivienda 97 por definicion de la regla
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_cargo_comp,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_signo_cargo,registro_ssv_importe *  v_signo_cargo,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                 IF v_tipo_solicitud = "1.31" THEN 
                                    LET v_tipo_solicitud = "1.32";
                                 END IF
                              END IF 
                           END IF 
                        END IF  
                     --END IF 
---- ACTUALIZACION DEL TIPO DE SOLICITUD SEGUN EL ESCENARIO QUE SE PRESENTO
                     
                     UPDATE ret_cargos_ssv_siaff
                     SET    tipo_sol = v_tipo_solicitud
                     WHERE  id_solicitud = registro_ssv_id_solicitud;
                  ELSE
                     IF v_saldo_tesofe_p > 0 THEN 
                        IF v_saldo_tesofe_p <= registro_ssv_importe THEN
                           INSERT INTO ret_preliquida 
                           VALUES (
                              TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_movimiento_cargo,
                              v_folio_liquida,registro_ssv_id_solicitud,v_saldo_tesofe *  v_valor_mov,v_saldo_tesofe_p *  v_valor_mov,
                              TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                           LET v_remanente =  (registro_ssv_importe - v_saldo_tesofe_p);
                           IF v_remanente > 0 THEN 
                              IF v_saldo_pesos_viv97 > 0 THEN 
                                 IF v_saldo_pesos_viv97 <= v_remanente THEN 
                                    INSERT INTO ret_preliquida 
                                    VALUES (
                                       TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                                       v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv97 *  v_valor_mov,v_saldo_pesos_viv97 *  v_valor_mov,
                                       TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                    LET v_remanente = ((registro_ssv_importe) - (v_saldo_tesofe_p + v_saldo_pesos_viv97));
                                    IF v_remanente > 0 THEN 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_movimiento_cargo,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,v_remanente *  v_valor_mov,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                    END IF 
                                 ELSE 
                                       LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                       INSERT INTO ret_preliquida 
                                       VALUES (
                                          TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                                          v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,v_remanente *  v_valor_mov,
                                          TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                                 END IF             
                              ELSE 
                                 LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_movimiento_cargo,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,v_remanente *  v_valor_mov,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                              END IF 
                           END IF
                        ELSE          
                           INSERT INTO ret_preliquida 
                           VALUES (
                              TODAY,registro_ssv_id_derechohabiente,v_subcuenta_tesofe,10,v_movimiento_cargo,
                              v_folio_liquida,registro_ssv_id_solicitud,registro_ssv_importe *  v_valor_mov,registro_ssv_importe *  v_valor_mov,
                              TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                        END IF 

                     ELSE 
                        IF v_saldo_pesos_viv97 > 0 THEN 
                           IF v_saldo_pesos_viv97 <= registro_ssv_importe THEN 
                              INSERT INTO ret_preliquida 
                              VALUES (
                                 TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                                 v_folio_liquida,registro_ssv_id_solicitud,v_saldo_aivs_viv97 *  v_valor_mov,v_saldo_pesos_viv97 *  v_valor_mov,
                                 TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                              LET v_remanente = (registro_ssv_importe - v_saldo_pesos_viv97);
                              IF v_remanente > 0 THEN 
                                 LET v_aivs_remanente = ROUND((v_remanente / v_precio_fondo),2);
                                 INSERT INTO ret_preliquida 
                                 VALUES (
                                    TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_movimiento_cargo,
                                    v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,v_remanente *  v_valor_mov,
                                    TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                              END IF 
                           ELSE    
                              LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                              INSERT INTO ret_preliquida 
                              VALUES (
                                 TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv97,11,v_movimiento_cargo,
                                 v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,registro_ssv_importe *  v_valor_mov,
                                 TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                           END IF 
                        ELSE 
                           LET v_aivs_remanente = ROUND((registro_ssv_importe / v_precio_fondo),2);
                           INSERT INTO ret_preliquida 
                           VALUES (
                              TODAY,registro_ssv_id_derechohabiente,v_subcuenta_viv92,11,v_movimiento_cargo,
                              v_folio_liquida,registro_ssv_id_solicitud,v_aivs_remanente *  v_valor_mov,registro_ssv_importe *  v_valor_mov,
                              TODAY,TODAY,CURRENT HOUR TO SECOND,v_origen);
                        END IF 
                     END IF 
                     UPDATE ret_cargos_ssv_siaff
                     SET    tipo_sol = "1.8"      ----- SIN DIFERENCIAS
                     WHERE  id_solicitud = registro_ssv_id_solicitud;

                  END IF 
               END IF 
            ELSE 
               --- Rechaza la solicitud por no haber precio para la fecha de valuacion y desmarca la cuenta
               EXECUTE FUNCTION fn_desmarca_cuenta(
                             registro_ssv_id_derechohabiente
                            ,v_marca_cargo_ssv
                            ,registro_ssv_id_solicitud
                            ,v_estado_marca
                            ,v_marca_causa
                            ,v_usuario_cod
                            ,v_proceso_cod)
                        INTO v_i_estado_marca;
               UPDATE ret_cargos_ssv_siaff
               SET    estado_solicitud   = 100,
                      cod_rechazo        = v_error_sin_precio
               WHERE  nss                = registro_ssv_nss
               AND    id_solicitud       = registro_ssv_id_solicitud
               AND    folio              = v_folio_liquida
               AND    estado_solicitud   = 10;

            END IF 
         ELSE 
         --- Rechaza la solicitud por fecha contable nula y desmarca la cuenta
            EXECUTE FUNCTION fn_desmarca_cuenta(
                          registro_ssv_id_derechohabiente
                         ,v_marca_cargo_ssv
                         ,registro_ssv_id_solicitud
                         ,v_estado_marca
                         ,v_marca_causa
                         ,v_usuario_cod
                         ,v_proceso_cod)
                     INTO v_i_estado_marca;
            UPDATE ret_cargos_ssv_siaff
            SET    estado_solicitud   = 100,
                   cod_rechazo        = v_error_fecha_contable
            WHERE  nss                = registro_ssv_nss
            AND    id_solicitud       = registro_ssv_id_solicitud
            AND    folio              = v_folio_liquida
            AND    estado_solicitud   = 10;

         END IF 

         -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
         LET v_bnd_preli = 1;

         -- se actualica la solicitud a estado liquidado y se le asigna el folio
         UPDATE ret_cargos_ssv_siaff
         SET    estado_solicitud   = 50
         WHERE  nss                = registro_ssv_nss
         AND    id_solicitud       = registro_ssv_id_solicitud
         AND    folio              = v_folio_liquida
         AND    estado_solicitud   = 10;
      END IF 

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
   RETURN v_si_resultado, isam_err, v_c_msj, registro_ssv_id_solicitud;
END FUNCTION;


