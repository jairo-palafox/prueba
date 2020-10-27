






CREATE FUNCTION "safreviv".fn_ret_preliquida_tipo_n(p_usuario_cod        CHAR(20), 
                                         p_folio              DECIMAL(10,0),
                                         p_nombre_archivo     VARCHAR(40), 
                                         p_pid                DECIMAL(9,0),
                                         p_proceso_cod        SMALLINT,
                                         p_max_aivs_sobregiro DECIMAL(20,6)
                                         ) 
  RETURNING SMALLINT, INTEGER, VARCHAR(255)

-- detalle de la tabla historica/integrada de retiros tipo N
DEFINE ret_tipo_n_id_solicitud           DECIMAL(9,0) ;
DEFINE ret_tipo_n_id_derechohabiente     DECIMAL(9,0) ;
DEFINE ret_tipo_n_id_decreto             DECIMAL(9,0) ;
DEFINE ret_tipo_n_diag_registro          CHAR(3)      ;
DEFINE ret_tipo_n_folio                  DECIMAL(9,0) ;
DEFINE ret_tipo_n_id_ret_matriz_derecho  SMALLINT     ;
DEFINE ret_tipo_n_nss_icefa              CHAR(11)     ;
DEFINE ret_tipo_n_rfc_icefa              CHAR(13)     ;
DEFINE ret_tipo_n_num_ctr_interno        CHAR(30)     ;
DEFINE ret_tipo_n_cve_icefa              CHAR(3)      ;
DEFINE ret_tipo_n_nombre_icefa           CHAR(120)    ;
DEFINE ret_tipo_n_rfc                    CHAR(13)     ;
DEFINE ret_tipo_n_nombre                 CHAR(120)    ;
DEFINE ret_tipo_n_cve_doc_probatorio     CHAR(1)      ;
DEFINE ret_tipo_n_num_referencia         CHAR(18)     ;
DEFINE ret_tipo_n_origen_retiro          CHAR(1)      ;
DEFINE ret_tipo_n_tpo_seguro             CHAR(2)      ;
DEFINE ret_tipo_n_tpo_pension            CHAR(2)      ;
DEFINE ret_tipo_n_tpo_prestacion         CHAR(2)      ;
DEFINE ret_tipo_n_regimen                SMALLINT     ;
DEFINE ret_tipo_n_f_inicio_pension       DATE         ;
DEFINE ret_tipo_n_f_resolucion           DATE         ;
DEFINE ret_tipo_n_porcentaje_valuacion   DECIMAL(5,2) ;
DEFINE ret_tipo_n_actuario               CHAR(7)      ;
DEFINE ret_tipo_n_num_plan_privado       CHAR(8)      ;
DEFINE ret_tipo_n_importe_sar92          DECIMAL(15,2);
DEFINE ret_tipo_n_aivs_viv92             DECIMAL(16,6);
DEFINE ret_tipo_n_cve_afore              SMALLINT     ;
DEFINE ret_tipo_n_estado_solicitud       SMALLINT     ;
   
-- para obtener el ind_consistencia del id_decreto
DEFINE v_ind_consistencia                SMALLINT;

-- campos de la tabla de preliquidacion de retiros
DEFINE ret_preliquida_f_liquida          DATE                   ;
--DEFINE ret_preliquida_id_derechohabiente DECIMAL(9,0)           ;
DEFINE ret_preliquida_id_decreto         DECIMAL(9,0)           ;
DEFINE ret_preliquida_subcuenta          SMALLINT               ;
DEFINE ret_preliquida_fondo_inversion    SMALLINT               ;
DEFINE ret_preliquida_movimiento         SMALLINT               ;
DEFINE ret_preliquida_folio_liquida      DECIMAL(9,0)           ;
DEFINE ret_preliquida_id_referencia      DECIMAL(9,0)           ;
DEFINE ret_preliquida_monto_acciones     DECIMAL(20,6)          ;
DEFINE ret_preliquida_monto_pesos        DECIMAL(20,2)          ;
DEFINE ret_preliquida_f_valor            DATE                   ;
DEFINE ret_preliquida_f_registro         DATE                   ;
DEFINE ret_preliquida_h_registro         DATETIME HOUR TO SECOND;
DEFINE ret_preliquida_origen             CHAR(20) ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
-- para calcular las AIVs a pesos
DEFINE v_valor_fondo                        DECIMAL(14,6);
DEFINE v_pesos_aiv97                        DECIMAL(14,6);
DEFINE v_pesos_aiv92                        DECIMAL(14,6);
DEFINE v_fecha_valor                        DATE         ;
-- =============================================================================
-- signo del movimiento
DEFINE v_signo_movimiento_CONSISTENTE             SMALLINT;
DEFINE v_signo_movimiento_sobregiro_CONSISTENTE   SMALLINT;
DEFINE v_signo_movimiento_INCONSISTENTE           SMALLINT;
DEFINE v_signo_movimiento_sobregiro_INCONSISTENTE SMALLINT;   
DEFINE v_diferencia_aivs                    DECIMAL(14,6);
DEFINE v_movimiento                         SMALLINT;
DEFINE v_movimiento_sobregiro_consistente_862    SMALLINT; 
DEFINE v_movimiento_sobregiro_inconsistente_1052 SMALLINT; 

-- para marcar las cuentas
DEFINE v_i_estado_marca                        INTEGER;
DEFINE v_marca_tipo_n                          INTEGER; -- 804 de acuerdo a catalogo

DEFINE v_datos_validos_tpo_registro            CHAR(2);
DEFINE v_datos_validos_id_servicio             CHAR(2);
DEFINE v_datos_validos_f_operacion             CHAR(8);
DEFINE v_dte_f_valido_f_operacion              DATE;
--
DEFINE v_datos_top_validos_tpo_registro        CHAR(2);
DEFINE v_datos_top_validos_id_servicio         CHAR(2);
DEFINE v_datos_top_validos_id_operacion        CHAR(2);
DEFINE v_datos_top_validos_tpo_entidad_destino CHAR(2);
DEFINE v_datos_top_validos_cve_entidad_destino CHAR(3);
DEFINE v_datos_top_validos_f_operacion         CHAR(8);
--
DEFINE v_sumario_importe_total                 DECIMAL(22,2);
DEFINE v_sumario_aiv_97                        DECIMAL(18,6);
DEFINE v_sumario_aiv_92                        DECIMAL(18,6);
DEFINE v_detalle_importe_total                 DECIMAL(22,2);
DEFINE v_detalle_aiv_97                        DECIMAL(18,6);
DEFINE v_detalle_aiv_92                        DECIMAL(18,6);

DEFINE v_saldo_92_aivs                         DECIMAL(16,6); -- saldo del derechohabiente en viv92
DEFINE v_saldo_92_pesos                        DECIMAL(12,2); -- saldo del derechohabiente en viv97
DEFINE v_resultado_consulta                    SMALLINT;

DEFINE v_rechazo_viv92                         SMALLINT; -- rechazo de retiro de vivienda 92 
DEFINE cod_rechazo_1                           SMALLINT;
DEFINE cod_rechazo_2                           SMALLINT;
DEFINE cod_rechazo_3                           SMALLINT;
--
DEFINE v_folio                                 DECIMAL(10);
DEFINE v_nombre_archivo                        CHAR(18);
DEFINE v_f_operacion_procesar                  DATE;
DEFINE v_f_carga_afore                         DATE;
DEFINE v_resultado_operacion                   SMALLINT;
DEFINE v_cod_rechazo_1                         SMALLINT;
DEFINE v_cod_rechazo_2                         SMALLINT;
DEFINE v_cod_rechazo_3                         SMALLINT;
DEFINE v_num_reg_preliquidados                 INTEGER; -- numero de registros preliquidados
DEFINE v_fecha_valuacion_nueva                 VARCHAR(10); -- fecha de valuacion nueva
DEFINE v_mes_en_texto                          VARCHAR(2);
DEFINE v_ano_en_texto                          VARCHAR(4);
DEFINE v_fecha_valuacion                       DATE; -- fecha de valuacion

DEFINE v_movimiento_202 SMALLINT; -- para ind_consistencia = 1
DEFINE v_movimiento_662 SMALLINT; -- para ind_consistencia = 0

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT    ;
DEFINE sql_err                                   INTEGER     ;
DEFINE isam_err                                  INTEGER     ;
DEFINE err_txt                                   VARCHAR(200);
DEFINE v_c_msj                                   VARCHAR(200);
-- saldo insuficiente
DEFINE v_monto_viv92_invalido   SMALLINT;
 
   -- se declara la excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se actualiza el folio en la tabla de control de operacion
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 3
      AND pid         = p_pid;
   
   -- Agregar folio a operacion de integracion
   UPDATE glo_folio
      SET status = 1 -- preliquidado
    WHERE folio = p_folio;

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/debug_ret_preliquida_tipoN.txt");

   -- se asume que no hay errores
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso finalizó correctamente';
   LET ret_tipo_n_id_derechohabiente = null ;
   LET v_id_derechohabiente = null ;

   -- se asigna el codigo de error de saldo insuficiente de PROCESAR
   LET v_monto_viv92_invalido = 766;

   -- se inician las variables para marca
   LET v_marca_tipo_n    = 804; -- marca para tipo_n de recursos
   LET v_i_estado_marca  = 0;

   -- se inicia sin registros preliquidados
   LET v_num_reg_preliquidados = 0;

   -- se definen los movimientos
   LET v_movimiento_202 = 202; -- ind_consistencia = 1
   LET v_movimiento_662 = 662; -- ind_consistencia = 0
   
   -- movimientos de sobregiro
   LET v_movimiento_sobregiro_consistente_862    = 862; -- sobregiro tipo N consistente
   LET v_movimiento_sobregiro_inconsistente_1052 = 1052; -- sobregiro tipo N inconsistente
   
   -- se obtienen los signos para los movimientos
   LET v_signo_movimiento_CONSISTENTE            = -1;
   LET v_signo_movimiento_sobregiro_CONSISTENTE  = -1;
   
   LET v_signo_movimiento_INCONSISTENTE           = -1;
   LET v_signo_movimiento_sobregiro_INCONSISTENTE = -1;

   -- ==========================================================
   -- movimientos CONSISTENTES
   SELECT tipo
   INTO   v_signo_movimiento_CONSISTENTE
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_202; -- ind_consistencia = 1
   
   SELECT tipo
   INTO   v_signo_movimiento_sobregiro_CONSISTENTE
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_sobregiro_consistente_862; -- ind_consistencia = 1

   -- ==========================================================
   -- movimientos INCONSISTENTES
   SELECT tipo
   INTO   v_signo_movimiento_INCONSISTENTE
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_662; -- ind_consistencia = 0
   
   -- se obtiene el signo del movimiento de sobregiro
   SELECT tipo
   INTO   v_signo_movimiento_sobregiro_INCONSISTENTE
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_sobregiro_inconsistente_1052; -- ind_consistencia = 0
     
   -- la fecha de valuacion se obtiene del primer dia del mes
   -- de la fecha de operacion que viene en el encabezado
   SELECT f_operacion_procesar
   INTO v_f_operacion_procesar
   FROM ret_cza_tipo_n
   WHERE folio = p_folio;
   
   -- se calcula al valor del primer dia del mes siguiente de la fecha de operacion
   -- MM/DD/YYYY
   IF ( (MONTH(v_f_operacion_procesar)+1) < 10 ) THEN
      LET v_mes_en_texto = "0" || MONTH(v_f_operacion_procesar) + 1;
      LET v_ano_en_texto = YEAR(v_f_operacion_procesar);
   ELSE
      -- si el mes de la fecha es diciembre, entonces el mes siguient enero y se incrementa el ano
      IF ( MONTH(v_f_operacion_procesar) = 12 ) THEN
         LET v_mes_en_texto = "01";
         LET v_ano_en_texto = YEAR(v_f_operacion_procesar) + 1;
      ELSE
         -- se incrementa el mes, y el ano queda igual
         LET v_mes_en_texto = MONTH(v_f_operacion_procesar) + 1;
         LET v_ano_en_texto = YEAR(v_f_operacion_procesar);
      END IF
   END IF
      
   LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || v_ano_en_texto;
   
   LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva);
 
   -- se obtiene el valor del fondo del dia
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = v_fecha_valuacion;
     
   -- si no hay valor del fondo es un error porque no se puede valuar
   IF ( v_valor_fondo IS NULL ) THEN
      LET v_si_resultado = 1000;
      LET isam_err       = 0;
      LET v_c_msj        = 'No existe valor de fondo. No se puede realizar la valuacion de AIVs. No se puede continuar.';

      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      id_solicitud           , 
      id_decreto             , 
      diag_registro          , 
      folio                  , 
      id_ret_matriz_derecho  , 
      nss_icefa              , 
      rfc_icefa              , 
      num_ctr_interno        , 
      cve_icefa              , 
      nombre_icefa           , 
      rfc                    , 
      nombre                 , 
      cve_doc_probatorio     , 
      num_referencia         , 
      origen_retiro          ,
      f_inicio_pension       , 
      f_resolucion           , 
      porcentaje_valuacion   , 
      actuario               , 
      num_plan_privado       , 
      importe_sar92          , 
      aivs_viv92             , 
      cve_afore              , 
      estado_solicitud         
   INTO
      ret_tipo_n_id_solicitud           ,
      ret_tipo_n_id_decreto             ,
      ret_tipo_n_diag_registro          ,
      ret_tipo_n_folio                  ,
      ret_tipo_n_id_ret_matriz_derecho  ,
      ret_tipo_n_nss_icefa              ,
      ret_tipo_n_rfc_icefa              ,
      ret_tipo_n_num_ctr_interno        ,
      ret_tipo_n_cve_icefa              ,
      ret_tipo_n_nombre_icefa           ,
      ret_tipo_n_rfc                    ,
      ret_tipo_n_nombre                 ,
      ret_tipo_n_cve_doc_probatorio     ,
      ret_tipo_n_num_referencia         ,
      ret_tipo_n_origen_retiro          ,
      ret_tipo_n_f_inicio_pension       ,
      ret_tipo_n_f_resolucion           ,
      ret_tipo_n_porcentaje_valuacion   ,
      ret_tipo_n_actuario               ,
      ret_tipo_n_num_plan_privado       ,
      ret_tipo_n_importe_sar92          ,
      ret_tipo_n_aivs_viv92             ,
      ret_tipo_n_cve_afore              ,
      ret_tipo_n_estado_solicitud       
   FROM  ret_tipo_n
   WHERE folio            = p_folio
     AND estado_solicitud = 30
     
      -- se inicia la diferencia
      LET v_diferencia_aivs = 0;
     
      -- se obtiene el ind_consistencia del id_decreto
      -- [SE USA AL ASIGNAR EL MOVIMIENTO EN LA PRELIQUIDACION]
      SELECT ind_consistencia
      INTO   v_ind_consistencia
      FROM   afi_decreto
      WHERE  id_decreto = ret_tipo_n_id_decreto;
          
      --se concatena la cadena RETIRO
      LET ret_preliquida_origen = 'RETIRO N';
      
      -- se inician las variables para comprobacion de saldo del derechohabiente
      LET v_saldo_92_aivs  = 0;
      LET v_saldo_92_pesos = 0;

      -- se asume que ninguno de los montos se rechaza
      LET v_rechazo_viv92 = 0;
      
      -- asignacion de valores generales al registro de la tabla de preliquidacion
      LET ret_preliquida_f_liquida          = TODAY; -- DATE                   ;
      --LET ret_preliquida_id_derechohabiente = ret_tipo_n_id_derechohabiente;
      LET ret_preliquida_id_decreto         = ret_tipo_n_id_decreto;
      --LET ret_preliquida_f_valor            = TODAY; -- 

      LET ret_preliquida_f_valor            = v_fecha_valuacion; -- el primer dia del mes de la fecha de operacion que viene en el encabezado

      LET ret_preliquida_f_registro         = TODAY; -- la fecha de ejecucion
      LET ret_preliquida_h_registro         = CURRENT HOUR TO SECOND;

      --=======================================
      -- Cuenta Decreto
      IF ( ret_tipo_n_aivs_viv92 > 0 ) THEN
         LET ret_preliquida_subcuenta          = 48; -- decreto
         LET ret_preliquida_fondo_inversion    = 11;
         LET ret_preliquida_folio_liquida      = p_folio;
         LET ret_preliquida_id_referencia      = ret_tipo_n_id_solicitud;
                  
         -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
         EXECUTE FUNCTION fn_saldo_dia_cta_decreto(NULL,
                                                   --ret_tipo_n_id_derechohabiente,
                                                   ret_tipo_n_id_decreto,
                                                   48,
                                                   NULL)
                          INTO v_resultado_consulta, v_saldo_92_aivs, v_saldo_92_pesos;

         -- se calcula la diferencia en AIVs
         IF ( ret_tipo_n_aivs_viv92 > v_saldo_92_aivs ) THEN
            LET v_diferencia_aivs = ret_tipo_n_aivs_viv92 - v_saldo_92_aivs;
            
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_tipo_n_id_solicitud       ,
                                                           ret_preliquida_subcuenta      ,
                                                           ret_preliquida_fondo_inversion,
                                                           v_saldo_92_aivs               ,
                                                           v_saldo_92_pesos              ,
                                                           p_folio                       ,
                                                           TODAY                         ,
                                                           CURRENT HOUR TO SECOND        );

         END IF

         -- si no se tiene saldo suficiente se rechaza la solicitud por insuficiencia de saldo
         IF ( v_diferencia_aivs > p_max_aivs_sobregiro ) THEN
            -- se marca que el monto de vivienda 92 se rechazo
            LET v_rechazo_viv92 = 1;

            -- se actualiza la solicitud a rechazada por no tener saldo suficiente
            UPDATE ret_tipo_n
               SET    estado_solicitud = 100, -- rechazada
                      cod_rechazo      = v_monto_viv92_invalido -- saldo insuficiente
               WHERE  id_solicitud = ret_tipo_n_id_solicitud;
               
            -- se desmarca la cuenta
            EXECUTE FUNCTION fn_desmarca_cuenta_decreto(
                    ret_tipo_n_id_decreto
                   ,v_marca_tipo_n -- marca de retiro tipo N
                   ,ret_tipo_n_id_solicitud -- identificador de registro de archivo o lote
                   ,40 -- estado marca / rechazo validacion
                   ,v_marca_tipo_n -- marca de la causa / rechazo por validacion
                   ,p_usuario_cod
                   ,p_proceso_cod)
               INTO v_i_estado_marca;


         END IF
         
         -- si no se rechazo el retiro
         IF ( v_rechazo_viv92 = 0 ) THEN
            -- si la diferencia es mayor a cero, el saldo se retira y la diferencia se sobregira
            IF ( v_diferencia_aivs > 0 ) THEN
               
               -- PRELIQUIDACION DEL SALDO POSEIDO
               -- se verifica el tipo de consistencia
               IF ( v_ind_consistencia = 1 ) THEN
                  LET ret_preliquida_monto_acciones = v_saldo_92_aivs * v_signo_movimiento_CONSISTENTE;
                  LET v_pesos_aiv92                 = v_saldo_92_aivs * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_CONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_202;
               ELSE
                  LET ret_preliquida_monto_acciones = v_saldo_92_aivs * v_signo_movimiento_INCONSISTENTE;
                  LET v_pesos_aiv92                 = v_saldo_92_aivs * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_INCONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_662;
               END IF
               
               INSERT INTO ret_preliquida92 (
                  f_liquida            ,
                  id_decreto           ,
                  subcuenta            ,
                  fondo_inversion      ,
                  movimiento           ,
                  folio_liquida        ,
                  id_referencia        ,
                  monto_acciones       ,
                  monto_pesos          ,
                  f_valor              ,
                  f_registro           ,
                  h_registro           ,
                  origen               
                  )
               VALUES (
                  ret_preliquida_f_liquida          ,
                  ret_preliquida_id_decreto         ,
                  ret_preliquida_subcuenta          ,
                  ret_preliquida_fondo_inversion    ,
                  ret_preliquida_movimiento         ,
                  ret_preliquida_folio_liquida      ,
                  ret_preliquida_id_referencia      ,
                  ret_preliquida_monto_acciones     ,
                  ret_preliquida_monto_pesos        ,   
                  ret_preliquida_f_valor            ,
                  ret_preliquida_f_registro         ,
                  ret_preliquida_h_registro         ,
                  ret_preliquida_origen              
               );
               
               -- PRELIQUIDACION DE LA DIFERENCIA CON SOBREGIRO               
               IF ( v_ind_consistencia = 1 ) THEN
                  LET ret_preliquida_monto_acciones = v_diferencia_aivs * v_signo_movimiento_sobregiro_CONSISTENTE;
                  LET v_pesos_aiv92                 = v_diferencia_aivs * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_sobregiro_CONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_sobregiro_consistente_862;
               ELSE
                  LET ret_preliquida_monto_acciones = v_diferencia_aivs * v_signo_movimiento_sobregiro_INCONSISTENTE;
                  LET v_pesos_aiv92                 = v_diferencia_aivs * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_sobregiro_INCONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_sobregiro_inconsistente_1052;
               END IF
               
               INSERT INTO ret_preliquida92 (
                  f_liquida            ,
                  id_decreto           ,
                  subcuenta            ,
                  fondo_inversion      ,
                  movimiento           ,
                  folio_liquida        ,
                  id_referencia        ,
                  monto_acciones       ,
                  monto_pesos          ,
                  f_valor              ,
                  f_registro           ,
                  h_registro           ,
                  origen               
                  )
               VALUES (
                  ret_preliquida_f_liquida          ,
                  ret_preliquida_id_decreto         ,
                  ret_preliquida_subcuenta          ,
                  ret_preliquida_fondo_inversion    ,
                  ret_preliquida_movimiento         ,
                  ret_preliquida_folio_liquida      ,
                  ret_preliquida_id_referencia      ,
                  ret_preliquida_monto_acciones     ,
                  ret_preliquida_monto_pesos        ,   
                  ret_preliquida_f_valor            ,
                  ret_preliquida_f_registro         ,
                  ret_preliquida_h_registro         ,
                  ret_preliquida_origen              
               );



            
            ELSE
               -- EL SALDO FUE SUFICIENTE Y SE REGISTRARA EL RETIRO SIN SOBREGIRO
               -- se inserta en la tabla historia de detalle de retiro por tipo_n de recursos
               
               -- PRELIQUIDACION DEL SALDO POSEIDO
               -- se verifica el tipo de consistencia
               IF ( v_ind_consistencia = 1 ) THEN
                  LET ret_preliquida_monto_acciones = ret_tipo_n_aivs_viv92 * v_signo_movimiento_CONSISTENTE;
                  LET v_pesos_aiv92                 = ret_tipo_n_aivs_viv92 * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_CONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_202;
               ELSE
                  LET ret_preliquida_monto_acciones = ret_tipo_n_aivs_viv92 * v_signo_movimiento_INCONSISTENTE;
                  LET v_pesos_aiv92                 = ret_tipo_n_aivs_viv92 * v_valor_fondo;
                  LET ret_preliquida_monto_pesos    = v_pesos_aiv92 * v_signo_movimiento_INCONSISTENTE;
                  LET ret_preliquida_movimiento     = v_movimiento_662;
               END IF

               INSERT INTO ret_preliquida92 (
                  f_liquida            ,
                  id_decreto           ,
                  subcuenta            ,
                  fondo_inversion      ,
                  movimiento           ,
                  folio_liquida        ,
                  id_referencia        ,
                  monto_acciones       ,
                  monto_pesos          ,
                  f_valor              ,
                  f_registro           ,
                  h_registro           ,
                  origen               
                  )
               VALUES (
                  ret_preliquida_f_liquida          ,
                  ret_preliquida_id_decreto         ,
                  ret_preliquida_subcuenta          ,
                  ret_preliquida_fondo_inversion    ,
                  ret_preliquida_movimiento         ,
                  ret_preliquida_folio_liquida      ,
                  ret_preliquida_id_referencia      ,
                  ret_preliquida_monto_acciones     ,
                  ret_preliquida_monto_pesos        ,   
                  ret_preliquida_f_valor            ,
                  ret_preliquida_f_registro         ,
                  ret_preliquida_h_registro         ,
                  ret_preliquida_origen              
               );
            END IF -- si hubo sobregiro

            -- se actualiza la solicitud como preliquidada
            UPDATE ret_tipo_n
               SET estado_solicitud = 50 -- preliquidada
             WHERE id_solicitud = ret_tipo_n_id_solicitud;

            -- se incrementa el numero de registros preliquidados
            LET v_num_reg_preliquidados = v_num_reg_preliquidados + 1;
         END IF -- si se preliquidara
      END IF -- si se solicito retiro de tipo N
   END FOREACH;

   UPDATE STATISTICS FOR TABLE ret_preliquida;
 
   -- si no se preliquidaron registros, el proceso termino erroneamente
   IF ( v_num_reg_preliquidados < 1 ) THEN
      LET v_si_resultado = 2000;
      LET isam_err       = 0;
      LET v_c_msj        = 'No se preliquidaron registros. Revise la consulta general para verificar el motivo.';

      RETURN v_si_resultado, isam_err, v_c_msj;   
   END IF

   -- si el proceso se ejecuto sin contratiempos se devuelve estatus 0 (correcto) 
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso de preliquidación finalizó correctamente.';
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION -- preliquidacion de retiros tipo n
;


