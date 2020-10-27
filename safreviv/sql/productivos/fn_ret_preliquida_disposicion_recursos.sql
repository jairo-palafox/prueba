






CREATE FUNCTION "safreviv".fn_ret_preliquida_disposicion_recursos(p_usuario_cod    CHAR(20)   , p_folio DECIMAL(10,0), 
                                                       p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                       p_proceso_cod    SMALLINT, p_marca  SMALLINT, 
                                                       p_movimiento     SMALLINT, p_movimiento_sobregiro SMALLINT,
                                                       p_max_aivs_sobregiro DECIMAL(20,6))
   RETURNING SMALLINT, INTEGER, VARCHAR(255), DECIMAL(9,0)

   -- detalle de la tabla historica/integrada de retiros por disposicion de recursos
   DEFINE ret_disposicion_id_solicitud          DECIMAL(9,0);
   DEFINE ret_disposicion_id_derechohabiente    DECIMAL(9,0);
   DEFINE ret_disposicion_f_solicitud           DATE;
   DEFINE ret_disposicion_id_ret_matriz_derecho SMALLINT;
   DEFINE ret_disposicion_sec_pension           CHAR(2);
   DEFINE ret_disposicion_diag_registro         CHAR(3);
   DEFINE ret_disposicion_folio                 DECIMAL(9,0);
   DEFINE ret_disposicion_curp                  CHAR(18);
   DEFINE ret_disposicion_nombre_afore          CHAR(40);
   DEFINE ret_disposicion_paterno_afore         CHAR(40);
   DEFINE ret_disposicion_materno_afore         CHAR(40);
   DEFINE ret_disposicion_f_inicio_pension      DATE;
   DEFINE ret_disposicion_f_resolucion          DATE;
   DEFINE ret_disposicion_porcentaje_valuacion  DECIMAL(5,2);
   DEFINE ret_disposicion_semanas_cotizadas     INTEGER;
   DEFINE ret_disposicion_cve_doc_probatorio    SMALLINT;
   DEFINE ret_disposicion_f_nacimiento          DATE;
   DEFINE ret_disposicion_aseguradora           CHAR(3);
   DEFINE ret_disposicion_actuario              CHAR(7);
   DEFINE ret_disposicion_num_plan_privado      CHAR(8);
   DEFINE ret_disposicion_periodo_primer_pago   INTEGER;
   DEFINE ret_disposicion_aivs_ret97            DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_cv               DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_cs               DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_sar92            DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_viv97            DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_viv92            DECIMAL(14,6);
   DEFINE ret_disposicion_consec_trabajador     DECIMAL(11,0);
   DEFINE ret_disposicion_importe_viv72         DECIMAL(14,2);
   DEFINE ret_disposicion_estado_sub_viv        SMALLINT;
   DEFINE ret_disposicion_cve_afore             SMALLINT;
   DEFINE ret_disposicion_cod_rechazo           SMALLINT;
   DEFINE ret_disposicion_estado_solicitud      SMALLINT;

   -- campos de la tabla de preliquidacion de retiros

   DEFINE ret_preliquida_f_liquida                DATE;
   DEFINE ret_preliquida_id_derechohabiente       DECIMAL(9,0);
   DEFINE ret_preliquida_subcuenta                SMALLINT;
   DEFINE ret_preliquida_fondo_inversion          SMALLINT;
   DEFINE ret_preliquida_movimiento               SMALLINT;
   DEFINE ret_preliquida_folio_liquida            DECIMAL(9,0);
   DEFINE ret_preliquida_id_referencia            DECIMAL(9,0);
   DEFINE ret_preliquida_monto_acciones           DECIMAL(20,6);
   DEFINE ret_preliquida_monto_pesos              DECIMAL(20,2);
   DEFINE ret_preliquida_f_valor                  DATE;
   DEFINE ret_preliquida_f_registro               DATE;
   DEFINE ret_preliquida_h_registro               DATETIME HOUR TO SECOND;
   DEFINE ret_preliquida_origen                   CHAR(20);
   DEFINE v_nss                                   CHAR(11); -- nss del trabajador
   DEFINE ret_preliquida_subcuenta_vol            SMALLINT;
   DEFINE ret_preliquida_movto_cargo_vol          SMALLINT;
   DEFINE ret_preliquida_movto_abono_vol          SMALLINT;


   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                    DECIMAL(9,0);
   DEFINE v_id_solicitud                          DECIMAL(9,0);

   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                           DECIMAL(14,6);
   DEFINE v_pesos_aiv97                           DECIMAL(14,6);
   DEFINE v_pesos_aiv92                           DECIMAL(14,6);
   DEFINE v_fecha_valor                           DATE;

   -- signo del movimiento y del movimiento del sobregiro
   DEFINE v_signo_movimiento                      SMALLINT;
   DEFINE v_signo_movimiento_sobregiro            SMALLINT;
   DEFINE v_diferencia_aivs                       DECIMAL(14,6);
   DEFINE v_fondo                                 SMALLINT;
   -- 001 signo del movimiento de abon o
   DEFINE v_signo_movimiento_abono                SMALLINT;
   DEFINE v_movimiento_abono                      SMALLINT;
    
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
   DEFINE v_saldo_92_aivs                         DECIMAL(18,6); -- saldo del derechohabiente en viv92
   DEFINE v_saldo_92_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv97
   DEFINE v_saldo_97_aivs                         DECIMAL(18,6); -- saldo del derechohabiente en viv92
   DEFINE v_saldo_97_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv97
   DEFINE v_saldo_vol_aivs                        DECIMAL(18,6); -- saldo del derechohabiente en aportaciones voluntarias
   DEFINE v_saldo_vol_pesos                       DECIMAL(18,6); -- saldo del derechohabiente en aportaciones voluntarias
   DEFINE v_saldo_72_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv72
   DEFINE v_resultado_consulta                    SMALLINT;
    
   DEFINE v_rechazo_viv92                         SMALLINT; -- rechazo de retiro de vivienda 92
   DEFINE v_rechazo_viv97                         SMALLINT; -- rechazo de retiro de vivienda 97
   DEFINE v_rechazo_viv72                         SMALLINT; -- rechazo de retiro de vivienda 72
   DEFINE v_viv92_sobregirada                     SMALLINT; -- indica si viv92 se sobregiro
   DEFINE v_viv97_sobregirada                     SMALLINT; -- indica si viv97 se sobregiro
    
   DEFINE cod_rechazo_1                           SMALLINT;
   DEFINE cod_rechazo_2                           SMALLINT;
   DEFINE cod_rechazo_3                           SMALLINT;

   -- para marcar las cuentas
   DEFINE v_i_estado_marca                        INTEGER;
   DEFINE v_marca_disposicion                     INTEGER; -- 805 de acuerdo a catalogo

   DEFINE v_num_regs_preliquidados                INTEGER; -- numero de registros preliquidados
   DEFINE v_folio                                 DECIMAL(10);
   DEFINE v_nombre_archivo                        CHAR(18);
   DEFINE v_f_operacion_procesar                  DATE;
   DEFINE v_f_carga_afore                         DATE;
   DEFINE v_resultado_operacion                   SMALLINT;
   DEFINE v_cod_rechazo_1                         SMALLINT;
   DEFINE v_cod_rechazo_2                         SMALLINT;
   DEFINE v_cod_rechazo_3                         SMALLINT;
   DEFINE v_conteo                                SMALLINT;

   -- Control de Excepciones
   DEFINE v_si_resultado                          SMALLINT;
   DEFINE sql_err                                 INTEGER;
   DEFINE isam_err                                INTEGER;
   DEFINE err_txt                                 VARCHAR(200);
   DEFINE v_c_msj                                 VARCHAR(200);
   DEFINE v_fecha                                 VARCHAR(10);
   DEFINE v_monto_viv92_invalido                  SMALLINT;
   DEFINE v_monto_viv97_invalido                  SMALLINT;
   DEFINE v_con_credito_43bis                     SMALLINT;

   -- para validar si el nss tiene credito 43 bis
   DEFINE v_tiene_credito43bis                    SMALLINT; -- booleana que indica que tiene credito
   DEFINE v_resultado                             SMALLINT;
   DEFINE v_tpo_originacion                       SMALLINT;
   DEFINE v_tpo_credito                           SMALLINT;
   DEFINE v_num_credito                           DECIMAL(10,0);
   DEFINE v_f_otorga                              DATE;
   DEFINE v_f_liquida                             DATE;

   -- se declara el 
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, ret_disposicion_id_solicitud;
   END EXCEPTION

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/debug_ret_disposicion.txt");

   -- se asume que no hay errores
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finalizó correctamente';

   -- se inician las variables para marca
   LET v_marca_disposicion = p_marca; --805; -- marca para disposicion de recursos
   LET v_i_estado_marca = 0;
   LET v_monto_viv92_invalido = 766;
   LET v_monto_viv97_invalido = 767;
   LET v_con_credito_43bis = 961; -- codigo de error cuando se tiene credito 43bis
   LET v_resultado = 0;
   LET v_tpo_originacion = 0;
   LET v_tpo_credito = 0;
   LET v_num_credito = 0;
   LET v_f_otorga = TODAY;
   LET v_f_liquida = TODAY;
   LET ret_disposicion_id_solicitud = 0;
   -- 001 se inicializa la variable de abono
   LET v_movimiento_abono = 1011;   -- movimiento para abono variación menor retiros
   LET ret_preliquida_subcuenta_vol = 55;
   LET ret_preliquida_movto_cargo_vol = 1662;
   LET ret_preliquida_movto_abono_vol = 911;


   -- Agregar folio a operacion de integracion
   UPDATE glo_folio
   SET    status = 1 -- preliquidado
   WHERE  folio  = p_folio;
 
   -- se actualiza el folio en la tabla de control de operacion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 3
   AND    pid         = p_pid;
 
   -- se obtiene el valor del fondo
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   ret_cza_disposicion
   WHERE  folio = p_folio;

   -- se obtiene el signo del movimiento
   SELECT tipo
   INTO   v_signo_movimiento
   FROM   cat_movimiento
   WHERE  movimiento = p_movimiento;

   -- se obtiene el signo del movimiento del sobregiro      
   SELECT tipo
   INTO   v_signo_movimiento_sobregiro
   FROM   cat_movimiento
   WHERE  movimiento = p_movimiento_sobregiro;
   
   -- 001 se obtiene el signo del movimiento de abono      
   SELECT tipo
   INTO   v_signo_movimiento_abono
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono;
   
   -- se inicia el contador de registros preliquidados
   LET v_num_regs_preliquidados = 0;
   
   -- se obtienen los datos del detalle
   FOREACH
      SELECT
               a.nss,
               b.id_solicitud,
               b.id_derechohabiente,
               b.f_solicitud,
               b.id_ret_matriz_derecho,
               b.sec_pension,
               b.diag_registro,
               b.folio,
               b.curp,
               b.nombre_afore,
               b.paterno_afore,
               b.materno_afore,
               b.f_inicio_pension,
               b.f_resolucion,
               b.porcentaje_valuacion,
               b.semanas_cotizadas,
               b.cve_doc_probatorio,
               b.f_nacimiento,
               b.aseguradora,
               b.actuario,
               b.num_plan_privado,
               b.periodo_primer_pago,
               b.aivs_viv97,
               b.aivs_viv92,
               b.consec_trabajador,
               b.importe_viv72,
               b.estado_sub_viv,
               b.cve_afore,
               b.cod_rechazo,
               b.estado_solicitud      
      INTO
               v_nss,
               ret_disposicion_id_solicitud,
               ret_disposicion_id_derechohabiente,
               ret_disposicion_f_solicitud,
               ret_disposicion_id_ret_matriz_derecho,
               ret_disposicion_sec_pension,
               ret_disposicion_diag_registro,
               ret_disposicion_folio,
               ret_disposicion_curp,
               ret_disposicion_nombre_afore,
               ret_disposicion_paterno_afore,
               ret_disposicion_materno_afore,
               ret_disposicion_f_inicio_pension,
               ret_disposicion_f_resolucion,
               ret_disposicion_porcentaje_valuacion,
               ret_disposicion_semanas_cotizadas,
               ret_disposicion_cve_doc_probatorio,
               ret_disposicion_f_nacimiento,
               ret_disposicion_aseguradora,
               ret_disposicion_actuario,
               ret_disposicion_num_plan_privado,
               ret_disposicion_periodo_primer_pago,
               ret_disposicion_aivs_viv97,
               ret_disposicion_aivs_viv92,
               ret_disposicion_consec_trabajador,
               ret_disposicion_importe_viv72,
               ret_disposicion_estado_sub_viv,
               ret_disposicion_cve_afore,
               ret_disposicion_cod_rechazo,
               ret_disposicion_estado_solicitud      
      FROM     ret_disposicion b,
               afi_derechohabiente a
      WHERE    b.folio              = p_folio
      AND      b.estado_solicitud   = 30 -- integradas
      AND      b.id_derechohabiente = a.id_derechohabiente
      
      -- se inicia la difefencia en cero
      LET v_diferencia_aivs   = 0;
      
      -- se asume que no hay sobregiro
      LET v_viv92_sobregirada = 0;
      LET v_viv97_sobregirada = 0;
      
      -- se obtiene la fecha del valor del fondo del encabezado
      SELECT f_valor_transferencia
      INTO   v_fecha_valor
      FROM   ret_cza_disposicion
      WHERE  folio = p_folio;
         
      -- la fecha del valor es el primer dia del mes de la fecha de valor transferencia
      LET v_fecha = MONTH(v_fecha_valor) || "/01/" || YEAR(v_fecha_valor);
     
      -- se transforma a fecha
      LET v_fecha_valor = DATE(v_fecha);
      
      --=========================================================================
      --se obtiene el origen del retiro 
      --=========================================================================
      SELECT tpo_retiro
      INTO   ret_preliquida_origen
      FROM   ret_matriz_derecho      
      WHERE  id_ret_matriz_derecho = ret_disposicion_id_ret_matriz_derecho;     

      --se concatena la cadena RETIRO
      LET ret_preliquida_origen = 'RETIRO '||ret_preliquida_origen;
      
      -- se inician las variables para comprobacion de saldo del derechohabiente
      LET v_saldo_92_aivs = 0;
      LET v_saldo_92_pesos = 0;
      LET v_saldo_97_aivs  = 0;
      LET v_saldo_97_pesos = 0;
      LET v_saldo_vol_aivs = 0;
      LET v_saldo_vol_pesos = 0;
      LET v_saldo_72_pesos = 0;

      -- se asume que ninguno de los montos se rechaza
      LET v_rechazo_viv92 = 0;
      LET v_rechazo_viv97 = 0;
      LET v_rechazo_viv72 = 0;
      
      -- se asume que no se tiene credito 43bis
      LET v_tiene_credito43bis = 0;
     
      -- ==========================================================================
      -- asignacion de valores generales al registro de la tabla de preliquidacion
      LET ret_preliquida_f_liquida = TODAY; -- DATE                   ;
      LET ret_preliquida_id_derechohabiente = ret_disposicion_id_derechohabiente;
      LET ret_preliquida_f_valor = v_fecha_valor; -- la fecha del valor del fondo del encabezado
      LET ret_preliquida_f_registro = TODAY; -- la fecha de ejecucion
      LET ret_preliquida_h_registro = CURRENT HOUR TO SECOND; -- VALIDAR QUE ESTA ES
                   
      -- 13Nov2013. Se verifica si tiene credito 43bis vigente
      EXECUTE FUNCTION fn_credito_vivienda(ret_disposicion_id_derechohabiente, 0)
              INTO v_resultado,
                   v_tpo_originacion,
                   v_tpo_credito,
                   v_num_credito,
                   v_f_otorga,
                   v_f_liquida;

      -- si el resultado de la consulta es 0 (CERO) entonces tiene un credito vigente
      IF ( v_resultado = 0 ) THEN
         -- si el tipo de credito es 43bis se activa la bandera
         IF ( v_tpo_credito = 2 ) THEN
            LET v_tiene_credito43bis = 1;
         END IF
      END IF

      -- ==========================================================================
      -- VIVIENDA 92
      IF ( ret_disposicion_aivs_viv92 > 0 ) THEN
         LET ret_preliquida_subcuenta = 8; -- vivienda 92
         LET ret_preliquida_fondo_inversion = 11; -- verificar
         LET ret_preliquida_movimiento = p_movimiento; --212; -- cargo por retiro por disposicion
         LET ret_preliquida_folio_liquida = p_folio;
         LET ret_preliquida_id_referencia = ret_disposicion_id_solicitud; -- VERIFICAR QUE CON ESTE QUEDA
         LET ret_preliquida_monto_acciones = ret_disposicion_aivs_viv92 * v_signo_movimiento;
         
         -- se calcula monto en pesos de las acciones
         LET v_pesos_aiv92 = ret_disposicion_aivs_viv92 * v_valor_fondo;
         LET ret_preliquida_monto_pesos = v_pesos_aiv92 * v_signo_movimiento; -- 
         
         -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                       ret_disposicion_id_derechohabiente,
                                       8,
                                       NULL)
                          INTO v_resultado_consulta, v_saldo_92_aivs, v_saldo_92_pesos;
                           

         IF v_saldo_92_aivs > 0 THEN 
            -- se verifica si el monto solicitado es mayor al saldo del trabajador
            IF ( ret_disposicion_aivs_viv92 > v_saldo_92_aivs ) THEN
               
               -- si la cuenta ya esta sobregirada, el saldo se considera cero
               IF ( v_saldo_92_aivs < 0 ) THEN
                  LET v_saldo_92_aivs = 0;
               END IF
                                                              
               -- se verifica cuanto es la diferencia
               LET v_diferencia_aivs = ret_disposicion_aivs_viv92 - v_saldo_92_aivs;
               
               -- si la diferencia es mayor a las AIVs permitidas, no se preliquida
               IF ( v_diferencia_aivs > p_max_aivs_sobregiro) THEN
                  -- se marca que el monto de vivienda 92 se rechazo
                  LET v_rechazo_viv92 = 1;
               
                  -- se actualiza la solicitud a rechazada por no tener saldo suficiente
                  UPDATE ret_disposicion
                  SET    estado_solicitud = 100, -- rechazada
                         --cod_rechazo      = 10 -- saldo insuficiente
                         cod_rechazo      = v_monto_viv92_invalido -- saldo insuficiente
                  WHERE  id_solicitud     = ret_disposicion_id_solicitud;
                         
                  -- se desmarca la cuenta
                  EXECUTE FUNCTION fn_desmarca_cuenta(ret_disposicion_id_derechohabiente
                                                     ,v_marca_disposicion -- marca de disposicion
                                                     ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                                                     ,40 -- estado marca / rechazo validacion
                                                     ,v_marca_disposicion -- marca de la causa /se usa la misma por ser rechazo por validacion de negocio
                                                     ,p_usuario_cod
                                                     ,p_proceso_cod)
                                                 INTO v_i_estado_marca;
               
                  -- se inserta el registro en la tabla historica de saldos
                  EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                                 ret_preliquida_subcuenta,
                                                                 ret_preliquida_fondo_inversion,
                                                                 v_saldo_92_aivs,
                                                                 v_saldo_92_pesos,
                                                                 p_folio,
                                                                 TODAY,
                                                                 CURRENT HOUR TO SECOND);

                  LET v_viv92_sobregirada = 1;
               ELSE
                  
                  LET v_viv92_sobregirada = 1;
                  
                  -- se inserta el registro en la tabla historica de saldos
                  EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                                 ret_preliquida_subcuenta,
                                                                 ret_preliquida_fondo_inversion,
                                                                 v_saldo_92_aivs,
                                                                 v_saldo_92_pesos,
                                                                 p_folio,
                                                                 TODAY,
                                                                 CURRENT HOUR TO SECOND);

               END IF

            END IF
         ELSE 
            LET v_rechazo_viv92 = 1;
         
            -- se actualiza la solicitud a rechazada por no tener saldo suficiente
            UPDATE ret_disposicion
            SET    estado_solicitud = 100, -- rechazada
                   --cod_rechazo      = 10 -- saldo insuficiente
                   cod_rechazo      = v_monto_viv92_invalido -- saldo insuficiente
            WHERE  id_solicitud     = ret_disposicion_id_solicitud;
                   
            -- se desmarca la cuenta
            EXECUTE FUNCTION fn_desmarca_cuenta(ret_disposicion_id_derechohabiente
                                               ,v_marca_disposicion -- marca de disposicion
                                               ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                                               ,40 -- estado marca / rechazo validacion
                                               ,v_marca_disposicion -- marca de la causa /se usa la misma por ser rechazo por validacion de negocio
                                               ,p_usuario_cod
                                               ,p_proceso_cod)
                                           INTO v_i_estado_marca;
         END IF
         -- si no se rechazo el retiro
         IF ( v_rechazo_viv92 = 0 ) THEN
         
            IF ( v_diferencia_aivs > 0 ) THEN
               -- el saldo se preliquida como parte del monto solicitado
               -- se genera un solo movimiento por el total del monto solicitado (el saldo mas el sobregiro) PRODINF-602 nuevo alcance
               LET ret_preliquida_monto_acciones = (v_saldo_92_aivs + v_diferencia_aivs) * v_signo_movimiento;
               LET v_pesos_aiv92 = (v_saldo_92_aivs + v_diferencia_aivs) * v_valor_fondo;
               LET ret_preliquida_monto_pesos = v_pesos_aiv92 * v_signo_movimiento;
               
               INSERT INTO ret_preliquida (
                           f_liquida,
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
                           origen
               )
               VALUES (
                           ret_preliquida_f_liquida,
                           ret_preliquida_id_derechohabiente,
                           ret_preliquida_subcuenta,
                           ret_preliquida_fondo_inversion,
                           ret_preliquida_movimiento,
                           ret_preliquida_folio_liquida,
                           ret_preliquida_id_referencia,
                           ret_preliquida_monto_acciones,
                           ret_preliquida_monto_pesos,
                           ret_preliquida_f_valor,
                           ret_preliquida_f_registro,
                           ret_preliquida_h_registro,
                           ret_preliquida_origen             
               );

               
--               -- la diferencia se preliquida con un movimiento de sobregiro
--               -- el saldo se preliquida como parte del monto solicitado
--               LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_sobregiro;
--               LET v_pesos_aiv92                     = v_diferencia_aivs * v_valor_fondo;
--               LET ret_preliquida_monto_pesos        = v_pesos_aiv92 * v_signo_movimiento_sobregiro;
--               LET ret_preliquida_movimiento         = p_movimiento_sobregiro;
--               
--               -- 02dic2013. No se sobregira, solo se paga lo que infonavit tiene
--               
--               INSERT INTO ret_preliquida (
--                  f_liquida          ,
--                  id_derechohabiente ,
--                  subcuenta          ,
--                  fondo_inversion    ,
--                  movimiento         ,
--                  folio_liquida      ,
--                  id_referencia      ,
--                  monto_acciones     ,
--                  monto_pesos        ,
--                  f_valor            ,
--                  f_registro         ,
--                  h_registro         ,
--                  origen
--               )
--               VALUES (
--                  ret_preliquida_f_liquida          ,
--                  ret_preliquida_id_derechohabiente ,
--                  ret_preliquida_subcuenta          ,
--                  ret_preliquida_fondo_inversion    ,
--                  ret_preliquida_movimiento         ,
--                  ret_preliquida_folio_liquida      ,
--                  ret_preliquida_id_referencia      ,
--                  ret_preliquida_monto_acciones     ,
--                  ret_preliquida_monto_pesos        ,
--                  ret_preliquida_f_valor            ,
--                  ret_preliquida_f_registro         ,
--                  ret_preliquida_h_registro         ,
--                  ret_preliquida_origen             
--               );
			   
			   -- 001 se inserta en la tabla de preliquidacion de retiros como un abono
			   -- 001 se hace un abono por la diferencia del sobregiro
			   
               LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_abono;
               LET v_pesos_aiv92                     = v_diferencia_aivs * v_valor_fondo;
               LET ret_preliquida_monto_pesos        = v_pesos_aiv92 * v_signo_movimiento_abono;
               LET ret_preliquida_movimiento         = v_movimiento_abono;
               INSERT INTO ret_preliquida (
                           f_liquida,
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
                           origen
               )
               VALUES (
                           ret_preliquida_f_liquida,
                           ret_preliquida_id_derechohabiente,
                           ret_preliquida_subcuenta,
                           ret_preliquida_fondo_inversion,
                           ret_preliquida_movimiento,
                           ret_preliquida_folio_liquida,
                           ret_preliquida_id_referencia,
                           ret_preliquida_monto_acciones,
                           ret_preliquida_monto_pesos,
                           ret_preliquida_f_valor,
                           ret_preliquida_f_registro,
                           ret_preliquida_h_registro,
                           ret_preliquida_origen             
               );
            
            ELSE
         
               -- se inserta en la tabla de preliquidacion de retiros
               INSERT INTO ret_preliquida (
                           f_liquida,
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
                           origen
               )
               VALUES (
                           ret_preliquida_f_liquida,
                           ret_preliquida_id_derechohabiente,
                           ret_preliquida_subcuenta,
                           ret_preliquida_fondo_inversion,
                           ret_preliquida_movimiento,
                           ret_preliquida_folio_liquida,
                           ret_preliquida_id_referencia,
                           ret_preliquida_monto_acciones,
                           ret_preliquida_monto_pesos,
                           ret_preliquida_f_valor,
                           ret_preliquida_f_registro,
                           ret_preliquida_h_registro,
                           ret_preliquida_origen             
               );
            END IF
            -- se cuenta un registro preliquidado
            LET v_num_regs_preliquidados = v_num_regs_preliquidados + 1;
         END IF
      END IF

      -- ==========================================================================
      -- se inicia la difefencia en cero
      LET v_diferencia_aivs = 0;
      
      -- 13Nov2013. Solo se paga si no se tiene credito43bis
      IF ( v_tiene_credito43bis = 0 ) THEN
         -- VIVIENDA 97
         IF ( ret_disposicion_aivs_viv97 > 0 ) THEN
            LET ret_preliquida_subcuenta = 4; -- vivienda 97
            LET ret_preliquida_fondo_inversion = 11; -- 
            LET ret_preliquida_movimiento = p_movimiento; --212; -- cargo de retiro por disposicion de recursos
            LET ret_preliquida_folio_liquida = p_folio;
            LET ret_preliquida_id_referencia = ret_disposicion_id_solicitud; -- VERIFICAR QUE CON ESTE QUEDA
            LET ret_preliquida_monto_acciones = ret_disposicion_aivs_viv97 * v_signo_movimiento; -- 
         
            -- se calcula monto en pesos de las acciones
            LET v_pesos_aiv97 = ret_disposicion_aivs_viv97 * v_valor_fondo;
            LET ret_preliquida_monto_pesos = v_pesos_aiv97 * v_signo_movimiento; -- 
            
            -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
            EXECUTE FUNCTION fn_saldo_dia(NULL,
                                          ret_disposicion_id_derechohabiente,
                                          4,
                                          NULL)
                             INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_97_pesos;

            -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
            EXECUTE FUNCTION fn_saldo_dia(NULL,
                                          ret_disposicion_id_derechohabiente,
                                          55,
                                          NULL)
                             INTO v_resultado_consulta, v_saldo_vol_aivs, v_saldo_vol_pesos;
         
            -- se verifica si el saldo es igual o mayor al monto solicitado
            IF (v_saldo_97_aivs + v_saldo_vol_aivs) > 0 THEN 
               IF ( ret_disposicion_aivs_viv97 > (v_saldo_97_aivs + v_saldo_vol_aivs)) THEN
               
                  -- si la cuenta ya esta sobregirada, se considera cero el saldo
                  IF ( v_saldo_97_aivs < 0 ) THEN
                     LET v_saldo_97_aivs = 0;
                  END IF
                                                                 
                  -- se verifica cuanto es la diferencia
                  LET v_diferencia_aivs = ret_disposicion_aivs_viv97 - (v_saldo_97_aivs + v_saldo_vol_aivs);
                     
                  -- si la diferencia excede las aivs permitidas, no se preliquida
                  IF ( v_diferencia_aivs > p_max_aivs_sobregiro) THEN
                     -- se marca que el monto de vivienda 97 se rechazo
                     LET v_rechazo_viv97 = 1;
                  
                     -- se actualiza la solicitud a rechazada por no tener saldo suficiente
                     UPDATE ret_disposicion
                     SET    estado_solicitud = 100, -- rechazada
                            cod_rechazo      = v_monto_viv97_invalido -- saldo insuficiente
                     WHERE  id_solicitud = ret_disposicion_id_solicitud;
                            
                     -- se desmarca la cuenta
                     EXECUTE FUNCTION fn_desmarca_cuenta(
                             ret_disposicion_id_derechohabiente
                            ,v_marca_disposicion -- marca de disposicion
                            ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                            ,40 -- estado marca
                            ,v_marca_disposicion -- marca de la causa / se usa la misma por ser rechazo por validacion
                            ,p_usuario_cod
                            ,p_proceso_cod)
                        INTO v_i_estado_marca;
                  
                     -- se inserta el registro en la tabla historica de saldos
                     EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud ,
                                                                    ret_preliquida_subcuenta,
                                                                    ret_preliquida_fondo_inversion,
                                                                    v_saldo_97_aivs,
                                                                    v_saldo_97_pesos,
                                                                    p_folio,
                                                                    TODAY,
                                                                    CURRENT HOUR TO SECOND);
                     -- se inserta el registro en la tabla historica de saldos
                     EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                                    55,  -- Aportaciones Voluntarias 
                                                                    ret_preliquida_fondo_inversion,
                                                                    v_saldo_vol_aivs,
                                                                    v_saldo_vol_pesos,
                                                                    p_folio,
                                                                    TODAY,
                                                                    CURRENT HOUR TO SECOND);
                     LET v_viv97_sobregirada = 1;
                                                                    
                  ELSE
               
                     -- se indica que la cuenta se sobregirara
                     LET v_viv97_sobregirada = 1;
                     
                     -- se inserta el registro en la tabla historica de saldos
                     EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                                    ret_preliquida_subcuenta,
                                                                    ret_preliquida_fondo_inversion,
                                                                    v_saldo_97_aivs,
                                                                    v_saldo_97_pesos,
                                                                    p_folio,
                                                                    TODAY,
                                                                    CURRENT HOUR TO SECOND);
                     -- se inserta el registro en la tabla historica de saldos
                     EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                                    55,  -- Aportaciones Voluntarias 
                                                                    ret_preliquida_fondo_inversion,
                                                                    v_saldo_vol_aivs,
                                                                    v_saldo_vol_pesos,
                                                                    p_folio,
                                                                    TODAY,
                                                                    CURRENT HOUR TO SECOND);
                     
                  END IF
            
               END IF
            ELSE 
               -- se marca que el monto de vivienda 97 se rechazo
               LET v_rechazo_viv97 = 1;

               -- se actualiza la solicitud a rechazada por no tener saldo suficiente
               UPDATE ret_disposicion
               SET    estado_solicitud = 100, -- rechazada
                      cod_rechazo      = v_monto_viv97_invalido -- saldo insuficiente
               WHERE  id_solicitud = ret_disposicion_id_solicitud;
                      
               -- se desmarca la cuenta
               EXECUTE FUNCTION fn_desmarca_cuenta(
                       ret_disposicion_id_derechohabiente
                      ,v_marca_disposicion -- marca de disposicion
                      ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                      ,40 -- estado marca
                      ,v_marca_disposicion -- marca de la causa / se usa la misma por ser rechazo por validacion
                      ,p_usuario_cod
                      ,p_proceso_cod)
                  INTO v_i_estado_marca;
            
            END IF 
            
            -- si no se rechazo el retiro
            IF ( v_rechazo_viv97 = 0 ) THEN
         
               IF ( v_diferencia_aivs > 0 ) THEN
                  -- si se cuenta con saldo en la subcuenta de aportaciones voluntarias
                  -- primero se hara el cargo a las voluntarias, luego se abona a la subcuenta
                  -- de vivienda 97 y al final se realiza un cargo por el total a la subcuenta de vivienda 97
                  IF v_saldo_vol_aivs > 0 THEN -- se generan los movimientos de cargo y abono de las aportaciones voluntarias
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento;
                      -- se hace el cargo a la voluntaria
                      INSERT INTO ret_preliquida (
                                  f_liquida,
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
                                  origen
                      )
                      VALUES (
                                  ret_preliquida_f_liquida,
                                  ret_preliquida_id_derechohabiente ,
                                  ret_preliquida_subcuenta_vol,
                                  ret_preliquida_fondo_inversion,
                                  ret_preliquida_movto_cargo_vol,
                                  ret_preliquida_folio_liquida,
                                  ret_preliquida_id_referencia,
                                  ret_preliquida_monto_acciones,
                                  ret_preliquida_monto_pesos,
                                  ret_preliquida_f_valor,
                                  ret_preliquida_f_registro,
                                  ret_preliquida_h_registro,
                                  ret_preliquida_origen             
                      );
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento_abono;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento_abono;
                      -- se hace el abono a vivienda 97
                      INSERT INTO ret_preliquida (
                                  f_liquida,
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
                                  origen
                      )
                      VALUES (
                                  ret_preliquida_f_liquida,
                                  ret_preliquida_id_derechohabiente,
                                  ret_preliquida_subcuenta,
                                  ret_preliquida_fondo_inversion,
                                  ret_preliquida_movto_abono_vol,
                                  ret_preliquida_folio_liquida,
                                  ret_preliquida_id_referencia,
                                  ret_preliquida_monto_acciones,
                                  ret_preliquida_monto_pesos,
                                  ret_preliquida_f_valor,
                                  ret_preliquida_f_registro,
                                  ret_preliquida_h_registro,
                                  ret_preliquida_origen             
                      );
                      
                  END IF 
                  -- el saldo se preliquida como parte del monto solicitado
                  
                  LET ret_preliquida_monto_acciones     = (v_saldo_97_aivs + v_saldo_vol_aivs + v_diferencia_aivs) * v_signo_movimiento;
                  LET v_pesos_aiv97                     = (v_saldo_97_aivs + v_saldo_vol_aivs + v_diferencia_aivs) * v_valor_fondo;
                  LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento;
                  
                  INSERT INTO ret_preliquida (
                              f_liquida,
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
                              origen
                  )
                  VALUES (
                              ret_preliquida_f_liquida,
                              ret_preliquida_id_derechohabiente,
                              ret_preliquida_subcuenta,
                              ret_preliquida_fondo_inversion,
                              ret_preliquida_movimiento,
                              ret_preliquida_folio_liquida,
                              ret_preliquida_id_referencia,
                              ret_preliquida_monto_acciones,
                              ret_preliquida_monto_pesos,
                              ret_preliquida_f_valor,
                              ret_preliquida_f_registro,
                              ret_preliquida_h_registro,
                              ret_preliquida_origen             
                  );
                  
--                  -- la diferencia se preliquida con un movimiento de sobregiro
--                  -- el saldo se preliquida como parte del monto solicitado
--                  LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_sobregiro;
--                  LET v_pesos_aiv97                     = v_diferencia_aivs * v_valor_fondo;
--                  LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento_sobregiro;
--                  LET ret_preliquida_movimiento         = p_movimiento_sobregiro;
--                  
--                  -- 02dic2013. No se sobregira, solo se paga lo que infonavit tiene
--                  
--                  INSERT INTO ret_preliquida (
--                     f_liquida          ,
--                     id_derechohabiente ,
--                     subcuenta          ,
--                     fondo_inversion    ,
--                     movimiento         ,
--                     folio_liquida      ,
--                     id_referencia      ,
--                     monto_acciones     ,
--                     monto_pesos        ,
--                     f_valor            ,
--                     f_registro         ,
--                     h_registro         ,
--                     origen
--                  )
--                  VALUES (
--                     ret_preliquida_f_liquida          ,
--                     ret_preliquida_id_derechohabiente ,
--                     ret_preliquida_subcuenta          ,
--                     ret_preliquida_fondo_inversion    ,
--                     ret_preliquida_movimiento         ,
--                     ret_preliquida_folio_liquida      ,
--                     ret_preliquida_id_referencia      ,
--                     ret_preliquida_monto_acciones     ,
--                     ret_preliquida_monto_pesos        ,
--                     ret_preliquida_f_valor            ,
--                     ret_preliquida_f_registro         ,
--                     ret_preliquida_h_registro         ,
--                     ret_preliquida_origen             
--                  );
				  -- 001 se inserta en la tabla de preliquidacion de retiros como un abono
				  -- 001 se hace un abono por la diferencia del sobregiro
			   
                  LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_abono;
                  LET v_pesos_aiv92                     = v_diferencia_aivs * v_valor_fondo;
                  LET ret_preliquida_monto_pesos        = v_pesos_aiv92 * v_signo_movimiento_abono;
                  LET ret_preliquida_movimiento         = v_movimiento_abono;
                  INSERT INTO ret_preliquida (
                              f_liquida,
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
                              origen
                  )
                  VALUES (
                              ret_preliquida_f_liquida,
                              ret_preliquida_id_derechohabiente,
                              ret_preliquida_subcuenta,
                              ret_preliquida_fondo_inversion,
                              ret_preliquida_movimiento,
                              ret_preliquida_folio_liquida,
                              ret_preliquida_id_referencia,
                              ret_preliquida_monto_acciones,
                              ret_preliquida_monto_pesos,
                              ret_preliquida_f_valor,
                              ret_preliquida_f_registro,
                              ret_preliquida_h_registro,
                              ret_preliquida_origen             
                  );
				  
                  
               
               ELSE
                  -- si se cuenta con saldo en la subcuenta de aportaciones voluntarias
                  -- primero se hara el cargo a las voluntarias, luego se abona a la subcuenta
                  -- de vivienda 97 y al final se realiza un cargo por el total a la subcuenta de vivienda 97
                  IF v_saldo_vol_aivs > 0 THEN -- se generan los movimientos de cargo y abono de las aportaciones voluntarias
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento;
                      -- se hace el cargo a la voluntaria
                      INSERT INTO ret_preliquida (
                         f_liquida,
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
                         origen
                      )
                      VALUES (
                         ret_preliquida_f_liquida,
                         ret_preliquida_id_derechohabiente,
                         ret_preliquida_subcuenta_vol,
                         ret_preliquida_fondo_inversion,
                         ret_preliquida_movto_cargo_vol,
                         ret_preliquida_folio_liquida,
                         ret_preliquida_id_referencia,
                         ret_preliquida_monto_acciones,
                         ret_preliquida_monto_pesos,
                         ret_preliquida_f_valor,
                         ret_preliquida_f_registro,
                         ret_preliquida_h_registro,
                         ret_preliquida_origen             
                      );
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento_abono;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento_abono;
                      -- se hace el abono a vivienda 97
                      INSERT INTO ret_preliquida (
                         f_liquida,
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
                         origen
                      )
                      VALUES (
                         ret_preliquida_f_liquida,
                         ret_preliquida_id_derechohabiente,
                         ret_preliquida_subcuenta,
                         ret_preliquida_fondo_inversion,
                         ret_preliquida_movto_abono_vol,
                         ret_preliquida_folio_liquida,
                         ret_preliquida_id_referencia,
                         ret_preliquida_monto_acciones,
                         ret_preliquida_monto_pesos,
                         ret_preliquida_f_valor,
                         ret_preliquida_f_registro,
                         ret_preliquida_h_registro,
                         ret_preliquida_origen             
                      );
                      
                  END IF 
                  -- el saldo se preliquida como parte del monto solicitado
                  
                  LET ret_preliquida_monto_acciones     = (ret_disposicion_aivs_viv97) * v_signo_movimiento;
                  LET v_pesos_aiv97                     = (ret_disposicion_aivs_viv97) * v_valor_fondo;
                  LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento;
                  -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
                  INSERT INTO ret_preliquida (
                     f_liquida ,
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
                     origen             
                  )
                  VALUES (
                     ret_preliquida_f_liquida,
                     ret_preliquida_id_derechohabiente,
                     ret_preliquida_subcuenta,
                     ret_preliquida_fondo_inversion,
                     ret_preliquida_movimiento,
                     ret_preliquida_folio_liquida,
                     ret_preliquida_id_referencia,
                     ret_preliquida_monto_acciones,
                     ret_preliquida_monto_pesos,
                     ret_preliquida_f_valor,
                     ret_preliquida_f_registro,
                     ret_preliquida_h_registro,
                     ret_preliquida_origen
                  );
               END IF
               
               -- se cuenta un registro preliquidado
               LET v_num_regs_preliquidados = v_num_regs_preliquidados + 1;
            END IF
         END IF
      END IF -- no tiene credito 43bis
      
      -- si alguna de las dos cuentas se sobregiro, y la otra no, se registran sus saldos
      IF ( v_viv92_sobregirada = 1 OR v_viv97_sobregirada = 1 ) THEN
         -- se verifica si ya esta registrado el sobregiro en ret_his_saldo
         -- si no se tiene
         IF ( v_viv92_sobregirada = 0 ) THEN
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                           8,
                                                           11,
                                                           v_saldo_92_aivs,
                                                           v_saldo_92_pesos,
                                                           p_folio,
                                                           TODAY,
                                                           CURRENT HOUR TO SECOND);
         END IF
         
         -- si no se tiene
         IF ( v_viv97_sobregirada = 0 ) THEN
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                           4,
                                                           11,
                                                           v_saldo_97_aivs,
                                                           v_saldo_97_pesos,
                                                           p_folio,
                                                           TODAY,
                                                           CURRENT HOUR TO SECOND);

            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                           55,
                                                           11,
                                                           v_saldo_vol_aivs,
                                                           v_saldo_vol_pesos,
                                                           p_folio,
                                                           TODAY,
                                                           CURRENT HOUR TO SECOND);
         END IF
      END IF
      
      -- ==========================================================================
      -- VIVIENDA 72
      IF ( ret_disposicion_importe_viv72 > 0 ) THEN
         LET ret_preliquida_subcuenta = 40; -- vivienda 72
         LET ret_preliquida_fondo_inversion = 11; -- 
         LET ret_preliquida_movimiento = p_movimiento; --212; -- cargo por retiro por disposicion de recursos
         LET ret_preliquida_folio_liquida = p_folio;
         LET ret_preliquida_id_referencia = ret_disposicion_id_solicitud; -- VERIFICAR QUE CON ESTE QUEDA
         LET ret_preliquida_monto_acciones = 0; -- 
         LET ret_preliquida_monto_pesos = ret_disposicion_importe_viv72 * v_signo_movimiento;
         
         
         -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                       ret_disposicion_id_derechohabiente,
                                       40,
                                       NULL)
                          INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_72_pesos;

         -- si no se tiene saldo suficiente se rechaza la solicitud por insuficiencia de saldo
         IF ( ret_disposicion_importe_viv72 > v_saldo_72_pesos ) THEN
            -- se marca que el monto de vivienda 72 se rechazo
            LET v_rechazo_viv72 = 1;
         
            -- se actualiza la solicitud a rechazada por no tener saldo suficiente
            UPDATE ret_disposicion
            SET    estado_solicitud = 100, -- rechazada
                   cod_rechazo      = 10 -- saldo insuficiente
            WHERE  id_solicitud = ret_disposicion_id_solicitud;
                   
            -- se desmarca la cuenta
            EXECUTE FUNCTION fn_desmarca_cuenta(
                    ret_disposicion_id_derechohabiente
                   ,v_marca_disposicion -- marca de disposicion
                   ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                   ,40 -- estado marca
                   ,v_marca_disposicion -- marca de la causa / se usa la misma por ser rechazo por validacion
                   ,p_usuario_cod
                   ,p_proceso_cod)
               INTO v_i_estado_marca;

            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,
                                                           ret_preliquida_subcuenta,
                                                           ret_preliquida_fondo_inversion,
                                                           0,
                                                           v_saldo_72_pesos,
                                                           p_folio,
                                                           TODAY,
                                                           CURRENT HOUR TO SECOND);
         END IF

         -- si no se rechazo
         IF ( v_rechazo_viv72 = 0 ) THEN
            -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
            INSERT INTO ret_preliquida (
               f_liquida,
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
               origen             
            )
            VALUES (
               ret_preliquida_f_liquida,
               ret_preliquida_id_derechohabiente,
               ret_preliquida_subcuenta,
               ret_preliquida_fondo_inversion,
               ret_preliquida_movimiento,
               ret_preliquida_folio_liquida,
               ret_preliquida_id_referencia,
               ret_preliquida_monto_acciones,
               ret_preliquida_monto_pesos,
               ret_preliquida_f_valor,
               ret_preliquida_f_registro,
               ret_preliquida_h_registro,
               ret_preliquida_origen             
            );

            -- se cuenta un registro preliquidado
            LET v_num_regs_preliquidados = v_num_regs_preliquidados + 1;
         END IF
      END IF
      
      -- si hubo algun rechazo no se actualiza
      IF ( v_rechazo_viv92 = 0 AND v_rechazo_viv97 = 0 AND v_rechazo_viv72 = 0 ) THEN
      
         -- 13Nov2013. si es un caso con 43bis
         IF ( v_tiene_credito43bis = 1 ) THEN
         
            -- si solo tenia solicitud de vivienda 97 este no se le paga y se rechaza la solicitud
            -- por tener credito 43 bis
            --IF ( ret_disposicion_aivs_viv92 = 0 AND ret_disposicion_aivs_viv97 <> 0 ) THEN
            -- si tiene 43bis y si se solicita el saldo 97 no se paga, se paga si sólo se solicita viv92
            IF ( ret_disposicion_aivs_viv97 <> 0 ) THEN
               -- solicitud rechazada
               UPDATE ret_disposicion
               SET    estado_solicitud = 100, -- rechazada
                      cod_rechazo      = v_con_credito_43bis -- con credito 43bis
               WHERE  id_solicitud     = ret_disposicion_id_solicitud;
                
               -- se borran los movimientos dados
               DELETE FROM ret_preliquida
               WHERE  folio_liquida = p_folio
               AND    id_referencia = ret_disposicion_id_solicitud;
               
               -- se desmarca la cuenta
               EXECUTE FUNCTION fn_desmarca_cuenta(
                       ret_disposicion_id_derechohabiente
                      ,v_marca_disposicion -- marca de disposicion
                      ,ret_disposicion_id_solicitud -- identificador de registro de archivo o lote
                      ,40 -- estado marca
                      ,v_marca_disposicion -- marca de la causa / se usa la misma por ser rechazo por validacion
                      ,p_usuario_cod
                      ,p_proceso_cod)
                  INTO v_i_estado_marca;

            ELSE
               -- solo se le pago vivienda 92, y se preliquida
               UPDATE ret_disposicion
               SET    estado_solicitud = 50 -- preliquidada
               WHERE  id_solicitud     = ret_disposicion_id_solicitud;
            END IF
         ELSE
            -- se actualiza la solicitud como preliquidada
            UPDATE ret_disposicion
            SET estado_solicitud = 50 -- preliquidada
            WHERE id_solicitud   = ret_disposicion_id_solicitud;
         END IF
      ELSE
         -- alguno de los montos solicitados excedio el saldo del trabajador
         -- se borran los movimientos dados
         DELETE FROM ret_preliquida
         WHERE  folio_liquida = p_folio
         AND    id_referencia = ret_disposicion_id_solicitud;
      END IF
       
   END FOREACH;

   UPDATE STATISTICS FOR TABLE ret_preliquida;

   -- si no se preliquido ningun registro supone un error
   IF ( v_num_regs_preliquidados < 1 ) THEN
      LET v_si_resultado = 1;
      LET isam_err = 0;
      LET v_c_msj = 'No se preliquidaron registros. Revise la consulta general para ver el motivo del rechazo.';
   END IF

   -- se devuelve el resultado
   RETURN v_si_resultado, isam_err, v_c_msj, ret_disposicion_id_solicitud;
END FUNCTION -- preliquidacion de retiros por disposicion de recursos
;


