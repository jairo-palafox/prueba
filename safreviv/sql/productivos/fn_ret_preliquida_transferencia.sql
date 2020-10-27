






CREATE FUNCTION "safreviv".fn_ret_preliquida_transferencia(p_usuario_cod    CHAR(20), 
                                                p_folio          DECIMAL(10,0),
                                                p_nombre_archivo VARCHAR(40),
                                                p_pid            DECIMAL(9,0),
                                                p_proceso_cod    SMALLINT,
                                                p_max_aivs_sobregiro DECIMAL(20,6)
                                                ) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)

   -- detalle de la tabla historica/integrada de retiros por transferencia
   DEFINE ret_transf_id_solicitud                   DECIMAL(9,0)           ;
   DEFINE ret_transf_id_derechohabiente             DECIMAL(9,0)           ;
   DEFINE ret_transf_id_ret_matriz_derecho          SMALLINT               ;
   DEFINE ret_transf_sec_pension                    SMALLINT               ;
   DEFINE ret_transf_diag_registro                  CHAR(3)                ;
   DEFINE ret_transf_folio                          DECIMAL(9,0)           ;
   DEFINE ret_transf_estado_solicitud               SMALLINT               ;
   DEFINE ret_transf_curp                           CHAR(18)               ;
   DEFINE ret_transf_nombre_datamart                CHAR(50)               ;
   DEFINE ret_transf_nombre_afore                   CHAR(40)               ;
   DEFINE ret_transf_paterno_afore                  CHAR(40)               ;
   DEFINE ret_transf_materno_afore                  CHAR(40)               ;
   DEFINE ret_transf_tpo_movimiento                 CHAR(3)                ;
   DEFINE ret_transf_f_inicio_pension               DATE                   ;
   DEFINE ret_transf_f_resolucion                   CHAR(18)               ;
   DEFINE ret_transf_porcentaje_valuacion           DECIMAL(5,2)           ;
   DEFINE ret_transf_semanas_cotizadas              INTEGER                ;
   DEFINE ret_transf_f_carga_datamart               DATE                   ;
   DEFINE ret_transf_estado_sub_viv                 SMALLINT               ;
   DEFINE ret_transf_aivs_viv97                     DECIMAL(14,6)          ;
   DEFINE ret_transf_cve_afore                      SMALLINT               ;
   DEFINE ret_transf_cod_rechazo                    SMALLINT               ;

   -- campos de la tabla de preliquidacion de retiros
   DEFINE ret_preliquida_f_liquida                  DATE                   ;
   DEFINE ret_preliquida_id_derechohabiente         DECIMAL(9,0)           ;
   DEFINE ret_preliquida_subcuenta                  SMALLINT               ;
   DEFINE ret_preliquida_fondo_inversion            SMALLINT               ;
   DEFINE ret_preliquida_movimiento                 SMALLINT               ;
   DEFINE ret_preliquida_folio_liquida              DECIMAL(9,0)           ;
   DEFINE ret_preliquida_id_referencia              DECIMAL(9,0)           ;
   DEFINE ret_preliquida_monto_acciones             DECIMAL(20,6)          ;
   DEFINE ret_preliquida_monto_pesos                DECIMAL(20,2)          ;
   DEFINE ret_preliquida_f_valor                    DATE                   ;
   DEFINE ret_preliquida_f_registro                 DATE                   ;
   DEFINE ret_preliquida_h_registro                 DATETIME HOUR TO SECOND;
   DEFINE ret_preliquida_origen                     CHAR(20);
   DEFINE ret_preliquida_subcuenta_vol              SMALLINT               ;
   DEFINE ret_preliquida_movto_cargo_vol            SMALLINT               ;
   DEFINE ret_preliquida_movto_abono_vol            SMALLINT               ;

   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                      DECIMAL(9,0)           ;
   DEFINE v_id_solicitud                            DECIMAL(9,0)           ;

   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                             DECIMAL(14,6)          ;
   DEFINE v_pesos_aiv97                             DECIMAL(14,6)          ;
   DEFINE v_pesos_aiv92                             DECIMAL(14,6)          ;
   DEFINE v_fecha_valor                             DATE                   ;

   -- signo del movimiento
   DEFINE v_signo_movimiento                        SMALLINT               ;
   DEFINE v_signo_movimiento_sobregiro              SMALLINT               ;
   DEFINE v_diferencia_aivs                         DECIMAL(14,6)          ;
   DEFINE v_movimiento                              SMALLINT               ;
   DEFINE v_movimiento_sobregiro                    SMALLINT               ;
   DEFINE v_movimiento_cargo_traspaso               SMALLINT               ;
   DEFINE v_movimiento_abono_traspaso               SMALLINT               ; 
   -- 001 signo del movimiento de abono
   DEFINE v_movimiento_abono                        SMALLINT               ;
   DEFINE v_signo_movimiento_abono                  SMALLINT               ;
    
   DEFINE v_datos_validos_tpo_registro              CHAR(2)                ;
   DEFINE v_datos_validos_id_servicio               CHAR(2)                ;
   DEFINE v_datos_validos_f_operacion               CHAR(8)                ;
   DEFINE v_dte_f_valido_f_operacion                DATE                   ;
   --
   DEFINE v_datos_top_validos_tpo_registro          CHAR(2)                ;
   DEFINE v_datos_top_validos_id_servicio           CHAR(2)                ;
   DEFINE v_datos_top_validos_id_operacion          CHAR(2)                ;
   DEFINE v_datos_top_validos_tpo_entidad_destino   CHAR(2)                ;
   DEFINE v_datos_top_validos_cve_entidad_destino   CHAR(3)                ;
   DEFINE v_datos_top_validos_f_operacion           CHAR(8)                ;
   --
   DEFINE v_sumario_importe_total                   DECIMAL(22,2)          ;
   DEFINE v_sumario_aiv_97                          DECIMAL(18,6)          ;
   DEFINE v_sumario_aiv_92                          DECIMAL(18,6)          ;
   DEFINE v_detalle_importe_total                   DECIMAL(22,2)          ;
   DEFINE v_detalle_aiv_97                          DECIMAL(18,6)          ;
   DEFINE v_detalle_aiv_92                          DECIMAL(18,6)          ;

   DEFINE v_saldo_97_aivs                           DECIMAL(18,6)          ; -- saldo del derechohabiente en viv92
   DEFINE v_saldo_97_pesos                          DECIMAL(18,6)          ; -- saldo del derechohabiente en viv97
   DEFINE v_saldo_vol_aivs                          DECIMAL(18,6)          ; -- saldo del derechohabiente en viv92
   DEFINE v_saldo_vol_pesos                         DECIMAL(18,6)          ; -- saldo del derechohabiente en viv97
   DEFINE v_resultado_consulta                      SMALLINT               ;

   DEFINE v_rechazo_viv97                           SMALLINT               ; -- rechazo de retiro de vivienda 97

   -- para marcar las cuentas
   DEFINE v_i_estado_marca                          INTEGER                ;
   DEFINE v_marca_transferencia                     INTEGER                ; -- 806 de acuerdo a catalogo

    
   DEFINE cod_rechazo_1                             SMALLINT               ;
   DEFINE cod_rechazo_2                             SMALLINT               ;
   DEFINE cod_rechazo_3                             SMALLINT               ;
   --
   DEFINE v_folio                                   DECIMAL(10)            ;
   DEFINE v_nombre_archivo                          CHAR(18)               ;
   DEFINE v_f_operacion_procesar                    DATE                   ;
   DEFINE v_f_carga_afore                           DATE                   ;
   DEFINE v_resultado_operacion                     SMALLINT               ;
   DEFINE v_cod_rechazo_1                           SMALLINT               ;
   DEFINE v_cod_rechazo_2                           SMALLINT               ;
   DEFINE v_cod_rechazo_3                           SMALLINT               ;

   DEFINE v_num_reg_preliquidados                   INTEGER                ; -- numero de registros preliquidados
   -- Control de Excepciones
   DEFINE v_si_resultado                            SMALLINT               ;
   DEFINE sql_err                                   INTEGER                ;
   DEFINE isam_err                                  INTEGER                ;
   DEFINE err_txt                                   VARCHAR(200)           ;
   DEFINE v_c_msj                                   VARCHAR(200)           ;
   DEFINE v_monto_viv97_invalido                    SMALLINT               ;

   -- se declara la excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/debug_ret_preliquida_transferencia.txt");

   -- se asume que no hay errores
   LET v_si_resultado                  = 0;
   LET isam_err                        = 0;
   LET v_c_msj                         = 'El proceso finalizó correctamente';

   -- se define el movimiento de retiro y el de sobregiro 
   LET v_movimiento                    = 222;
   LET v_movimiento_sobregiro          = 842;
   LET v_monto_viv97_invalido          = 767;
   LET v_movimiento_cargo_traspaso     = 1662;
   LET v_movimiento_abono_traspaso     = 911;
   LET ret_preliquida_subcuenta_vol    = 55;
   LET ret_preliquida_movto_cargo_vol  = 1662;
   LET ret_preliquida_movto_abono_vol  = 911;   
   
   -- 001 se inicializa la variable de movimiento de abono
   LET v_movimiento_abono              = 1011;   -- movimiento para abono variación menor retiros

   -- se actualiza el folio en la tabla de control de operacion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 3
   AND    pid         = p_pid;
   
   -- Agregar folio a operacion de integracion
   UPDATE glo_folio
   SET    status  = 1 -- preliquidado
   WHERE  folio   = p_folio;
 
   -- se obtiene el valor del fondo
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   ret_cza_transferencia
   WHERE  folio = p_folio;
   
   -- se obtiene la fecha del valor del fondo del encabezado
   SELECT f_valor_transferencia
   INTO   v_fecha_valor
   FROM   ret_cza_transferencia
   WHERE  folio = p_folio;

   -- se obtiene el signo del movimiento
   SELECT tipo
   INTO   v_signo_movimiento
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento;

   -- se obtiene el signo del movimiento de sobregiro
   SELECT tipo
   INTO   v_signo_movimiento_sobregiro
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_sobregiro;
	
-- 001 se obtiene el signo del movimiento de abono      
   SELECT tipo
   INTO   v_signo_movimiento_abono
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono;

   -- para marcar las cuentas
   LET v_i_estado_marca       = 0;
   LET v_marca_transferencia  = 806; -- 806 Retiro por transferencia

   -- se inicia el contador de registros preliquidados
   LET v_num_reg_preliquidados = 0;

   -- se obtienen los datos del detalle
   FOREACH
      SELECT
               id_solicitud                     ,
               id_derechohabiente               ,
               id_ret_matriz_derecho            ,
               sec_pension                      ,
               diag_registro                    ,
               folio                            ,
               estado_solicitud                 ,
               curp                             ,
               nombre_datamart                  ,
               nombre_afore                     ,
               paterno_afore                    ,
               materno_afore                    ,
               tpo_movimiento                   ,
               f_inicio_pension                 ,
               f_resolucion                     ,
               porcentaje_valuacion             ,
               semanas_cotizadas                ,
               f_carga_datamart                 ,
               estado_sub_viv                   ,
               aivs_viv97                       ,
               cve_afore                        ,
               cod_rechazo           
      INTO
               ret_transf_id_solicitud          ,
               ret_transf_id_derechohabiente    ,
               ret_transf_id_ret_matriz_derecho ,
               ret_transf_sec_pension           ,
               ret_transf_diag_registro         ,
               ret_transf_folio                 ,
               ret_transf_estado_solicitud      ,
               ret_transf_curp                  ,
               ret_transf_nombre_datamart       ,
               ret_transf_nombre_afore          ,
               ret_transf_paterno_afore         ,
               ret_transf_materno_afore         ,
               ret_transf_tpo_movimiento        ,
               ret_transf_f_inicio_pension      ,
               ret_transf_f_resolucion          ,
               ret_transf_porcentaje_valuacion  ,
               ret_transf_semanas_cotizadas     ,
               ret_transf_f_carga_datamart      ,
               ret_transf_estado_sub_viv        ,
               ret_transf_aivs_viv97            ,
               ret_transf_cve_afore             ,
               ret_transf_cod_rechazo           
      FROM     ret_transferencia
      WHERE    folio            = p_folio   
      AND      estado_solicitud = 30 -- integrada / recibida procesar
   
      -- se inicia la diferencia en cero
      LET v_diferencia_aivs                 = 0;
   
      LET v_saldo_97_aivs                   = 0;
      LET v_saldo_97_pesos                  = 0;
      LET v_resultado_consulta              = 0;
      LET v_rechazo_viv97                   = 0;
   
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- asignacion de valores generales al registro de la tabla de preliquidacion
      LET ret_preliquida_f_liquida          = TODAY; -- DATE                   ;
      LET ret_preliquida_id_derechohabiente = ret_transf_id_derechohabiente;
      LET ret_preliquida_f_valor            = v_fecha_valor; -- la fecha del valor del fondo del encabezado
      LET ret_preliquida_f_registro         = TODAY; -- la fecha de ejecucion
      LET ret_preliquida_h_registro         = CURRENT HOUR TO SECOND; -- VALIDAR QUE ESTA ES

      --=========================================================================
      --se obtiene el origen del retiro 
      --=========================================================================
      SELECT tpo_retiro
      INTO   ret_preliquida_origen
      FROM   ret_matriz_derecho      
      WHERE  id_ret_matriz_derecho = ret_transf_id_ret_matriz_derecho;
       
      --se concatena la cadena RETIRO
      LET ret_preliquida_origen              = 'RETIRO '||ret_preliquida_origen;
       
      -- VIVIENDA 97
       LET ret_preliquida_subcuenta          = 4; -- vivienda 97
       LET ret_preliquida_fondo_inversion    = 11; -- 
       LET ret_preliquida_movimiento         = 222; -- CARGO RETIRO POR TRANSFERENCIA
       LET ret_preliquida_folio_liquida      = p_folio;
       LET ret_preliquida_id_referencia      = ret_transf_id_solicitud; -- VERIFICAR QUE CON ESTE QUEDA
       LET ret_preliquida_monto_acciones     = ret_transf_aivs_viv97 * v_signo_movimiento; -- 

       -- se calcula monto en pesos de las acciones
       LET v_pesos_aiv97                     = ret_transf_aivs_viv97 * v_valor_fondo;
       LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento; -- 
       
       
       -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
       EXECUTE FUNCTION fn_saldo_dia(NULL,
                                     ret_transf_id_derechohabiente,
                                     4, -- viv97
                                     NULL)
                        INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_97_pesos;

                        -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
       EXECUTE FUNCTION fn_saldo_dia(NULL,
                                     ret_transf_id_derechohabiente,
                                     55, -- aportaciones voluntarias
                                     NULL)
                        INTO v_resultado_consulta, v_saldo_vol_aivs, v_saldo_vol_pesos;

       -- se calcula la diferencia en AIVs
       IF (v_saldo_97_aivs + v_saldo_vol_aivs) > 0 THEN 
          IF ( ret_transf_aivs_viv97 > (v_saldo_97_aivs + v_saldo_vol_aivs)) THEN
             LET v_diferencia_aivs = ret_transf_aivs_viv97 - (v_saldo_97_aivs + v_saldo_vol_aivs);
             
             -- se inserta el registro en la tabla historica de saldos
             EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud             ,
                                                            ret_preliquida_subcuenta            ,
                                                            ret_preliquida_fondo_inversion      ,
                                                            v_saldo_97_aivs                     ,
                                                            v_saldo_97_pesos                    ,
                                                            p_folio                             ,
                                                            TODAY                               ,
                                                            CURRENT HOUR TO SECOND              );
             -- se inserta el registro en la tabla historica de saldos
             EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud             ,
                                                            55                                  , 
                                                            ret_preliquida_fondo_inversion      ,
                                                            v_saldo_vol_aivs                    ,
                                                            v_saldo_vol_aivs                    ,
                                                            p_folio                             ,
                                                            TODAY                               ,
                                                            CURRENT HOUR TO SECOND              );
          END IF


          -- si la diferencia es mayor a las AIVs permitidas, no se preliquida
          IF ( v_diferencia_aivs > p_max_aivs_sobregiro ) THEN
             -- se marca que el monto de vivienda 92 se rechazo
             LET v_rechazo_viv97 = 1;
          
             -- se actualiza la solicitud a rechazada por no tener saldo suficiente
             UPDATE ret_transferencia
                SET estado_solicitud = 100, -- rechazada
                    cod_rechazo      = v_monto_viv97_invalido -- saldo insuficiente segun PROCESAR
              WHERE id_solicitud     = ret_transf_id_solicitud;
                    
             -- se desmarca la cuenta
             EXECUTE FUNCTION fn_desmarca_cuenta(
                     ret_transf_id_derechohabiente
                    ,v_marca_transferencia -- marca de transferencia
                    ,ret_transf_id_solicitud -- identificador de registro de archivo o lote
                    ,40 -- estado marca / rechazo validacion
                    ,v_marca_transferencia -- marca de la causa / rechazo por validacion
                    ,p_usuario_cod
                    ,p_proceso_cod)
                INTO v_i_estado_marca;

          END IF
       ELSE 
          -- se marca que el monto de vivienda 97 se rechazo
          LET v_rechazo_viv97 = 1;
       
          -- se actualiza la solicitud a rechazada por no tener saldo suficiente
          UPDATE ret_transferencia
             SET estado_solicitud = 100, -- rechazada
                 cod_rechazo      = v_monto_viv97_invalido -- saldo insuficiente segun PROCESAR
           WHERE id_solicitud     = ret_transf_id_solicitud;
                 
          -- se desmarca la cuenta
          EXECUTE FUNCTION fn_desmarca_cuenta(
                  ret_transf_id_derechohabiente
                 ,v_marca_transferencia -- marca de transferencia
                 ,ret_transf_id_solicitud -- identificador de registro de archivo o lote
                 ,40 -- estado marca / rechazo validacion
                 ,v_marca_transferencia -- marca de la causa / rechazo por validacion
                 ,p_usuario_cod
                 ,p_proceso_cod)
             INTO v_i_estado_marca;
       END IF
       -- si la solicitud tuvo saldo suficiente, se preliquida
       IF ( v_rechazo_viv97 = 0 ) THEN
       
          IF ( v_diferencia_aivs > 0 ) THEN
             -- el saldo se preliquida como parte del monto solicitado                  -- si se cuenta con saldo en la subcuenta de aportaciones voluntarias
                  -- primero se hara el cargo a las voluntarias, luego se abona a la subcuenta
                  -- de vivienda 97 y al final se realiza un cargo por el total a la subcuenta de vivienda 97
                  IF v_saldo_vol_aivs > 0 THEN -- se generan los movimientos de cargo y abono de las aportaciones voluntarias
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento;
                      -- se hace el cargo a la voluntaria
                      INSERT INTO ret_preliquida (
                         f_liquida          ,
                         id_derechohabiente ,
                         subcuenta          ,
                         fondo_inversion    ,
                         movimiento         ,
                         folio_liquida      ,
                         id_referencia      ,
                         monto_acciones     ,
                         monto_pesos        ,
                         f_valor            ,
                         f_registro         ,
                         h_registro         ,
                         origen
                      )
                      VALUES (
                         ret_preliquida_f_liquida          ,
                         ret_preliquida_id_derechohabiente ,
                         ret_preliquida_subcuenta_vol      ,
                         ret_preliquida_fondo_inversion    ,
                         ret_preliquida_movto_cargo_vol    ,
                         ret_preliquida_folio_liquida      ,
                         ret_preliquida_id_referencia      ,
                         ret_preliquida_monto_acciones     ,
                         ret_preliquida_monto_pesos        ,
                         ret_preliquida_f_valor            ,
                         ret_preliquida_f_registro         ,
                         ret_preliquida_h_registro         ,
                         ret_preliquida_origen             
                      );
                      LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento_abono;
                      LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento_abono;
                      -- se hace el abono a vivienda 97
                      INSERT INTO ret_preliquida (
                         f_liquida          ,
                         id_derechohabiente ,
                         subcuenta          ,
                         fondo_inversion    ,
                         movimiento         ,
                         folio_liquida      ,
                         id_referencia      ,
                         monto_acciones     ,
                         monto_pesos        ,
                         f_valor            ,
                         f_registro         ,
                         h_registro         ,
                         origen
                      )
                      VALUES (
                         ret_preliquida_f_liquida          ,
                         ret_preliquida_id_derechohabiente ,
                         ret_preliquida_subcuenta          ,
                         ret_preliquida_fondo_inversion    ,
                         ret_preliquida_movto_abono_vol    ,
                         ret_preliquida_folio_liquida      ,
                         ret_preliquida_id_referencia      ,
                         ret_preliquida_monto_acciones     ,
                         ret_preliquida_monto_pesos        ,
                         ret_preliquida_f_valor            ,
                         ret_preliquida_f_registro         ,
                         ret_preliquida_h_registro         ,
                         ret_preliquida_origen             
                      );
                      
                  END IF 

             LET ret_preliquida_monto_acciones     = (v_saldo_97_aivs + v_saldo_vol_aivs + v_diferencia_aivs) * v_signo_movimiento;
             LET v_pesos_aiv97                     = (v_saldo_97_aivs + v_saldo_vol_aivs + v_diferencia_aivs) * v_valor_fondo;
             LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento;
             
             INSERT INTO ret_preliquida (
                f_liquida          ,
                id_derechohabiente ,
                subcuenta          ,
                fondo_inversion    ,
                movimiento         ,
                folio_liquida      ,
                id_referencia      ,
                monto_acciones     ,
                monto_pesos        ,
                f_valor            ,
                f_registro         ,
                h_registro         ,
                origen
             )
             VALUES (
                ret_preliquida_f_liquida          ,
                ret_preliquida_id_derechohabiente ,
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
             
             -- la diferencia se preliquida con un movimiento de sobregiro
             -- el saldo se preliquida como parte del monto solicitado
			 -- 13feb2014. No se sobregira la cuenta
			 
--             LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_sobregiro;
--             LET v_pesos_aiv97                     = v_diferencia_aivs * v_valor_fondo;
--             LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento_sobregiro;
--             LET ret_preliquida_movimiento         = v_movimiento_sobregiro;
             
			 
--             INSERT INTO ret_preliquida (
--                f_liquida          ,
--                id_derechohabiente ,
--                subcuenta          ,
--                fondo_inversion    ,
--                movimiento         ,
--                folio_liquida      ,
--                id_referencia      ,
--                monto_acciones     ,
--                monto_pesos        ,
--                f_valor            ,
--                f_registro         ,
--                h_registro         ,
--                origen
--             )
--             VALUES (
--                ret_preliquida_f_liquida          ,
--                ret_preliquida_id_derechohabiente ,
--                ret_preliquida_subcuenta        ,
--                ret_preliquida_fondo_inversion    ,
--                ret_preliquida_movimiento         ,
--                ret_preliquida_folio_liquida      ,
--                ret_preliquida_id_referencia      ,
--                ret_preliquida_monto_acciones     ,
--                ret_preliquida_monto_pesos        ,
--                ret_preliquida_f_valor            ,
--                ret_preliquida_f_registro         ,
--                ret_preliquida_h_registro         ,
--                ret_preliquida_origen             
--             );
             
			 -- 001 se inserta en la tabla de preliquidacion de retiros como un abono
			 -- 001 se hace un abono por la diferencia del sobregiro
			   
			 LET ret_preliquida_monto_acciones     = v_diferencia_aivs * v_signo_movimiento_abono;
             LET v_pesos_aiv92                     = v_diferencia_aivs * v_valor_fondo;
             LET ret_preliquida_monto_pesos        = v_pesos_aiv92 * v_signo_movimiento_abono;
			 LET ret_preliquida_movimiento         = v_movimiento_abono;
             INSERT INTO ret_preliquida (
                f_liquida          ,
                id_derechohabiente ,
                subcuenta          ,
                fondo_inversion    ,
                movimiento         ,
                folio_liquida      ,
                id_referencia      ,
                monto_acciones     ,
                monto_pesos        ,
                f_valor            ,
                f_registro         ,
                h_registro         ,
                origen
             )
             VALUES (
                ret_preliquida_f_liquida          ,
                ret_preliquida_id_derechohabiente ,
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
              IF v_saldo_vol_aivs > 0 THEN -- se generan los movimientos de cargo y abono de las aportaciones voluntarias
                  LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento;
                  LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento;
                  -- se hace el cargo a la voluntaria
                  INSERT INTO ret_preliquida (
                     f_liquida          ,
                     id_derechohabiente ,
                     subcuenta          ,
                     fondo_inversion    ,
                     movimiento         ,
                     folio_liquida      ,
                     id_referencia      ,
                     monto_acciones     ,
                     monto_pesos        ,
                     f_valor            ,
                     f_registro         ,
                     h_registro         ,
                     origen
                  )
                  VALUES (
                     ret_preliquida_f_liquida          ,
                     ret_preliquida_id_derechohabiente ,
                     ret_preliquida_subcuenta_vol      ,
                     ret_preliquida_fondo_inversion    ,
                     ret_preliquida_movto_cargo_vol    ,
                     ret_preliquida_folio_liquida      ,
                     ret_preliquida_id_referencia      ,
                     ret_preliquida_monto_acciones     ,
                     ret_preliquida_monto_pesos        ,
                     ret_preliquida_f_valor            ,
                     ret_preliquida_f_registro         ,
                     ret_preliquida_h_registro         ,
                     ret_preliquida_origen             
                  );
                  LET ret_preliquida_monto_acciones     = (v_saldo_vol_aivs) * v_signo_movimiento_abono;
                  LET ret_preliquida_monto_pesos        = (v_saldo_vol_aivs * v_valor_fondo) * v_signo_movimiento_abono;
                  -- se hace el abono a vivienda 97
                  INSERT INTO ret_preliquida (
                     f_liquida          ,
                     id_derechohabiente ,
                     subcuenta          ,
                     fondo_inversion    ,
                     movimiento         ,
                     folio_liquida      ,
                     id_referencia      ,
                     monto_acciones     ,
                     monto_pesos        ,
                     f_valor            ,
                     f_registro         ,
                     h_registro         ,
                     origen
                  )
                  VALUES (
                     ret_preliquida_f_liquida          ,
                     ret_preliquida_id_derechohabiente ,
                     ret_preliquida_subcuenta          ,
                     ret_preliquida_fondo_inversion    ,
                     ret_preliquida_movto_abono_vol    ,
                     ret_preliquida_folio_liquida      ,
                     ret_preliquida_id_referencia      ,
                     ret_preliquida_monto_acciones     ,
                     ret_preliquida_monto_pesos        ,
                     ret_preliquida_f_valor            ,
                     ret_preliquida_f_registro         ,
                     ret_preliquida_h_registro         ,
                     ret_preliquida_origen             
                  );
                  
              END IF 

             LET ret_preliquida_monto_acciones     = (ret_transf_aivs_viv97) * v_signo_movimiento;
             LET v_pesos_aiv97                     = (ret_transf_aivs_viv97) * v_valor_fondo;
             LET ret_preliquida_monto_pesos        = v_pesos_aiv97 * v_signo_movimiento;
         
             -- se inserta en la tabla de preliquidacion de retiros
             INSERT INTO ret_preliquida (
                f_liquida          ,
                id_derechohabiente ,
                subcuenta          ,
                fondo_inversion    ,
                movimiento         ,
                folio_liquida      ,
                id_referencia      ,
                monto_acciones     ,
                monto_pesos        ,
                f_valor            ,
                f_registro         ,
                h_registro         ,
                origen
             )
             VALUES (
                ret_preliquida_f_liquida          ,
                ret_preliquida_id_derechohabiente ,
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
          END IF
       
         -- se actualiza la solicitud
         UPDATE ret_transferencia
            SET estado_solicitud = 50 -- preliquidada
          WHERE folio = p_folio
            AND id_solicitud = ret_transf_id_solicitud;
            
         -- se cuenta un registro preliquidado
         LET v_num_reg_preliquidados = v_num_reg_preliquidados + 1;
      END IF

   END FOREACH;
 
   UPDATE STATISTICS  FOR TABLE ret_preliquida;
 
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
END FUNCTION -- preliquidacion de retiros por transferencia
;


