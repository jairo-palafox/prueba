






CREATE FUNCTION "safreviv".fn_ret_preliquida_fc(p_usuario_cod CHAR(20), p_folio DECIMAL(10,0),
                                     p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                     p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)

-- detalle de la tabla historica/integrada de retiros por fortalecimiento al credito
-- ret_fortalecimiento_credito
DEFINE ret_fc_id_solicitud        decimal(9,0)          ;
DEFINE ret_fc_id_derechohabiente  decimal(9,0)          ;
DEFINE ret_fc_folio               decimal(9,0)          ;
DEFINE ret_fc_f_solicitud         date                  ;
DEFINE ret_fc_h_solicitud         datetime day to second;
DEFINE ret_fc_importe_viv         decimal(12,2)         ;
DEFINE ret_fc_estado_solicitud    smallint              ;
DEFINE ret_fc_cod_rechazo         smallint              ;
DEFINE ret_fc_usuario             char(20)              ;


-- campos de la tabla de preliquidacion de retiros
DEFINE ret_preliquida_f_liquida          DATE                   ;
DEFINE ret_preliquida_id_derechohabiente DECIMAL(9,0)           ;
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
DEFINE ret_preliquida_origen             CHAR(20);
   


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
DEFINE v_signo_movimiento                   SMALLINT;
-- RECUPERADOS

 
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
DEFINE v_saldo_72_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv72
DEFINE v_resultado_consulta                    SMALLINT;
 
DEFINE v_rechazo_viv92                         SMALLINT; -- rechazo de retiro de vivienda 92
DEFINE v_rechazo_viv97                         SMALLINT; -- rechazo de retiro de vivienda 97
DEFINE v_rechazo_viv72                         SMALLINT; -- rechazo de retiro de vivienda 72
 
DEFINE cod_rechazo_1                           SMALLINT;
DEFINE cod_rechazo_2                           SMALLINT;
DEFINE cod_rechazo_3                           SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                        INTEGER;
DEFINE v_marca_fc                              INTEGER; -- 805 de acuerdo a catalogo

DEFINE v_num_regs_preliquidados                INTEGER; -- numero de registros preliquidados
DEFINE v_folio                                 DECIMAL(10);
DEFINE v_nombre_archivo                 CHAR(18);
DEFINE v_f_operacion_procesar           DATE;
DEFINE v_f_carga_afore                  DATE;
DEFINE v_resultado_operacion            SMALLINT;
DEFINE v_cod_rechazo_1                  SMALLINT;
DEFINE v_cod_rechazo_2                  SMALLINT;
DEFINE v_cod_rechazo_3                  SMALLINT;
DEFINE v_movimiento                     SMALLINT; -- clave de movimiento

-- Control de Excepciones
DEFINE v_si_resultado                   SMALLINT    ;
DEFINE sql_err                          INTEGER     ;
DEFINE isam_err                         INTEGER     ;
DEFINE err_txt                          VARCHAR(200);
DEFINE v_c_msj                          VARCHAR(200);

   -- se declara el comportamiento al ocurrir una excepcion
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se asume que no hay errores
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finaliz� correctamente';

   -- se inician las variables para marca
   LET v_marca_fc       = 807; -- marca para retiro fortalecimiento credito
   LET v_i_estado_marca = 0;
 
   -- se obtiene el signo del movimiento
   LET v_movimiento = 462; -- CARGO, RETIRO POR FORTALECIMIENTO AL CREDITO
   
   SELECT tipo
   INTO v_signo_movimiento
   FROM
      cat_movimiento
   WHERE
      movimiento = v_movimiento; 
   
   -- se inicia el contador de registros preliquidados
   LET v_num_regs_preliquidados = 0;
   
   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      id_solicitud        ,
      id_derechohabiente  ,
      folio               ,
      f_solicitud         ,
      h_solicitud         ,
      importe_viv         ,
      estado_solicitud    ,
      cod_rechazo         ,
      usuario             
   INTO
      ret_fc_id_solicitud        ,
      ret_fc_id_derechohabiente  ,
      ret_fc_folio               ,
      ret_fc_f_solicitud         ,
      ret_fc_h_solicitud         ,
      ret_fc_importe_viv         ,
      ret_fc_estado_solicitud    ,
      ret_fc_cod_rechazo         ,
      ret_fc_usuario             
   FROM
      ret_fortalecimiento_credito
   WHERE folio            = p_folio
     AND estado_solicitud = 30 -- integradas
           
      --=========================================================================
      --se concatena la cadena RETIRO
      LET ret_preliquida_origen = "RETIRO FC";
      --=========================================================================
      --=========================================================================     
      -- asignacion de valores generales al registro de la tabla de preliquidacion
      LET ret_preliquida_f_liquida          = TODAY; -- DATE                   ;
      LET ret_preliquida_id_derechohabiente = ret_fc_id_derechohabiente;
      LET ret_preliquida_f_valor            = TODAY; -- la fecha del valor del fondo del encabezado
      LET ret_preliquida_f_registro         = TODAY; -- la fecha de ejecucion
      LET ret_preliquida_h_registro         = CURRENT HOUR TO SECOND;

      LET ret_preliquida_subcuenta          = 49; -- fortalecimiento al credito
      LET ret_preliquida_fondo_inversion    = 10; -- siefore 10 pesos
      LET ret_preliquida_movimiento         = v_movimiento;
      LET ret_preliquida_folio_liquida      = p_folio;
      LET ret_preliquida_id_referencia      = ret_fc_id_solicitud;
      LET ret_preliquida_monto_acciones     = ret_fc_importe_viv * v_signo_movimiento; -- la paridad peso-AIV es 1 a 1
      
      LET ret_preliquida_monto_pesos        = ret_fc_importe_viv * v_signo_movimiento;
           
      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
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
      
      -- se cuenta un registro preliquidado
      LET v_num_regs_preliquidados = v_num_regs_preliquidados + 1;
            
      -- se actualiza la solicitud como preliquidada
      UPDATE ret_fortalecimiento_credito
         SET estado_solicitud = 50 -- preliquidada
       WHERE id_solicitud = ret_fc_id_solicitud;
      
         
   END FOREACH;

   UPDATE STATISTICS FOR TABLE ret_preliquida;
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
 
   -- si no se preliquido ningun registro supone un error
   IF ( v_num_regs_preliquidados < 1 ) THEN
      LET v_si_resultado = 1;
      LET isam_err = 0;
      LET v_c_msj = 'No se preliquidaron registros. Revise la consulta generar para ver el motivo del rechazo.';
   END IF
   
   -- se devuelve el resultado
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION -- preliquidacion de retiros por disposicion de recursos
;


