






CREATE FUNCTION "safreviv".fn_restitucion_ret_excep_devol_ssv(p_folio_liquida      DECIMAL(10,0),
                                                   p_proceso_cod        SMALLINT,
                                                   p_opera_cod          SMALLINT,
                                                   p_usuario_cod        VARCHAR(20),
                                                   p_pid                DECIMAL(9,0),
                                                   p_estado_solicitud   SMALLINT,
                                                   p_estado_restitucion SMALLINT,
                                                   p_cod_rechazo        SMALLINT)
RETURNING SMALLINT, CHAR(3), INTEGER, INTEGER, CHAR(254);

DEFINE v_ret_preliquida_f_liquida          DATE;
DEFINE v_ret_preliquida_id_derechohabiente DECIMAL(9,0);
DEFINE v_ret_preliquida_subcuenta          SMALLINT;
DEFINE v_ret_preliquida_fondo_inversion    SMALLINT;
DEFINE v_ret_preliquida_movimiento         SMALLINT;
DEFINE v_ret_preliquida_folio_liquida      DECIMAL(9,0);
DEFINE v_ret_preliquida_id_referencia      DECIMAL(9,0);
DEFINE v_ret_preliquida_monto_acciones     DECIMAL(16,6);
DEFINE v_ret_preliquida_monto_pesos        DECIMAL(12,2);
DEFINE v_ret_preliquida_f_valor            DATE;
DEFINE v_ret_preliquida_f_registro         DATE;
DEFINE v_ret_preliquida_h_registro         DATETIME HOUR TO SECOND;
DEFINE v_ret_preliquida_origen             CHAR(20);

-- para calcular el rendimiento
DEFINE v_pesos_liquidados      DECIMAL(12,2);
DEFINE v_pesos_restitucion     DECIMAL(12,2);
DEFINE v_rendimiento           DECIMAL(12,2);
DEFINE v_folio_liquidacion     DECIMAL(9,0);
DEFINE v_folio_restitucion     DECIMAL(9,0);

DEFINE v_ret_excep_devol_ssv_id_solicitud DECIMAL(9,0);

-- precio del fondo para valuar AIVs
DEFINE v_valor_fondo            DECIMAL(19,14);
DEFINE v_tipo_movimiento        SMALLINT;
DEFINE v_tipo_mov_cargo         SMALLINT; -- cargo  -1
DEFINE v_tipo_mov_abono         SMALLINT; -- abono   1

DEFINE v_mov_abono_excep        SMALLINT;
DEFINE v_mov_cargo_excep        SMALLINT;
DEFINE v_mov_abono_acb_excep    SMALLINT;
DEFINE v_mov_cargo_acb_excep    SMALLINT;

DEFINE v_mov_excep              SMALLINT;
DEFINE v_mov_aexcep             SMALLINT;
DEFINE v_mov_acb_672            SMALLINT;
DEFINE v_mov_acb_721            SMALLINT;

-- variables para el control de excepciones
DEFINE v_ind        SMALLINT; 
DEFINE v_diag       CHAR(3);
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

   -- en caso de excepcion
   ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
            
      RETURN v_ind, v_diag, v_sql_error, v_isam_error, v_msg_error;
   END EXCEPTION;
   
   -- se inician las variables
   LET v_ind         = 0;
   LET v_diag        = "000";
   LET v_sql_error   = 0;
   LET v_isam_error  = 0;
   LET v_msg_error   = "";
   LET v_valor_fondo = 0;
   LET v_tipo_movimiento = 0;
   LET v_tipo_mov_cargo = -1;
   LET v_tipo_mov_abono = 1;
   
   -- Coloca actualiza folio a preliquidado
   UPDATE glo_folio
   SET    status = 1
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = p_opera_cod
   AND    folio       = p_folio_liquida
   AND    status      = 0;
   
   -- se actualiza el folio en las tablas de control de procesos
   UPDATE bat_ctr_proceso
   SET    folio = p_folio_liquida
   WHERE  pid   = p_pid;
   
   UPDATE bat_ctr_operacion
   SET    folio     = p_folio_liquida
   WHERE  pid       = p_pid
   AND    opera_cod = 1;
   
   -- se inicia el movimiento y su tipo 
   LET v_mov_abono_excep      = 1791;
   LET v_mov_cargo_excep      = 2032;
   LET v_mov_abono_acb_excep  = 1801;
   LET v_mov_cargo_acb_excep  = 2042;
   LET v_mov_excep            = 2052;
   LET v_mov_aexcep           = 1811;
   LET v_mov_acb_672          = 672;
   LET v_mov_acb_721          = 721;
   
   -- se obtiene el valor del fondo del primer dia natural del mes
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  fondo       = 11
   AND    f_valuacion = MDY(MONTH(TODAY),1,YEAR(TODAY));
   
   -- si no hay precio de fondo es un error
   IF ( v_valor_fondo IS NULL ) THEN
      LET v_msg_error  = "No se encontro precio de AIV para la fecha " || TODAY;
      LET v_ind        = 1000;
      LET v_diag       = "000";
      LET v_sql_error  = 0;
      LET v_isam_error = 0;
      
      -- se detiene el proceso
      RETURN v_ind, v_diag, v_sql_error, v_isam_error, v_msg_error;
   END IF
  
   -- recupera los movimientos de retiro amortizacion excedente y los duplica como abonos
   FOREACH 
   SELECT pre.f_liquida,
          pre.id_derechohabiente,
          pre.subcuenta,
          pre.fondo_inversion,
          pre.movimiento,
          pre.folio_liquida,
          pre.id_referencia,
          abs(pre.monto_acciones), -- valor absoluto
          abs(pre.monto_pesos), -- valor absoluto
          pre.f_valor,
          pre.f_registro,
          pre.h_registro,
          pre.origen,
          ret.id_solicitud
    INTO v_ret_preliquida_f_liquida,
         v_ret_preliquida_id_derechohabiente,
         v_ret_preliquida_subcuenta,
         v_ret_preliquida_fondo_inversion,
         v_ret_preliquida_movimiento,
         v_ret_preliquida_folio_liquida,
         v_ret_preliquida_id_referencia,
         v_ret_preliquida_monto_acciones,
         v_ret_preliquida_monto_pesos,
         v_ret_preliquida_f_valor,
         v_ret_preliquida_f_registro,
         v_ret_preliquida_h_registro,
         v_ret_preliquida_origen,
         v_ret_excep_devol_ssv_id_solicitud
   FROM ret_preliquida pre 
        JOIN ret_excep_devol_ssv ret
          ON pre.folio_liquida = ret.folio
         AND pre.id_referencia = ret.id_solicitud
   WHERE ret.estado_solicitud = p_estado_solicitud
   AND   ret.cod_rechazo IN (64, 65)
   
      -- se actualizan las fechas a las actuales
      LET v_ret_preliquida_f_valor       = MDY(MONTH(TODAY),1,YEAR(TODAY)); -- se reevaluan las aivs a la fecha de liquidacion
      LET v_ret_preliquida_f_liquida     = TODAY; 
      LET v_ret_preliquida_f_registro    = TODAY;
      LET v_ret_preliquida_h_registro    = CURRENT HOUR TO SECOND;

      IF v_ret_preliquida_movimiento = v_mov_excep THEN
         LET v_ret_preliquida_movimiento = v_mov_abono_excep;
         LET v_tipo_movimiento = v_tipo_mov_abono;
      END IF 
      IF v_ret_preliquida_movimiento = v_mov_aexcep THEN 
         LET v_ret_preliquida_movimiento = v_mov_cargo_excep;
         LET v_tipo_movimiento = v_tipo_mov_cargo;
      END IF 
      IF v_ret_preliquida_movimiento = v_mov_acb_672 THEN 
         LET v_ret_preliquida_movimiento = v_mov_abono_acb_excep;
         LET v_tipo_movimiento = v_tipo_mov_abono;
      END IF 
      IF v_ret_preliquida_movimiento = v_mov_acb_721 THEN 
         LET v_ret_preliquida_movimiento = v_mov_cargo_acb_excep;
         LET v_tipo_movimiento = v_tipo_mov_cargo;
      END IF 

      -- se asigna el monto de pesos liquidados para calcular el rendimiento posteriormente
      LET v_pesos_liquidados  = v_ret_preliquida_monto_pesos;
      LET v_folio_liquidacion = v_ret_preliquida_folio_liquida; -- el folio con que se liquido
      
      -- se abonan las AIVs
      LET v_ret_preliquida_monto_acciones = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- cambia el monto a abono
      
      -- las AIVs se valuan a la fecha de restitucion
      IF v_ret_preliquida_subcuenta = 47 THEN 
         LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- cambia el monto a abono
      ELSE
         LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_valor_fondo * v_tipo_movimiento; -- cambia el monto a abono
      END IF 
      -- el monto de pesos calculado se guarda en pesos de restitucion
      LET v_pesos_restitucion = v_ret_preliquida_monto_pesos;
      
      LET v_ret_preliquida_folio_liquida  = p_folio_liquida; -- actualiza al folio de restitucion
      
      -- ingresa registro de restitucion para el mismo monto pero positivo con el movimiento correspondiente de abono
      INSERT INTO ret_preliquida (
         f_liquida         ,
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
         origen )
      VALUES (
         v_ret_preliquida_f_liquida,
         v_ret_preliquida_id_derechohabiente,
         v_ret_preliquida_subcuenta,
         v_ret_preliquida_fondo_inversion,
         v_ret_preliquida_movimiento,
         v_ret_preliquida_folio_liquida,
         v_ret_preliquida_id_referencia,
         v_ret_preliquida_monto_acciones,
         v_ret_preliquida_monto_pesos,
         v_ret_preliquida_f_valor,
         v_ret_preliquida_f_registro,
         v_ret_preliquida_h_registro,
         v_ret_preliquida_origen
      );

      -- se calcula y registra el rendimiento
      LET v_rendimiento = v_pesos_restitucion - v_pesos_liquidados;
      LET v_folio_restitucion = p_folio_liquida; -- el folio de restitucion
      -- Pendiente definir si se inserta en esta tabla o no
--      IF v_rendimiento <> 0 THEN 
--         -- se inserta el rendimiento
--         INSERT INTO ret_rendimiento_restitucion (
--            id_solicitud      ,
--            folio_liquida     ,
--            folio_restitucion ,
--            subcuenta         ,
--            fondo_inversion   ,
--            monto_acciones    ,
--            pesos_liquidacion ,
--            pesos_restitucion ,
--            rendimiento       )
--         VALUES (
--            v_ret_preliquida_id_referencia  ,
--            v_folio_liquidacion             ,
--            v_folio_restitucion             ,
--            v_ret_preliquida_subcuenta      , 
--            v_ret_preliquida_fondo_inversion, 
--            v_ret_preliquida_monto_acciones ,
--            v_pesos_liquidados              ,
--            v_pesos_restitucion             ,
--            v_rendimiento                  
--         );
--      END IF 

      -- Actualiza el estado de la solicitud a preliquidado
      UPDATE ret_excep_devol_ssv
      SET    estado_solicitud = p_estado_restitucion -- 209 restituyendo
      WHERE  id_solicitud     = v_ret_excep_devol_ssv_id_solicitud;
       
   END FOREACH

   UPDATE STATISTICS FOR TABLE ret_preliquida;
   UPDATE STATISTICS FOR TABLE ret_rendimiento_restitucion;
             
   -- se devuelve el resultado del procesamiento
   RETURN v_ind, v_diag, v_sql_error, v_isam_error, v_msg_error;                                    
END FUNCTION;


