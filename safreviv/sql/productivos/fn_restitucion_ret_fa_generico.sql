






CREATE FUNCTION "safreviv".fn_restitucion_ret_fa_generico(p_folio_liquida      DECIMAL(10,0),
                                               p_proceso_cod        SMALLINT,
                                               p_opera_cod          SMALLINT,
                                               p_usuario_cod        VARCHAR(20),
                                               p_pid                DECIMAL(9,0),
                                               p_estado_solicitud   SMALLINT,
                                               p_estado_restitucion SMALLINT,
                                               p_cod_rechazo        SMALLINT)
RETURNING INTEGER,
          INTEGER,
          CHAR(255)

DEFINE v_ret_preliquida72_id_afi_fondo72 DECIMAL(9,0);
DEFINE v_ret_preliquida72_f_liquida      DATE;
DEFINE v_ret_preliquida72_subcuenta      SMALLINT;
DEFINE v_ret_preliquida72_movimiento     SMALLINT;
DEFINE v_ret_preliquida72_folio_liquida  DECIMAL(9,0);
DEFINE v_ret_preliquida72_id_referencia  DECIMAL(9,0);
DEFINE v_ret_preliquida72_importe        DECIMAL(12,2);
DEFINE v_ret_preliquida72_estado_pago    CHAR(1);
DEFINE v_ret_preliquida72_f_registro     DATE;
DEFINE v_ret_preliquida72_h_registro     DATETIME HOUR TO SECOND;
DEFINE v_ret_preliquida72_origen         CHAR(20);

DEFINE v_ret_solicitud_generico_id_solicitud DECIMAL(9,0);

DEFINE v_movimiento_rest_ahorro          SMALLINT;
DEFINE v_movimiento_rest_tanto_ad        SMALLINT;
DEFINE v_id_solicitud_pago_dap           DECIMAL(9,0); -- para buscar si es por DAP

-- variables para el control de excepciones
DEFINE v_ind        SMALLINT; 
DEFINE v_diag       CHAR(3);
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);

   ON EXCEPTION SET v_sql_error, 
                    v_isam_error, 
                    v_msg_error
      
      -- se devuelven los valores de error      
      RETURN v_sql_error, v_isam_error, v_msg_error;
             
   END EXCEPTION;
   
   LET v_ind        = 0;
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = "";
   
   LET v_movimiento_rest_ahorro   = 141;
   LET v_movimiento_rest_tanto_ad = 601;
   
   -- Coloca actualiza folio a preliquidado
   UPDATE glo_folio
      SET status =  1
    WHERE folio  = p_folio_liquida;

   -- actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion
      SET folio       = p_folio_liquida
    WHERE pid         = p_pid
      AND proceso_cod = p_proceso_cod
      AND opera_cod   = p_opera_cod;
	  
   UPDATE bat_ctr_proceso
      SET folio       = p_folio_liquida
    WHERE pid         = p_pid;
	
   -- recupera los movimientos de retiro y los duplica como abonos
   FOREACH 
   SELECT pre.id_afi_fondo72,
          pre.f_liquida,
          pre.subcuenta,
          pre.movimiento,
          pre.folio_liquida,
          pre.id_referencia,
          pre.importe,
          pre.estado_pago,
          pre.f_registro,
          pre.h_registro,
          pre.origen,
          sol.id_solicitud                  

   INTO v_ret_preliquida72_id_afi_fondo72 ,
        v_ret_preliquida72_f_liquida,
        v_ret_preliquida72_subcuenta,
        v_ret_preliquida72_movimiento,
        v_ret_preliquida72_folio_liquida,
        v_ret_preliquida72_id_referencia,
        v_ret_preliquida72_importe,
        v_ret_preliquida72_estado_pago,
        v_ret_preliquida72_f_registro,
        v_ret_preliquida72_h_registro,
        v_ret_preliquida72_origen,
        v_ret_solicitud_generico_id_solicitud
   FROM  ret_preliquida72 pre,
         ret_fondo_ahorro_generico ret,
		 ret_solicitud_generico sol
   WHERE sol.estado_solicitud = p_estado_solicitud
   AND   sol.cod_rechazo      = p_cod_rechazo
   AND   sol.id_solicitud     = pre.id_referencia
   AND   sol.id_solicitud     = ret.id_solicitud
   AND   pre.importe          < 0
   
      -- si el movimiento de retiro es 182 se abona como movimiento 141
      -- si el movimiento de retiro es 422 se abona como movimiento 601
      IF( v_ret_preliquida72_movimiento = 182 )THEN
         LET v_ret_preliquida72_movimiento = v_movimiento_rest_ahorro;
      ELSE
         LET v_ret_preliquida72_movimiento = v_movimiento_rest_tanto_ad;
      END IF
      
      -- se actualizan las fechas a las actuales
      LET v_ret_preliquida72_f_liquida     = TODAY; 
      LET v_ret_preliquida72_f_registro    = TODAY;
      LET v_ret_preliquida72_h_registro    = CURRENT HOUR TO SECOND;
      LET v_ret_preliquida72_importe       = v_ret_preliquida72_importe * -1; -- cambia el monto a abono
      LET v_ret_preliquida72_folio_liquida = p_folio_liquida; -- actualiza al folio de restitucion
      
      -- ingresa registro de restitucion para el mismo monto pero positivo con el movimiento correspondiente de abono
      INSERT INTO ret_preliquida72 (
         id_afi_fondo72,
         f_liquida     ,
         subcuenta     ,
         movimiento    ,
         folio_liquida ,
         id_referencia ,
         importe       ,
         estado_pago   ,
         f_registro    ,
         h_registro    ,
         origen        )
      VALUES ( 
	     v_ret_preliquida72_id_afi_fondo72,
         v_ret_preliquida72_f_liquida     ,
         v_ret_preliquida72_subcuenta     ,
         v_ret_preliquida72_movimiento    ,
         v_ret_preliquida72_folio_liquida ,
         v_ret_preliquida72_id_referencia ,
         v_ret_preliquida72_importe       ,
         v_ret_preliquida72_estado_pago   ,
         v_ret_preliquida72_f_registro    ,
         v_ret_preliquida72_h_registro    ,
         v_ret_preliquida72_origen       );
             
      -- Actualiza el estado de la solicitud a preliquidado
      UPDATE ret_solicitud_generico
      SET    estado_solicitud  = p_estado_restitucion, -- 209 restituyendo
		     folio_restitucion = p_folio_liquida
      WHERE  id_solicitud      = v_ret_solicitud_generico_id_solicitud; 
       
      UPDATE ret_fondo_ahorro_generico
      SET    estado_solicitud = p_estado_restitucion -- 209 restituyendo
      WHERE  id_solicitud     = v_ret_solicitud_generico_id_solicitud; 
   
   END FOREACH;
   
   UPDATE STATISTICS FOR TABLE ret_preliquida72;

   -- se devuelve el resultado del procesamiento
   RETURN v_sql_error, v_isam_error, v_msg_error;                                    
END FUNCTION;


