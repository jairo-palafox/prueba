






CREATE FUNCTION "safreviv".fn_restitucion_ret_ley73_ws(p_folio_liquida      DECIMAL(10,0),
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
DEFINE v_ret_preliquida_consec_benef       SMALLINT;
DEFINE v_beneficiario_procesados           SMALLINT;
DEFINE v_total_beneficiarios               SMALLINT;


-- para calcular el rendimiento
DEFINE v_pesos_liquidados      DECIMAL(12,2);
DEFINE v_pesos_restitucion     DECIMAL(12,2);
DEFINE v_rendimiento           DECIMAL(12,2);
DEFINE v_folio_liquidacion     DECIMAL(9,0);
DEFINE v_folio_restitucion     DECIMAL(9,0);
DEFINE v_id_solicitud          DECIMAL(9,0);
DEFINE v_consec_beneficiario   SMALLINT;

DEFINE v_ret_solicitud_generico_id_solicitud DECIMAL(9,0);

-- precio del fondo para valuar AIVs
DEFINE v_valor_fondo            DECIMAL(19,14);
DEFINE v_tipo_movimiento        SMALLINT; -- cargo/abono

DEFINE v_movimiento_ley73_cargo     SMALLINT;
DEFINE v_movimiento_ley73_sobregiro SMALLINT;

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
   
--SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_restitucion_ret_ley73_ws.trace';
   
   -- se inician las variables
   LET v_ind         = 0;
   LET v_diag        = "000";
   LET v_sql_error   = 0;
   LET v_isam_error  = 0;
   LET v_msg_error   = "";
   LET v_valor_fondo = 0;
   LET v_ret_preliquida_f_liquida = NULL;
   LET v_ret_preliquida_id_derechohabiente = 0;
   LET v_ret_preliquida_subcuenta = 0;
   LET v_ret_preliquida_fondo_inversion = 0;
   LET v_ret_preliquida_movimiento = 0;
   LET v_ret_preliquida_folio_liquida = 0;
   LET v_ret_preliquida_id_referencia = 0;
   LET v_ret_preliquida_monto_acciones = 0;
   LET v_ret_preliquida_monto_pesos = 0;
   LET v_ret_preliquida_f_valor = NULL;
   LET v_ret_preliquida_f_registro = NULL;
   LET v_ret_preliquida_h_registro = NULL;
   LET v_ret_preliquida_origen = "";
   LET v_ret_solicitud_generico_id_solicitud = 0;
   LET v_ret_preliquida_consec_benef = 0;
   LET v_tipo_movimiento = 0;
   LET v_id_solicitud = 0;
   LET v_pesos_liquidados  = 0;
   LET v_folio_liquidacion = 0;
   LET v_pesos_restitucion = 0; 
   LET v_rendimiento = 0;
   LET v_folio_restitucion = 0; -- el folio de restitucion
   LET v_beneficiario_procesados = 0;
   LET v_total_beneficiarios     = 0;

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
   
   -- se obtiene el valor del fondo
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  fondo       = 11
   AND    f_valuacion = TODAY;
   
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
  
   -- se recuperan las id_solicitudes que se van a restituir de TITULARES
   FOREACH 
   SELECT sol.id_solicitud
   INTO   v_id_solicitud
   FROM   ret_solicitud_generico sol ,
          ret_ley73_generico ret,
          ret_beneficiario_generico rbg
   WHERE  sol.modalidad_retiro = 3 -- retiro ley73
   AND    sol.estado_solicitud = p_estado_solicitud
   AND    sol.cod_rechazo      = p_cod_rechazo
   AND    sol.id_solicitud     = ret.id_solicitud
   AND    sol.id_solicitud     = rbg.id_solicitud
   AND    rbg.tpo_beneficiario = 1
   
      -- para cada solicitud, se buscan sus movimientos
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
             sol.id_solicitud
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
            v_ret_solicitud_generico_id_solicitud
      FROM ret_preliquida pre    ,
           ret_ley73_generico ret,
           ret_solicitud_generico sol 
      WHERE sol.id_solicitud     = v_id_solicitud
      AND   sol.estado_solicitud = p_estado_solicitud
      AND   sol.cod_rechazo      = p_cod_rechazo
      AND   sol.id_solicitud     = ret.id_solicitud
      AND   sol.id_solicitud     = pre.id_referencia
   
         -- se actualizan las fechas a las actuales
         LET v_ret_preliquida_f_valor       = TODAY; -- se reevaluan las aivs a la fecha de liquidacion
         LET v_ret_preliquida_f_liquida     = TODAY; 
         LET v_ret_preliquida_f_registro    = TODAY;
         LET v_ret_preliquida_h_registro    = CURRENT HOUR TO SECOND;
--   trace "Movimiento original:" || v_ret_preliquida_movimiento;
   	  -- se reasignan nuevos movimientos
   	  -- retiro Ley 73
   	  IF ( v_ret_preliquida_movimiento = 192 ) THEN
   	  	LET v_ret_preliquida_movimiento = 621; --cargo
   	  END IF
   	  
   	  -- sobregiro Ley73
   	  IF ( v_ret_preliquida_movimiento = 812 ) THEN
   	  	LET v_ret_preliquida_movimiento = 631; --sobregiro
   	  END IF
   	  
   	  -- retiro Anexo 1
   	  IF ( v_ret_preliquida_movimiento = 1442 ) THEN
   	  	LET v_ret_preliquida_movimiento = 661; -- ABONO RETIRO LEY83 NO PAGADO ANEXO 1
   	  END IF
   	  IF ( v_ret_preliquida_movimiento = 2072 ) THEN
   	  	LET v_ret_preliquida_movimiento = 1951; -- ABONO RETIRO LEY83 NO PAGADO ANEXO 1 SOLO TESOFE
   	  END IF
        
--   trace "Movimiento destino: " || v_ret_preliquida_movimiento;
   	  -- se obtiene el tipo de movimiento del movimiento
   	  SELECT tipo
   	  INTO   v_tipo_movimiento
   	  FROM   cat_movimiento
   	  WHERE  movimiento = v_ret_preliquida_movimiento;
         
         -- se asigna el monto de pesos liquidados para calcular el rendimiento posteriormente
         LET v_pesos_liquidados  = v_ret_preliquida_monto_pesos;
         LET v_folio_liquidacion = v_ret_preliquida_folio_liquida; -- el folio con que se liquido
         
         -- se abonan las AIVs
         LET v_ret_preliquida_monto_acciones = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- cambia el monto a abono
         
         -- las AIVs se valuan a la fecha de restitucion
   	  -- caso ANEXO 1 YA COMO MOVIMIENTO RESTITUIDO
   	  IF ( v_ret_preliquida_movimiento = 661 OR v_ret_preliquida_movimiento = 1951) THEN
   	     -- en el caso de ANEXO 1 el valor de pesos y aivs es el mismo
   		 LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- pesos = AIVS para anexo 1
   	  ELSE
   	     -- caso Vivienda 92/97
   	     LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_valor_fondo * v_tipo_movimiento; -- cambia el monto a abono
   	  END IF
         
         -- el monto de pesos calculado se guarda en pesos de restitucion
         LET v_pesos_restitucion = v_ret_preliquida_monto_pesos;
         
         LET v_ret_preliquida_folio_liquida  = p_folio_liquida; -- actualiza al folio de restitucion
         
--   trace "pesos liquidados: " || v_pesos_restitucion;
--   trace "aivs liquidados: " || v_ret_preliquida_monto_acciones;
   	  
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
   
         -- se inserta el rendimiento segun la
         IF v_ret_preliquida_movimiento = 621 OR v_ret_preliquida_movimiento = 1951 THEN 
            INSERT INTO ret_rendimiento_restitucion (
               id_solicitud      ,
               folio_liquida     ,
               folio_restitucion ,
               subcuenta         ,
               fondo_inversion   ,
               monto_acciones    ,
               pesos_liquidacion ,
               pesos_restitucion ,
               rendimiento       )
            VALUES (
               v_ret_preliquida_id_referencia  ,
               v_folio_liquidacion             ,
               v_folio_restitucion             ,
               v_ret_preliquida_subcuenta      , -- subcuenta preliquidada
               v_ret_preliquida_fondo_inversion, -- fondo de inversion en AIVs
               v_ret_preliquida_monto_acciones ,
               v_pesos_liquidados              ,
               v_pesos_restitucion             ,
               v_rendimiento                  
            );
         END IF 

      END FOREACH;
		 
      -- Actualiza el estado de la solicitud a preliquidado
      UPDATE ret_solicitud_generico
      SET    estado_solicitud  = p_estado_restitucion, -- se recibe de parametro
             folio_restitucion = p_folio_liquida 
      WHERE  id_solicitud     = v_ret_solicitud_generico_id_solicitud;
       
      UPDATE ret_ley73_generico
      SET    estado_solicitud = p_estado_restitucion -- se recibe de parametro
      WHERE  id_solicitud     = v_ret_solicitud_generico_id_solicitud;
   
   END FOREACH;

   -- se recuperan las id_solicitudes que se van a restituir de BENEFICIARIOS
   FOREACH 
   SELECT sol.id_solicitud, rbj.consec_beneficiario
   INTO   v_id_solicitud, v_consec_beneficiario
   FROM   ret_solicitud_generico sol ,
          ret_ley73_generico ret,
          ret_beneficiario_generico rbg,
          ret_beneficiario_juridico rbj
   WHERE  sol.modalidad_retiro    = 3 -- retiro ley73
   AND    rbj.estado_solicitud    = p_estado_solicitud
   AND    rbj.cod_rechazo         = p_cod_rechazo
   AND    sol.id_solicitud        = ret.id_solicitud
   AND    sol.id_solicitud        = rbg.id_solicitud
   AND    sol.id_solicitud        = rbj.id_solicitud
   AND    rbg.consec_beneficiario = rbj.consec_beneficiario
   AND    rbg.tpo_beneficiario    = 2
   AND    rbg.porcentaje > 0
   
      -- para cada solicitud, se buscan sus movimientos
      FOREACH
	  SELECT pre.f_liquida,
             pre.id_derechohabiente,
             pre.subcuenta,
             pre.fondo_inversion,
             pre.movimiento,
             pre.folio_liquida,
             pre.id_referencia,
             abs(pre.monto_acciones) * (rbg.porcentaje/100), -- valor absoluto
             abs(pre.monto_pesos) * (rbg.porcentaje/100), -- valor absoluto
             pre.f_valor,
             pre.f_registro,
             pre.h_registro,
             pre.origen,
             sol.id_solicitud,
             rbg.consec_beneficiario
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
            v_ret_solicitud_generico_id_solicitud,
            v_ret_preliquida_consec_benef
      FROM ret_preliquida pre    ,
           ret_ley73_generico ret,
           ret_solicitud_generico sol,
           ret_beneficiario_generico rbg,
           ret_beneficiario_juridico rbj
      WHERE sol.id_solicitud        = v_id_solicitud
      AND   rbj.estado_solicitud    = p_estado_solicitud
      AND   rbj.cod_rechazo         = p_cod_rechazo
      AND   sol.id_solicitud        = ret.id_solicitud
      AND   sol.id_solicitud        = pre.id_referencia
      AND   sol.id_solicitud        = rbg.id_solicitud
      AND   sol.id_solicitud        = rbj.id_solicitud
      AND   rbg.consec_beneficiario = rbj.consec_beneficiario
      AND   rbj.consec_beneficiario = v_consec_beneficiario
   
         -- se actualizan las fechas a las actuales
         LET v_ret_preliquida_f_valor       = TODAY; -- se reevaluan las aivs a la fecha de liquidacion
         LET v_ret_preliquida_f_liquida     = TODAY; 
         LET v_ret_preliquida_f_registro    = TODAY;
         LET v_ret_preliquida_h_registro    = CURRENT HOUR TO SECOND;
--   trace "Movimiento original:" || v_ret_preliquida_movimiento;
   	  -- se reasignan nuevos movimientos
   	  -- retiro Ley 73
   	  IF ( v_ret_preliquida_movimiento = 192 ) THEN
   	  	LET v_ret_preliquida_movimiento = 621; --cargo
   	  END IF
   	  
   	  -- sobregiro Ley73
   	  IF ( v_ret_preliquida_movimiento = 812 ) THEN
   	  	LET v_ret_preliquida_movimiento = 631; --sobregiro
   	  END IF
   	  
   	  -- retiro Anexo 1
   	  IF ( v_ret_preliquida_movimiento = 1442 ) THEN
   	  	LET v_ret_preliquida_movimiento = 661; -- ABONO RETIRO LEY83 NO PAGADO ANEXO 1
   	  END IF
   	  IF ( v_ret_preliquida_movimiento = 2072 ) THEN
   	  	LET v_ret_preliquida_movimiento = 1951; -- ABONO RETIRO LEY83 NO PAGADO ANEXO 1 SOLO TESOFE
   	  END IF
        
--   trace "Movimiento destino: " || v_ret_preliquida_movimiento;
   	  -- se obtiene el tipo de movimiento del movimiento
   	  SELECT tipo
   	  INTO   v_tipo_movimiento
   	  FROM   cat_movimiento
   	  WHERE  movimiento = v_ret_preliquida_movimiento;
         
         -- se asigna el monto de pesos liquidados para calcular el rendimiento posteriormente
         LET v_pesos_liquidados  = v_ret_preliquida_monto_pesos;
         LET v_folio_liquidacion = v_ret_preliquida_folio_liquida; -- el folio con que se liquido
         
         -- se abonan las AIVs
         LET v_ret_preliquida_monto_acciones = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- cambia el monto a abono
         
         -- las AIVs se valuan a la fecha de restitucion
   	  -- caso ANEXO 1 YA COMO MOVIMIENTO RESTITUIDO
   	  IF ( v_ret_preliquida_movimiento = 661 OR v_ret_preliquida_movimiento = 1951) THEN
   	     -- en el caso de ANEXO 1 el valor de pesos y aivs es el mismo
   		 LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_tipo_movimiento; -- pesos = AIVS para anexo 1
   	  ELSE
   	     -- caso Vivienda 92/97
   	     LET v_ret_preliquida_monto_pesos    = v_ret_preliquida_monto_acciones * v_valor_fondo * v_tipo_movimiento; -- cambia el monto a abono
   	  END IF
         
         -- el monto de pesos calculado se guarda en pesos de restitucion
         LET v_pesos_restitucion = v_ret_preliquida_monto_pesos;
         
         LET v_ret_preliquida_folio_liquida  = p_folio_liquida; -- actualiza al folio de restitucion
         
--   trace "pesos liquidados: " || v_pesos_restitucion;
--   trace "aivs liquidados: " || v_ret_preliquida_monto_acciones;
   	  
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
   
         -- se inserta el rendimiento segun la
         IF v_ret_preliquida_movimiento = 621 OR v_ret_preliquida_movimiento = 1951 THEN 
            INSERT INTO ret_rendimiento_restitucion (
               id_solicitud      ,
               folio_liquida     ,
               folio_restitucion ,
               subcuenta         ,
               fondo_inversion   ,
               monto_acciones    ,
               pesos_liquidacion ,
               pesos_restitucion ,
               rendimiento       )
            VALUES (
               v_ret_preliquida_id_referencia  ,
               v_folio_liquidacion             ,
               v_folio_restitucion             ,
               v_ret_preliquida_subcuenta      , -- subcuenta preliquidada
               v_ret_preliquida_fondo_inversion, -- fondo de inversion en AIVs
               v_ret_preliquida_monto_acciones ,
               v_pesos_liquidados              ,
               v_pesos_restitucion             ,
               v_rendimiento                  
            );
         END IF 

         UPDATE ret_beneficiario_juridico
         SET    estado_solicitud = p_estado_restitucion -- se recibe de parametro
         WHERE  id_solicitud     = v_ret_solicitud_generico_id_solicitud
         AND    consec_beneficiario = v_ret_preliquida_consec_benef;


      END FOREACH;
      --  Valida si se restituyeron todos los beneficiarios para actualizar la solicitud
      SELECT COUNT(*)
      INTO   v_beneficiario_procesados
      FROM   ret_beneficiario_juridico a,
             ret_beneficiario_generico b
      WHERE  a.id_solicitud        = b.id_solicitud
      AND    a.consec_beneficiario = b.consec_beneficiario
      AND    a.estado_solicitud    IN (p_estado_restitucion)
      AND    b.porcentaje          > 0
      AND    a.id_solicitud        IN (v_ret_solicitud_generico_id_solicitud);

      SELECT COUNT(*)
      INTO   v_total_beneficiarios
      FROM   ret_beneficiario_generico
      WHERE  id_solicitud IN (v_ret_solicitud_generico_id_solicitud)
      AND    porcentaje   > 0;
      
      IF v_beneficiario_procesados = v_total_beneficiarios THEN 
         -- Busca el estado que se le pondrá a la solicitud
         UPDATE ret_solicitud_generico 
         SET    estado_solicitud     = p_estado_restitucion,
                cod_rechazo          = p_cod_rechazo,
                folio_restitucion    = p_folio_liquida
         WHERE  id_solicitud         = v_ret_solicitud_generico_id_solicitud;
            
         UPDATE ret_ley73_generico
         SET    estado_solicitud     = p_estado_restitucion,
                cod_rechazo          = p_cod_rechazo
         WHERE  id_solicitud         = v_ret_solicitud_generico_id_solicitud;
      END IF 
   
   END FOREACH;


   
   UPDATE STATISTICS FOR TABLE ret_preliquida;
   UPDATE STATISTICS FOR TABLE ret_rendimiento_restitucion;
             
   -- se devuelve el resultado del procesamiento
   RETURN v_ind, v_diag, v_sql_error, v_isam_error, v_msg_error;                                    
END FUNCTION;


