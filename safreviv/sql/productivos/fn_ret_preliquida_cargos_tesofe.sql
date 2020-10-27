






CREATE FUNCTION "safreviv".fn_ret_preliquida_cargos_tesofe(p_folio 		DECIMAL(10,0),
												 p_proceso_cod 	SMALLINT     ,
												 p_opera_cod 	SMALLINT     ,
												 p_usuario_cod 	CHAR(20)     ,
												 p_pid			DECIMAL(9,0) )

	RETURNING SMALLINT, INTEGER, VARCHAR(255), DECIMAL(9,0)
	
	-- campos de la tabla de preliquidacion de retiros
	DEFINE v_ret_preliquida_f_liquida          DATE                   ;
	DEFINE v_ret_preliquida_id_derechohabiente DECIMAL(9,0)           ;
	DEFINE v_ret_preliquida_subcuenta          SMALLINT               ;
	DEFINE v_ret_preliquida_fondo_inversion    SMALLINT               ;
	DEFINE v_ret_preliquida_movimiento         SMALLINT               ;
	DEFINE v_ret_preliquida_folio_liquida      DECIMAL(9,0)           ;
	DEFINE v_ret_preliquida_id_referencia      DECIMAL(9,0)           ;
	DEFINE v_ret_preliquida_monto_acciones     DECIMAL(20,6)          ;
	DEFINE v_ret_preliquida_monto_pesos        DECIMAL(20,2)          ;
	DEFINE v_ret_preliquida_f_valor            DATE                   ;
	DEFINE v_ret_preliquida_f_registro         DATE                   ;
	DEFINE v_ret_preliquida_h_registro         DATETIME HOUR TO SECOND;
	DEFINE v_ret_preliquida_origen             CHAR(20)               ;
	
	-- campos de la tabla ret_his_respuesta_siaff
	DEFINE v_ret_his_respuesta_siaff_id_solicitud             DECIMAL(9,0) ;
	DEFINE v_ret_his_respuesta_siaff_folio                    DECIMAL(9,0) ;
	DEFINE v_ret_his_respuesta_siaff_des_ramo                 CHAR(100)    ;
	DEFINE v_ret_his_respuesta_siaff_des_unidad               CHAR(100)    ;
	DEFINE v_ret_his_respuesta_siaff_uni_folio                CHAR(12)     ;
	DEFINE v_ret_his_respuesta_siaff_archivo_envio            CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_archivo_salida           CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_archivo_acuse            CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_archivo_devol            CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_archivo_regreso          CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_total_rechazo            DECIMAL(16,2);
	DEFINE v_ret_his_respuesta_siaff_estatus_nom              SMALLINT     ;
	DEFINE v_ret_his_respuesta_siaff_cod_banco                INTEGER      ;
	DEFINE v_ret_his_respuesta_siaff_clave_rastreo            DECIMAL(30,0);
	DEFINE v_ret_his_respuesta_siaff_estatus_det              SMALLINT     ;
	DEFINE v_ret_his_respuesta_siaff_des_estatus_det          CHAR(150)    ;
	DEFINE v_ret_his_respuesta_siaff_ramo                     SMALLINT     ;
	DEFINE v_ret_his_respuesta_siaff_unidad                   CHAR(3)      ;
	DEFINE v_ret_his_respuesta_siaff_folio_clc                DECIMAL(10,0);
	DEFINE v_ret_his_respuesta_siaff_f_presenta               DATE         ;
	DEFINE v_ret_his_respuesta_siaff_f_pago                   DATE         ;
	DEFINE v_ret_his_respuesta_siaff_archivo_entrada          CHAR(250)    ;
	DEFINE v_ret_his_respuesta_siaff_nss                      CHAR(11)     ;
	DEFINE v_ret_his_respuesta_siaff_nombre                   CHAR(150)    ;
	DEFINE v_ret_his_respuesta_siaff_cta_bancaria             DECIMAL(20,0);
	DEFINE v_ret_his_respuesta_siaff_importe                  DECIMAL(16,2);
	DEFINE v_ret_his_respuesta_siaff_numero_oprbanc           DECIMAL(10,0);
	DEFINE v_ret_his_respuesta_siaff_cod_rechazo              SMALLINT     ;
	DEFINE v_ret_his_respuesta_siaff_des_rechazo              CHAR(150)    ;
	DEFINE v_ret_his_respuesta_siaff_estado_solicitud         SMALLINT     ; 
	DEFINE v_ret_his_respuesta_siaff_cod_rechazo_efp          SMALLINT     ;

	-- Control de Excepciones
	DEFINE v_si_resultado	SMALLINT    ;
	DEFINE sql_err       	INTEGER     ;
	DEFINE isam_err      	INTEGER     ;
	DEFINE err_txt       	VARCHAR(200);
	DEFINE v_c_msj       	VARCHAR(200);

	DEFINE v_marca			 			SMALLINT	  ;
	DEFINE v_movimiento	            	SMALLINT	  ;
	DEFINE v_signo_movimiento 			SMALLINT	  ;	DEFINE v_num_regs_preliquidados 	INTEGER		  ;	DEFINE v_num_regs_rechazados 		INTEGER		  ;	DEFINE v_id_derechohabiente			DECIMAL(9,0)  ;	-- Para la consulta del saldo
	DEFINE r_saldo_acciones             DECIMAL(16,6);
	DEFINE r_total_saldo                DECIMAL(16,6);
	DEFINE v_ctrl_val                   SMALLINT     ;

	-- se declara el comportamiento al ocurrir una excepcion
	ON EXCEPTION SET sql_err, isam_err, err_txt 
		LET v_si_resultado = sql_err;

		RETURN v_si_resultado, isam_err, err_txt, v_ret_his_respuesta_siaff_id_solicitud;
	END EXCEPTION

	-- se actualiza el folio en la tabla de control de operacion
	UPDATE bat_ctr_operacion 
	SET    folio       = p_folio
	WHERE  proceso_cod = p_proceso_cod
	AND    opera_cod   = p_opera_cod
	AND    pid         = p_pid;

	UPDATE bat_ctr_proceso 
	SET    folio       = p_folio
	WHERE  proceso_cod = p_proceso_cod
	AND    pid         = p_pid;

	UPDATE glo_folio
	SET    opera_cod = p_opera_cod,
	       status    = 1 -- preliquidado
	WHERE  folio     = p_folio;

	-- se asume que no hay errores
	LET v_si_resultado = 0;
	LET isam_err = 0;
	LET v_c_msj = 'El proceso finalizó correctamente';
	LET v_ret_his_respuesta_siaff_id_solicitud = NULL;

	-- se obtiene el signo del movimiento
	LET v_movimiento = 1432; -- establecer movimiento para los retiros cargos tesofe

	SELECT tipo
	INTO   v_signo_movimiento
	FROM   cat_movimiento
	WHERE  movimiento = v_movimiento;
      
    -- se inicia el contador de registros preliquidados y rechazados
	LET v_num_regs_preliquidados = 0;
	LET v_num_regs_rechazados = 0;

	FOREACH
   		SELECT id_solicitud    ,    
			   folio           ,    
			   des_ramo        ,    
			   des_unidad      ,    
			   uni_folio       ,    
			   archivo_envio   ,    
			   archivo_salida  ,    
			   archivo_acuse   ,    
			   archivo_devol   ,    
			   archivo_regreso ,    
			   total_rechazo   ,    
			   estatus_nom     ,    
			   cod_banco       ,    
			   clave_rastreo   ,    
			   estatus_det     ,    
			   des_estatus_det ,    
			   ramo            ,    
			   unidad          ,    
			   folio_clc       ,    
			   f_presenta      ,    
			   f_pago          ,    
			   archivo_entrada ,    
			   nss             ,    
			   nombre          ,    
			   cta_bancaria    ,    
			   importe         ,    
			   numero_oprbanc  ,    
			   cod_rechazo     ,    
			   des_rechazo     ,    
			   estado_solicitud,    
			   cod_rechazo_efp
   		INTO   v_ret_his_respuesta_siaff_id_solicitud    ,
               v_ret_his_respuesta_siaff_folio           ,
               v_ret_his_respuesta_siaff_des_ramo        ,
               v_ret_his_respuesta_siaff_des_unidad      ,
               v_ret_his_respuesta_siaff_uni_folio       ,
               v_ret_his_respuesta_siaff_archivo_envio   ,
               v_ret_his_respuesta_siaff_archivo_salida  ,
               v_ret_his_respuesta_siaff_archivo_acuse   ,
               v_ret_his_respuesta_siaff_archivo_devol   ,
               v_ret_his_respuesta_siaff_archivo_regreso ,
               v_ret_his_respuesta_siaff_total_rechazo   ,
               v_ret_his_respuesta_siaff_estatus_nom     ,
               v_ret_his_respuesta_siaff_cod_banco       ,
               v_ret_his_respuesta_siaff_clave_rastreo   ,
               v_ret_his_respuesta_siaff_estatus_det     ,
               v_ret_his_respuesta_siaff_des_estatus_det ,
               v_ret_his_respuesta_siaff_ramo            ,
               v_ret_his_respuesta_siaff_unidad          ,
               v_ret_his_respuesta_siaff_folio_clc       ,
               v_ret_his_respuesta_siaff_f_presenta      ,
               v_ret_his_respuesta_siaff_f_pago          ,
               v_ret_his_respuesta_siaff_archivo_entrada ,
               v_ret_his_respuesta_siaff_nss             ,
               v_ret_his_respuesta_siaff_nombre          ,
               v_ret_his_respuesta_siaff_cta_bancaria    ,
               v_ret_his_respuesta_siaff_importe         ,
               v_ret_his_respuesta_siaff_numero_oprbanc  ,
               v_ret_his_respuesta_siaff_cod_rechazo     ,
               v_ret_his_respuesta_siaff_des_rechazo     ,
               v_ret_his_respuesta_siaff_estado_solicitud,
               v_ret_his_respuesta_siaff_cod_rechazo_efp
   		FROM  ret_his_respuesta_siaff
   		WHERE estado_solicitud    = 42 -- integrada
   		AND   cod_rechazo_efp     = 0  -- Sin codigo de rechazo por parte de EFP
   		AND   cod_rechazo         = 0  -- Con codigo de rechazo por parte de Tesofe, para evitar procesar solicitudes aceptadas
   		
   		-- Se verifica que no se sobregire la cuenta 47
		CALL fn_saldo_dia(v_ret_his_respuesta_siaff_nss,NULL,47,TODAY)
		     RETURNING v_ctrl_val, r_saldo_acciones, r_total_saldo;
   		IF ( r_total_saldo >= v_ret_his_respuesta_siaff_importe) THEN
   			  -- Se obtiene  el id_derechohabiente
			  SELECT id_derechohabiente
			  INTO   v_id_derechohabiente
			  FROM   afi_derechohabiente
			  WHERE  nss = v_ret_his_respuesta_siaff_nss;
			   
		      --se concatena la cadena RETIRO
		      LET v_ret_preliquida_origen = "RETIRO RESP SIAFF";
		
		      -- asignacion de valores generales al registro de la tabla de preliquidacion
		      LET v_ret_preliquida_f_liquida          = TODAY; -- DATE                   ;
		      LET v_ret_preliquida_id_derechohabiente = v_id_derechohabiente;
		      LET v_ret_preliquida_f_valor            = TODAY; -- la fecha del valor del fondo del encabezado
		      LET v_ret_preliquida_f_registro         = TODAY; -- la fecha de ejecucion
		      LET v_ret_preliquida_h_registro         = CURRENT HOUR TO SECOND;
		
		      LET v_ret_preliquida_subcuenta          = 47; -- tesofe
		      LET v_ret_preliquida_fondo_inversion    = 10;
		      LET v_ret_preliquida_movimiento         = v_movimiento;
		      LET v_ret_preliquida_folio_liquida      = p_folio;
		      LET v_ret_preliquida_id_referencia      = v_ret_his_respuesta_siaff_id_solicitud;
		      --Se asigna el mismo monto en ambos casos
		      LET v_ret_preliquida_monto_acciones     = v_ret_his_respuesta_siaff_importe * v_signo_movimiento;
		      LET v_ret_preliquida_monto_pesos        = v_ret_his_respuesta_siaff_importe * v_signo_movimiento;
		
		      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
		      INSERT INTO ret_preliquida (f_liquida          ,
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
		        						  origen             )
		      VALUES (v_ret_preliquida_f_liquida          ,
		         	  v_ret_preliquida_id_derechohabiente ,
		         	  v_ret_preliquida_subcuenta          ,
		         	  v_ret_preliquida_fondo_inversion    ,
		         	  v_ret_preliquida_movimiento         ,
		         	  v_ret_preliquida_folio_liquida      ,
		         	  v_ret_preliquida_id_referencia      ,
		         	  v_ret_preliquida_monto_acciones     ,
		         	  v_ret_preliquida_monto_pesos        ,
		         	  v_ret_preliquida_f_valor            ,
		         	  v_ret_preliquida_f_registro         ,
		         	  v_ret_preliquida_h_registro         ,
		         	  v_ret_preliquida_origen             );
		
		      -- se cuenta un registro preliquidado
		      LET v_num_regs_preliquidados = v_num_regs_preliquidados + 1;
		            
		      -- se actualiza la solicitud como preliquidada y el monto valuado
		      UPDATE ret_his_respuesta_siaff
		      SET    estado_solicitud = 50, -- preliquidada
		             importe          = v_ret_preliquida_monto_pesos
		      WHERE  id_solicitud     = v_ret_his_respuesta_siaff_id_solicitud;

		ELSE
			-- se cuenta un registro rechazado
		    LET v_num_regs_rechazados = v_num_regs_rechazados + 1;
		    -- se establece el error 
			UPDATE ret_his_respuesta_siaff
		    SET    cod_rechazo_efp  = 784
		    WHERE  id_solicitud     = v_ret_his_respuesta_siaff_id_solicitud
		      AND  folio            = p_folio;
   		END IF
   END FOREACH;

   UPDATE STATISTICS FOR TABLE ret_preliquida;

	LET v_c_msj = v_c_msj||' Registros preliquidados: '||v_num_regs_preliquidados||'. Registros rechazados: '||v_num_regs_rechazados||".";
	
	-- si no se preliquidaron registros
   IF ( v_num_regs_preliquidados = 0 ) THEN
      -- se marca el procesoe en error
      LET v_si_resultado = 1000;
      LET isam_err       = 0;
      LET v_c_msj        = "Error. No se preliquidaron solicitudes para el folio.";
   END IF;
   
   -- se devuelve el resultado
   RETURN v_si_resultado, isam_err, v_c_msj,v_ret_his_respuesta_siaff_id_solicitud;
   
   TRACE OFF;

END FUNCTION;


