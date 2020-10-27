






CREATE FUNCTION "safreviv".fn_preliquidacion_retiro_ley73_conting(v_folio_liquida   DECIMAL(10,0),
                                                       v_proceso_cod     SMALLINT,
                                                       v_opera_cod       SMALLINT,
                                                       v_usuario_cod     VARCHAR(20),
                                                       v_pid             DECIMAL(9,0))
RETURNING INTEGER, INTEGER, VARCHAR(250)

DEFINE  v_b_paso               SMALLINT;
DEFINE  v_tpo_proceso          SMALLINT;
DEFINE  v_id_derechohabiente   DECIMAL(9,0);
DEFINE  v_id_solicitud         DECIMAL(9,0);
DEFINE  v_movimiento_tesofe    SMALLINT;
DEFINE  v_movimiento           SMALLINT;
DEFINE  v_movimiento_cargo_vol SMALLINT;
DEFINE  v_movimiento_abono_vol SMALLINT;
DEFINE  v_movimiento_sobregiro SMALLINT;
DEFINE  v_movto_abono_sobreg   SMALLINT;
DEFINE  v_valor_mov_cargo      SMALLINT;
DEFINE  v_valor_sobregiro      SMALLINT;
DEFINE  v_valor_mov_cargo_vol  SMALLINT;
DEFINE  v_valor_mov_cargo_tes  SMALLINT;
DEFINE  v_valor_mov_abono_vol  SMALLINT;
DEFINE  v_valor_mov_var_menor  SMALLINT;
DEFINE  v_origen               CHAR(20);
DEFINE  v_subcuenta_92         SMALLINT;
DEFINE  v_subcuenta_97         SMALLINT;
DEFINE  v_subcuenta_tesofe     SMALLINT;
DEFINE  v_subcuenta_vol        SMALLINT;
DEFINE  v_resultado_consulta   SMALLINT;

DEFINE  v_importe_tesofe_viv97_pesos   DECIMAL(20,2); -- restante del pago FICO
DEFINE  v_importe_tesofe_viv97_aivs    DECIMAL(18,6); -- restante del pago FICO

DEFINE  v_importe_tesofe_viv92_pesos   DECIMAL(20,2); -- restante del pago FICO - importe a viv97
DEFINE  v_importe_tesofe_viv92_aivs    DECIMAL(18,6); -- restante del pago FICO - importe a viv97

DEFINE  v_importe_tesofe_sobregiro_pesos   DECIMAL(20,2); -- restante del pago FICO - importe a viv97 - importe a viv92
DEFINE  v_importe_tesofe_sobregiro_aivs    DECIMAL(18,6); -- restante del pago FICO - importe a viv97 - importe a viv92

DEFINE  v_saldo_tesofe_pesos   DECIMAL(20,2);
DEFINE  v_saldo_tesofe_aivs    DECIMAL(18,6); -- Solo para el uso de fn_saldo_dia, pues sera 0

DEFINE  v_saldo_92_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_92_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92

DEFINE  v_saldo_97_aivs        DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_97_pesos       DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92

DEFINE  v_saldo_vol_aivs       DECIMAL(18,6); -- total de acciones de la subcuenta de aportaciones voluntarias
DEFINE  v_saldo_vol_pesos      DECIMAL(20,2); -- total de acciones en pesos de la subcuenta de aportaciones voluntarias

DEFINE v_i_estado_marca        INTEGER;
DEFINE v_marca_ley73           INTEGER; -- 803 de acuerdo a catalogo
DEFINE v_bnd_preli             SMALLINT;
DEFINE v_monto_acciones        DECIMAL(18,6); -- acciones
DEFINE v_monto_pesos           DECIMAL(20,2); -- pesos

-- Montos auxiliares para los grupos 400's
DEFINE v_monto_acciones_tesofe        DECIMAL(18,6); -- acciones
DEFINE v_monto_pesos_tesofe           DECIMAL(20,2); -- pesos
DEFINE v_monto_acciones_viv97         DECIMAL(18,6); -- acciones
DEFINE v_monto_pesos_viv97            DECIMAL(20,2); -- pesos
DEFINE v_monto_acciones_viv92         DECIMAL(18,6); -- acciones
DEFINE v_monto_pesos_viv92            DECIMAL(20,2); -- pesos
DEFINE v_monto_acciones_var_menor     DECIMAL(18,6); -- acciones
DEFINE v_monto_pesos_var_menor        DECIMAL(20,2); -- pesos

DEFINE r_solicitado_pes_viv97  DECIMAL(14,2);
DEFINE r_solicitado_pes_viv92  DECIMAL(14,2);
DEFINE r_importe_tesofe        DECIMAL(14,2);

DEFINE r_solicitado_aivs_viv92 DECIMAL(18,6);
DEFINE r_solicitado_aivs_viv97 DECIMAL(18,6);
DEFINE v_fondo                 SMALLINT;
DEFINE v_fondo_tesofe          SMALLINT;

DEFINE v_fecha_pago_fico       DATE;
DEFINE v_fecha_primer_dia      DATE;

-- fecha de valuacion          
DEFINE v_f_valuacion_tmp         DATE;
DEFINE v_nss                     CHAR(11);
DEFINE v_precio_fondo            DECIMAL(19,14);
DEFINE v_precio_fondo_primer_dia DECIMAL(19,14);

-- Control de Excepciones
DEFINE v_si_resultado          SMALLINT;
DEFINE sql_err                 INTEGER;
DEFINE isam_err                INTEGER;
DEFINE err_txt                 VARCHAR(250);
DEFINE v_c_msj                 VARCHAR(250);
DEFINE v_viv92_sobregirada      SMALLINT; -- indica si viv92 se sobregiro
DEFINE v_viv97_sobregirada      SMALLINT; -- indica si viv97 se sobregiro


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


   -- se inician las variables para marca
   LET v_marca_ley73        = 803; -- marca para disposicion de recursos
   LET v_i_estado_marca     = 0;
   LET v_bnd_preli          = 2;

   LET v_b_paso             = 0; 
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;

   LET v_movimiento_tesofe    = 1432; -- establecer movimiento para los retiros cargos tesofe
   LET v_movimiento           = 192;
   LET v_movimiento_cargo_vol = 1662;
   LET v_movimiento_abono_vol = 911;
   LET v_movimiento_sobregiro = 812;
   let v_movto_abono_sobreg   = 1671; -- Abono para cubrir sobregiro
   LET v_origen               = "RETIRO U";
   LET v_subcuenta_92         = 8;
   LET v_subcuenta_97         = 4;
   LET v_subcuenta_tesofe     = 47;
   LET v_subcuenta_vol        = 55;
   LET v_resultado_consulta   = 0;
   
   LET r_solicitado_pes_viv97  = 0;
   LET r_solicitado_pes_viv92  = 0;
   LET v_saldo_97_aivs         = 0;
   LET v_saldo_97_pesos        = 0;
   LET v_saldo_vol_aivs        = 0;
   LET v_saldo_vol_pesos       = 0;
   LET v_saldo_92_aivs         = 0;
   LET v_saldo_92_pesos        = 0;
   LET v_saldo_tesofe_pesos    = 0;
   LET v_saldo_tesofe_aivs     = 0;
   LET r_solicitado_aivs_viv92 = 0;
   LET r_solicitado_aivs_viv97 = 0;
   
   LET v_fondo                 = 11;
   LET v_fondo_tesofe          = 10;
   
   LET v_importe_tesofe_viv97_pesos = 0;
   LET v_importe_tesofe_viv97_aivs  = 0;
   LET v_importe_tesofe_viv92_pesos = 0;
   LET v_importe_tesofe_viv92_aivs  = 0;

   
   -- cambia el estatus del folio a preliquidado
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio  = v_folio_liquida;
      
   -- actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    opera_cod    = v_opera_cod;

	 -- Variacion menor
   SELECT tipo
   INTO   v_valor_mov_var_menor
   FROM   cat_movimiento
   WHERE  movimiento = v_movto_abono_sobreg ;

	 -- Cargo tesofe
   SELECT tipo
   INTO   v_valor_mov_cargo_tes
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_tesofe;
   
   -- Cargo ley 73
	 SELECT tipo
	 INTO   v_valor_mov_cargo
	 FROM   cat_movimiento
	 WHERE  movimiento = v_movimiento;
	
	 -- Cargo voluntarias
	 SELECT tipo
	 INTO   v_valor_mov_cargo_vol
	 FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo_vol;

	 -- Sobregiro	
   SELECT tipo
	 INTO   v_valor_sobregiro
	 FROM   cat_movimiento
	 WHERE  movimiento = v_movimiento_sobregiro;
   
   -- se asume que el proceso termina bien
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

   --SET DEBUG FILE TO '/safreviv_int/BD/fn_preliquidacion_retiro_ley73_conting.trace';
   
   --busca registros en estatus de capturado
   FOREACH
   SELECT 
      id_derechohabiente ,
      id_solicitud       ,
      importe_viv92      ,
      importe_viv97      ,
      aivs_viv92         ,
      aivs_viv97         ,
      f_valuacion        ,
      tpo_cambio         ,
      importe_tesofe     ,
      tpo_proceso
   INTO 
      v_id_derechohabiente   ,
      v_id_solicitud         ,
      r_solicitado_pes_viv92 ,
      r_solicitado_pes_viv97 ,
      r_solicitado_aivs_viv92,
      r_solicitado_aivs_viv97,
      v_f_valuacion_tmp      ,
      v_precio_fondo         ,
      r_importe_tesofe       ,
      v_tpo_proceso
   FROM  ret_ley73
   WHERE estado_solicitud = 15
   AND   folio = v_folio_liquida

      -- se busca la fecha de valuacion de la solicitud del derechohabuente
      SELECT nss
      INTO   v_nss
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = v_id_derechohabiente;
      
      -- se obtiene el precio del fondo
      -- Se comentan estas lineas ya que el precio con el que se liquidara es el que viene en el archivo,

      --SELECT precio_fondo
      --INTO   v_precio_fondo
      --FROM   glo_valor_fondo
      --WHERE  f_valuacion = v_f_valuacion_tmp
      --AND    fondo = 11;

      -- se asume que no se preliquidara ningun monto solicitado
      LET v_bnd_preli      = 0;           
      LET v_monto_acciones = 0;
      LET v_monto_pesos    = 0;

      -- se asume que no hay sobregiro
      LET v_viv92_sobregirada = 0;
      LET v_viv97_sobregirada = 0;
             
      -- se obtiene el saldo de vivienda 97 segun la fecha de valuacion
      {
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_97,
                                                v_f_valuacion_tmp,
                                                v_fondo)
                        INTO v_saldo_97_pesos, v_saldo_97_aivs, v_resultado_consulta;
      }                  

      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_97,
                                                TODAY,
                                                v_fondo)
                        INTO v_saldo_97_pesos, v_saldo_97_aivs, v_resultado_consulta;

                      
      IF ( v_saldo_97_aivs < 0 ) THEN 
         LET v_saldo_97_aivs  = 0;
         LET v_saldo_97_pesos = 0;
      ELSE
         -- se valuan los pesos
         LET v_saldo_97_pesos = v_saldo_97_aivs * v_precio_fondo;
      END IF 

      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_vol,
                                                TODAY,
                                                v_fondo)
                        INTO v_saldo_vol_pesos, v_saldo_vol_aivs, v_resultado_consulta;

                      
      IF ( v_saldo_vol_aivs < 0 ) THEN 
         LET v_saldo_vol_aivs  = 0;
         LET v_saldo_vol_pesos = 0;
      ELSE
         -- se valuan los pesos
         LET v_saldo_vol_pesos = v_saldo_vol_aivs * v_precio_fondo;
      END IF 
      
      -- se obtiene el saldo de vivienda 92
      {
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_92,
                                                v_f_valuacion_tmp,
                                                v_fondo)
                        INTO v_saldo_92_pesos, v_saldo_92_aivs, v_resultado_consulta;
      }
      
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_92,
                                                TODAY,
                                                v_fondo)
                        INTO v_saldo_92_pesos, v_saldo_92_aivs, v_resultado_consulta;
                        
      IF ( v_saldo_92_aivs < 0 ) THEN 
         LET v_saldo_92_aivs  = 0;
         LET v_saldo_92_pesos = 0;
      ELSE
         -- se valuan los pesos
         LET v_saldo_92_pesos = v_saldo_92_aivs * v_precio_fondo;
      END IF
      
      
      -- =================================================================================================
	    -- Requerimiento 878
      -- Se recupera el saldo de TESOFE
      -----------------------------------------------------------------------------------------------------
      EXECUTE FUNCTION fn_saldo_dia(NULL,v_id_derechohabiente,47,TODAY)
                       INTO v_resultado_consulta, v_saldo_tesofe_aivs, v_saldo_tesofe_pesos;
                       
      -- Validacion de grupos 0102, 0103, 0124
      IF v_tpo_proceso IN('0102','0103','0124') THEN
      
         IF r_importe_tesofe > 0 THEN
         
             LET v_monto_acciones = r_importe_tesofe * v_valor_mov_cargo_tes;
             LET v_monto_pesos    = r_importe_tesofe * v_valor_mov_cargo_tes;

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
			                    origen             )
			            VALUES (TODAY                ,
			                   v_id_derechohabiente  ,
			                   v_subcuenta_tesofe    ,
			                   v_fondo_tesofe        ,
			                   v_movimiento_tesofe   ,             
			                   v_folio_liquida       ,
			                   v_id_solicitud        ,
			                   v_monto_acciones      , -- En el caso de tesofe se manejan AIV's a un peso, es decir solo se manejan pesos
			                   v_monto_pesos         , -- El id 33 "imp_transf_ssv"
			                   TODAY                 ,
			                   TODAY                 ,
			                   CURRENT HOUR TO SECOND,
			                   v_origen  			);
	       END IF -- r_importe_tesofe > 0
      END IF -- v_tpo_proceso IN('0102','0103','0124')

      -- Validacion de grupos 0401, 0402, 0403, 0404, 0411, 0412, 0413, 0414
	IF v_tpo_proceso IN('0401','0402','0403','0404','0411','0412','0413','0414') THEN
	  
		-- Para estos grupos no debe de haber solicitudes de viv92 y viv97, solo debe de venir el importe de tesofe
		-- obtenido del id 24: importe de pago fico.
		LET r_solicitado_pes_viv92  = 0;
		LET r_solicitado_pes_viv97  = 0;
		LET r_solicitado_aivs_viv92 = 0;
		LET r_solicitado_aivs_viv97 = 0;
		
		LET v_monto_acciones_tesofe    = 0;
		LET v_monto_pesos_tesofe       = 0;
		LET v_monto_acciones_viv97     = 0;
		LET v_monto_pesos_viv97        = 0;
		LET v_monto_acciones_viv92     = 0;
		LET v_monto_pesos_viv92        = 0;
		LET v_monto_acciones_var_menor = 0;
		LET v_monto_pesos_var_menor    = 0;
		
		-- Aplicar el cargo del importe FICO al saldo de la cuenta TESOFE,
		-- si el pago del importe es igual o menor al saldo TESOFE, ahí concluiría el cargo
		IF  r_importe_tesofe <= v_saldo_tesofe_pesos AND r_importe_tesofe > 0 THEN
				
				LET v_monto_pesos_tesofe    = r_importe_tesofe * v_valor_mov_cargo_tes;
				LET v_monto_acciones_tesofe = r_importe_tesofe * v_valor_mov_cargo_tes;

		-- r_importe_tesofe <= v_saldo_tesofe_pesos AND r_importe_tesofe > 0
		ELSE-- Si el importe de pago FICO es mayor al saldo de la cuenta TESOFE aplicar el importe hasta el saldo de la cuenta TESOFE
			
			IF v_saldo_tesofe_pesos > 0 THEN
				  
				  	LET v_monto_pesos_tesofe    = v_saldo_tesofe_pesos * v_valor_mov_cargo_tes;
				  	LET v_monto_acciones_tesofe = v_saldo_tesofe_pesos * v_valor_mov_cargo_tes;

			END IF -- v_saldo_tesofe_pesos > 0
			
			-- y el restante aplicar contra el saldo de la subcuenta de vivienda 97, 
			-- determinando el número de AIVS a aplicar; dividiendo el importe de pago FICO aún no cubierto con el saldo TESOFE,
			-- entre el tipo de cambio del 1er día natural del mes de la fecha de pago FICO del id 23
			SELECT f_pago_fico
			INTO   v_fecha_pago_fico
			FROM   ret_pago_trm
			WHERE  id_solicitud = v_id_solicitud; 
			
			SELECT precio_fondo,f_valuacion
			INTO   v_precio_fondo_primer_dia,v_fecha_primer_dia
			FROM   glo_valor_fondo
			WHERE  f_valuacion = (v_fecha_pago_fico - DAY(v_fecha_pago_fico - 1)) -- El primer dia del mes de la fecha de pago FICO
			AND    fondo       = v_fondo;
			
			-- El importe tesofe solicitado menos el importe cubierto por el saldo de la subcuenta de TESOFE
			LET v_importe_tesofe_viv97_pesos = r_importe_tesofe - v_saldo_tesofe_pesos; -- el importe de pago FICO aún no cubierto con el saldo TESOFE
			LET v_importe_tesofe_viv97_aivs  = v_importe_tesofe_viv97_pesos / v_precio_fondo_primer_dia;
			
			-- Si el importe por cubrir es menor o igual al saldo de la subcuenta de vivienda 97, ahí concluiría el cargo
			IF  v_importe_tesofe_viv97_aivs <= v_saldo_97_aivs AND v_importe_tesofe_viv97_aivs > 0 THEN
         
				LET v_monto_pesos_viv97    = v_importe_tesofe_viv97_pesos * v_valor_mov_cargo;
				LET v_monto_acciones_viv97 = v_importe_tesofe_viv97_aivs  * v_valor_mov_cargo;

			-- v_importe_tesofe_viv97_aivs <= v_saldo_97_aivs AND v_importe_tesofe_viv97_aivs > 0
			ELSE-- En caso contrario el importe de pago FICO no este cubierto con el saldo de la subcuenta de vivienda 97,
			
				IF v_saldo_97_aivs > 0 THEN

					-- Se calcula los montos de acuerdo a las AIVS de la subcuenta, usando el precio del primer dia natural del mes
					LET v_monto_pesos_viv97    = v_saldo_97_aivs * v_valor_mov_cargo * v_precio_fondo_primer_dia;
					LET v_monto_acciones_viv97 = v_saldo_97_aivs * v_valor_mov_cargo;

				END IF -- v_saldo_97_aivs > 0
				
				-- Se deberá de aplicar el importe restante al saldo de la subcuenta de vivienda 92
				-- Se resta de lo que queda por liquidar (importe solicitado - saldo tesofe) el saldo de vivienda 97
				LET v_importe_tesofe_viv92_pesos = v_importe_tesofe_viv97_pesos - (v_saldo_97_aivs * v_precio_fondo_primer_dia);
				-- determinando el número de AIVS a aplicar dividiendo el importe de pago FICO aún no cubierto
				-- entre el tipo de cambio al 1er día natural del mes de la fecha de pago FICO del id 23,
				LET v_importe_tesofe_viv92_aivs  = v_importe_tesofe_viv92_pesos / v_precio_fondo_primer_dia;
			
				-- si el importe cubrir restante es menor o igual al saldo de la subcuenta de vivienda 92, ahí debe de concluir el cargo.
				IF v_importe_tesofe_viv92_pesos <= v_saldo_92_pesos AND v_importe_tesofe_viv92_pesos > 0 THEN

					LET v_monto_pesos_viv92    = v_importe_tesofe_viv92_pesos * v_valor_mov_cargo;
					LET v_monto_acciones_viv92 = v_importe_tesofe_viv92_aivs  * v_valor_mov_cargo;

				ELSE -- v_importe_tesofe_viv92_pesos <= v_saldo_92_pesos AND v_importe_tesofe_viv92_pesos > 0
				
					IF v_saldo_92_aivs > 0 THEN

						LET v_monto_pesos_viv92    = v_saldo_92_aivs * v_valor_mov_cargo * v_precio_fondo_primer_dia;
						LET v_monto_acciones_viv92 = v_saldo_92_aivs * v_valor_mov_cargo;

					END IF -- v_saldo_92_aivs > 0 
					
					-- En caso de que el importe de pago FICO, no se agote con los cargos a la subcuenta TESOFE, SSV 97 y SSV 92 
					-- se deberá de aplicar un sobregiro en AIVS  a la subcuenta de vivienda 97
					-- Se resta de lo que queda por liquidar (importe solicitado - saldo tesofe - saldo vivienda 97) el saldo de vivienda 92
					LET v_importe_tesofe_sobregiro_pesos = v_importe_tesofe_viv92_pesos - (v_saldo_92_aivs * v_precio_fondo_primer_dia);
					-- determinado por el importe no cubierto por las subcuentas entre el tipo de cambio del 1er día natural de la fecha de pago FICO (id 23).
					LET v_importe_tesofe_sobregiro_aivs  = v_importe_tesofe_sobregiro_pesos / v_precio_fondo_primer_dia;
					
					IF v_importe_tesofe_sobregiro_aivs > 0 THEN 
						
						-- Se aplica el sobregiro
						LET v_monto_pesos_viv97     = v_monto_pesos_viv97    + (v_importe_tesofe_sobregiro_pesos * v_valor_sobregiro);
						LET v_monto_acciones_viv97  = v_monto_acciones_viv97 + (v_importe_tesofe_sobregiro_aivs  * v_valor_sobregiro);
            
				   	LET v_monto_pesos_var_menor     = v_importe_tesofe_sobregiro_pesos * v_valor_mov_var_menor;
						LET v_monto_acciones_var_menor  = v_importe_tesofe_sobregiro_aivs  * v_valor_mov_var_menor;
				    
					END IF -- v_importe_tesofe_sobregiro_aivs > 0
				END IF --  v_importe_tesofe_viv92_pesos <= v_saldo_92_pesos AND v_importe_tesofe_viv92_pesos > 0
			END IF -- v_importe_tesofe_viv97_aivs <= v_saldo_97_aivs AND v_importe_tesofe_viv97_aivs > 0
		END IF -- r_importe_tesofe <= v_saldo_tesofe_pesos AND r_importe_tesofe > 0
		
		-- Con los montos calculados, se insertan en ret_preliquida
		
		--Si habra cargo en tesofe
		IF v_monto_acciones_tesofe <> 0 THEN
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
				          origen             )
				  VALUES (TODAY                ,
				          v_id_derechohabiente   ,
				          v_subcuenta_tesofe     ,
				          v_fondo_tesofe         ,
				          v_movimiento_tesofe    ,             
				          v_folio_liquida        ,
				          v_id_solicitud         ,
				          v_monto_acciones_tesofe, -- En el caso de tesofe se manejan AIV's a un peso, es decir solo se manejan pesos
				          v_monto_pesos_tesofe   , -- El id 24 "imp_pago_fico"
				          TODAY                  ,
				          TODAY                  ,
				          CURRENT HOUR TO SECOND ,
				          v_origen               );
		END IF
		
		--Si habra cargo en vivienda 97
		IF v_monto_acciones_viv97 <> 0 THEN
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
						      origen             )
					VALUES (TODAY                 ,
						      v_id_derechohabiente  ,
						      v_subcuenta_97        ,
						      v_fondo               ,
						      v_movimiento          ,             
						      v_folio_liquida       ,
						      v_id_solicitud        ,
						      v_monto_acciones_viv97,
						      v_monto_pesos_viv97   ,
						      v_fecha_primer_dia    ,
						      TODAY                 ,
						      CURRENT HOUR TO SECOND,
						      v_origen              );
		END IF
		
		--Si habra cargo por variacion menor
		IF v_monto_acciones_var_menor <> 0 THEN
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
                  origen             )
          VALUES (TODAY,
                  v_id_derechohabiente      ,
                  v_subcuenta_97            ,
                  v_fondo                   ,
                  v_movto_abono_sobreg      ,
                  v_folio_liquida           ,
                  v_id_solicitud            ,
                  v_monto_acciones_var_menor,
                  v_monto_pesos_var_menor   ,
                  v_fecha_primer_dia        ,
                  TODAY                     ,
                  CURRENT HOUR TO SECOND    ,
                  v_origen                  );
		END IF
		
		--Si habra cargo en vivienda 92
		IF v_monto_acciones_viv92 <> 0 THEN
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
							    origen             )
					VALUES (TODAY                 ,
					 		    v_id_derechohabiente  ,
							    v_subcuenta_92        ,
							    v_fondo               ,
							    v_movimiento          ,             
							    v_folio_liquida       ,
							    v_id_solicitud        ,
							    v_monto_acciones_viv92, 
							    v_monto_pesos_viv92   ,
							    v_fecha_primer_dia    ,
							    TODAY                 ,
							    CURRENT HOUR TO SECOND,
						   	  v_origen              );
		END IF
	END IF -- v_tpo_proceso IN('0401','0402','0403','0404','0411','0412','0413','0414')
	-- ================================================================================================


      -- ===========================================================
      -- si se solicito retiro de viv97
      IF ( r_solicitado_aivs_viv97 > 0 ) THEN
          IF v_saldo_vol_aivs > 0 THEN      

            LET v_monto_acciones = v_saldo_vol_aivs   * v_valor_mov_cargo_vol;
            LET v_monto_pesos    = v_saldo_vol_pesos  * v_valor_mov_cargo_vol;

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
            VALUES
               (TODAY                ,
               v_id_derechohabiente  ,
               v_subcuenta_vol       ,
               v_fondo               ,
               v_movimiento_cargo_vol,             
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      , 
               v_monto_pesos         ,  
               v_f_valuacion_tmp     ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
               );    

            LET v_monto_acciones = v_saldo_vol_aivs   * v_valor_mov_abono_vol;
            LET v_monto_pesos    = v_saldo_vol_pesos  * v_valor_mov_abono_vol;

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
            VALUES
               (TODAY                ,
               v_id_derechohabiente  ,
               v_subcuenta_97        ,
               v_fondo               ,
               v_movimiento_abono_vol,             
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      , 
               v_monto_pesos         ,  
               v_f_valuacion_tmp     ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
               );
          END IF -- v_saldo_vol_aivs > 0
          
         -- el saldo del trabajador cubre completamente su retiro
         IF ( (v_saldo_97_aivs + v_saldo_vol_aivs) >= r_solicitado_aivs_viv97 ) THEN     
            
            -- se calculan los montos
            LET v_monto_acciones = r_solicitado_aivs_viv97 * v_valor_mov_cargo;
            LET v_monto_pesos    = r_solicitado_pes_viv97  * v_valor_mov_cargo;
            
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
            VALUES
               (TODAY                ,
               v_id_derechohabiente  ,
               v_subcuenta_97        ,
               v_fondo               ,
               v_movimiento          ,             
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      , 
               v_monto_pesos         ,  
               v_f_valuacion_tmp     ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
               );

         ELSE
         
            -- el saldo con que cuenta el trabajador se preliquida con el movimiento de retiro
            --IF ( (v_saldo_97_aivs + v_saldo_vol_aivs) > 0 ) THEN
                                            
               -- se calculan los montos
               -- se liquida el monto solicitado y posteriormente se hace el ajuste con un movimiento de abono
               -- Requerimiento PRODINF-895
               --LET v_monto_acciones = (v_saldo_97_aivs + v_saldo_vol_aivs)  * v_valor_mov; 
               --LET v_monto_pesos    = (v_saldo_97_pesos + v_saldo_vol_pesos) * v_valor_mov;
               LET v_monto_acciones = r_solicitado_aivs_viv97 * v_valor_mov_cargo;
               LET v_monto_pesos    = r_solicitado_pes_viv97  * v_valor_mov_cargo;
                    
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
                  TODAY                 ,
                  v_id_derechohabiente  ,
                  v_subcuenta_97        ,
                  v_fondo               ,
                  v_movimiento          ,             
                  v_folio_liquida       ,
                  v_id_solicitud        ,
                  v_monto_acciones      , 
                  v_monto_pesos         , 
                  v_f_valuacion_tmp     ,
                  TODAY                 ,
                  CURRENT HOUR TO SECOND,
                  v_origen
               );      
                                   
            --END IF -- ( (v_saldo_97_aivs + v_saldo_vol_aivs) > 0 )
               
            -- se calcula la diferencia
            LET r_solicitado_pes_viv97  = r_solicitado_pes_viv97  - (v_saldo_97_pesos + v_saldo_vol_pesos);
            LET r_solicitado_aivs_viv97 = r_solicitado_aivs_viv97 - (v_saldo_97_aivs  + v_saldo_vol_aivs);    
            
            -- la diferencia se registra como un sobregiro                        
            -- se calculan los montos
            -- se calculan los montos
            LET v_monto_acciones = r_solicitado_aivs_viv97 * v_valor_mov_var_menor; 
            LET v_monto_pesos    = r_solicitado_pes_viv97  * v_valor_mov_var_menor; 

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
               TODAY                  ,
               v_id_derechohabiente   ,
               v_subcuenta_97         ,
               v_fondo                ,
               v_movto_abono_sobreg   ,             
               v_folio_liquida        ,
               v_id_solicitud         ,
               v_monto_acciones       , 
               v_monto_pesos          , 
               v_f_valuacion_tmp      ,
               TODAY                  ,
               CURRENT HOUR TO SECOND ,
               v_origen
            );
            
            -- se registra el saldo
            LET v_viv97_sobregirada = 1;
            
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           v_subcuenta_97        ,
                                                           v_fondo               ,
                                                           v_saldo_97_aivs       ,
                                                           v_saldo_97_pesos      ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           v_subcuenta_vol       ,
                                                           v_fondo               ,
                                                           v_saldo_vol_aivs      ,
                                                           v_saldo_vol_pesos     ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);
            
         END IF -- if para sobregiro de viv97
      END IF -- se solicito retiro de viv97
      
      -- ===========================================================
      -- si se solicito retiro de viv92
      IF ( r_solicitado_aivs_viv92 > 0 ) THEN
         -- si el saldo del trabajador cubre por completo el retiro solicitado
         IF ( v_saldo_92_aivs >= r_solicitado_aivs_viv92 ) THEN

            -- se calculan los montos
            LET v_monto_acciones = r_solicitado_aivs_viv92  * v_valor_mov_cargo;
            LET v_monto_pesos    = r_solicitado_pes_viv92   * v_valor_mov_cargo;

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
               TODAY                 ,
               v_id_derechohabiente  ,
               v_subcuenta_92        ,
               v_fondo               ,
               v_movimiento          ,             
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      , 
               v_monto_pesos         ,  
               v_f_valuacion_tmp     ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
               );
         ELSE -- se debe sobregirar
         
            -- el monto del saldo se preliquida usando el movimiento de retiro 
            --IF ( v_saldo_92_aivs > 0 ) THEN 
                
               -- se calculan los montos
               --LET v_monto_acciones = v_saldo_92_aivs    * v_valor_mov;
               --LET v_monto_pesos    = v_saldo_92_pesos   * v_valor_mov;
               LET v_monto_acciones = r_solicitado_aivs_viv92  * v_valor_mov_cargo;
               LET v_monto_pesos    = r_solicitado_pes_viv92   * v_valor_mov_cargo;

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
                  TODAY,
                  v_id_derechohabiente,
                  v_subcuenta_92,
                  v_fondo,
                  v_movimiento,             
                  v_folio_liquida,
                  v_id_solicitud,
                  v_monto_acciones, 
                  v_monto_pesos, 
                  v_f_valuacion_tmp,
                  TODAY,
                  CURRENT HOUR TO SECOND,
                  v_origen
               );
               
            --END IF -- ( v_saldo_92_aivs > 0 )
            
            -- la diferencia se preliquida con el movimiento de sobregiro
            -- se calcula la diferencia
            LET r_solicitado_pes_viv92  = r_solicitado_pes_viv92  - v_saldo_92_pesos;
            LET r_solicitado_aivs_viv92 = r_solicitado_aivs_viv92 - v_saldo_92_aivs;
            
            -- se calculan los montos
            LET v_monto_acciones = r_solicitado_aivs_viv92 * v_valor_mov_var_menor; 
            LET v_monto_pesos    = r_solicitado_pes_viv92  * v_valor_mov_var_menor; 

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
               TODAY,
               v_id_derechohabiente,
               v_subcuenta_92,
               v_fondo,
               v_movto_abono_sobreg,             
               v_folio_liquida,
               v_id_solicitud,
               v_monto_acciones, 
               v_monto_pesos,  
               v_f_valuacion_tmp,
               TODAY,
               CURRENT HOUR TO SECOND,
               v_origen
            );
            
            -- se registra el saldo
            LET v_viv92_sobregirada = 1;
            
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           v_subcuenta_92        ,
                                                           v_fondo               ,
                                                           v_saldo_92_aivs       ,
                                                           v_saldo_92_pesos      ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);

         END IF -- si hubo sobregiro
      END IF -- si se solicito viv92
      
      -- si alguna de las dos cuentas se sobregiro, y la otra no, se registran sus saldos
      /*IF ( v_viv92_sobregirada = 1 OR v_viv97_sobregirada = 1 ) THEN
         -- se verifica si ya esta registrado el sobregiro en ret_his_saldo
         -- vivienda 92
        
         -- si no se tiene
         IF ( v_viv92_sobregirada = 0 ) THEN
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           8                     ,
                                                           11                    ,
                                                           v_saldo_92_aivs       ,
                                                           v_saldo_92_pesos      ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);
         END IF
         
         -- si no se tiene
         IF ( v_viv97_sobregirada = 0 ) THEN
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           4                     ,
                                                           11                    ,
                                                           v_saldo_97_aivs       ,
                                                           v_saldo_97_pesos      ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);
            -- se inserta el registro en la tabla historica de saldos
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud        ,
                                                           55                    ,
                                                           11                    ,
                                                           v_saldo_vol_aivs       ,
                                                           v_saldo_vol_pesos      ,
                                                           v_folio_liquida       ,
                                                           TODAY                 ,
                                                           CURRENT HOUR TO SECOND);
         END IF
      END IF*/

      -- cambia estatus de campo ingresado a  preliquidacion
      UPDATE  ret_ley73 
      SET     estado_solicitud   = 50,
              folio              = v_folio_liquida
      WHERE   id_derechohabiente = v_id_derechohabiente
      AND     id_solicitud       = v_id_solicitud;
      
      -- busca si se preliquido por lo menos 1 para pasar a liquidar
      LET v_bnd_preli = 1;     
                    
      IF ( v_bnd_preli >= 1 ) THEN
         -- se activa la bandera de que al menos se preliquido 1 registro por todo el proceso
         LET v_b_paso = 1;
      END IF
   END FOREACH;
  
   IF ( v_b_paso = 1 ) THEN
      LET v_c_msj   = 'El proceso finalizó exitosamente.';
   ELSE 
      LET v_si_resultado = 2000;
      LET v_c_msj        = 'El proceso no preliquidó registros. Verifique en la consulta general de retiros.';
   END IF;

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida;

   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


