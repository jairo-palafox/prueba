






CREATE FUNCTION "safreviv".fn_preliquida_ley73_ws(v_folio_liquida    DECIMAL(10,0),
                                       v_proceso_cod      SMALLINT,
                                       v_opera_cod        SMALLINT,
                                       v_usuario_cod      VARCHAR(20),
                                       v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

DEFINE  v_b_paso                 SMALLINT;
DEFINE  v_id_derechohabiente     DECIMAL(9,0);
DEFINE  v_id_solicitud           DECIMAL(9,0);
DEFINE  v_movimiento             SMALLINT;
DEFINE  v_movimiento_cargo_vol   SMALLINT;
DEFINE  v_movimiento_abono_vol   SMALLINT;
DEFINE  v_movimiento_sobregiro   SMALLINT;
DEFINE  v_movimiento_tesofe      SMALLINT; -- movimiento de retiro de tesofe
DEFINE  v_movimiento_solo_tesofe SMALLINT;
DEFINE  v_movimiento_tesofe_paso SMALLINT;
DEFINE  v_valor_mov              SMALLINT;
DEFINE  v_valor_mov_cargo_vol    SMALLINT;
DEFINE  v_valor_mov_abono_vol    SMALLINT;
DEFINE  v_valor_mov_sobregiro    SMALLINT;
DEFINE  v_valor_mov_tesofe       SMALLINT;
DEFINE  v_valor_mov_abono_ley73  SMALLINT;
DEFINE  v_movimiento_abono_ley73 SMALLINT;
DEFINE  v_origen                 CHAR(20);
DEFINE  v_subcuenta_92           SMALLINT;
DEFINE  v_subcuenta_97           SMALLINT;
DEFINE  v_subcuenta_vol          SMALLINT;
DEFINE  v_subcuenta_tesofe       SMALLINT; -- para montos transferidos a la tesofe

-- para calcular el saldo del trabajador
DEFINE  v_resultado_consulta    SMALLINT;
DEFINE  v_saldo_92_aivs         DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_92_pesos        DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92
DEFINE  v_saldo_97_aivs         DECIMAL(18,6); -- total de acciones de la cuenta viv97         
DEFINE  v_saldo_97_pesos        DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv92
DEFINE  v_saldo_vol_aivs        DECIMAL(18,6); -- total de acciones de la subcuenta de aportaciones voluntarias
DEFINE  v_saldo_vol_pesos       DECIMAL(20,2); -- total de acciones en pesos de la subcuenta de aportaciones voluntarias
DEFINE  v_saldo_97_tesofe       DECIMAL(20,2); -- salto transferido a la tesofe de viv 97
DEFINE  v_fondo                 SMALLINT; -- fondo de inversion
DEFINE  v_monto_acciones        DECIMAL(18,6); -- acciones
DEFINE  v_monto_pesos           DECIMAL(20,2); -- pesos
DEFINE  v_diferencia_pesos      DECIMAL(14,2);
DEFINE  v_diferencia_aivs       DECIMAL(18,6);

DEFINE  v_f_valuacion           DATE;
DEFINE  v_f_solicitud           DATE;
DEFINE  v_precio_fondo          DECIMAL(19,14);

DEFINE v_i_estado_marca        INTEGER;
DEFINE v_marca_ley73           INTEGER; -- 803 de acuerdo a catalogo
DEFINE v_bnd_preli             SMALLINT;

DEFINE r_pes_viv97             DECIMAL(14,2);
DEFINE r_pes_vol               DECIMAL(14,2);
DEFINE r_pes_viv92             DECIMAL(14,2);

DEFINE r_aivs_viv92            DECIMAL(18,6);
DEFINE r_aivs_viv97            DECIMAL(18,6);
DEFINE r_aivs_vol              DECIMAL(18,6);

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, v_id_solicitud;
   END EXCEPTION

   -- se actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status =  1
   WHERE  proceso_cod = v_proceso_cod
   AND    opera_cod   = v_opera_cod
   AND    folio       = v_folio_liquida;

   -- actualiza folio en la operacion y proceso
   UPDATE bat_ctr_proceso
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod;

   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;

   -- se inician las variables para marca
   LET v_marca_ley73            = 803; -- marca de retiro ley73
   LET v_i_estado_marca         = 0;
   LET v_bnd_preli              = 0;
   LET v_b_paso                 = 0; 
   LET v_id_derechohabiente     = 0;
   LET v_id_solicitud           = 0;
   LET v_movimiento             = 192;
   LET v_movimiento_cargo_vol   = 1662;
   LET v_movimiento_abono_vol   = 911;
   LET v_movimiento_abono_ley73 = 1671;
                               
   LET v_movimiento_sobregiro  = 812; -- sobregiro ley73
   LET v_movimiento_tesofe     = 1442; -- retiro del monto transferido a la TESOFE
   LET v_movimiento_solo_tesofe = 2072; 
   LET v_movimiento_tesofe_paso = 0; 
   LET v_origen                = "RETIRO U";
   LET v_subcuenta_92          = 8; -- vivienda 92
   LET v_subcuenta_97          = 4; -- vivienda 97
   LET v_subcuenta_vol         = 55; -- aportaciones voluntarias
   LET v_subcuenta_tesofe      = 47; -- fondos en la TESOFE
   LET r_pes_viv97             = 0;
   LET r_pes_vol               = 0;
   LET r_pes_viv92             = 0;
   LET r_aivs_viv92            = 0;
   LET r_aivs_viv97            = 0;
   LET r_aivs_vol              = 0;
   LET v_c_msj                 = "El proceso de preliquidación finalizó correctamente.";
   LET isam_err                = 0;
   LET v_si_resultado          = 0;
   
   -- se inician las variables para calculo de saldo
   LET v_resultado_consulta = 0;  
   LET v_saldo_97_aivs      = 0;
   LET v_saldo_vol_aivs     = 0;
   LET v_saldo_97_pesos     = 0;
   LET v_saldo_vol_pesos    = 0;
   LET v_saldo_92_aivs      = 0;
   LET v_saldo_92_pesos     = 0;
   LET v_saldo_97_tesofe    = 0;
   LET v_fondo              = 11;
   LET v_monto_acciones     = 0;
   LET v_monto_pesos        = 0;
   LET v_diferencia_pesos   = 0;
   LET v_diferencia_aivs    = 0;



   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_preliquida_ley73_ws.trace';

	 -- Se obtienen la fecha y el precio de las aivs al primer dia del mes de preliquidacion
	SELECT f_valuacion,precio_fondo
	INTO   v_f_valuacion,v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  f_valuacion = (TODAY - (DAY(TODAY)-1))
   AND    fondo = 11;

   -- se obtiene el signo del movimiento de retiro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento;

   -- se obtiene el signo del movimiento de cargo para las aportaciones voluntarias
   SELECT tipo
   INTO   v_valor_mov_cargo_vol
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo_vol;

   -- se obtiene el signo del movimiento de abono de las aportaciones voluntarias
   SELECT tipo
   INTO   v_valor_mov_abono_vol
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono_vol;
   
   -- se obtiene el signo del movimiento de sobregiro retiro
   SELECT tipo
   INTO   v_valor_mov_sobregiro
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_sobregiro;

   -- se obtiene el signo del movimiento de retiro de monto tesofe
   SELECT tipo
   INTO   v_valor_mov_tesofe
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_tesofe;
   
   -- se obtiene el signo del movimiento de abono variacion menor ley73
   SELECT tipo
   INTO   v_valor_mov_abono_ley73
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_abono_ley73;
   
   -- busca registros en estatus de capturado
   FOREACH 
   SELECT 
      id_solicitud        ,
      id_derechohabiente  ,
      aivs_viv92          ,
      aivs_viv97          ,
      importe_viv92       ,
      importe_viv97       ,
      importe_viv97_anexo1,
      f_solicitud
   INTO 
      v_id_solicitud      ,
      v_id_derechohabiente,
      r_aivs_viv92        ,
      r_aivs_viv97        ,
      r_pes_viv92         ,
      r_pes_viv97         ,
      v_saldo_97_tesofe   ,
      v_f_solicitud
   FROM  ret_ley73_generico
   WHERE estado_solicitud = 15
   
      /*-- se obtiene el saldo del trabajador en vivienda 97
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_97,
                                                v_f_valuacion,
                                                v_fondo)
                        INTO v_saldo_97_pesos, v_saldo_97_aivs, v_resultado_consulta;

      -- se obtiene el saldo del trabajador en las aportaciones voluntarias
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_vol,
                                                v_f_valuacion,
                                                v_fondo)
                        INTO v_saldo_vol_pesos, v_saldo_vol_aivs, v_resultado_consulta;

      -- se obtiene el saldo del trabajador en vivienda 92
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta_92,
                                                v_f_valuacion,
                                                v_fondo)
                        INTO v_saldo_92_pesos, v_saldo_92_aivs, v_resultado_consulta;*/

      -- puesto que fn_recupera_saldo_valuado trabaja con la valuacion del dia y no la de la fecha que se le pasa
      -- vivienda 97
      SELECT f_valuacion,precio_fondo
      INTO   v_f_valuacion,v_precio_fondo
      FROM   glo_valor_fondo
      WHERE  f_valuacion = (v_f_solicitud - (DAY(v_f_solicitud)-1))
      AND    fondo = 11;
      
      SELECT SUM(monto_acciones)
      INTO   v_saldo_97_aivs
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = v_fondo
      AND    subcuenta          = v_subcuenta_97;
      
      LET v_saldo_97_pesos = v_saldo_97_aivs * v_precio_fondo;
      
      -- voluntarias
      SELECT SUM(monto_acciones)
      INTO   v_saldo_vol_aivs
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = v_fondo
      AND    subcuenta          = v_subcuenta_vol;
      
      LET v_saldo_vol_pesos = v_saldo_vol_aivs * v_precio_fondo;
      
      -- vivienda 92
      SELECT SUM(monto_acciones)
      INTO   v_saldo_92_aivs
      FROM   cta_movimiento
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    fondo_inversion    = v_fondo
      AND    subcuenta          = v_subcuenta_92;
      
      LET v_saldo_92_pesos = v_saldo_92_aivs * v_precio_fondo;
      

      -- si la cuenta ya esta sobregirada se dejan los saldos en cero
      IF ( v_saldo_97_aivs < 0 OR v_saldo_97_aivs IS NULL ) THEN
         LET v_saldo_97_aivs  = 0;
         LET v_saldo_97_pesos = 0;
      END IF

      -- si la cuenta ya esta sobregirada se dejan los saldos en cero
      IF ( v_saldo_vol_aivs < 0 OR v_saldo_vol_aivs IS NULL ) THEN
         LET v_saldo_vol_aivs  = 0;
         LET v_saldo_vol_pesos = 0;
      END IF
      
      IF ( v_saldo_92_aivs < 0 OR v_saldo_92_aivs IS NULL ) THEN
         LET v_saldo_92_aivs  = 0;
         LET v_saldo_92_pesos = 0;
      END IF

	  -- =======================================================================
	  -- MONTO DE VIVIENDA 92
      -- si se solicito retiro de vivienda 92
      IF ( r_aivs_viv92 > 0 ) THEN
      
         -- si el saldo es suficiente
         IF ( r_aivs_viv92 <= v_saldo_92_aivs ) THEN
      
            LET v_monto_pesos    = r_aivs_viv92 * v_valor_mov * v_precio_fondo;
            LET v_monto_acciones = r_aivs_viv92 * v_valor_mov;         
            
            -- se inserta el monto en la tabla de preliquidacion
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
               origen            )
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
               v_f_valuacion         ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
            );

         ELSE -- se debe sobregirar
            -- el monto del saldo se preliquida usando el movimiento de retiro

            -- Se debe hacer el cargo tenga o no saldo el trabajador (para evitar realizar abono
            -- por variacion menor sin un cargo)

            --IF ( v_saldo_92_aivs > 0 ) THEN                
               -- se calculan los montos
               --LET v_monto_acciones = v_saldo_92_aivs * v_valor_mov;
               --LET v_monto_pesos    = v_saldo_92_aivs * v_valor_mov * v_precio_fondo;
               LET v_monto_acciones = r_aivs_viv92 * v_valor_mov;
               LET v_monto_pesos    = r_aivs_viv92 * v_valor_mov * v_precio_fondo;

               -- se inserta el monto en la tabla de preliquidacion
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
                  origen            )
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
                  v_f_valuacion         ,
                  TODAY                 ,
                  CURRENT HOUR TO SECOND,
                  v_origen
               );
            --END IF -- ( v_saldo_92_aivs > 0 )

            -- la diferencia se preliquida con el movimiento de sobregiro
            -- se calcula la diferencia
            LET v_diferencia_pesos = (r_aivs_viv92 * v_precio_fondo) - v_saldo_92_pesos;
            LET v_diferencia_aivs  = r_aivs_viv92 - v_saldo_92_aivs;                  
           
            -- se calculan los montos, (ahora se incluye variacion menor y no solo sobregiro)
            --LET v_monto_acciones = v_diferencia_aivs  * v_valor_mov_sobregiro; 
            --LET v_monto_pesos    = v_diferencia_aivs  * v_valor_mov_sobregiro * v_precio_fondo;
            LET v_monto_acciones = v_diferencia_aivs  * v_valor_mov_abono_ley73; 
            LET v_monto_pesos    = v_diferencia_aivs  * v_valor_mov_abono_ley73 * v_precio_fondo;

            -- se inserta el monto en la tabla de preliquidacion
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
               origen            )
            VALUES (
               TODAY                   ,
               v_id_derechohabiente    ,
               v_subcuenta_92          ,
               v_fondo                 ,
               --v_movimiento_sobregiro  ,
               v_movimiento_abono_ley73,
               v_folio_liquida         ,
               v_id_solicitud          ,
               v_monto_acciones        ,
               v_monto_pesos           ,
               v_f_valuacion           ,
               TODAY                   ,
               CURRENT HOUR TO SECOND  ,
               v_origen
            );
            
            -- si existe, se actualiza la tabla de historicos de sobregiro
            UPDATE ret_his_saldo
            SET    folio           = v_folio_liquida
            WHERE  id_solicitud    = v_id_solicitud
            AND    fondo_inversion = v_fondo
            AND    subcuenta       = v_subcuenta_92;

         END IF -- si hubo sobregiro
      END IF -- si se solicito viv92

	  -- =======================================================================
	  -- MONTO DE VIVIENDA 97
      -- si se tiene monto de vivienda 97 para liquidar
      IF ( r_aivs_viv97 > 0 ) THEN
        IF v_saldo_vol_aivs > 0 THEN 
            LET v_monto_pesos    = v_saldo_vol_aivs * v_valor_mov_cargo_vol * v_precio_fondo; 
            LET v_monto_acciones = v_saldo_vol_aivs * v_valor_mov_cargo_vol; 
            -- Inserta el cargo a la subcuenta de aportaciones voluntarias
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
               origen            )
            VALUES (
               TODAY                 ,
               v_id_derechohabiente  ,
               v_subcuenta_vol       ,
               v_fondo               ,
               v_movimiento_cargo_vol,
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      ,
               v_monto_pesos         ,
               v_f_valuacion         ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
            );
            LET v_monto_pesos    = v_saldo_vol_aivs * v_valor_mov_abono_vol * v_precio_fondo; 
            LET v_monto_acciones = v_saldo_vol_aivs * v_valor_mov_abono_vol; 
            -- Inserta el abono a la subcuenta de aportaciones voluntarias
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
               origen            )
            VALUES (
               TODAY                 ,
               v_id_derechohabiente  ,
               v_subcuenta_97        ,
               v_fondo               ,
               v_movimiento_abono_vol,
               v_folio_liquida       ,
               v_id_solicitud        ,
               v_monto_acciones      ,
               v_monto_pesos         ,
               v_f_valuacion         ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
            );

        END IF 
            -- si el saldo es suficiente
         IF ( r_aivs_viv97 <= (v_saldo_97_aivs + v_saldo_vol_aivs)) THEN
            -- se calculan los montos
            LET v_monto_pesos    = r_aivs_viv97 * v_valor_mov * v_precio_fondo;
            LET v_monto_acciones = r_aivs_viv97 * v_valor_mov;

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
               origen            )
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
               v_f_valuacion         ,
               TODAY                 ,
               CURRENT HOUR TO SECOND,
               v_origen
            );
            
         ELSE -- se debe sobregirar
            -- el monto del saldo se preliquida usando el movimiento de retiro
						
            -- Se debe hacer el cargo tenga o no saldo el trabajador (para evitar realizar abono
            -- por variacion menor sin un cargo)

            --IF ( v_saldo_97_aivs > 0 ) THEN               
               -- se calculan los montos
               --LET v_monto_acciones = v_saldo_97_aivs * v_valor_mov;
               --LET v_monto_pesos    = v_saldo_97_aivs * v_valor_mov * v_precio_fondo;
               LET v_monto_acciones = r_aivs_viv97 * v_valor_mov;
               LET v_monto_pesos    = r_aivs_viv97 * v_valor_mov * v_precio_fondo;

               -- se inserta el monto en la tabla de preliquidacion
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
                  origen            )
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
                  v_f_valuacion         ,
                  TODAY                 ,
                  CURRENT HOUR TO SECOND,
                  v_origen
               );
               
            --END IF -- ( v_saldo_97_aivs > 0 )

            -- la diferencia se preliquida con el movimiento de sobregiro
            -- se calcula la diferencia
            LET v_diferencia_pesos = r_pes_viv97  - (v_saldo_97_pesos + v_saldo_vol_pesos);
            LET v_diferencia_aivs  = r_aivs_viv97 - (v_saldo_97_aivs  + v_saldo_vol_aivs);
            
            -- se calculan los montos, (ahora se incluye variacion menor y no solo sobregiro)
            --LET v_monto_acciones = v_diferencia_aivs  * v_valor_mov_sobregiro; 
            --LET v_monto_pesos    = v_diferencia_aivs  * v_valor_mov_sobregiro * v_precio_fondo;
            LET v_monto_acciones = v_diferencia_aivs  * v_valor_mov_abono_ley73; 
            LET v_monto_pesos    = v_diferencia_aivs  * v_valor_mov_abono_ley73 * v_precio_fondo;

            -- se inserta el monto en la tabla de preliquidacion
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
               origen            )
            VALUES (
               TODAY                   ,
               v_id_derechohabiente    ,
               v_subcuenta_97          ,
               v_fondo                 ,
               --v_movimiento_sobregiro  ,
               v_movimiento_abono_ley73,
               v_folio_liquida         ,
               v_id_solicitud          ,
               v_monto_acciones        ,
               v_monto_pesos           ,
               v_f_valuacion           ,
               TODAY                   ,
               CURRENT HOUR TO SECOND  ,
               v_origen
            );
            
            -- si existe, se actualiza la tabla de historicos de sobregiro
            UPDATE ret_his_saldo
            SET    folio           = v_folio_liquida
            WHERE  id_solicitud    = v_id_solicitud
            AND    fondo_inversion = v_fondo
            AND    subcuenta       = v_subcuenta_97;

         END IF -- si hubo sobregiro
	  END IF

	  -- =======================================================================
	  -- MONTO TRANSFERIDO A LA TESOFE
      -- si se tiene monto de vivienda 97 transferido a la tesofe
      IF ( v_saldo_97_tesofe > 0 ) THEN
         -- se calculan los montos
         LET v_monto_pesos    = v_saldo_97_tesofe * v_valor_mov_tesofe;
         LET v_monto_acciones = v_saldo_97_tesofe * v_valor_mov_tesofe; 
         IF r_aivs_viv97 = 0 AND r_aivs_viv92 = 0 THEN
            LET v_movimiento_tesofe_paso = v_movimiento_solo_tesofe;
         ELSE 
            LET v_movimiento_tesofe_paso = v_movimiento_tesofe;
         END IF
         
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
            origen            )
         VALUES (
            TODAY                 ,
            v_id_derechohabiente  ,
            v_subcuenta_tesofe    ,
            10                    , -- subcuenta 10. Pesos
            v_movimiento_tesofe_paso   ,
            v_folio_liquida       ,
            v_id_solicitud        ,
            v_monto_acciones      ,
            v_monto_pesos         ,
            TODAY                 ,
            TODAY                 ,
            CURRENT HOUR TO SECOND,
            v_origen
         );
		 
      END IF

      -- se actualica la solicitud a estado liquidado y se le asigna el folio
      UPDATE ret_ley73_generico 
      SET    estado_solicitud   = 50,
             folio              = v_folio_liquida
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    id_solicitud       = v_id_solicitud;

      -- se actualiza la tabla de control de solicitudes
      UPDATE ret_solicitud_generico
      SET    folio            = v_folio_liquida,
             estado_solicitud = 50
      WHERE  id_solicitud     = v_id_solicitud
      AND    modalidad_retiro = 3;

      -- Actualiza las solicitudes que tengan beneficiarios 
      UPDATE ret_beneficiario_juridico
      SET    estado_solicitud = 50
      WHERE  id_solicitud     = v_id_solicitud
      AND    estado_solicitud = 15;
      

      -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
      LET v_bnd_preli = 1;

      -- se asgina el folio a la tabla de marcas
      UPDATE sfr_marca_activa
      SET    folio              = v_folio_liquida
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    marca              = v_marca_ley73
      AND    n_referencia       = v_id_solicitud;

      UPDATE sfr_marca_historica
      SET    folio                = v_folio_liquida
      WHERE  id_derechohabiente   = v_id_derechohabiente
      AND    marca                = v_marca_ley73
      AND    n_referencia         = v_id_solicitud;

   END FOREACH;

   -- si no se preliquidaron registros
   IF ( v_bnd_preli = 0 ) THEN
      -- se marca el procesoe en error
      LET v_si_resultado = 1000;
      LET isam_err       = 0;
      LET v_c_msj        = "Error. No se preliquidaron solicitudes para el folio.";
   END IF;

   -- se actullizan las estadisticas de los registros cargados
   UPDATE STATISTICS FOR TABLE ret_preliquida;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, v_id_solicitud;
END FUNCTION;


