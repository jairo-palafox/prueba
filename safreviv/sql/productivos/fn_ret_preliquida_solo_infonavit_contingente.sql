






CREATE FUNCTION "safreviv".fn_ret_preliquida_solo_infonavit_contingente(v_folio_liquida  DECIMAL(10,0),
                                                          v_proceso_cod         SMALLINT,
                                                          v_opera_cod           SMALLINT,
                                                          v_usuario_cod         VARCHAR(20),
                                                          v_pid                 DECIMAL(9,0))
   RETURNING INTEGER, INTEGER, VARCHAR(250)
                                                 
DEFINE  v_id_derechohabiente   DECIMAL(9,0); -- identificador de derechohabiente
DEFINE  v_id_solicitud         DECIMAL(9,0); -- identificador solicitud de retiro
DEFINE  v_acc_solicitadas      DECIMAL(18,6);
DEFINE  v_pes_solicitadas      DECIMAL(20,2);
DEFINE  v_movimiento           INTEGER; -- moovimiento de retiro SOLO INF
DEFINE  v_movimiento_sobregiro INTEGER; -- movimiento de sobregiro de retiro SOLO INF
DEFINE  v_movto_cargo_trasp    SMALLINT;
DEFINE  v_movto_abono_trasp    SMALLINT;
DEFINE  v_valor_mov            INTEGER;
DEFINE  v_origen               CHAR(20);
DEFINE  v_subcuenta            INTEGER;
DEFINE  v_subcuenta_97         SMALLINT;
DEFINE  v_fondo                INTEGER;
DEFINE  v_saldo_subcta44_aivs  DECIMAL(18,6); -- total de acciones de la subcuenta 44
DEFINE  v_saldo_subcta44_pesos DECIMAL(20,2); -- total de acciones en pesos de la subcuenta 44
DEFINE  v_saldo_viv97_aivs     DECIMAL(18,6); -- total de acciones de la cuenta viv97
DEFINE  v_saldo_viv97_pesos    DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv97   
DEFINE  v_resultado_consulta   INTEGER;
DEFINE  v_bnd_preli            INTEGER;

-- variables para calcular la diferencia de saldo
DEFINE  v_dif_aivs             DECIMAL(18,6); -- total de acciones de la cuenta viv97
DEFINE  v_dif_pesos            DECIMAL(20,2); -- total de acciones en pesos de la cuenta viv97
DEFINE  v_aivs                 DECIMAL(18,6);
DEFINE  v_pesos                DECIMAL(20,2);
DEFINE  v_signo_cargo          SMALLINT;
DEFINE  v_signo_abono          SMALLINT;

DEFINE v_i_estado_marca        INTEGER;
DEFINE v_marca_solo_infonavit  INTEGER; -- 801 de acuerdo a catalogo

-- variables para recepcion de errores
DEFINE v_sql_error  INTEGER     ;
DEFINE v_isam_error INTEGER     ;
DEFINE v_mensaje    VARCHAR(250);
DEFINE v_f_valuacion           DATE;
DEFINE v_valor_fondo           DECIMAL(19,14);
   
   -- en caso de error
   ON EXCEPTION SET v_sql_error, v_isam_error, v_mensaje
      
      RETURN v_sql_error, v_isam_error, v_mensaje;
   END EXCEPTION
   
   -- se inician las variables para marca
   LET v_marca_solo_infonavit = 801; -- marca para disposicion de recursos
   LET v_i_estado_marca       = 0;
   
   -- se asume que no hay error
   LET v_sql_error          = 0;
   LET v_isam_error         = 0;
   LET v_mensaje            = "Preliquidación finalizada correctamente.";
   
   LET v_id_derechohabiente = 0;
   LET v_id_solicitud       = 0;
   LET v_acc_solicitadas    = 0; 
   LET v_pes_solicitadas    = 0;
   LET v_valor_mov          = 1;
   LET v_dif_aivs           = 0;
   LET v_dif_pesos          = 0;
   LET v_aivs               = 0;
   LET v_pesos              = 0;
   LET v_signo_cargo        = -1;
   LET v_signo_abono        = 1;
   
   -- se inician las variables de movimientos
   LET v_movimiento           = 172; -- retiro Solo Infonavit
   LET v_movimiento_sobregiro = 792; -- sobregiro de retiro Solo Infonavit
   LET v_movto_cargo_trasp    = 2062; -- Cargo traspaso al saldo solo infonavit por retiro
   LET v_movto_abono_trasp    = 1831; -- Abono traspaso al saldo solo infonavit por retiro
   
   LET v_origen                = "RETIRO Z";
   LET v_subcuenta             = 44;
   LET v_subcuenta_97          = 4;
   LET v_resultado_consulta    = 0;
   LET v_saldo_subcta44_aivs   = 0;
   LET v_saldo_subcta44_pesos  = 0;
   LET v_saldo_viv97_aivs      = 0;
   LET v_saldo_viv97_pesos     = 0;
   LET v_bnd_preli             = 0;
   LEt v_fondo                 = 11;
   --LEt v_proceso_cod        = 1501;
     
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/debug_fn_ret_preliquida_sinf_contingente.debug';

   -- se marca el folio como preliquidado
   UPDATE glo_folio 
   SET    status =  1 
   WHERE  folio  = v_folio_liquida;
           
   --actualiza folio en la operacion (preliquidacion)
   UPDATE bat_ctr_operacion 
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;

   --busca registros en estatus de capturado
   FOREACH cu_ret_solo_infonavit FOR 
   SELECT 
      id_derechohabiente ,
      id_solicitud       ,
      aivs_viv97         ,
      importe_viv97      ,
      f_valuacion
   INTO   
      v_id_derechohabiente,
      v_id_solicitud      ,
      v_acc_solicitadas         ,
      v_pes_solicitadas         ,
      v_f_valuacion
   FROM  ret_solo_infonavit
   WHERE estado_solicitud = 10

      -- se obtiene el valor del fondo segun la fecha de valuacion del registro
      SELECT precio_fondo
      INTO   v_valor_fondo
      FROM   glo_valor_fondo
      WHERE  f_valuacion = v_f_valuacion
      AND    fondo = 11;
   
      -- se obtiene el saldo
      EXECUTE FUNCTION fn_recupera_saldo_valuado(NULL,
                                                v_id_derechohabiente,
                                                v_subcuenta,
                                                NULL,
                                                v_fondo)
         INTO v_saldo_subcta44_pesos,v_saldo_subcta44_aivs,v_resultado_consulta;

      -- si el saldo es mayor o igual al solicitado
      IF ( v_saldo_subcta44_aivs >= v_acc_solicitadas ) THEN
         -- =====================================================================
         --  SALDO SUFICIENTE. SE PRELQIUIDA LA CUENTA NORMALMENTE
         -- =====================================================================      
         -- se obtiene el signo del movimiento de retiro
         SELECT tipo 
         INTO   v_valor_mov
         FROM   cat_movimiento
         WHERE  movimiento = v_movimiento;
      
         -- valuacion de AIVs y pesos
         LET v_aivs  = v_acc_solicitadas * v_valor_mov;
         LET v_pesos = v_acc_solicitadas * v_valor_mov * v_valor_fondo;
         
         --TRACE ('-------------------------------------');
         --TRACE ('Total monto en pesos a preliquidar');
         --TRACE (v_pes_solicitadas);
         --TRACE ('Valor antes de hacer el cambio');
         --TRACE (v_pes_solicitadas);
         --TRACE ('Tipo de movimiento');
         --TRACE (v_valor_mov);
         --TRACE (v_saldo_subcta44_pesos);
         --TRACE (v_pes_solicitadas);    

         --inserta registros en estatus de capturado a la tabla de preliquidacion
         INSERT INTO ret_preliquida (
            f_liquida     , id_derechohabiente , subcuenta     , fondo_inversion    ,
            movimiento    , folio_liquida      , id_referencia , monto_acciones     ,
            monto_pesos   , f_valor            , f_registro    , h_registro         ,
            origen             
            )
         VALUES (
            TODAY          , v_id_derechohabiente  , v_subcuenta    , v_fondo               ,
            v_movimiento   , v_folio_liquida       , v_id_solicitud , v_aivs                ,
            v_pesos        , TODAY                 , TODAY          , CURRENT HOUR TO SECOND,
            v_origen
         );

      ELSE
         -- =====================================================================
         --  SALDO INSUFICIENTE. SE SOBREGIRA LA CUENTA
         -- =====================================================================
         --- Se verifica si tiene saldo en vivienda 97 por si es así se hará el traspaso a la subcta 44 
         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                      v_id_derechohabiente,
                                      v_subcuenta_97,
                                      TODAY)
         INTO v_resultado_consulta,v_saldo_viv97_aivs, v_saldo_viv97_pesos;
         IF v_saldo_viv97_aivs > 0 THEN 
            IF v_saldo_subcta44_aivs >= 0 THEN 
               LET v_dif_aivs = v_acc_solicitadas - v_saldo_subcta44_aivs;
            ELSE 
               LET v_dif_aivs = v_acc_solicitadas;
            END IF 
            IF v_saldo_viv97_aivs >= v_dif_aivs THEN  --- Se traspasa la diferencia a la subcta 44
               LET v_aivs = v_dif_aivs;
               LET v_saldo_subcta44_aivs = v_saldo_subcta44_aivs + v_dif_aivs; 
            ELSE    
               LET v_aivs = v_saldo_viv97_aivs;
               LET v_saldo_subcta44_aivs = v_saldo_subcta44_aivs + v_saldo_viv97_aivs; 
            END IF  
            --- Se registra el cargo a vivienda 97
            LET v_aivs  = v_aivs * v_signo_cargo;
            LET v_pesos = v_aivs * v_valor_fondo;    
            INSERT INTO ret_preliquida ( 
               f_liquida   , id_derechohabiente , subcuenta     , fondo_inversion ,
               movimiento  , folio_liquida      , id_referencia , monto_acciones  ,
               monto_pesos , f_valor            , f_registro    , h_registro      ,
               origen       
               )
            VALUES (
               TODAY               , v_id_derechohabiente  , v_subcuenta_97 , v_fondo               ,
               v_movto_cargo_trasp , v_folio_liquida       , v_id_solicitud , v_aivs                ,
               v_pesos             , TODAY                 , TODAY          , CURRENT HOUR TO SECOND,
               v_origen
               );
            --- Se registra el abono a la subcuenta 44 
            LET v_aivs  = v_aivs * v_signo_cargo;    --- Se hace positivo el movimiento
            LET v_pesos = v_aivs * v_valor_fondo;    

            INSERT INTO ret_preliquida ( 
               f_liquida   , id_derechohabiente , subcuenta     , fondo_inversion ,
               movimiento  , folio_liquida      , id_referencia , monto_acciones  ,
               monto_pesos , f_valor            , f_registro    , h_registro      ,
               origen       
               )
            VALUES (
               TODAY               , v_id_derechohabiente  , v_subcuenta    , v_fondo               ,
               v_movto_abono_trasp , v_folio_liquida       , v_id_solicitud , v_aivs                ,
               v_pesos             , TODAY                 , TODAY          , CURRENT HOUR TO SECOND,
               v_origen
               );
            LET v_dif_aivs  = 0;
            LET v_dif_pesos = 0;
            LET v_aivs      = 0;
            LET v_pesos     = 0;
         END IF          

         --trace ("saldo insuficiente");
         -- si el saldo del trabajor es mayor a cero
         IF ( v_saldo_subcta44_aivs > 0 ) THEN
            -- se preliquida el saldo que el derechohabiente tiene       
            -- se obtiene el signo del movimiento de retiro
            SELECT tipo 
            INTO   v_valor_mov
            FROM   cat_movimiento
            WHERE  movimiento = v_movimiento;
            
            -- se asigna el signo y el monto a preliquidar
            LET v_aivs  = v_saldo_subcta44_aivs  * v_valor_mov;
            LET v_pesos = v_saldo_subcta44_aivs * v_valor_mov * v_valor_fondo;
                       
            --inserta registros en estatus de capturado a la tabla de preliquidacion
            INSERT INTO ret_preliquida (
               f_liquida     , id_derechohabiente , subcuenta     , fondo_inversion    ,
               movimiento    , folio_liquida      , id_referencia , monto_acciones     ,
               monto_pesos   , f_valor            , f_registro    , h_registro         ,
               origen             
               )
            VALUES (
               TODAY          , v_id_derechohabiente  , v_subcuenta    , v_fondo               ,
               v_movimiento   , v_folio_liquida       , v_id_solicitud , v_aivs                ,
               v_pesos        , TODAY                 , TODAY          , CURRENT HOUR TO SECOND,
               v_origen
            );
         END IF

         -- se preliquida la diferencia con el movimiento de sobregiro
         -- se obtiene el signo del movimiento de retiro
         SELECT tipo 
         INTO   v_valor_mov
         FROM   cat_movimiento
         WHERE  movimiento = v_movimiento_sobregiro;
      
         -- si el saldo ya viene sobregirado, se deja en cero
         IF ( v_saldo_subcta44_aivs < 0 ) THEN
            LET v_saldo_subcta44_aivs = 0;
         END IF
      
         IF ( v_saldo_subcta44_pesos < 0 ) THEN
            LET v_saldo_subcta44_pesos = 0;
         END IF
         -- se obtiene la diferencia
         LET v_dif_aivs  = v_acc_solicitadas - v_saldo_subcta44_aivs;
         LET v_dif_pesos = (v_acc_solicitadas - v_saldo_subcta44_aivs) * v_valor_fondo;    

         IF v_dif_aivs > 0 THEN 

            -- se registra en ret_his_saldo que se sobregiro la cuenta
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(v_id_solicitud       ,
                                                           v_subcuenta          ,
                                                           v_fondo              ,
                                                           v_saldo_subcta44_aivs      ,
                                                           v_saldo_subcta44_pesos     ,
                                                           v_folio_liquida      ,   
                                                           TODAY                ,
                                                           CURRENT HOUR TO SECOND);
         

            -- se asigna el signo y el monto a preliquidar
            LET v_aivs  = v_dif_aivs  * v_valor_mov;
            LET v_pesos = v_dif_pesos * v_valor_mov;

            --inserta registros en estatus de capturado a la tabla de preliquidacion
            INSERT INTO ret_preliquida (
               f_liquida     , id_derechohabiente , subcuenta     , fondo_inversion    ,
               movimiento    , folio_liquida      , id_referencia , monto_acciones     ,
               monto_pesos   , f_valor            , f_registro    , h_registro         ,
               origen             
               )
            VALUES (
               TODAY                 , v_id_derechohabiente  , v_subcuenta    , v_fondo               ,
               v_movimiento_sobregiro, v_folio_liquida       , v_id_solicitud , v_aivs                ,
               v_pesos               , TODAY                 , TODAY          , CURRENT HOUR TO SECOND,
               v_origen
            );
         END IF 

      END IF;
      
      -- se cambia el estado de la solicitud a preliquidado
      UPDATE ret_solo_infonavit 
      SET    estado_solicitud   = 50,
             folio              = v_folio_liquida,
             f_captura          = TODAY 
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    id_solicitud       = v_id_solicitud;
      
      LET v_bnd_preli = 1;     -- indica que se preliquido al menos un registro
   END FOREACH;
    
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_preliquida;
   
   RETURN v_sql_error, v_isam_error, v_mensaje;
END FUNCTION;


