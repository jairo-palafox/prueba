






CREATE FUNCTION "safreviv".fn_ret_preliquida_his_ley73_contra(v_folio_liquida    DECIMAL(10,0),
                                                   v_proceso_cod      SMALLINT,
                                                   v_opera_cod        SMALLINT,
                                                   v_usuario_cod      VARCHAR(20),
                                                   v_pid              DECIMAL(9,0))
       RETURNING INTEGER, INTEGER, VARCHAR(250), DECIMAL(9,0)

DEFINE  v_movimiento_cargo     SMALLINT;
DEFINE  v_valor_mov            SMALLINT;
DEFINE  v_origen               char(20);
DEFINE  v_subcuenta            SMALLINT;

-- para calcular el saldo del trabajador
DEFINE  v_fondo                SMALLINT; -- fondo de inversion
DEFINE  v_monto_acciones       DECIMAL(20,6); -- acciones

-- ret_his_anexo1
DEFINE ret_his_anexo1_id_derechohabiente        DECIMAL(9,0) ;
DEFINE ret_his_anexo1_id_solicitud              DECIMAL(9,0) ;
DEFINE ret_his_anexo1_folio                     DECIMAL(9,0) ;
DEFINE ret_his_anexo1_estado_solicitud          SMALLINT     ;
DEFINE ret_his_anexo1_cod_rechazo               SMALLINT     ;
DEFINE ret_his_anexo1_id_consecutivo            INTEGER      ;
DEFINE ret_his_anexo1_nss                       CHAR(11)     ;
DEFINE ret_his_anexo1_monto                     DECIMAL(13,2);
DEFINE ret_his_anexo1_num_operacion             SMALLINT     ;
DEFINE ret_his_anexo1_monto_total               DECIMAL(13,2);

DEFINE v_bnd_preli             SMALLINT;

-- Control de Excepciones
DEFINE v_si_resultado          SMALLINT;
DEFINE sql_err                 INTEGER;
DEFINE isam_err                INTEGER;
DEFINE err_txt                 VARCHAR(250);
DEFINE v_c_msj                 VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, ret_his_anexo1_id_solicitud;
   END EXCEPTION

   -- se actualiza el folio a preliquidado
   UPDATE glo_folio
   SET    status      =  1
   WHERE  folio       = v_folio_liquida;

   -- actualiza folio en la operacion y proceso
   UPDATE bat_ctr_operacion
   SET    folio        = v_folio_liquida
   WHERE  pid          = v_pid
   AND    proceso_cod  = v_proceso_cod
   AND    opera_cod    = v_opera_cod;

   -- se inician las variables para marca
   LET v_bnd_preli                       = 0;
   LET ret_his_anexo1_id_derechohabiente = 0 ;
   LET ret_his_anexo1_id_solicitud       = 0;
   LET v_movimiento_cargo                = 1792; -- CARGA INICIAL LEY 73 ANEXO 1
   LET v_origen                          = "AJUSTE ANEXO 1";
   LET v_subcuenta                       = 47; -- TESOFE
   LET v_c_msj                           = "El proceso de preliquidación finalizó correctamente.";
   LET isam_err                          = 0;
   LET v_si_resultado                    = 0;
   
   -- se inician las variables para calculo de saldo 
   LET v_fondo              = 10; -- FONDO cuya aiv cuesta 1 peso siempre
   LET v_monto_acciones     = 0;

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_insert_preliquidacion_retiro_ley73.log';
   
   -- se obtiene el signo del movimiento de retiro
   SELECT tipo
   INTO   v_valor_mov
   FROM   cat_movimiento
   WHERE  movimiento = v_movimiento_cargo;

   -- busca registros en estatus de capturado
   FOREACH 
      SELECT 
         id_solicitud              ,
         folio                     ,
         estado_solicitud          ,
         cod_rechazo               ,
         id_consecutivo            ,
         nss                       ,
         monto                     ,
         num_operacion             ,
         monto_total               
      INTO 
          ret_his_anexo1_id_solicitud              ,
          ret_his_anexo1_folio                     ,
          ret_his_anexo1_estado_solicitud          ,
          ret_his_anexo1_cod_rechazo               ,
          ret_his_anexo1_id_consecutivo            ,
          ret_his_anexo1_nss                       ,
          ret_his_anexo1_monto                     ,
          ret_his_anexo1_num_operacion             ,
          ret_his_anexo1_monto_total               
      FROM  ret_his_anexo1_contracargos
      WHERE estado_solicitud = 10 -- aceptados
      AND   folio            = v_folio_liquida
      -- BUsca el id_derechohabiente para su inegracion en la tabla de preliquidacion
      LET ret_his_anexo1_id_derechohabiente = 0;
      SELECT id_derechohabiente 
      INTO   ret_his_anexo1_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = ret_his_anexo1_nss;
      -- se inserta el monto en la tabla de preliquidacion
      IF ret_his_anexo1_id_derechohabiente IS NULL THEN
         -- no se podra insertar en movimientos si no tiene el id_derechohabiente
         UPDATE ret_his_anexo1_contracargos
         SET    estado_solicitud   = 100,
                cod_rechazo        = 7
         WHERE  nss                = ret_his_anexo1_nss
         AND    id_solicitud       = ret_his_anexo1_id_solicitud
         AND    folio              = v_folio_liquida
         AND    estado_solicitud   = 10;
      ELSE 
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
            TODAY                               ,
            ret_his_anexo1_id_derechohabiente   ,
            v_subcuenta                         ,
            v_fondo                             ,
            v_movimiento_cargo                  ,
            v_folio_liquida                     ,
            ret_his_anexo1_id_solicitud         ,
            ret_his_anexo1_monto *  v_valor_mov ,
            ret_his_anexo1_monto *  v_valor_mov ,
            TODAY                               ,
            TODAY                               ,
            CURRENT HOUR TO SECOND              ,
            v_origen
         );
          
         -- se activa la bandera de preliquidacion indicando que se preliquido al menos una solicitud
         LET v_bnd_preli = 1;

         -- se actualica la solicitud a estado liquidado y se le asigna el folio
         UPDATE ret_his_anexo1_contracargos
         SET    estado_solicitud   = 50
         WHERE  nss                = ret_his_anexo1_nss
         AND    id_solicitud       = ret_his_anexo1_id_solicitud
         AND    folio              = v_folio_liquida
         AND    estado_solicitud   = 10;
      END IF 

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
   RETURN v_si_resultado, isam_err, v_c_msj, ret_his_anexo1_id_solicitud;
END FUNCTION;


