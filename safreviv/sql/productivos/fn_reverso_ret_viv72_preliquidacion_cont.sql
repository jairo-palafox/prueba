






CREATE FUNCTION "safreviv".fn_reverso_ret_viv72_preliquidacion_cont(v_folio_liquida DECIMAL(10,0),
                                                         v_proceso_cod   SMALLINT,
                                                         v_opera_cod     SMALLINT)

   RETURNING SMALLINT

   DEFINE v_id_solicitud         DECIMAL(9,0);
   DEFINE v_pid                  SMALLINT;
   DEFINE v_nss                  CHAR(12);
   DEFINE v_marca_entra          SMALLINT;
   DEFINE v_id_afi_fondo72       DECIMAL(9,0);
   DEFINE v_origen                  CHAR(20)                 ;
   DEFINE v_movimiento              SMALLINT                 ;
   DEFINE v_error                   integer ;

   DEFINE g_ar_id_afi_fondo72       decimal(9,0)             ;
   DEFINE g_ar_f_liquida            date                     ;
   DEFINE g_ar_subcuenta            smallint                 ;
   DEFINE g_ar_movimiento           smallint                 ;
   DEFINE g_ar_folio_liquida        decimal(9,0)             ;
   DEFINE g_ar_id_referencia        decimal(9,0)             ;
   DEFINE g_ar_rfc                  char(13)                 ;
   DEFINE g_ar_nss                  char(11)                 ;
   DEFINE g_ar_nombre               char(40)                 ;
   DEFINE g_ar_importe              decimal(22,2)            ;
   DEFINE g_ar_estado_pago          char(1)                  ;
   DEFINE g_ar_tipo_pension         char(1)                  ;
   DEFINE g_ar_prescripcion         char(1)                  ;
   DEFINE g_ar_f_registro           date                     ;
   DEFINE g_ar_h_registro           datetime hour to second  ;
   DEFINE g_ar_origen               char(20)                 ;

   LET v_marca_entra    = 802;
   LET v_nss            = 0;
   LET v_id_solicitud   = 0.0;
   LET v_id_afi_fondo72 = 0;
   LET v_pid            = 0;
   LET v_origen         = "REVERSO W";
   LET v_movimiento     = 141;
   LET v_error          = 0 ;  

   LET g_ar_id_afi_fondo72 = 0;
   LET g_ar_f_liquida      = today;
   LET g_ar_subcuenta      = 0;
   LET g_ar_movimiento     = 0;
   LET g_ar_folio_liquida  = 0;
   LET g_ar_id_referencia  = 0;
   LET g_ar_rfc            = NULL;
   LET g_ar_nss            = NULL;
   LET g_ar_nombre         = NULL;
   LET g_ar_importe        = 0;
   LET g_ar_estado_pago    = NULL;
   LET g_ar_tipo_pension   = NULL;
   LET g_ar_prescripcion   = NULL;
   LET g_ar_f_registro     = today;
   LET g_ar_h_registro     = CURRENT HOUR TO SECOND;
   LET g_ar_origen         = NULL;
   LET v_id_afi_fondo72    = 0;

   --busca todos las solicitudes con el folio dado
/*
   FOREACH
   SELECT id_referencia ,id_afi_fondo72
   INTO v_id_solicitud,v_id_afi_fondo72
   FROM ret_preliquida72
   WHERE folio_liquida = v_folio_liquida
   
      -- regresa a estatus capturado solicitud
      SELECT nss
      INTO v_nss
      FROM afi_fondo72
      WHERE id_afi_fondo72 = v_id_afi_fondo72;

      FOREACH   
      SELECT
                    id_afi_fondo72
                    ,f_liquida
                    ,subcuenta
                    ,movimiento
                    ,folio_liquida
                    ,id_referencia
                    ,importe
                    ,estado_pago
                    ,f_registro
                    ,h_registro
                    ,origen
      INTO  g_ar_id_afi_fondo72,
               g_ar_f_liquida     ,
               g_ar_subcuenta     ,
               g_ar_movimiento    ,
               g_ar_folio_liquida ,
               g_ar_id_referencia ,
               g_ar_importe       ,
               g_ar_estado_pago   ,
               g_ar_f_registro    ,
               g_ar_h_registro    ,
               g_ar_origen
      FROM ret_preliquida72
      WHERE movimiento     in ( 182, 422, 802 ) -- retiro, tanto adicional y sobregiro
      AND folio_liquida    = v_folio_liquida
      AND id_referencia    = v_id_solicitud
      AND id_afi_fondo72   = v_id_afi_fondo72


        LET  g_ar_importe       = g_ar_importe * -1;
        LET g_ar_f_liquida      = TODAY                       ;
        LET g_ar_f_registro     = TODAY                       ;
        LET g_ar_h_registro     = CURRENT HOUR TO SECOND      ;
        LET g_ar_origen         = v_origen                    ;
        LET g_ar_movimiento     = v_movimiento                ;

            INSERT INTO ret_preliquida72 VALUES(g_ar_id_afi_fondo72,
                                                 g_ar_f_liquida     ,
                                                 g_ar_subcuenta     ,
                                                 g_ar_movimiento    ,
                                                 g_ar_folio_liquida ,
                                                 g_ar_id_referencia ,                                                 
                                                 g_ar_importe       ,
                                                 g_ar_estado_pago   ,
                                                 g_ar_f_registro    ,
                                                 g_ar_h_registro    ,
                                                 g_ar_origen
                                                );

           UPDATE  ret_fondo_ahorro SET estado_solicitud     = 15
                                  WHERE id_solicitud         = v_id_solicitud
                                    AND estado_solicitud     = 50;

      END FOREACH; 
  END FOREACH ;
*/
  -- se actualizan las solicitudes preliquidadas a aceptadas
  UPDATE  ret_fondo_ahorro 
  SET estado_solicitud     = 15
  WHERE folio              = v_folio_liquida
  AND estado_solicitud     = 50;

  -- se borran los movimientos en la tabla de preliquidacion
  DELETE FROM ret_preliquida72
  WHERE  folio_liquida = v_folio_liquida;

      --coloca operacion 3 como reversado
/*
      UPDATE bat_ctr_operacion 
      SET estado_cod   = 1
      WHERE folio        = v_folio_liquida
      AND proceso_cod  = v_proceso_cod
      AND opera_cod    = v_opera_cod;
*/
      UPDATE glo_folio 
      SET    status = 0
      WHERE  folio  = v_folio_liquida;

   RETURN v_error ;
END FUNCTION;


