






CREATE FUNCTION  "safreviv".fn_reverso_liquidacion_fondo72(p_folio  decimal(10,0))
   RETURNING SMALLINT;
   DEFINE v_error SMALLINT;

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
   
   DEFINE v_id_afi_fondo72          LIKE cta_fondo72.id_afi_fondo72;
   DEFINE v_movimiento              SMALLINT                 ;
   DEFINE v_origen                  CHAR(20)                           ;

   ON EXCEPTION SET v_error
      --Ocurrio un error al realizar el reverso de la liquidación
      --Se regresa el número de error que ocurrio
      RETURN v_error;
   END EXCEPTION

   LET v_movimiento = 141;
   LET v_origen     = "REVERSO W";

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

   --Se inicia el error en 0 para indicar que por default no ocurrio un error
   LET v_error = 0;

   DELETE FROM cta_fondo72
   WHERE folio_liquida = p_folio;
   

   RETURN v_error;

END FUNCTION;


