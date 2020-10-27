






CREATE FUNCTION  "safreviv".fn_ret_rev_liquidacion_fondo_ahorro_ws(p_folio  decimal(10,0))
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
   
   DEFINE v_id_afi_fondo72          DECIMAL(9,0);
   DEFINE v_id_solicitud            DECIMAL(9,0);
   DEFINE v_movimiento              SMALLINT                 ;
   DEFINE v_origen                  CHAR(20)                 ;
   DEFINE v_marca                   SMALLINT;

   ON EXCEPTION SET v_error
      --Ocurrio un error al realizar el reverso de la liquidación
      --Se regresa el número de error que ocurrio
      RETURN v_error;
   END EXCEPTION
   
   -- se crea la marca
   LET v_marca = 802;
   
   --Se inicia el error en 0 para indicar que por default no ocurrio un error
   LET v_error = 0;

   -- se borran los movimientos liquidados
   DELETE FROM cta_fondo72
   WHERE folio_liquida = p_folio;
   
   LET v_id_afi_fondo72 = 0;
   LET v_id_solicitud   = 0;
   
   -- se cambian a estado preliquidado las solicitudes que fueron liquidadas
   FOREACH
   SELECT id_solicitud,
          id_derechohabiente
   INTO   v_id_solicitud,
          v_id_afi_fondo72
   FROM   ret_fondo_ahorro_generico
   WHERE  folio            = p_folio
   AND    estado_solicitud = 60

      -- se reversa la desmarca de las cuentas
      EXECUTE PROCEDURE sp_reversa_desmarca(v_id_afi_fondo72,
                                            v_marca,
                                            v_id_solicitud,
                                            p_folio);
                                            
      -- se actualiza el estatus de la solicitud
      UPDATE ret_fondo_ahorro_generico
      SET    estado_solicitud = 50
      WHERE  folio = p_folio
      AND    estado_solicitud = 60
      AND    id_solicitud = v_id_solicitud;

      UPDATE ret_solicitud_generico
      SET    estado_solicitud = 50
      WHERE  folio = p_folio
      AND    estado_solicitud = 60
      AND    id_solicitud = v_id_solicitud;

   END FOREACH;

   -- se devuelve el resultado de la ejecucion del SP
   RETURN v_error;
END FUNCTION;


