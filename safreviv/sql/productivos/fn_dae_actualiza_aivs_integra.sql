






CREATE FUNCTION "safreviv".fn_dae_actualiza_aivs_integra (p_folio         DECIMAL(9,0),
                                               p_folio_liquida DECIMAL(9,0) )

   RETURNING INTEGER,
             INTEGER,
             CHAR(200)

DEFINE v_id_derechohabiente  DECIMAL(9,0) ;
DEFINE v_id_referencia       DECIMAL(9,0) ;
DEFINE v_monto_acciones      DECIMAL(16,6);
DEFINE v_f_valor             DATE         ;
DEFINE v_precio_fondo        DECIMAL(16,6);
DEFINE sql_err               INTEGER;
DEFINE isam_err              INTEGER;
DEFINE v_isam_err            INTEGER;
DEFINE err_txt               CHAR(200) ;
DEFINE v_i_resultado         SMALLINT;
DEFINE v_nss                 CHAR(11);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_i_resultado = sql_err;
      LET v_isam_err = isam_err;
      LET err_txt = v_nss;

      RETURN v_i_resultado,
             isam_err,
             err_txt;
   END EXCEPTION

LET v_i_resultado         = 0;
LET err_txt               = "Ok";
LET v_isam_err            = 0;
LET v_id_derechohabiente  = 0;
LET v_id_referencia       = 0;
LET v_monto_acciones      = 0;
LET v_f_valor             = NULL;
LET v_precio_fondo        = 0;
LET sql_err               = 0;
LET isam_err              = 0;
LET v_nss                 = "";

FOREACH
   SELECT id_derechohabiente,
          id_referencia,
          monto_acciones,
          f_valor
   INTO   v_id_derechohabiente,
          v_id_referencia,
          v_monto_acciones,
          v_f_valor
   FROM   dae_preliquida
   WHERE  folio_liquida = p_folio_liquida
   AND    subcuenta = 46

   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = v_f_valor;

   UPDATE dae_det_solicitud
   SET    precio_aivs = v_precio_fondo,
          monto_aivs = v_monto_acciones
   WHERE  folio = p_folio
   AND    id_dae_referencia  = v_id_referencia
   AND    id_derechohabiente = v_id_derechohabiente  ;
END FOREACH

 RETURN v_i_resultado,
        v_isam_err,
        err_txt;
END FUNCTION
;


