






CREATE FUNCTION "safreviv".fn_dpe_liquida_resp_procesar()
RETURNING INTEGER, INTEGER, VARCHAR(255), DECIMAL(9,0), INTEGER 


DEFINE v_aivs_viv_dev       DECIMAL(16,6);
DEFINE v_id_dpe_patron      DECIMAL(16,6);
DEFINE v_id_dpe_referencia  DECIMAL(9,0);
DEFINE v_f_valor_viv        DATE;
DEFINE v_monto_pesos        DECIMAL(12,2);
DEFINE v_monto_pesos_liq    DECIMAL(12,2);
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_folio              DECIMAL(9,0);
DEFINE v_precio_fondo       DECIMAL(19,14);
DEFINE v_error_code         INTEGER;
DEFINE v_error_isam         INTEGER;
DEFINE v_mensaje            VARCHAR(255);
DEFINE i                    INTEGER;

   -- en caso de error
   ON EXCEPTION SET v_error_code, v_error_isam, v_mensaje
      RETURN v_error_code, v_error_isam, v_mensaje, v_id_dpe_referencia, i;
   END EXCEPTION

LET v_aivs_viv_dev       = 0;
LET v_id_dpe_patron      = 0;
LET v_id_dpe_referencia  = 0;
LET v_f_valor_viv        = NULL;
LET v_monto_pesos        = 0;
LET v_monto_pesos_liq    = 0;
LET v_id_derechohabiente = 0;
LET v_folio              = 0;
LET v_precio_fondo       = 0;
LET v_error_code         = 0;
LET v_error_isam         = 0;
LET i = 0;
LET v_mensaje            = "Actualizacion exitosa";

   FOREACH
      SELECT id_dpe_referencia,
             aivs_viv_dev,
             folio
      INTO   v_id_dpe_referencia,
             v_aivs_viv_dev,
             v_folio
      FROM   dpe_resp_procesar
      WHERE  f_respuesta = TODAY
      AND    resul_op = 1
      
      SELECT id_dpe_patron,
             id_derechohabiente
      INTO   v_id_dpe_patron,
             v_id_derechohabiente
      FROM   dpe_sol_trabajador 
      WHERE  id_dpe_referencia = v_id_dpe_referencia;
      
      SELECT f_valor_viv
      INTO   v_f_valor_viv
      FROM   dpe_patron
      WHERE  id_dpe_referencia = v_id_dpe_patron;
      
      SELECT precio_fondo
      INTO   v_precio_fondo
      FROM   glo_valor_fondo
      WHERE   fondo = 11
      AND    f_valuacion = v_f_valor_viv;
      
      LET v_monto_pesos = (v_aivs_viv_dev * v_precio_fondo);
      LET v_monto_pesos_liq = (v_monto_pesos * -1);
      
      UPDATE dpe_resp_procesar
      SET    imp_viv_dev = v_monto_pesos
      WHERE  id_dpe_referencia = v_id_dpe_referencia;
      
      
      UPDATE cta_movimiento
      SET    monto_pesos = v_monto_pesos_liq,
             f_valor = v_f_valor_viv
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    subcuenta = 4
      AND    movimiento = 342
      AND    folio_liquida = v_folio
      AND    id_referencia = v_id_dpe_referencia;
      
      UPDATE dpe_preliquida
      SET    monto_pesos = v_monto_pesos_liq,
             f_valor = v_f_valor_viv
      WHERE  id_derechohabiente = v_id_derechohabiente
      AND    subcuenta = 4
      AND    movimiento = 342
      AND    folio_liquida = v_folio
      AND    id_referencia = v_id_dpe_referencia;
      
      LET i = i + 1;
   END FOREACH;
   
   
   
   -- se devuelve el resultado de la integracion
   RETURN v_error_code,
          v_error_isam,
          v_mensaje,
          v_id_dpe_referencia,
          i;
END FUNCTION
;


