






CREATE FUNCTION "safreviv".fn_reversa_liquida_duplicada()
RETURNING INTEGER, CHAR(200);

DEFINE v_f_liquida date;
DEFINE v_id_derechohabiente decimal(9,0);
DEFINE v_subcuenta smallint;
DEFINE v_fondo_inversion smallint;
DEFINE v_movimiento smallint;
DEFINE v_folio_liquida decimal(9,0);
DEFINE v_id_referencia decimal(9,0);
DEFINE v_monto_acciones decimal(16,6);
DEFINE v_monto_pesos decimal(12,2);
DEFINE v_f_valor date;
DEFINE v_f_registro date;
DEFINE v_h_registro datetime hour to second;
DEFINE v_origen char(20);
DEFINE v_tot_aivs decimal(16,6);
DEFINE v_tot_pesos decimal(12,2);
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt CHAR(200);
DEFINE v_resultado SMALLINT ;

ON EXCEPTION SET sql_err,
                 isam_err

   LET v_resultado = sql_err;

   RETURN v_resultado, 
          err_txt;

END EXCEPTION

LET v_resultado = 0;
LET v_f_liquida = TODAY;
LET v_tot_aivs = 0; 
LET v_tot_pesos = 0;
LET err_txt = "El ajuste ha concluido exitosamente";

FOREACH

   SELECT id_derechohabiente,
          subcuenta,
          fondo_inversion,
          movimiento,
          id_referencia,
          monto_acciones,
          monto_pesos,
          f_valor,
          origen
   INTO   v_id_derechohabiente,
          v_subcuenta,
          v_fondo_inversion,
          v_movimiento,
          v_id_referencia,
          v_monto_acciones,
          v_monto_pesos,
          v_f_valor,
          v_origen
   FROM   cta_movimiento
   WHERE  folio_liquida IN (125020, 125422, 126112, 127066)
   ORDER BY movimiento
   
   -- Si es cargo(392), aplica 521 ABONO REVERSO OPERATIVO
   IF (v_movimiento = 392) THEN   
      LET v_f_liquida = TODAY;
      LET v_folio_liquida = 127471;
      LET v_f_registro = TODAY;
      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_tot_aivs = (v_monto_acciones * -1);
      LET v_tot_pesos = (v_monto_pesos * -1);
      LET v_movimiento = 521;

      INSERT INTO cta_movimiento
      VALUES(v_f_liquida,
             v_id_derechohabiente,
             v_subcuenta,
             v_fondo_inversion,
             v_movimiento,
             v_folio_liquida,
             v_id_referencia,
             v_tot_aivs,
             v_tot_pesos,
             v_f_valor,
             v_f_registro,
             v_h_registro,
             v_origen);
   END IF

   --Si es abono(151), aplica 1212 CARGO REVERSO OPERATIVO
   IF (v_movimiento = 151) THEN
      LET v_f_liquida = TODAY;
      LET v_folio_liquida = 127471;
      LET v_f_registro = TODAY;
      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_tot_aivs = (v_monto_acciones * -1);
      LET v_tot_pesos = (v_monto_pesos * -1);
      LET v_movimiento = 1212;

      INSERT INTO cta_movimiento
      VALUES(v_f_liquida,
             v_id_derechohabiente,
             v_subcuenta,
             v_fondo_inversion,
             v_movimiento,
             v_folio_liquida,
             v_id_referencia,
             v_tot_aivs,
             v_tot_pesos,
             v_f_valor,
             v_f_registro,
             v_h_registro,
             v_origen);
   END IF
END FOREACH;

RETURN v_resultado, 
       err_txt;
END FUNCTION;


