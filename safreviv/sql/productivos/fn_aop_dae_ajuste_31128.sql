






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste_31128()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado          SMALLINT;
   DEFINE v_mensaje            VARCHAR(100);
   
   LET v_resultado = 0;
   LET v_mensaje   = "AJUSTE DE AJUSTE DE AMORTIZACION EXCEDENTE SE EJECUTO CORRECTAMENTE";
   
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          31171,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 12787098
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    folio_liquida      = 31128
   AND    id_referencia      IN (8981,8982)
   ;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          31171,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 26013815
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    folio_liquida      = 31128
   AND    id_referencia      IN (8974, 8975)
   ; 

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          31171,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 27960464
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    folio_liquida      = 31128
   AND    id_referencia      IN (8971, 8972)
   ; 

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          31171,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 5081343
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    folio_liquida      = 31128
   AND    id_referencia      IN (8977, 8978)   
   ;
   
   UPDATE glo_folio
   SET    folio_referencia = 31128,
          status = 2
   WHERE  folio = 31171
   ;

   RETURN v_resultado, v_mensaje;
END FUNCTION;


