






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste_839()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado SMALLINT;
   DEFINE v_mensaje   VARCHAR(100);
   
   LET v_resultado = 0;
   LET v_mensaje = "AJUSTE DE AJUSTE DE AMORTIZACION EXCEDENTE SE EJECUTO CORRECTAMENTE";

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --movimiento de ajuste de ajuste de DAE
          38833,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 5108992
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 7901
   AND    folio_liquida      = 29144
   ;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          38833,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 5108992
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 7900
   AND    folio_liquida      = 29144
   ; 

   RETURN v_resultado, v_mensaje;
END FUNCTION;


