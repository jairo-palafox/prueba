






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste_22751()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado          SMALLINT;
   DEFINE v_mensaje            VARCHAR(100);
   
   LET v_resultado = 0;
   LET v_mensaje = "AJUSTE DE AJUSTE DE AMORTIZACION EXCEDENTE SE EJECUTO CORRECTAMENTE";


   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          23611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 5527123
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    f_valor            <> "01/07/2014"
   AND    folio_liquida      = 22751
   ;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          23611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 19513286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    f_valor           <> "01/07/2014"
   AND    folio_liquida      = 22751
   ; 

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          23611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 20842618
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    f_valor           <> "04/19/2012"
   AND    folio_liquida      = 22751
   ; 

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          23611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 30544787
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    f_valor           <> "07/31/2013"
   AND    folio_liquida      = 22751
   ;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          23611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 34925588
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    f_valor            > "05/07/2013"
   AND    folio_liquida      = 22751
   ;

   RETURN v_resultado, v_mensaje;
END FUNCTION;


