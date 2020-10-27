






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste_010916()
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
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente =  1211286
   AND    subcuenta          =  46
   AND    movimiento         =  1422
   AND    id_referencia      =  5488
   AND    folio_liquida      =  28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5489
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5490
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5491
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5492
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5493
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5494
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5495
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5496
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5497
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5498
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5499
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5500
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5501
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5502
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 1211286
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 5503
   AND    folio_liquida      = 28213
   ;
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --ABONO REVERSO OPERATIVO
          56611,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 236190
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    id_referencia      = 3478
   AND    folio_liquida      = 25833
   ;

   RETURN v_resultado, v_mensaje;

END FUNCTION;


