






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste_787815()

RETURNING SMALLINT, VARCHAR(100)

   DEFINE v_resultado          SMALLINT;
   DEFINE v_mensaje            VARCHAR(100);
   
   LET v_resultado = 0;
   LET v_mensaje   = "REV OP DE AJUSTE DE AMORTIZACION EXCEDENTE SE EJECUTO CORRECTAMENTE";
   
   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521,                --movimiento de ajuste de ajuste de DAE
          33497,
          id_referencia,
          monto_acciones * -1,
          monto_pesos * -1,
          f_liquida,
          today,
          h_registro,
          "ROP-FOLIO-" ||folio_liquida
   FROM   cta_movimiento mov
   WHERE  id_derechohabiente = 9404702
   AND    subcuenta          = 46
   AND    movimiento         = 1422
   AND    folio_liquida      = 27843
   AND    id_referencia      IN (4293, 4294, 4292)
   ;

   UPDATE glo_folio
   SET    folio_referencia = 27843,
          status     = 2,
          opera_cod  = 3
   WHERE  folio      = 33497
   ;

   RETURN v_resultado, v_mensaje;
END FUNCTION;


