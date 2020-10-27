






CREATE PROCEDURE "safreviv".reverso_folio_21285()

DEFINE v_precio_fondo DECIMAL(19,14);
DEFINE v_acciones_abono DECIMAL(16,6);
DEFINE v_pesos_abono DECIMAL(12,2);
DEFINE v_acciones_cargo DECIMAL(16,6);
DEFINE v_pesos_cargo DECIMAL(12,2);


LET v_precio_fondo = 0;
LET v_acciones_abono = 0;
LET v_pesos_abono = 0;
LET v_acciones_cargo = 0;
LET v_pesos_cargo = 0;

   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = TODAY;

   SELECT monto_acciones * -1
   INTO   v_acciones_abono
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 33819694
   AND    subcuenta          = 46
   AND    movimiento         = 412
   ;

   LET v_pesos_abono = v_acciones_abono * v_precio_fondo;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          521, --abono
          109810,
          109810,
          v_acciones_abono,
          v_pesos_abono,
          today,
          today,
          h_registro,
          "ROP-FOLIO-21285"
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 33819694
   AND    subcuenta          = 46
   AND    movimiento         = 412
   ;

   SELECT monto_acciones
   INTO   v_acciones_cargo
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 21968541
   AND    subcuenta          = 46
   AND    movimiento         = 171
   ;

   LET v_pesos_cargo = v_acciones_cargo * v_precio_fondo;

   INSERT INTO cta_movimiento
   SELECT today,
          id_derechohabiente,
          subcuenta,
          fondo_inversion,
          1192, -- cargo
          109810,
          109810,
          v_acciones_cargo * -1,
          v_pesos_cargo * -1,
          today,
          today,
          h_registro,
          "ROP-FOLIO-21285"
   FROM   cta_movimiento14 mov
   WHERE  id_derechohabiente = 21968541
   AND    subcuenta          = 46
   AND    movimiento         = 171
   ;

END PROCEDURE;


