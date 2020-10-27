






CREATE FUNCTION "safreviv".fn_cbd_prepara_con_sep()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_fproceso             DATE;
   DEFINE v_finicio              DATE;
   DEFINE v_ffin                 DATE;
   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   SET PDQPRIORITY HIGH;
   LET v_fproceso = "10012013";
   LET v_ffin = MDY(MONTH(v_fproceso), 1, YEAR(v_fproceso)) - 1;
   LET v_finicio = MDY(MONTH(v_ffin), 1, YEAR(v_ffin));

   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "El saldo para conciliacion del " || v_finicio || " al " || v_ffin || " se generno correctamente";

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/bdnsviv/debug_prepara_conciliacion_" || YEAR(v_ffin) || "_" || MONTH(v_ffin) || ".txt");
   
   --TRACE("Se inicia el proceso para preparar la conciliacion del periodo " || v_finicio || " al " || v_ffin);

   --TRACE("Inicia la generacion de las tablas para el manejo de los movimientos en el proceso de conciliacion");
   
   --TRACE("Tabla de movimientos del periodo");
   DROP TABLE IF EXISTS cbd_movimiento_periodo;
   CREATE TABLE cbd_movimiento_periodo
     (
       f_liquida date not null ,
       modulo char(3),
       subcuenta smallint not null ,
       movimiento smallint not null ,
       fondo_inversion smallint not null ,
       folio_liquida decimal(9),
       id_referencia decimal(9),
       monto_acciones decimal(22,6),
       monto_pesos decimal(22,2)
     ) fragment by round robin in  cbd_1_dbs, cbd_2_dbs
        extent size 32000 next size 16000 lock mode row;

   --TRACE("Tabla para los movimientos adelantados");
   DROP TABLE IF EXISTS cbd_movimiento_adelanto;
   CREATE TABLE cbd_movimiento_adelanto
     (
       nss char(11) not null,
       f_liquida date not null ,
       modulo char(3),
       subcuenta smallint not null ,
       movimiento smallint not null ,
       fondo_inversion smallint not null ,
       folio_liquida decimal(9),
       id_referencia decimal(9),
       monto_acciones decimal(22,6),
       monto_pesos decimal(22,2),
       ind_periodo smallint not null
     ) fragment by round robin in  cbd_1_dbs, cbd_2_dbs
        extent size 32000 next size 16000 lock mode row;


   --TRACE("Inicia la carga de los movimientos del periodo en la tabla temporal");
   
   DROP TABLE IF EXISTS tmp_movimientos;

   --Se guarda en una tabla temporal todos los movivientos del preriodo
   SELECT mov.f_liquida,
      cat.modulo_cod,
      mov.subcuenta,
      mov.movimiento,
      mov.fondo_inversion,
      mov.folio_liquida,
      mov.id_referencia,
      SUM(mov.monto_acciones) monto_acciones,
      SUM(mov.monto_pesos) monto_pesos
   FROM cta_movimiento mov
   INNER JOIN cat_movimiento cat ON cat.movimiento = mov.movimiento
   WHERE mov.f_liquida >= v_finicio
      AND mov.f_liquida <= v_ffin
   GROUP BY mov.f_liquida,
      cat.modulo_cod,
      mov.subcuenta,
      mov.movimiento,
      mov.fondo_inversion,
      mov.folio_liquida,
      mov.id_referencia
   INTO TEMP tmp_movimientos;

   --TRACE("Se cargan los movimientos con fondo de inversion distinto a cero con precio de accion a la fecha de corte");
   
   INSERT INTO cbd_movimiento_periodo
   SELECT tm.f_liquida,
          tm.modulo_cod,
          tm.subcuenta,
          tm.movimiento,
          tm.fondo_inversion,
          tm.folio_liquida,
          tm.id_referencia,
          tm.monto_acciones,
          (tm.monto_acciones * gf.precio_fondo)
     FROM tmp_movimientos tm,
          glo_valor_fondo gf
   WHERE	 tm.fondo_inversion		 <> 0
       AND gf.fondo              = tm.fondo_inversion
      AND gf.f_valuacion        = v_ffin;

   --TRACE("Se cargan los movimientos con fondo de inversion igual a cero los cuales no tienen precio de accion");

   INSERT INTO cbd_movimiento_periodo
   SELECT tm.f_liquida,
          tm.modulo_cod,
          tm.subcuenta,
          tm.movimiento,
          tm.fondo_inversion,
          tm.folio_liquida,
          tm.id_referencia,
          tm.monto_acciones,
          tm.monto_pesos
     FROM tmp_movimientos tm
   WHERE	 tm.fondo_inversion = 0;

   --TRACE("Inicia la creacion de indices para la tabla de movimientos del periodo");
   CREATE INDEX xie1cbd_movimiento_periodo on cbd_movimiento_periodo (subcuenta,movimiento,modulo) IN cbd_2ix_dbs;
   UPDATE statistics FOR TABLE cbd_movimiento_periodo;
   

   --TRACE("Inicia la generacion de saldos por modulo del periodo");

   DELETE FROM cbd_modulo_periodo WHERE f_saldo = v_ffin;
   
   INSERT INTO cbd_modulo_periodo
   SELECT
      v_ffin,
      mp.modulo,
      mp.subcuenta,
      mp.fondo_inversion,
      cat.tipo,
      SUM(mp.monto_acciones),
      SUM(mp.monto_pesos)
   FROM cbd_movimiento_periodo mp
   INNER JOIN cat_movimiento cat ON cat.movimiento = mp.movimiento
   WHERE mp.modulo IS NOT NULL
   GROUP BY mp.modulo,
      mp.subcuenta,
      mp.fondo_inversion,
      cat.tipo;
   
   DROP TABLE tmp_movimientos;

   --TRACE("Inicia la generacion de informacion con los movimientos adelantados...");
   CALL fn_cbd_adelantos(v_finicio, v_ffin) RETURNING v_resultado;

   --TRACE("Inicia la creacion de indices para la tabla de movimientos adelantados");
   CREATE INDEX xie1cbd_movimiento_adelanto ON cbd_movimiento_adelanto (subcuenta,movimiento,modulo) IN cbd_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_movimiento_adelanto;

   --TRACE("Se generan los saldos adelantados por modulo");
   CALL fn_cbd_adelantos_globales(v_ffin) RETURNING v_resultado;
   
   --TRACE("Finaliza la generacion de datos para la conciliacion");
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;
END FUNCTION;


