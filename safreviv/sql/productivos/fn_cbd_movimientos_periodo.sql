






CREATE FUNCTION "safreviv".fn_cbd_movimientos_periodo(v_finicio DATE, v_ffin DATE)
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado         SMALLINT;
   DEFINE v_mensaje           VARCHAR(100);

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "Los movimientos del periodo " || TO_CHAR(v_finicio,'%d-%m-%Y') || " al " || TO_CHAR(v_ffin,'%d-%m-%Y') || " se calcularon correctamente";

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
     ) fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
        extent size 128000 next size 16000 lock mode row;
   
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
   AND mov.subcuenta IN (4,8,55)
   GROUP BY mov.f_liquida,
      cat.modulo_cod,
      mov.subcuenta,
      mov.movimiento,
      mov.fondo_inversion,
      mov.folio_liquida,
      mov.id_referencia
   INTO TEMP tmp_movimientos;

   LOCK TABLE cbd_movimiento_periodo IN EXCLUSIVE MODE;
   
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
   WHERE	 tm.fondo_inversion	  = 11
      AND gf.fondo              = tm.fondo_inversion
      AND gf.f_valuacion        = v_ffin;

   UNLOCK TABLE cbd_movimiento_periodo;

   CREATE INDEX xie1cbd_movimiento_periodo on cbd_movimiento_periodo (subcuenta,movimiento,modulo) IN cbd_2ix_dbs;
   UPDATE statistics FOR TABLE cbd_movimiento_periodo;
   
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

   UPDATE statistics FOR TABLE cbd_modulo_periodo;
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;
END FUNCTION;


