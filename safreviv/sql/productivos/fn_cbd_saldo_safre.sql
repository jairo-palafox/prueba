






CREATE FUNCTION "safreviv".fn_cbd_saldo_safre(v_fcorte DATE)
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   DEFINE v_precio               DECIMAL(20,6);

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "El saldo fue generado con fecha de corte = " || TO_CHAR(v_fcorte,'%d-%m-%Y');

   DROP TABLE IF EXISTS cbd_saldo_safre;
   CREATE TABLE cbd_saldo_safre
     (
       id_derechohabiente decimal(9,0) not null ,
       subcuenta smallint not null ,
       fondo_inversion smallint not null ,
       monto_acciones decimal(20,6),
       monto_pesos decimal(20,2),
       f_saldo date
     )
  fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
  extent size 128000 next size 16000 lock mode row;

  --Se valida el precio de accion
   SELECT precio_fondo
   INTO v_precio
   FROM glo_valor_fondo 
   WHERE fondo = 11
   AND f_valuacion = v_fcorte;

   IF (v_precio IS NULL OR v_precio <= 0) THEN
      LET v_resultado = 1;
      LET v_mensaje = "ERROR: No existe el precio de accion para el dia " || TO_CHAR(v_fcorte,'%d-%m-%Y');
      RETURN v_resultado, v_mensaje;
   END IF


   DROP TABLE IF EXISTS tmp_cdb_saldo_especial;
   SELECT id_derechohabiente,
          subcuenta,
          fondo_inversion,
          v_fcorte v_fcorte,
          SUM(monto_acciones) monto_acciones,
          SUM(monto_pesos) monto_pesos
   FROM cta_movimiento
   WHERE f_liquida <= v_fcorte
   GROUP BY id_derechohabiente,
          subcuenta,
          fondo_inversion
   INTO TEMP tmp_cdb_saldo_especial;

   UPDATE STATISTICS FOR TABLE tmp_cdb_saldo_especial;

   LOCK TABLE cbd_saldo_safre IN EXCLUSIVE MODE;

   --Se genera el saldo para cada derechohabiente
   INSERT INTO cbd_saldo_safre
   SELECT ts.id_derechohabiente,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          (ts.monto_acciones * gf.precio_fondo),
          ts.v_fcorte
     FROM tmp_cdb_saldo_especial ts,
          glo_valor_fondo gf
   WHERE	 ts.fondo_inversion		= 11
      AND subcuenta              IN (4,8,55)
      AND gf.fondo               = ts.fondo_inversion
      AND gf.f_valuacion         = v_fcorte
   ;

   UNLOCK TABLE cbd_saldo_safre;
   
   create unique index xpkcbd_saldo_safre on cbd_saldo_safre(id_derechohabiente,subcuenta) using btree  in cbd_1ix_dbs;
   UPDATE STATISTICS FOR TABLE cbd_saldo_safre;

   --Se guarda el saldo global
   DELETE FROM cbd_saldo_global_safre WHERE f_saldo = v_fcorte;
   INSERT INTO cbd_saldo_global_safre
   SELECT
      f_saldo,
      subcuenta,
      fondo_inversion,
      SUM(monto_acciones),
      SUM(monto_pesos)
   FROM cbd_saldo_safre
   GROUP BY f_saldo,subcuenta,fondo_inversion;

   DROP TABLE IF EXISTS tmp_cdb_saldo_especial;
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;

END FUNCTION;


