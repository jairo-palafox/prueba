






CREATE FUNCTION "safreviv".fn_cbd_adelantos_globales(p_fcorte DATE)
RETURNING SMALLINT;

   SET PDQPRIORITY HIGH;

   --Se crean los indices en la tabla de movimientos
   DROP INDEX IF EXISTS ix1_cbd_movimiento_adelanto;
   CREATE INDEX ix1_cbd_movimiento_adelanto ON cbd_movimiento_adelanto (id_derechohabiente) USING btree  IN cbd_1ix_dbs;
   DROP INDEX IF EXISTS ix2_cbd_movimiento_adelanto;
   CREATE INDEX ix2_cbd_movimiento_adelanto ON cbd_movimiento_adelanto (nss) USING btree  IN cbd_1ix_dbs;
   DROP INDEX IF EXISTS ix3_cbd_movimiento_adelanto;
   CREATE INDEX ix3_cbd_movimiento_adelanto ON cbd_movimiento_adelanto (subcuenta,movimiento,modulo) USING btree  IN cbd_1ix_dbs;
   DROP INDEX IF EXISTS ix4_cbd_movimiento_adelanto;
   CREATE INDEX ix4_cbd_movimiento_adelanto ON cbd_movimiento_adelanto (fondo_inversion) USING btree  IN cbd_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_movimiento_adelanto;

   DROP TABLE IF EXISTS cbd_cuenta_adelanto;
   CREATE TABLE cbd_cuenta_adelanto
   (
      id_derechohabiente decimal(9,0) NOT NULL ,
      nss char(11) NOT NULL ,
      subcuenta smallint NOT NULL ,
      fondo_inversion smallint NOT NULL ,
      monto_acciones decimal(22,6),
      monto_pesos decimal(22,2)
   )fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
   extent size 64000 next size 16000 lock mode row;

   --Se genera una grupacion por modulo, subcuenta y fondo de inversion para los movimientos adelantados
   DELETE FROM cbd_modulo_adelanto WHERE f_saldo = p_fcorte;
   
   --Se calculan los adelantos por modulo para el fondo 11
   INSERT INTO cbd_modulo_adelanto
   SELECT
      p_fcorte,
      ade.modulo,
      ade.subcuenta,
      ade.fondo_inversion,
      SUM(ade.monto_acciones),
      SUM(ade.monto_acciones * gf.precio_fondo),
      ind_periodo
   FROM cbd_movimiento_adelanto ade
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = ade.fondo_inversion
                           AND gf.f_valuacion = p_fcorte)
   WHERE ade.fondo_inversion = 11
   GROUP BY 
      ade.modulo,
      ade.subcuenta,
      ade.fondo_inversion,
      ind_periodo;

   --Se calculan los adelantos por cuenta para el fondo 11
   LOCK TABLE cbd_cuenta_adelanto IN EXCLUSIVE MODE;
   
   INSERT INTO cbd_cuenta_adelanto
   SELECT
      ade.id_derechohabiente,
      ade.nss,
      ade.subcuenta,
      ade.fondo_inversion,
      SUM(ade.monto_acciones),
      SUM(ade.monto_acciones * gf.precio_fondo)
   FROM cbd_movimiento_adelanto ade
   INNER JOIN glo_valor_fondo gf ON (gf.fondo = ade.fondo_inversion
                              AND gf.f_valuacion = p_fcorte)
   WHERE ade.fondo_inversion = 11
   GROUP BY 
      ade.id_derechohabiente, 
      ade.nss, ade.subcuenta, 
      ade.fondo_inversion;

   UNLOCK TABLE cbd_cuenta_adelanto;

   CREATE INDEX ix1_cbd_cuenta_adelanto ON cbd_cuenta_adelanto (id_derechohabiente) USING btree  IN cbd_1ix_dbs;
   CREATE INDEX ix2_cbd_cuenta_adelanto ON cbd_cuenta_adelanto (nss) USING btree  IN cbd_1ix_dbs;
   CREATE INDEX ix3_cbd_cuenta_adelanto ON cbd_cuenta_adelanto (subcuenta) USING btree  IN cbd_1ix_dbs;
   UPDATE statistics FOR TABLE cbd_movimiento_adelanto;
   UPDATE statistics FOR TABLE cbd_modulo_adelanto;

   SET PDQPRIORITY DEFAULT;
   RETURN 0;

END FUNCTION;


