






CREATE FUNCTION "safreviv".fn_cbd_saldo_sin_adelanto()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "El saldo sin adelantos fue generado correctamente";

   DROP TABLE IF EXISTS cbd_saldo_sin_adelanto;
   CREATE TABLE cbd_saldo_sin_adelanto
   (
      id_derechohabiente decimal(9,0) NOT NULL ,
      subcuenta smallint NOT NULL ,
      fondo_inversion smallint NOT NULL ,
      monto_acciones decimal(22,6),
      f_saldo date
   )fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
   extent size 64000 next size 16000 lock mode row;

   --Se calcula el saldo de viv97 sumandole el saldo en la subcuenta de EFRISS

   --Primero se calcula la suma para el saldo con adelantos
   SELECT 
      sdo.id_derechohabiente,
      4 subcuenta,
      sdo.fondo_inversion,
      SUM(sdo.monto_acciones) monto_acciones,
      sdo.f_saldo
   FROM cbd_saldo_safre sdo
   WHERE sdo.subcuenta IN (4,55)
   GROUP BY sdo.id_derechohabiente,
   sdo.fondo_inversion,
   sdo.f_saldo 
   INTO TEMP viv97_tmp;

   --Despues se calcula la suma para los movimientos adelantados
   SELECT 
      ade.id_derechohabiente,
      4 subcuenta,
      ade.fondo_inversion,
      SUM(ade.monto_acciones) monto_acciones
   FROM cbd_cuenta_adelanto ade
   WHERE ade.subcuenta IN (4,55)
   GROUP BY ade.id_derechohabiente,
   ade.fondo_inversion
   INTO TEMP adelanto_viv97_tmp;

   UPDATE STATISTICS FOR TABLE viv97_tmp;
   UPDATE STATISTICS FOR TABLE adelanto_viv97_tmp;

   LOCK TABLE cbd_saldo_sin_adelanto IN EXCLUSIVE MODE;
   
   --Se calcula el saldo sin adelantos usando las tablas temporales 
   INSERT INTO cbd_saldo_sin_adelanto
   SELECT
   sdo.id_derechohabiente,
   sdo.subcuenta,
   sdo.fondo_inversion,
   (sdo.monto_acciones - NVL(ade.monto_acciones,0)),
   sdo.f_saldo
   FROM viv97_tmp sdo
   LEFT JOIN adelanto_viv97_tmp ade ON (
                  ade.id_derechohabiente = sdo.id_derechohabiente 
                  AND ade.subcuenta = sdo.subcuenta
                  AND ade.fondo_inversion = sdo.fondo_inversion
                        );
   
   DROP TABLE IF EXISTS viv97_tmp;
   DROP TABLE IF EXISTS adelanto_viv97_tmp;
   
   --Se genera el saldo sin adelanto para viv92
   INSERT INTO cbd_saldo_sin_adelanto
   SELECT
   sdo.id_derechohabiente,
   sdo.subcuenta,
   sdo.fondo_inversion,
   (sdo.monto_acciones - NVL(ade.monto_acciones,0)),
   sdo.f_saldo
   FROM cbd_saldo_safre sdo
   LEFT JOIN cbd_cuenta_adelanto ade ON (
                  ade.id_derechohabiente = sdo.id_derechohabiente 
                  AND ade.subcuenta = sdo.subcuenta
                  AND ade.fondo_inversion = sdo.fondo_inversion
                        )
   WHERE sdo.subcuenta = 8;

   UNLOCK TABLE cbd_saldo_sin_adelanto;

   create unique index xpkcbd_saldo_sin_adelanto on cbd_saldo_sin_adelanto(id_derechohabiente,subcuenta) using btree  in cbd_1ix_dbs;
   create index cbd_saldo_sin_adelanto on cbd_saldo_sin_adelanto(subcuenta) using btree  in cbd_1ix_dbs;
   UPDATE STATISTICS FOR TABLE cbd_saldo_sin_adelanto;

   --Se llena la tabla con el resumen de saldos a conciliar
   INSERT INTO cbd_saldo_conciliacion
   SELECT
   f_saldo,
   subcuenta,
   SUM(monto_acciones),
   1
   FROM cbd_saldo_sin_adelanto sdo
   GROUP BY 1,2;

   DROP TABLE IF EXISTS tmp_cdb_saldo_especial;
   
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;

END FUNCTION;


