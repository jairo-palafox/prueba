






CREATE FUNCTION "safreviv".fn_concilia_cuentas()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado            SMALLINT;
   DEFINE v_mensaje              VARCHAR(100);

   SET PDQPRIORITY HIGH;

   --Se inicializan las variables de salida
   LET v_resultado = 0;
   LET v_mensaje = "La diferencia de saldos por cuenta se calculo correctamente";

   DROP TABLE IF EXISTS cbd_diferencia_saldo;
   CREATE TABLE cbd_diferencia_saldo
   (
      id_derechohabiente    decimal(9,0)  ,
      nss                   char(11)  ,
      subcuenta             smallint  ,
      acciones_safre        decimal(22,6)  ,
      acciones_bdnsviv      decimal(22,6)  ,
      diferencia            decimal(16,6)  
   )fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
   extent size 128000 next size 16000 lock mode row;

   LOCK TABLE cbd_diferencia_saldo IN EXCLUSIVE MODE;
   
   --Se saca la diferencia de saldo para cada cuenta
   INSERT INTO cbd_diferencia_saldo
   SELECT
      sdo_bd.id_derechohabiente,
      sdo_bd.nss,
      sdo_sfr.subcuenta,
      sdo_sfr.monto_acciones,
      sdo_bd.monto_acciones,
      (sdo_sfr.monto_acciones - sdo_bd.monto_acciones)
   FROM cbd_saldo_bdnsviv sdo_bd
   INNER JOIN cbd_saldo_sin_adelanto sdo_sfr ON (sdo_sfr.id_derechohabiente = sdo_bd.id_derechohabiente
                               AND sdo_sfr.subcuenta = sdo_bd.subcuenta);

   UNLOCK TABLE cbd_diferencia_saldo;
   
   create index xi1_cbd_diferencia_saldo on cbd_diferencia_saldo(id_derechohabiente) using btree  in cbd_2ix_dbs;
   create index xi2_cbd_diferencia_saldo on cbd_diferencia_saldo(nss) using btree  in cbd_2ix_dbs;
   UPDATE STATISTICS FOR TABLE cbd_diferencia_saldo;

   
   SET PDQPRIORITY DEFAULT;
   RETURN v_resultado, v_mensaje;

END FUNCTION;


