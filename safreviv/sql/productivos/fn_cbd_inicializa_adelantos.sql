






CREATE FUNCTION "safreviv".fn_cbd_inicializa_adelantos()
RETURNING SMALLINT;

   DROP TABLE IF EXISTS cbd_movimiento_adelanto;
   CREATE TABLE cbd_movimiento_adelanto
     (
         id_derechohabiente decimal(9,0) not null,
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
     ) fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
     lock mode row;

   DROP TABLE IF EXISTS cbd_adelanto_dis;
   CREATE TABLE cbd_adelanto_dis
     (
         id_derechohabiente decimal(9,0) not null,
         subcuenta smallint not null ,
         movimiento smallint not null ,
         id_referencia decimal(9),
         folio_sua decimal(6,0),
         periodo_pago char(6),
         proceso_pag smallint
     ) fragment by round robin in cbd_1_dbs,cbd_2_dbs,cbd_3_dbs,cbd_4_dbs
     lock mode row;

   DROP TABLE IF EXISTS cbd_control_adelanto;
   CREATE TABLE cbd_control_adelanto
      (
         modulo char(3),
         estado smallint not null
      ) in  cbd_1_dbs;

   RETURN 0;

END FUNCTION;


