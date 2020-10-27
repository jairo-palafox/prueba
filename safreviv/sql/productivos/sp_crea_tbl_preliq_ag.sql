






CREATE PROCEDURE "safreviv".sp_crea_tbl_preliq_ag()

   DROP TABLE IF EXISTS cre_ag_preliquida;

   CREATE TABLE cre_ag_preliquida
   (f_liquida          DATE NOT NULL,
    id_derechohabiente DECIMAL(9,0) ,
    subcuenta          SMALLINT NOT NULL,
    fondo_inversion    SMALLINT NOT NULL,
    movimiento         SMALLINT NOT NULL,
    folio_liquida      DECIMAL(9,0) ,
    id_referencia      DECIMAL(9,0) ,
    monto_acciones     DECIMAL(22,2),
    monto_pesos        DECIMAL(22,2),
    f_valor            DATE,
    f_registro         DATE,
    h_registro         DATETIME HOUR TO SECOND,
    origen             CHAR(20)
   ) IN cre_dbs;

END PROCEDURE;


