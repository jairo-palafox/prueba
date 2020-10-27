






CREATE PROCEDURE "safreviv".sp_afore_fisica()

   DROP TABLE IF EXISTS afi_afore;

   CREATE TABLE afi_afore
   (
      id_derechohabiente  DECIMAL(9,0),
      afore_cod           SMALLINT,
      usuario             CHAR(20),
      f_actualiza         DATE
   ) IN afi_3_dbs ;

   CREATE INDEX xid_dh_afore ON afi_afore(id_derechohabiente) IN afi_3_dbs ;

END PROCEDURE
;


