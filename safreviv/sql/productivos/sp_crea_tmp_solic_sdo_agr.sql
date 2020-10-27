






CREATE PROCEDURE "safreviv".sp_crea_tmp_solic_sdo_agr()
   DROP TABLE IF EXISTS tmp_agr_solic_sdo;

   CREATE TABLE tmp_agr_solic_sdo
   (
      nss                CHAR(11),
      id_derechohabiente DECIMAL(9,0),
      modulo_cod         CHAR(2),
      f_proceso          DATE,
      id_referencia      DECIMAL(9,0),
      aivs97             DECIMAL(12,2),
      aivs92             DECIMAL(12,2)
   );

   DROP TABLE IF EXISTS tmp_agr_solic_sdo_ua;

   CREATE TABLE tmp_agr_solic_sdo_ua
   (
      nss                CHAR(11),
      id_derechohabiente DECIMAL(9,0),
      modulo_cod         CHAR(2),
      f_proceso          DATE,
      id_referencia      DECIMAL(9,0),
      aivs97             DECIMAL(12,2),
      aivs92             DECIMAL(12,2)
   );

END PROCEDURE;


