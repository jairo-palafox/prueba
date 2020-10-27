






CREATE PROCEDURE "safreviv".sp_afi_his_modifica_ws()

   DROP TABLE IF EXISTS afi_his_modifica_ws;

   CREATE TABLE afi_his_modifica_ws
   (
   id_derechohabiente   decimal(9,0),
   f_modifica           datetime year to second,
   ind_modifica         smallint,
   curp                 char(18),
   rfc                  char(13),
   nombre_af            char(40),
   ap_paterno_af        char(40),
   ap_materno_af        char(40)
   ) IN afi_1_dbs;

   CREATE index xaafi_his_modifica_ws on afi_his_modifica_ws(id_derechohabiente) IN afi_1_dbs;

END PROCEDURE;


