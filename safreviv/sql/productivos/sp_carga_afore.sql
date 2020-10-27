






CREATE PROCEDURE "safreviv".sp_carga_afore(p_usuario CHAR(20))
   DEFINE cod_error SMALLINT ;

   INSERT INTO afi_afore
        SELECT a.id_derechohabiente,
               t.afore,
               p_usuario,
               TODAY
          FROM afi_derechohabiente a,
               safre_tmp:tmp_afi_afore t
         WHERE t.nss = a.nss;

   LET cod_error = 11;

   INSERT INTO safre_tmp:tmp_afi_afore_nss
        SELECT nss, afore,cod_error
          FROM safre_tmp:tmp_afi_afore       
         WHERE nss NOT IN(
               SELECT nss
                 FROM afi_derechohabiente); 

   UPDATE STATISTICS FOR TABLE afi_afore;
   UPDATE STATISTICS FOR TABLE safre_tmp:tmp_afi_afore_nss;

END PROCEDURE;


