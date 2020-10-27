






CREATE PROCEDURE "safreviv".sp_crea_tabla_homologacion()

   --set debug file to "crea_tabla_homologacion";
   --trace on;

   DROP TABLE IF EXISTS tmp_reg_homologa;

   CREATE TABLE tmp_reg_homologa
      (nss                 CHAR(11),
       tpo_credito         SMALLINT,
       num_credito         DECIMAL(10,0),
       edo_credito         SMALLINT,
       tpo_credito1        SMALLINT,
       num_credito1        DECIMAL(10,0),
       edo_credito1        SMALLINT,
       edo_homologa        SMALLINT,
       id_cre_acreditado   DECIMAL(9,0))
IN tmp_3_dbs;

   RETURN ;

END PROCEDURE;


