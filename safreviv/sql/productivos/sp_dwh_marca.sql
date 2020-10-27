






CREATE PROCEDURE "safreviv".sp_dwh_marca()
RETURNING INTEGER, INTEGER, CHAR(200);

   DEFINE r_sql_err        INTEGER;
   DEFINE r_isam_err       INTEGER;
   DEFINE r_err_txt        CHAR(200);

   ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt
      RETURN r_sql_err, r_isam_err, r_err_txt;
   END EXCEPTION

   LET r_sql_err  = 0;
   LET r_isam_err = 0;
   LET r_err_txt  = "";

   SET PDQPRIORITY HIGH;

   INSERT INTO safre_sdo@vivws_tcp:saci_marca
        SELECT b.nss,
               a.marca,
               c.descripcion_marca,
               1,
               a.f_inicio,
               "",
               0
          FROM sfr_marca_activa a,
               afi_derechohabiente b,
               sfr_marca c
         WHERE b.id_derechohabiente = a.id_derechohabiente
           AND c.marca = a.marca;

   INSERT INTO safre_sdo@vivws_tcp:saci_marca
        SELECT b.nss,
               a.marca,
               c.descripcion_marca,
               2,
               a.f_inicio,
               a.f_fin,
               a.estado_marca
          FROM sfr_marca_historica a,
               afi_derechohabiente b,
               sfr_marca c
         WHERE b.id_derechohabiente = a.id_derechohabiente
           AND c.marca = a.marca;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_marca",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_marca ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;

   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE
;


