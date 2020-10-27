






CREATE PROCEDURE "safreviv".sp_dwh_consulta_roja()
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

   INSERT INTO safre_sdo@vivws_tcp:saci_consulta_roja
        SELECT a.nss,
               c.estado_rojo_desc,
               b.categoria_desc,
               a.f_actualiza
          FROM afi_nss_rojo a,
               cat_categoria_roja b,
               cat_estado_rojo c
         WHERE b.categoria = a.categoria
           AND c.estado_rojo = a.estado_rojo;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_consulta_roja",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_consulta_roja ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;

   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE
;


