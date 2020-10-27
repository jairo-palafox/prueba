






CREATE PROCEDURE "safreviv".sp_dwh_valor_fondo()
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

   INSERT INTO safre_sdo@vivws_tcp:saci_valor_fondo
        SELECT f_valuacion,
               fondo,
               precio_fondo,
               f_operacion
          FROM glo_valor_fondo;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_valor_fondo",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_valor_fondo ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;
     
   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE


;


