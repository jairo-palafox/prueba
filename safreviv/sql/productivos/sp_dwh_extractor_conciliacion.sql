






CREATE PROCEDURE "safreviv".sp_dwh_extractor_conciliacion()
RETURNING INTEGER, INTEGER, CHAR(200);

   DEFINE v_tabla                     CHAR(30);
   DEFINE v_query                     CHAR(350);

   DEFINE v_nss                       CHAR(11);
   DEFINE v_num_credito               CHAR(10);
   DEFINE v_f_pago                    DATE;
   DEFINE v_importe_amort             DECIMAL(16,6);
   DEFINE v_monto_aivs                DECIMAL(16,6);
   DEFINE v_folio_liquida             DECIMAL(9,0) ;
   DEFINE v_folio_retiro              DECIMAL(9,0) ;
   DEFINE v_folio_ajuste              DECIMAL(9,0) ;
   DEFINE v_fecha_cargo               DATE         ;
   DEFINE v_movimiento                SMALLINT     ;
   DEFINE v_subcuenta                 SMALLINT     ;

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

   CREATE TEMP TABLE tmp_mov_concilia(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));

   LET v_query = "INSERT INTO tmp_mov_concilia " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (672,721,1681,1712) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_query = "INSERT INTO tmp_mov_concilia " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (672,721,1681,1712) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_concilia ON tmp_mov_concilia (id_derechohabiente);

   INSERT INTO safre_sdo@vivws_tcp:saci_conciliacion
        SELECT c.nss,
               b.monto_pesos,
               b.monto_acciones,
               YEAR(f_liquida),
               MONTH(f_liquida),
               DAY(f_liquida),
               "",
               "",
               "",
               "",
               "",
               b.movimiento,
               b.subcuenta
          FROM tmp_mov_concilia b,
               afi_derechohabiente c
         WHERE b.subcuenta IN ( 4 , 44)
           AND c.id_derechohabiente = b.id_derechohabiente;

   INSERT INTO safre_sdo@vivws_tcp:saci_conciliacion
        SELECT c.nss,
               "",
               "",
               "",
               "",
               "",
               b.monto_pesos,
               b.monto_acciones,
               YEAR(f_liquida),
               MONTH(f_liquida),
               DAY(f_liquida),
               b.movimiento,
               b.subcuenta
          FROM tmp_mov_concilia b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND c.id_derechohabiente = b.id_derechohabiente;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_conciliacion",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_conciliacion ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;

   RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE
;


