






CREATE PROCEDURE "safreviv".sp_dwh_extractor_uso_garantia()
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

   CREATE TEMP TABLE tmp_mov_gar(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));
   
   LET v_query = "INSERT INTO tmp_mov_gar " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"|| 
        " WHERE subcuenta IN (4,8) "||
          " AND movimiento IN (82,152,162,492,1222,1252,2002) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento
        
      LET v_query = "INSERT INTO tmp_mov_gar " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (62,242,432,542,552,1962,1972,1982,1992,2012) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_gar ON tmp_mov_gar (folio_liquida, id_derechohabiente);

   INSERT INTO safre_sdo@vivws_tcp:saci_uso_garantia
        SELECT c.nss,
               a.num_credito,
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
          FROM cre_uso_garantia a,
               tmp_mov_gar b,
               afi_derechohabiente c
         WHERE b.subcuenta = 4
           AND a.folio_liquida = b.folio_liquida
           AND a.id_derechohabiente = b.id_derechohabiente
           AND a.id_cre_uso_garantia = b.id_referencia
           AND c.id_derechohabiente = b.id_derechohabiente;

   INSERT INTO safre_sdo@vivws_tcp:saci_uso_garantia
        SELECT c.nss,
               a.num_credito,
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
          FROM cre_uso_garantia a,
               tmp_mov_gar b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND a.folio_liquida = b.folio_liquida
           AND a.id_derechohabiente = b.id_derechohabiente
           AND a.id_cre_uso_garantia = b.id_referencia
           AND c.id_derechohabiente = b.id_derechohabiente;
           
   INSERT INTO safre_sdo@vivws_tcp:saci_uso_garantia
        SELECT c.nss,
               a.num_credito,
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
          FROM cre_uso_garantia a,
               tmp_mov_gar b,
               afi_derechohabiente c
         WHERE b.subcuenta = 44
           AND a.folio_liquida = b.folio_liquida
           AND a.id_derechohabiente = b.id_derechohabiente
           AND a.id_cre_uso_garantia = b.id_referencia
           AND c.id_derechohabiente = b.id_derechohabiente;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_uso_garantia",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_uso_garantia ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;

   RETURN r_sql_err, r_isam_err, r_err_txt;
END PROCEDURE


;


