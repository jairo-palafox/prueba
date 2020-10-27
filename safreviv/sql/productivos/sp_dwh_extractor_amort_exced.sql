






CREATE PROCEDURE "safreviv".sp_dwh_extractor_amort_exced()
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

   CREATE TEMP TABLE tmp_mov_amort(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));

   LET v_query = "INSERT INTO tmp_mov_amort " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"||
        " WHERE subcuenta IN (4,8,46) "||
          " AND movimiento IN (501,511,1402,1422) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_query = "INSERT INTO tmp_mov_amort " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,46) "||
          " AND movimiento IN (501,511,1402,1422) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_amort ON tmp_mov_amort (id_derechohabiente);

   CREATE TEMP TABLE tmp_det_amort (
       id_derechohabiente DECIMAL(10,0),
       nss CHAR(11),
       num_credito CHAR(10),
       anio_pago SMALLINT,
       mes_pago SMALLINT,
       dia_pago SMALLINT,
       importe_amort DECIMAL(16,6),
       aivs DECIMAL(16,6),
       anio_retiro SMALLINT,
       mes_retiro SMALLINT,
       dia_retiro SMALLINT,
       movimiento SMALLINT,
       subcuenta SMALLINT,
       folio_liquida DECIMAL(10,0),
       id_referencia  DECIMAL(9,0) );

   INSERT INTO tmp_det_amort
        SELECT c.id_derechohabiente,
               c.nss,
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
               b.subcuenta,
               b.folio_liquida,
               b.id_referencia
          FROM tmp_mov_amort b,
               afi_derechohabiente c
         WHERE c.id_derechohabiente = b.id_derechohabiente;

   UPDATE tmp_det_amort
      SET ( num_credito, anio_pago, mes_pago, dia_pago)  = ( (SELECT dae_det_solicitud.num_credito,
                                                                  YEAR(dae_det_solicitud.fecha_pago),    
                                                                  MONTH(dae_det_solicitud.fecha_pago),
                                                                  DAY(dae_det_solicitud.fecha_pago)
                                                             FROM dae_det_solicitud
                                                            WHERE dae_det_solicitud.folio_liquida = tmp_det_amort.folio_liquida
                                                              AND dae_det_solicitud.id_derechohabiente = tmp_det_amort.id_derechohabiente
                                                              AND dae_det_solicitud.id_dae_referencia = tmp_det_amort.id_referencia ) );

   INSERT INTO safre_sdo@vivws_tcp:saci_amort_excedente
        SELECT nss           ,
               num_credito   ,
               anio_pago     ,
               mes_pago      ,
               dia_pago      ,
               importe_amort ,
               aivs          ,
               anio_retiro   ,
               mes_retiro    ,
               dia_retiro    ,
               movimiento    ,
               subcuenta
          FROM tmp_det_amort;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_amort_excedente",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_amort_excedente ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;

   RETURN r_sql_err, r_isam_err, r_err_txt;
END PROCEDURE


;


