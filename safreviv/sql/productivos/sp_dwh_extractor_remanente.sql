






CREATE PROCEDURE "safreviv".sp_dwh_extractor_remanente()
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

   CREATE TEMP TABLE tmp_mov_remanente(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));

   LET v_query = "INSERT INTO tmp_mov_remanente " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (82,162,572,582) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_query = "INSERT INTO tmp_mov_remanente " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (82,162,572,582) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_remanente ON tmp_mov_remanente (id_derechohabiente);

   SELECT b.id_derechohabiente id_dh,
          c.nss nss,
          b.monto_pesos pesos97,
          b.monto_acciones acciones97,
          b.f_liquida f_liquida97,
          "                 " pesos92,
          "                 " acciones92,
          "          " f_liquida92,
          b.movimiento,
          b.subcuenta,
          b.folio_liquida,
          b.id_referencia,
          "             " num_credito
     FROM tmp_mov_remanente b,
          afi_derechohabiente c
    WHERE b.subcuenta IN (4,44)
      AND c.id_derechohabiente = b.id_derechohabiente
     INTO TEMP tmp_paso_remanente;
      
   INSERT INTO tmp_paso_remanente
        SELECT b.id_derechohabiente,
               c.nss,
               "",
               "",
               "",
               b.monto_pesos,
               b.monto_acciones,
               b.f_liquida,
               b.movimiento,
               b.subcuenta,
               b.folio_liquida,
               b.id_referencia,
               ""
          FROM tmp_mov_remanente b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND c.id_derechohabiente = b.id_derechohabiente;

   CREATE INDEX xtmp_paso_remanente ON tmp_paso_remanente (folio_liquida,id_dh);

   UPDATE tmp_paso_remanente
      SET num_credito = ( (SELECT cre_acreditado.num_credito 
                             FROM cre_acreditado 
                            WHERE cre_acreditado.folio_liquida = tmp_paso_remanente.folio_liquida
                              AND cre_acreditado.id_derechohabiente = tmp_paso_remanente.id_dh
                              AND cre_acreditado.id_cre_acreditado  = tmp_paso_remanente.id_referencia) ) ;

   INSERT INTO safre_sdo@vivws_tcp:saci_remanente
        SELECT nss,
               num_credito,
               pesos97,
               acciones97,
               YEAR(f_liquida97),
               MONTH(f_liquida97),
               DAY(f_liquida97),
               pesos92,
               acciones92,
               YEAR(f_liquida92),
               MONTH(f_liquida92),
               DAY(f_liquida92),
               movimiento,
               subcuenta
          FROM tmp_paso_remanente;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_remanente",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_remanente ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;
  RETURN r_sql_err, r_isam_err, r_err_txt;

END PROCEDURE


;


