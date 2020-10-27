






CREATE PROCEDURE "safreviv".sp_dwh_extractor_separacion()
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

   CREATE TEMP TABLE tmp_mov_separa(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));

   LET v_query = "INSERT INTO tmp_mov_separa " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (381,382,383,384) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_query = "INSERT INTO tmp_mov_separa " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (381,382,383,384) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_separa ON tmp_mov_separa (folio_liquida, id_derechohabiente);

        SELECT c.nss nss,
               "          " num_credito,
               b.monto_pesos pesos97,
               b.monto_acciones acciones97,
               YEAR(f_liquida) anio97, 
               MONTH(f_liquida) mes97,
               DAY(f_liquida) dia97,
               "                 " pesos92,
               "                 " acciones92,
               "    " anio92,
               "    " mes92,
               "    " dia92,
               b.movimiento mov,
               b.subcuenta subcta,
               a.id_his_preliquida_op28 id_op28,
               a.id_expediente id_exp,
               c.id_derechohabiente id_dh
          FROM sep_his_preliquida_op28 a,
               tmp_mov_separa b,
               afi_derechohabiente c
         WHERE b.subcuenta = 4
           AND a.folio = b.folio_liquida
           AND a.id_his_preliquida_op28 = b.id_referencia
           AND c.id_derechohabiente     = b.id_derechohabiente
         INTO TEMP tmp_paso_separa;

   INSERT INTO tmp_paso_separa
        SELECT c.nss,
               "",
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
               b.subcuenta,
               a.id_his_preliquida_op28 id_op28,
               a.id_expediente id_exp,
               c.id_derechohabiente id_dh
          FROM sep_his_preliquida_op28 a,
               tmp_mov_separa b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND a.folio = b.folio_liquida
           AND a.id_his_preliquida_op28 = b.id_referencia
           AND c.id_derechohabiente     = b.id_derechohabiente;

   INSERT INTO tmp_paso_separa
        SELECT c.nss,
               "",
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
               b.subcuenta,
               a.id_his_preliquida_op28 id_op28,
               a.id_expediente id_exp,
               c.id_derechohabiente id_dh
          FROM sep_his_preliquida_op28 a,
               tmp_mov_separa b,
               afi_derechohabiente c
         WHERE b.subcuenta = 44
           AND a.folio = b.folio_liquida
           AND a.id_his_preliquida_op28 = b.id_referencia
           AND c.id_derechohabiente     = b.id_derechohabiente;

   UPDATE tmp_paso_separa
      SET tmp_paso_separa.num_credito = ( (SELECT sep_nss_expediente.num_credito 
                                             FROM sep_nss_expediente 
                                            WHERE sep_nss_expediente.id_expediente = tmp_paso_separa.id_exp
                                              AND sep_nss_expediente.id_derechohabiente = tmp_paso_separa.id_dh) )
    WHERE id_exp > 0;

   INSERT INTO safre_sdo@vivws_tcp:saci_separacion
        SELECT nss,
               num_credito,
               pesos97,
               acciones97,
               anio97, 
               mes97,
               dia97,
               pesos92,
               acciones92,
               anio92,
               mes92,
               dia92,
               mov,
               subcta
          FROM tmp_paso_separa;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_separacion",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_separacion ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;
END PROCEDURE



;


