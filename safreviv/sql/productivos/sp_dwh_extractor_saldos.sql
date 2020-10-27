






CREATE PROCEDURE "safreviv".sp_dwh_extractor_saldos(p_f_corte   DATE)
RETURNING INTEGER, INTEGER, CHAR(200);

   DEFINE v_nss                 CHAR(11);
   DEFINE v_monto_aivs_viv92    DECIMAL (20,2);
   DEFINE v_monto_pesos_viv92   DECIMAL (20,2);
   DEFINE v_monto_aivs_viv97    DECIMAL (20,2);
   DEFINE v_monto_pesos_viv97   DECIMAL (20,2);
   DEFINE v_monto_aivs_ifv97    DECIMAL (20,2);
   DEFINE v_monto_pesos_ifv97   DECIMAL (20,2);
   DEFINE v_monto_aivs_amort    DECIMAL (20,2);
   DEFINE v_monto_pesos_amort   DECIMAL (20,2);

   DEFINE v_subcuenta           SMALLINT;
   DEFINE v_fondo               SMALLINT;
   DEFINE v_acciones            DECIMAL (20,2);
   DEFINE v_pesos               DECIMAL (20,2);

   DEFINE v_total_registros     INTEGER;

   DEFINE r_sql_err        INTEGER;
   DEFINE r_isam_err       INTEGER;
   DEFINE r_err_txt        CHAR(200);

   ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt
      RETURN r_sql_err, r_isam_err, r_err_txt;
   END EXCEPTION

   LET r_sql_err  = 0;
   LET r_isam_err = 0;
   LET r_err_txt  = "";

   CREATE TEMP TABLE tmp_indicadores (
      id_derechohabiente   DECIMAL(10,0),
      nss                  CHAR(11), 
      monto_aivs_viv92    DECIMAL (20,2),
      monto_pesos_viv92   DECIMAL (20,2),
      monto_aivs_viv97    DECIMAL (20,2),
      monto_pesos_viv97   DECIMAL (20,2),
      monto_aivs_ifv97    DECIMAL (20,2),
      monto_pesos_ifv97   DECIMAL (20,2),
      monto_aivs_amort    DECIMAL (20,2),
      monto_pesos_amort   DECIMAL (20,2) );

   SET PDQPRIORITY HIGH;

   DROP TABLE IF EXISTS tmp_saldos;
   DROP TABLE IF EXISTS tmp_saldo_corte;

   SELECT mov.id_derechohabiente,
          mov.subcuenta,
          mov.fondo_inversion,
          p_f_corte f_corte,
          SUM(mov.monto_acciones) monto_acciones,
          SUM(mov.monto_pesos) monto_pesos
     FROM cta_movimiento mov
    WHERE mov.f_liquida <= p_f_corte
    GROUP BY mov.id_derechohabiente,
             mov.subcuenta,
             mov.fondo_inversion
    INTO TEMP tmp_saldos;
    UPDATE STATISTICS FOR TABLE tmp_saldos;

   SELECT ts.id_derechohabiente,
          afi.nss,
          ts.subcuenta,
          ts.fondo_inversion,
          ts.monto_acciones,
          (ts.monto_acciones * gf.precio_fondo) monto_pesos,
           ts.f_corte
     FROM tmp_saldos ts,
          afi_derechohabiente afi,
          glo_valor_fondo gf
    WHERE ts.fondo_inversion		 <> 0
      AND afi.id_derechohabiente = ts.id_derechohabiente
      AND gf.fondo              = ts.fondo_inversion
      AND gf.f_valuacion        = p_f_corte
     INTO TEMP tmp_saldo_corte;

   DROP TABLE IF EXISTS tmp_saldos;
   CREATE INDEX xpk_tmp_ind_dwh ON tmp_saldo_corte (nss);
   SET PDQPRIORITY DEFAULT;

   FOREACH SELECT UNIQUE nss
             INTO v_nss
             FROM safre_sdo@vivws_tcp:saci_indicador

      LET v_monto_aivs_viv92   = 0;
      LET v_monto_pesos_viv92  = 0;
      LET v_monto_aivs_viv97   = 0;
      LET v_monto_pesos_viv97  = 0;
      LET v_monto_aivs_ifv97   = 0;
      LET v_monto_pesos_ifv97  = 0;
      LET v_monto_aivs_amort   = 0;
      LET v_monto_pesos_amort  = 0;

      FOREACH SELECT
            sdo.subcuenta,
            sdo.fondo_inversion,
            sdo.monto_acciones,
            sdo.monto_pesos
         INTO
            v_subcuenta,
            v_fondo,
            v_acciones,
            v_pesos
         FROM tmp_saldo_corte sdo
         WHERE sdo.nss = v_nss
         AND sdo.subcuenta IN (4,8,44,46)

         IF(v_subcuenta = 4 AND v_fondo = 11) THEN
            LET v_monto_pesos_viv97 = v_pesos;
            LET v_monto_aivs_viv97  = v_acciones;
         ELIF(v_subcuenta = 8 AND v_fondo = 11) THEN
            LET v_monto_pesos_viv92 = v_pesos;
            LET v_monto_aivs_viv92  = v_acciones;
         ELIF(v_subcuenta = 44 AND v_fondo = 11) THEN
            LET v_monto_pesos_ifv97 = v_pesos;
            LET v_monto_aivs_ifv97  = v_acciones;
         ELIF(v_subcuenta = 46 AND v_fondo = 11) THEN
            LET v_monto_pesos_amort = v_pesos;
            LET v_monto_aivs_amort  = v_acciones;
         END IF
      END FOREACH
      INSERT INTO safre_sdo@vivws_tcp:saci_saldo VALUES (v_nss              ,
                                                         v_monto_aivs_viv92 ,
                                                         v_monto_pesos_viv92,
                                                         v_monto_aivs_viv97 ,
                                                         v_monto_pesos_viv97,
                                                         v_monto_aivs_ifv97 ,
                                                         v_monto_pesos_ifv97,
                                                         v_monto_aivs_amort ,
                                                         v_monto_pesos_amort,
                                                         p_f_corte );

   END FOREACH
   SELECT count(*)
     INTO v_total_registros
     FROM safre_sdo@vivws_tcp:saci_saldo;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh VALUES (p_f_corte,
                                   "saci_saldo",
                                   TODAY,
                                   v_total_registros,
                                   1);

END PROCEDURE;


