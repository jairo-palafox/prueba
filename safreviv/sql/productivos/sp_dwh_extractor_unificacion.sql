






CREATE PROCEDURE "safreviv".sp_dwh_extractor_unificacion()
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

   DEFINE v_id_uni                    DECIMAL(10,0);
   DEFINE v_id_unir                   DECIMAL(10,0);

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

   CREATE TEMP TABLE tmp_mov_uni(f_liquida DATE ,
                                 id_derechohabiente DECIMAL(9,0),
                                 subcuenta SMALLINT  ,
                                 fondo_inversion SMALLINT ,
                                 movimiento SMALLINT  ,
                                 folio_liquida DECIMAL(9,0)  ,
                                 id_referencia DECIMAL(9,0)  ,
                                 monto_acciones DECIMAL(16,6),
                                 monto_pesos DECIMAL(12,2));

   LET v_query = "INSERT INTO tmp_mov_uni " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM cta_movimiento"||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (151,161,171,392,402,412) ";
   EXECUTE IMMEDIATE v_query;

   FOREACH SELECT tabla
             INTO v_tabla
             FROM cat_tab_movimiento

      LET v_query = "INSERT INTO tmp_mov_uni " ||
      "SELECT f_liquida, id_derechohabiente, "||
            " subcuenta, fondo_inversion, "||
            " movimiento, folio_liquida, "||
            " id_referencia, monto_acciones, "||
            " monto_pesos "||
         " FROM "|| TRIM(v_tabla)||
        " WHERE subcuenta IN (4,8,44) "||
          " AND movimiento IN (151,161,171,392,402,412) ";
      EXECUTE IMMEDIATE v_query;
   END FOREACH

   CREATE INDEX xtmp_mov_uni ON tmp_mov_uni (id_referencia, id_derechohabiente);

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
               a.id_unificado id_uni,
               d.id_unificador id_unir
          FROM uni_det_unificado a,
               uni_det_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 4
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_unificador          = d.id_unificador
           AND a.id_unificado           = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
         INTO TEMP tmp_paso_uni;

   INSERT INTO tmp_paso_uni
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
               a.id_unificado,
               d.id_unificador
          FROM uni_det_unificado a,
               uni_det_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_unificador          = d.id_unificador
           AND a.id_unificado           = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
;
   INSERT INTO tmp_paso_uni
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
               a.id_unificado,
               d.id_unificador
          FROM uni_det_unificado a,
               uni_det_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 44
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_unificador          = d.id_unificador
           AND a.id_unificado           = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
;

   INSERT INTO tmp_paso_uni
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
               a.id_inf_unificado,
               d.id_inf_unificador
          FROM uni_inf_unificado a,
               uni_inf_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 4
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_inf_unificado       = d.id_inf_unificador
           AND a.id_inf_unificado       = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
;

   INSERT INTO tmp_paso_uni
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
               a.id_inf_unificado,
               d.id_inf_unificador
          FROM uni_inf_unificado a,
               uni_inf_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 8
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_inf_unificado       = d.id_inf_unificador
           AND a.id_inf_unificado       = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
;

   INSERT INTO tmp_paso_uni
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
               a.id_inf_unificado,
               d.id_inf_unificador
          FROM uni_inf_unificado a,
               uni_inf_unificador d,
               tmp_mov_uni b,
               afi_derechohabiente c
         WHERE b.subcuenta = 44
           AND a.folio_unificacion      = d.folio_unificacion
           AND a.id_inf_unificado       = d.id_inf_unificador
           AND a.id_inf_unificado       = b.id_referencia
           AND a.id_derechohabiente     = c.id_derechohabiente
           AND b.id_derechohabiente     = c.id_derechohabiente
;

   FOREACH cur_upd_uni WITH HOLD FOR SELECT id_uni, id_unir
                                       INTO v_id_uni, v_id_unir
                                       FROM tmp_paso_uni
                                      WHERE id_uni IN (SELECT uni_cre_unificado.id_unificado
                                                         FROM uni_cre_unificado  )

      SELECT FIRST 1 uni_cre_unificado.num_credito
        INTO v_num_credito
        FROM uni_cre_unificado, uni_cre_unificador
       WHERE uni_cre_unificado.id_unificado = v_id_uni
         AND uni_cre_unificado.id_unificado = uni_cre_unificador.id_unificador 
         AND uni_cre_unificador.id_unificador = v_id_unir;

      UPDATE tmp_paso_uni SET num_credito = v_num_credito WHERE CURRENT OF cur_upd_uni;
   END FOREACH

   INSERT INTO safre_sdo@vivws_tcp:saci_unificacion
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
          FROM tmp_paso_uni;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh
        SELECT TODAY,
               "saci_unificacion",
               TODAY,
               ( SELECT COUNT(*) FROM safre_sdo@vivws_tcp:saci_unificacion ),
               1
          FROM SYSTABLES
         WHERE tabid = 1;
END PROCEDURE


;


