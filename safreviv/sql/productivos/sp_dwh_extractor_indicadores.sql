






CREATE PROCEDURE "safreviv".sp_dwh_extractor_indicadores(p_f_corte   DATE)
RETURNING INTEGER, INTEGER, CHAR(200);

   DEFINE v_nss                       CHAR(11)  ;
   DEFINE v_tpo_trabajador            CHAR(1)   ;
   DEFINE v_tpo_trabajador_riss       SMALLINT  ;
   DEFINE v_anio_riss                 CHAR(4)   ;
   DEFINE v_mes_riss                  CHAR(2)   ;
   DEFINE v_dia_riss                  CHAR(2)   ;
   DEFINE v_ind_notifica              CHAR(1)   ;
   DEFINE v_ind_notifica_bloque       CHAR(1)   ;
   DEFINE v_anio_registro             CHAR(4)   ;
   DEFINE v_mes_registro              CHAR(2)   ;
   DEFINE v_dia_registro              CHAR(2)   ;
   DEFINE v_mercado_laboral           CHAR(2)   ;
   DEFINE v_ind_credito               CHAR(1)   ;
   DEFINE v_sexo                      CHAR(1)   ;
   DEFINE v_anio_nacimiento           CHAR(4)   ;
   DEFINE v_mes_nacimiento            CHAR(2)   ;
   DEFINE v_dia_nacimiento            CHAR(2)   ;
   DEFINE v_ent_nacimiento            CHAR(2)   ;
   DEFINE v_afore                     SMALLINT  ;
   DEFINE v_marca_portabilidad        CHAR(1)   ;

   DEFINE v_id_derechohabiente        DECIMAL(10,0);
   DEFINE v_id_riss                   INTEGER;
   DEFINE v_f_riss                    DATE;
   DEFINE v_tpo_notificacion          SMALLINT;
   DEFINE v_not_sms                   SMALLINT;
   DEFINE v_not_correo                SMALLINT;
   DEFINE v_bloq_sms                  SMALLINT;
   DEFINE v_bloq_correo               SMALLINT;

   DEFINE v_f_ini_bim1                DATE;
   DEFINE v_f_fin_bim1                DATE;
   DEFINE v_f_ini_bim2                DATE;
   DEFINE v_f_fin_bim2                DATE;
   DEFINE v_f_paso                    DATE;
   DEFINE v_f_mes                     DATE;

   DEFINE v_tabla                     CHAR(30);
   DEFINE v_query_1                   CHAR(350);
   DEFINE v_query_2                   CHAR(350);
   DEFINE v_query_3                   CHAR(80);
   DEFINE v_bnd_historia              SMALLINT;

   DEFINE v_total_registros           INTEGER;

   DEFINE r_sql_err        INTEGER;
   DEFINE r_isam_err       INTEGER;
   DEFINE r_err_txt        CHAR(200);

   ON EXCEPTION SET r_sql_err, r_isam_err, r_err_txt
      RETURN r_sql_err, r_isam_err, r_err_txt;
   END EXCEPTION

   LET r_sql_err  = 0;
   LET r_isam_err = 0;
   LET r_err_txt  = "";
   LET v_bnd_historia = 0;
--   LET p_f_corte = TODAY;

   CREATE TEMP TABLE tmp_indicadores (
      id_derechohabiente   DECIMAL(10,0),
      nss                  CHAR(11),
      tpo_trabajador       CHAR(1),
      tpo_trabajador_riss  SMALLINT,
      anio_riss            CHAR(4),
      mes_riss             CHAR(2),
      dia_riss             CHAR(2),
      ind_notifica         CHAR(1),
      ind_notifica_bloque  CHAR(1),
      f_registro           DATE   ,
      mercado_laboral      CHAR(2),
      ind_credito          CHAR(1),
      sexo                 CHAR(1),
      anio_nacimiento      CHAR(4),
      mes_nacimiento       CHAR(2),
      dia_nacimiento       CHAR(2),
      ent_nacimiento       CHAR(2),
      afore                SMALLINT,
      marca_portabilidad   CHAR(1),
      ind_fallecimiento    SMALLINT,
      rfc CHAR(13),
      curp CHAR(18),
      nombre VARCHAR(40),
      paterno VARCHAR(40),
      materno VARCHAR(40),
      nombre_procanase VARCHAR(50)
)fragment by round robin in tmpdbs1, tmpdbs2, tmpdbs3, tmpdbs4;

   INSERT INTO tmp_indicadores (id_derechohabiente,
                                nss            ,
                                tpo_trabajador ,
                                f_registro     ,
                                mercado_laboral,
                                sexo           ,
                                anio_nacimiento,
                                mes_nacimiento ,
                                dia_nacimiento ,
                                ent_nacimiento ,
                                ind_credito    ,
                                marca_portabilidad,
                                afore,
                                rfc    ,
                                curp   ,
                                nombre ,
                                paterno,
                                materno,
                                nombre_procanase)
   SELECT afi.id_derechohabiente,
         afi.nss                ,
         afi.tipo_trabajador    ,
         afi.f_apertura_inf     ,
         "IN"                   ,
         CASE afi.sexo
            WHEN "1" THEN "H"
            WHEN "2" THEN "M"
            ELSE "I"
         END                    ,
         TO_CHAR(afi.f_nacimiento, "%Y") ,
         TO_CHAR(afi.f_nacimiento, "%m"),
         TO_CHAR(afi.f_nacimiento, "%d")  ,
         afi.curp[12,13]        ,
         "N"                    ,
         "S"                    ,
         afo.afore_cod          ,
         afi.rfc,afi.curp,afi.nombre_af,afi.ap_paterno_af,afi.ap_materno_af,afi.nombre_imss
    FROM afi_derechohabiente afi,
         OUTER afi_afore afo
   WHERE afo.id_derechohabiente = afi.id_derechohabiente
     AND f_apertura_inf         <= p_f_corte;


   CREATE INDEX xpk_tmp_ind_dwh ON tmp_indicadores (id_derechohabiente);

--   UPDATE tmp_indicadores
--      SET afore = (SELECT afore_cod FROM afi_afore WHERE afi_afore.id_derechohabiente = tmp_indicadores.id_derechohabiente );

   FOREACH SELECT UNIQUE id_derechohabiente
             INTO v_id_derechohabiente
             FROM afi_riss

      FOREACH SELECT riss.id_riss,
                     riss.f_proceso
                INTO v_id_riss,
                     v_f_riss
                FROM afi_riss riss
               WHERE riss.id_derechohabiente = v_id_derechohabiente
               ORDER BY id_riss, f_proceso DESC

         IF v_id_riss = 1 OR
            v_id_riss = 2 OR
            v_id_riss = 4 THEN
            EXIT FOREACH;
         END IF
      END FOREACH

      UPDATE tmp_indicadores
         SET tpo_trabajador_riss = v_id_riss ,
             anio_riss           = TO_CHAR(v_f_riss, "%Y"),
             mes_riss            = TO_CHAR(v_f_riss, "%m"),
             dia_riss            = TO_CHAR(v_f_riss, "%d")
       WHERE id_derechohabiente = v_id_derechohabiente;
   END FOREACH;

   LET v_not_sms     = NULL;
   LET v_not_correo  = NULL;
   LET v_bloq_sms    = NULL;
   LET v_bloq_correo = NULL;

   FOREACH SELECT UNIQUE id_derechohabiente
             INTO v_id_derechohabiente
             FROM afi_ind_notifica

      FOREACH SELECT tpo_notificacion
                INTO v_tpo_notificacion
                FROM afi_ind_notifica
               WHERE id_derechohabiente = v_id_derechohabiente
               ORDER BY tpo_notificacion
         IF v_tpo_notificacion = 1 THEN
            LET v_not_sms = 1;
         END IF
         IF v_tpo_notificacion = 2 THEN
            LET v_not_correo = 1;
         END IF
         IF v_tpo_notificacion = 3 THEN
            LET v_bloq_correo = 1;
         END IF
         IF v_tpo_notificacion = 4 THEN
            LET v_bloq_sms = 1;
         END IF
      END FOREACH

      IF v_not_sms = 1 OR v_not_correo = 1 THEN
         IF v_not_sms = 1 THEN
            IF v_not_correo = 1 THEN
               LET v_ind_notifica = "A";
            ELSE
               LET v_ind_notifica = "S";
            END IF
         ELSE
            LET v_ind_notifica = "E";
         END IF
      ELSE
         LET v_ind_notifica = "";
      END IF

      IF v_bloq_sms = 1 OR v_bloq_correo = 1 THEN
         IF v_bloq_sms = 1 THEN
            IF v_bloq_correo = 1 THEN
               LET v_ind_notifica_bloque = "A";
            ELSE
               LET v_ind_notifica_bloque = "S";
            END IF
         ELSE
            LET v_ind_notifica_bloque = "E";
         END IF
      ELSE
         LET v_ind_notifica_bloque = "";
      END IF

      UPDATE tmp_indicadores
         SET ind_notifica = v_ind_notifica,
             ind_notifica_bloque = v_ind_notifica_bloque
       WHERE id_derechohabiente = v_id_derechohabiente ;

      LET v_ind_notifica = NULL;
      LET v_ind_notifica_bloque = NULL;
      LET v_not_sms     = NULL;
      LET v_not_correo  = NULL;
      LET v_bloq_sms    = NULL;
      LET v_bloq_correo = NULL;
   END FOREACH

   UPDATE tmp_indicadores
      SET ind_credito = 'C'
    WHERE id_derechohabiente IN (SELECT UNIQUE cta.id_derechohabiente
                                          FROM cta_credito cta,
                                               cre_acreditado cre
                                         WHERE cre.id_derechohabiente = cta.id_derechohabiente
                                           AND cre.tpo_credito        = cta.tpo_credito
                                           AND cre.num_credito        = cta.num_credito);

   UPDATE tmp_indicadores
      SET marca_portabilidad = "C"
    WHERE id_derechohabiente IN (SELECT id_derechohabiente
                                   FROM sfr_marca_activa
                                  WHERE marca = 705 );

   UPDATE tmp_indicadores
      SET marca_portabilidad = "R"
    WHERE id_derechohabiente IN (SELECT id_derechohabiente
                                   FROM sfr_marca_activa
                                  WHERE marca = 704 );

   LET v_mercado_laboral = '';

   LET v_f_paso     = p_f_corte - 3 UNITS MONTH;
   LET v_f_ini_bim1 = MDY (MONTH(v_f_paso),1,YEAR(v_f_paso));

   LET v_f_mes      = p_f_corte - 1 UNITS MONTH;
   LET v_f_paso     = MDY (MONTH(v_f_mes),1,YEAR(v_f_mes));
   LET v_f_fin_bim1 = v_f_paso -1 UNITS DAY;

   LET v_f_paso     = p_f_corte - 1 UNITS MONTH;
   LET v_f_ini_bim2 = MDY (MONTH(v_f_paso),1,YEAR(v_f_paso));

   LET v_f_fin_bim2 = p_f_corte;

   --ID_DERECHOHABIENTES CON MOVIMIENTOS EN BIMESTRE ANTERIOR
   LET v_query_1 =  "SELECT id_derechohabiente "||
                     " FROM cta_movimiento "||
                    " WHERE f_liquida BETWEEN '"||v_f_ini_bim1||"' AND '"||v_f_fin_bim1||"' "||
                      " AND subcuenta IN (4,8,44,46) "||
                      " AND movimiento IN (SELECT movimiento FROM cat_movimiento WHERE categoria = 1) ";
   LET v_query_3 =  " INTO TEMP tmp_dh_mov_bim1 ";

   SELECT c.tabla
     INTO v_tabla
     FROM cat_tab_movimiento c
    WHERE v_f_ini_bim1 BETWEEN c.f_inicial AND c.f_final;

   IF DBINFO('sqlca.sqlerrd2') == 1 THEN
      LET v_query_2 =  " UNION "||
                       "SELECT id_derechohabiente "||
                        " FROM "|| TRIM(v_tabla)||
                       " WHERE f_liquida BETWEEN '"||v_f_ini_bim1||"' AND '"||v_f_fin_bim1||"' "||
                         " AND subcuenta IN (4,8,44,46) "||
                         " AND movimiento IN (SELECT movimiento FROM cat_movimiento WHERE categoria = 1) ";
      LET v_bnd_historia = 1;
   END IF

   DROP TABLE IF EXISTS tmp_dh_mov_bim1;
   SET PDQPRIORITY HIGH;
   IF v_bnd_historia = 1 THEN
      EXECUTE IMMEDIATE v_query_1 || v_query_2 || v_query_3;
   ELSE
      EXECUTE IMMEDIATE v_query_1 || v_query_3;
   END IF

   LET v_bnd_historia = 0;
   --ID_DERECHOHABIENTES CON MOVIMIENTOS EN BIMESTRE ACTUAL
   LET v_query_1 =  "SELECT UNIQUE id_derechohabiente "||
                     " FROM cta_movimiento "||
                    " WHERE f_liquida BETWEEN '"||v_f_ini_bim2||"' AND '"||v_f_fin_bim2||"' "||
                      " AND subcuenta IN (4,8,44,46) "||
                      " AND movimiento IN (SELECT movimiento FROM cat_movimiento WHERE categoria = 1) ";
   LET v_query_3 =  " INTO TEMP tmp_dh_mov_bim2 ";

   SELECT c.tabla
     INTO v_tabla
     FROM cat_tab_movimiento c
    WHERE v_f_ini_bim2 BETWEEN f_inicial AND f_final;

   IF DBINFO('sqlca.sqlerrd2') == 1 THEN
      LET v_query_2 =  " UNION "||
                       "SELECT UNIQUE id_derechohabiente "||
                        " FROM "|| TRIM(v_tabla)||
                       " WHERE f_liquida BETWEEN '"||v_f_ini_bim2||"' AND '"||v_f_fin_bim2||"' "||
                         " AND subcuenta IN (4,8,44,46) "||
                         " AND movimiento IN (SELECT movimiento FROM cat_movimiento WHERE categoria = 1) ";
      LET v_bnd_historia = 1;
   END IF

   DROP TABLE IF EXISTS tmp_dh_mov_bim2;
   IF v_bnd_historia = 1 THEN
      EXECUTE IMMEDIATE v_query_1 || v_query_2 || v_query_3;
   ELSE
      EXECUTE IMMEDIATE v_query_1 || v_query_3;
   END IF

   CREATE INDEX tmp_dh_mov_bim1 ON tmp_dh_mov_bim1(id_derechohabiente);
   CREATE INDEX tmp_dh_mov_bim2 ON tmp_dh_mov_bim2(id_derechohabiente);

   --TRABAJADORES NUEVOS
   UPDATE tmp_indicadores
      SET mercado_laboral = "NU"
    WHERE f_registro BETWEEN v_f_ini_bim2 AND v_f_fin_bim2;

   --TRABAJADORES REINGRESO
   UPDATE tmp_indicadores
      SET mercado_laboral = "RE"
    WHERE id_derechohabiente IN (SELECT id_derechohabiente
                                   FROM tmp_dh_mov_bim2
                                  WHERE id_derechohabiente NOT IN (SELECT id_derechohabiente FROM tmp_dh_mov_bim1))
      AND mercado_laboral = "IN";

   --TRABAJADORES ACTIVOS
   UPDATE tmp_indicadores
      SET mercado_laboral = "AC"
    WHERE id_derechohabiente IN (SELECT id_derechohabiente
                                   FROM tmp_dh_mov_bim1)
      AND mercado_laboral = "IN";

 -- TRABAJADORES FALLECIDOS
    UPDATE tmp_indicadores
      SET ind_fallecimiento = 1
    WHERE id_derechohabiente  IN (SELECT id_derechohabiente
                                   FROM afi_fallecido);

   INSERT INTO safre_sdo@vivws_tcp:saci_indicador
        SELECT nss                 ,
               tpo_trabajador      ,
               tpo_trabajador_riss ,
               anio_riss           ,
               mes_riss            ,
               dia_riss            ,
               ind_notifica        ,
               ind_notifica_bloque ,
               TO_CHAR(f_registro, "%Y") ,
               TO_CHAR(f_registro, "%m") ,
               TO_CHAR(f_registro, "%d") ,
               mercado_laboral     ,
               ind_credito         ,
               sexo                ,
               anio_nacimiento     ,
               mes_nacimiento      ,
               dia_nacimiento      ,
               ent_nacimiento      ,
               afore               ,
               marca_portabilidad  ,
               ind_fallecimiento   ,
               rfc    ,
               curp   ,
               nombre ,
               paterno,
               materno,
               nombre_procanase
        FROM tmp_indicadores;

   SELECT count(*)
     INTO v_total_registros
     FROM safre_sdo@vivws_tcp:saci_indicador;

   INSERT INTO safre_sdo@vivws_tcp:glo_ctr_dwh VALUES (p_f_corte,
                                   "saci_indicador",
                                   TODAY,
                                   v_total_registros,
                                   1);

   RETURN r_sql_err, r_isam_err, r_err_txt;
END PROCEDURE;


