GLOBALS
   DEFINE v_id_dh    DECIMAL(9,0)
END GLOBALS

FUNCTION fn_datos(v_nss)

   DEFINE v_nss   CHAR(11)

   SELECT id_derechohabiente
     INTO v_id_dh
     FROM afi_derechohabiente
    WHERE nss = v_nss

   RETURN v_id_dh

END FUNCTION


FUNCTION fn_incons_01(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol )  -- Cambia tipo de trabajador

   DEFINE v_nss      CHAR(11)
   DEFINE v_bnd      SMALLINT
   DEFINE v_cv_ef    smallint
   DEFINE v_tc       char(1)
   DEFINE v_nci      char(18)
   DEFINE v_bim      char(6)
   DEFINE v_imp_ap   decimal(9,0)
   DEFINE v_f_liq    date
   DEFINE v_imp_gtia decimal(9,0)
   DEFINE v_causa    smallint
   DEFINE v_f_dep    date
   DEFINE v_sol      SMALLINT
   DEFINE v_f_envio  DATE
   
   IF v_f_envio IS NULL THEN
      LET v_f_envio = TODAY
   END IF

   IF v_bnd = 1 THEN
      UPDATE afi_derechohabiente
         SET tipo_trabajador = "B"
       WHERE nss = v_nss
   ELSE
      UPDATE afi_derechohabiente
         SET tipo_trabajador = "I"
       WHERE nss = v_nss
   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION


FUNCTION fn_incons_02(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol) -- NSS INCORRECTO

   DEFINE v_nss      CHAR(11)
   DEFINE v_nss2     CHAR(12)
   DEFINE v_bnd      SMALLINT
   DEFINE v_cv_ef    smallint
   DEFINE v_tc       char(1)
   DEFINE v_nci      char(18)
   DEFINE v_bim      char(6)
   DEFINE v_imp_ap   decimal(9,0)
   DEFINE v_f_liq    date
   DEFINE v_imp_gtia decimal(9,0)
   DEFINE v_causa    smallint
   DEFINE v_f_dep    date
   DEFINE v_sol      SMALLINT
   DEFINE v_f_envio  DATE

   IF v_f_envio IS NULL THEN
      LET v_f_envio = TODAY
   END IF

   IF v_bnd = 1 THEN
      LET v_nss2 = "a",v_nss[1,11]
   ELSE
      LET v_nss2 = v_nss
   END IF

   RETURN v_nss2,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION


FUNCTION fn_incons_26(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol) -- entidad financiera diferente, función que ambienta entidad financiera

   DEFINE v_nss      CHAR(11)
   DEFINE v_bnd      SMALLINT
   DEFINE v_cv_ef    SMALLINT
   DEFINE v_tc       CHAR(1)
   DEFINE v_nci      CHAR(18)
   DEFINE v_bim      CHAR(6)
   DEFINE v_imp_ap   DECIMAL(9,0)
   DEFINE v_f_liq    DATE
   DEFINE v_imp_gtia DECIMAL(9,0)
   DEFINE v_causa    SMALLINT
   DEFINE v_f_dep    DATE
   DEFINE v_sol      SMALLINT
   DEFINE v_f_envio  DATE

   IF v_f_envio IS NULL THEN
      LET v_f_envio = TODAY
   END IF

   CALL fn_datos(v_nss) RETURNING v_id_dh

   IF v_bnd = 1 THEN

      LET v_cv_ef = 500

   ELSE
      SELECT cve_ent_financiera
        INTO v_cv_ef
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_dh
         AND diagnostico = 1
         AND situacion >= 60
         AND situacion < 140
   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION


FUNCTION fn_incons_28(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol)  -- pendiente 

   DEFINE v_nss          CHAR(11)
   DEFINE v_bnd          SMALLINT
   DEFINE v_cnt          SMALLINT
   DEFINE v_id_ocg_liq   DECIMAL(9,0)
   DEFINE v_cv_ef        SMALLINT
   DEFINE v_tc           CHAR(1)
   DEFINE v_nci          CHAR(18)
   DEFINE v_bim          CHAR(6)
   DEFINE v_imp_ap       DECIMAL(9,0)
   DEFINE v_f_liq        DATE
   DEFINE v_imp_gtia     DECIMAL(9,0)
   DEFINE v_causa        SMALLINT
   DEFINE v_f_dep        DATE
   DEFINE v_sol          SMALLINT
   DEFINE v_f_envio      DATE

   IF v_bnd = 1 THEN
      SELECT COUNT(*)
        INTO v_cnt
        FROM ocg_liquidacion_cofi
       WHERE id_derechohabiente = v_id_dh

      IF v_cnt >= 1 THEN
      ELSE

       --  INSERT INTO ocg_liquidacion_cofi
         --     VALUES (seq_ocg_liq_cofi.nextval)
                      
      END IF
   ELSE
   
   END IF
END FUNCTION


FUNCTION fn_incons_43(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol)  -- sin crédito en EF por cuenta inhabilitada

   DEFINE v_nss      CHAR(11)
   DEFINE v_bnd      SMALLINT
   DEFINE v_cv_ef    SMALLINT
   DEFINE v_tc       CHAR(1)
   DEFINE v_nci      CHAR(18)
   DEFINE v_bim      CHAR(6)
   DEFINE v_imp_ap   DECIMAL(9,0)
   DEFINE v_f_liq    DATE
   DEFINE v_imp_gtia DECIMAL(9,0)
   DEFINE v_causa    SMALLINT
   DEFINE v_f_dep    DATE
   DEFINE v_sol      SMALLINT
   DEFINE v_f_envio  DATE

   IF v_f_envio IS NULL THEN
      LET v_f_envio = TODAY
   END IF

   CALL fn_datos(v_nss) RETURNING v_id_dh

   IF v_bnd = 1 THEN

      UPDATE afi_derechohabiente
         SET ind_estado_cuenta = 5
       WHERE id_derechohabiente = v_id_dh

   ELSE
      UPDATE afi_derechohabiente
         SET ind_estado_cuenta = 0
       WHERE id_derechohabiente = v_id_dh

   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION

FUNCTION fn_incons_46(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol) -- fecha de vencimiento inválida

   DEFINE v_nss      CHAR(11)
   DEFINE v_bnd      SMALLINT
   DEFINE v_f_envio  DATE
   DEFINE v_cv_ef    SMALLINT
   DEFINE v_tc       CHAR(1)
   DEFINE v_nci      CHAR(18)
   DEFINE v_bim      CHAR(6)
   DEFINE v_imp_ap   DECIMAL(9,0)
   DEFINE v_f_liq    DATE
   DEFINE v_imp_gtia DECIMAL(9,0)
   DEFINE v_causa    SMALLINT
   DEFINE v_f_dep    DATE
   DEFINE v_sol      SMALLINT

   IF v_bnd = 1 THEN
      LET v_f_envio = TODAY + 2
   ELSE
      LET v_f_envio = TODAY
   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION

FUNCTION fn_incons_51(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol) -- fecha de liberación no válida

   DEFINE v_nss           CHAR(11)
   DEFINE v_bnd           SMALLINT
   DEFINE v_cv_ef         SMALLINT
   DEFINE v_tc            CHAR(1)
   DEFINE v_nci           CHAR(18)
   DEFINE v_bim           CHAR(6)
   DEFINE v_imp_ap        DECIMAL(9,0)
   DEFINE v_f_liq         DATE
   DEFINE v_imp_gtia      DECIMAL(9,0)
   DEFINE v_causa         SMALLINT
   DEFINE v_f_dep         DATE
   DEFINE v_sol           SMALLINT
   DEFINE v_f_envio       DATE

   IF v_f_envio IS NULL THEN
      LET v_f_envio = TODAY
   END IF

   IF v_f_liq IS NULL THEN
      LET v_f_liq = TODAY
   END IF

   IF v_bnd > 1 THEN
      LET v_f_liq = TODAY + 2
   ELSE
      IF v_f_liq <= TODAY THEN
         LET v_f_liq = v_f_liq
      ELSE
         LET v_f_liq = TODAY
      END IF
   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION

FUNCTION fn_incons_53(v_nss,
                      v_bnd,
                      v_cv_ef,
                      v_tc,
                      v_nci,
                      v_bim,
                      v_imp_ap,
                      v_f_liq,
                      v_imp_gtia,
                      v_causa,
                      v_f_dep,
                      v_sol)  -- garantia liberada con anterioridad
   DEFINE v_nss            CHAR(11)
   DEFINE v_bnd            SMALLINT
   DEFINE v_cnt            SMALLINT
   DEFINE v_cv_ef    smallint
   DEFINE v_tc       char(1)
   DEFINE v_nci      char(18)
   DEFINE v_bim      char(6)
   DEFINE v_imp_ap   decimal(9,0)
   DEFINE v_f_liq    date
   DEFINE v_imp_gtia decimal(9,0)
   DEFINE v_causa    smallint
   DEFINE v_f_dep    date
   DEFINE v_sol      SMALLINT
   DEFINE v_f_envio  DATE

   CALL fn_datos(v_nss) RETURNING v_id_dh

   IF v_bnd = 1 THEN
      LET v_f_liq    = NULL
      LET v_f_dep    = NULL
      
      SELECT count(*)
        INTO v_cnt
        FROM ocg_formalizacion
       WHERE id_derechohabiente = v_id_dh
         AND situacion BETWEEN 55 AND 80
         AND diagnostico = 1

      IF v_cnt >= 1 THEN
         UPDATE ocg_formalizacion
            SET situacion = 140
          WHERE v_id_derechohabiente = v_id_dh

          UPDATE ocg_tramite
             SET situacion = 140
           WHERE id_derechohabiente = v_id_dh

      ELSE
         SELECT count(*)
           INTO v_cnt
           FROM ocg_formalización
          WHERE id_derechohabiente = v_id_dh
            AND situacion >= 140

            IF v_cnt >= 1 THEN
               UPDATE ocg_formalizacion
                  SET situacion = 80
                WHERE id_derechohabiente = v_id_dh

                DELETE FROM ocg_liquidacion 
                   WHERE id_derechohabiente = v_id_dh
                     AND situacion >= 140
            END IF
            
      END IF
   ELSE
      IF v_f_liq IS NOT NULL THEN
         LET v_f_liq = v_f_liq
      ELSE
         LET v_f_liq    = TODAY -1
         LET v_f_dep    = TODAY - 1
      END IF
   END IF

   RETURN v_nss,v_cv_ef,v_tc,v_nci,v_bim,v_imp_ap,v_f_liq,v_imp_gtia,v_causa,v_f_dep,v_sol,v_f_envio

END FUNCTION