






CREATE FUNCTION "safreviv".fn_credito_43bis(p_id_derechohabiente DECIMAL(9,0))

   RETURNING SMALLINT, DECIMAL(9,0), CHAR(1), DATE, DATE, SMALLINT, CHAR(18), DECIMAL(9,0);
         --- resultado, id_derecho,  tpo_cred, f_cred, f_liq, ent fin, num_crt, id_form

   DEFINE v_resultado       SMALLINT;
   DEFINE v_f_otorga        DATE;
   DEFINE v_f_liquida       DATE;
   DEFINE v_estado          SMALLINT;
   DEFINE v_tpo_credito     CHAR(1);
   DEFINE v_id_ocg_formal   DECIMAL(9,0);
   DEFINE v_cve_ef          SMALLINT;
   DEFINE v_num_ctr_int     CHAR(18);
   DEFINE v_id_dh           DECIMAL(9,0);
   DEFINE v_nss             CHAR(11);

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado, p_id_derechohabiente, v_tpo_credito, v_f_otorga, v_f_liquida, v_cve_ef, v_num_ctr_int, v_id_ocg_formal;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_int/archivos/cred43bis.trace';
   --SET DEBUG FILE TO '/safreviv_req/SAC43BIS/PruebasUnitarias/cred43bis.trace';
   --TRACE ON;

   LET v_tpo_credito     = "";
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";
   LET v_estado          = 0;
   LET v_resultado       = 0; --  SIN CREDITO
   LET v_cve_ef          = "";
   LET v_num_ctr_int     = "";
   LET v_id_ocg_formal   = "";

   IF p_id_derechohabiente IS NULL THEN
      LET v_resultado = 0; --  SIN CREDITO

      RETURN v_resultado, p_id_derechohabiente, v_tpo_credito, v_f_otorga, v_f_liquida, v_cve_ef, v_num_ctr_int, v_id_ocg_formal;
   END IF

   FOREACH
      SELECT FIRST 1 acred.f_formalizacion,
             acred.f_liquida_credito,
             acred.situacion,
             forma.tpo_credito,
             forma.cve_ent_financiera,
             forma.num_ctr_int_ef,
             forma.id_ocg_formalizacion
        INTO v_f_otorga,
             v_f_liquida,
             v_estado,
             v_tpo_credito,
             v_cve_ef,
             v_num_ctr_int,
             v_id_ocg_formal
        FROM ocg_formalizacion forma, ocg_acreditado acred
       WHERE forma.id_derechohabiente   = p_id_derechohabiente
         AND forma.id_ocg_formalizacion = acred.id_ocg_formalizacion
         AND forma.situacion IN(55,60,70,80,140,150,160)
       ORDER BY acred.f_formalizacion DESC
   END FOREACH;

   IF v_estado = 0 OR v_estado IS NULL THEN
      LET v_resultado     = 0; --  SIN CREDITO
      LET v_f_otorga      = "";
      LET v_f_liquida     = "";
      LET v_cve_ef        = "";
      LET v_num_ctr_int   = "";
      LET v_id_ocg_formal = "";
   END IF

   IF v_estado >= 55 AND v_estado <= 80 THEN
      LET v_resultado = 1; -- CRÉDITO VIGENTE
      LET v_f_liquida = "";
   ELSE
      IF v_estado >= 140 AND v_estado <= 160 THEN
         LET v_resultado = 2; -- CRÉDITO LQUIDADO

         IF v_f_liquida IS NULL THEN
            LET v_f_liquida = TODAY;
         END IF
      ELSE
         LET v_resultado    = 0; --  SIN CREDITO
         LET v_f_otorga     = "";
         LET v_f_liquida    = "";
        LET v_cve_ef        = "";
        LET v_num_ctr_int   = "";
        LET v_id_ocg_formal = "";
      END IF
   END IF

   RETURN v_resultado, p_id_derechohabiente, v_tpo_credito, v_f_otorga, v_f_liquida, v_cve_ef, v_num_ctr_int, v_id_ocg_formal;

END FUNCTION;


