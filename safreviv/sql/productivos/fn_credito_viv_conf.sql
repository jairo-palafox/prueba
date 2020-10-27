






CREATE FUNCTION "safreviv".fn_credito_viv_conf(p_id_derechohabiente DECIMAL(9,0), p_valida SMALLINT)
   RETURNING SMALLINT, SMALLINT, SMALLINT, DECIMAL(10,0), DATE, DATE, SMALLINT;

--Última modificación 06062017
   DEFINE v_resultado          SMALLINT;
   DEFINE v_tpo_originacion    SMALLINT;
   DEFINE v_tpo_credito        SMALLINT;
   DEFINE v_num_credito        DECIMAL(10,0);
   DEFINE v_f_otorga           DATE;
   DEFINE v_f_liquida          DATE;
   DEFINE v_edo_procesar       SMALLINT;
   DEFINE v_id_dh              DECIMAL(9,0);
   DEFINE v_nss                CHAR(11);
   DEFINE v_confirma           SMALLINT;
   DEFINE v_entidad            SMALLINT;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_confirma;
   END EXCEPTION

   --SET DEBUG FILE TO 'credviv.trace';
   --TRACE ON;

   LET v_tpo_originacion = NULL;
   LET v_tpo_credito     = NULL;
   LET v_num_credito     = NULL;
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";
   LET v_edo_procesar    = NULL;
   LET v_id_dh           = "";
   LET v_nss             = NULL;
   LET v_confirma        = NULL;

   --LET v_resultado = 0; -- se asume que no ocurrirá ningun error
   LET v_resultado = 1; --  SIN CREDITO

   FOREACH
         SELECT FIRST 1 ccr.tpo_originacion,
                cta.tpo_credito            ,
                cta.num_credito            ,
                cta.f_credito
           INTO v_tpo_originacion,
                v_tpo_credito    ,
                v_num_credito    ,
                v_f_otorga
           FROM cta_credito cta,
          OUTER cat_tipo_credito ccr
          WHERE cta.id_derechohabiente = p_id_derechohabiente
            AND cta.tpo_credito = ccr.tpo_credito
            AND ccr.f_actualiza <= cta.f_credito
          ORDER BY f_actualiza DESC

      LET v_resultado = 0; -- CREDITO VIGENTE

      IF v_tpo_originacion = 2 THEN
         LET v_confirma = 1;
      END IF

   END FOREACH;

   IF v_resultado = 1 THEN
      FOREACH
         SELECT cre.tpo_originacion,
                cre.tpo_credito    ,
                cre.num_credito    ,
                cre.f_otorga       ,
                cre.edo_procesar   ,
                edo.entidad
           INTO v_tpo_originacion,
                v_tpo_credito    ,
                v_num_credito    ,
                v_f_otorga       ,
                v_edo_procesar   ,
                v_entidad
           FROM cre_acreditado cre,
                cat_maq_credito edo
          WHERE cre.id_derechohabiente = p_id_derechohabiente
            AND cre.estado = edo.estado
            AND edo.entidad IN(1, 3)
          ORDER BY cre.f_otorga DESC, cre.estado

          IF v_entidad = 1 THEN
             LET v_resultado = 0;  -- CREDITO VIGENTE
          ELSE
             LET v_resultado = 3;  -- CREDITO EN TRÁMITE
          END IF

          IF v_tpo_originacion = 2 THEN
             IF (v_edo_procesar = 60) OR
                (v_edo_procesar >= 120) THEN
                LET v_confirma = 1;
                EXIT FOREACH;
             ELSE
                CONTINUE FOREACH;
             END IF
          ELSE
             EXIT FOREACH;
          END IF
      END FOREACH;
   END IF

   IF v_resultado = 1 AND p_valida = 1 THEN
      EXECUTE FUNCTION fn_cred_liq_vivienda (p_id_derechohabiente, p_valida)
      INTO v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida;
   END IF

   RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_confirma;

END FUNCTION;


