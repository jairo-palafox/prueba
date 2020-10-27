






CREATE FUNCTION "safreviv".fn_edo_cred_viv(p_id_derechohabiente DECIMAL(9,0), p_valida SMALLINT)

   RETURNING SMALLINT, SMALLINT, SMALLINT, DECIMAL(10,0), DATE, DATE, SMALLINT;

   DEFINE v_resultado       SMALLINT;
   DEFINE v_tpo_originacion SMALLINT;
   DEFINE v_tpo_credito     SMALLINT;
   DEFINE v_num_credito     DECIMAL(10,0);
   DEFINE v_f_otorga        DATE;
   DEFINE v_f_liquida       DATE;
   DEFINE v_edo_procesar    SMALLINT;
   DEFINE v_id_dh           DECIMAL(9,0);
   DEFINE v_nss             CHAR(11);
   DEFINE v_entidad         SMALLINT;
   DEFINE v_bnd             SMALLINT;
   DEFINE v_tpo_dscto       SMALLINT;
   DEFINE v3_tpo_originacion SMALLINT;
   DEFINE v3_tpo_credito     SMALLINT;
   DEFINE v3_num_credito     DECIMAL(10,0);
   DEFINE v3_f_otorga        DATE;
   DEFINE v3_edo_procesar    SMALLINT;
   DEFINE v3_tpo_dscto       SMALLINT;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/edocredviv.trace';
   ---TRACE ON;

   LET v_tpo_originacion = NULL;
   LET v_tpo_credito     = NULL;
   LET v_num_credito     = NULL;
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";
   LET v_edo_procesar    = NULL;
   LET v_id_dh           = "";
   LET v_nss             = NULL;
   LET v_bnd             = 0;
   LET v_entidad         = 0;
   LET v_tpo_dscto       = 0;   

   --LET v_resultado = 0; -- se asume que no ocurrirá ningun error
   LET v_resultado = 1; --  SIN CREDITO

   FOREACH
         SELECT FIRST 1 cre.tpo_originacion,
                cre.tpo_credito            ,
                cre.num_credito            ,
                cre.f_otorga               ,
                cre.tpo_dscto --nuevo agregado
           INTO v_tpo_originacion,
                v_tpo_credito    ,
                v_num_credito    ,
                v_f_otorga       ,
                v_tpo_dscto --nuevo agregado
           FROM cre_acreditado cre,
                cat_maq_credito cmq,
          OUTER cta_credito cta,
                cat_tipo_credito ccr
          WHERE cre.id_derechohabiente = p_id_derechohabiente
            AND cre.estado             = cmq.estado
            AND cmq.entidad            = 1
            AND cre.id_derechohabiente = cta.id_derechohabiente --
            AND cre.tpo_credito        = cta.tpo_credito --
            AND cre.num_credito        = cta.num_credito --
            AND cta.tpo_credito        = ccr.tpo_credito
            AND ccr.f_actualiza       <= cta.f_credito
          ORDER BY ccr.f_actualiza DESC

      LET v_resultado = 0; -- CREDITO VIGENTE
   END FOREACH;

   IF v_resultado = 1 THEN
      FOREACH
         SELECT cre.tpo_originacion,
                cre.tpo_credito    ,
                cre.num_credito    ,
                cre.f_otorga       ,
                cre.edo_procesar   ,
                cre.tpo_dscto --nuevo agregado
           INTO v_tpo_originacion,
                v_tpo_credito    ,
                v_num_credito    ,
                v_f_otorga       ,
                v_edo_procesar   ,
                v_tpo_dscto --nuevo agregado
           FROM cre_acreditado cre,
                cat_maq_credito edo
          WHERE cre.id_derechohabiente = p_id_derechohabiente
            AND cre.estado = edo.estado
            AND edo.entidad = 1
          ORDER BY cre.f_otorga DESC, cre.estado

          IF v_tpo_originacion = 2 THEN
             IF (v_edo_procesar = 60) OR
                (v_edo_procesar >= 120) THEN

                LET v_resultado = 0; -- CREDITO VIGENTE

                EXIT FOREACH;
             ELSE
                CONTINUE FOREACH;
             END IF
          ELSE
             LET v_resultado = 0; -- CREDITO VIGENTE

             EXIT FOREACH;
          END IF
      END FOREACH;
   END IF

   IF v_resultado = 1 AND p_valida = 1 THEN
      EXECUTE FUNCTION fn_cred_liq_viv (p_id_derechohabiente, p_valida)
      INTO v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;   END IF

   IF v_resultado IN(1,2) THEN
      FOREACH
         SELECT cre.tpo_originacion,
                cre.tpo_credito    ,
                cre.num_credito    ,
                cre.f_otorga       ,
                cre.edo_procesar   ,
                cre.tpo_dscto  --nuevo
           INTO v3_tpo_originacion,
                v3_tpo_credito    ,
                v3_num_credito    ,
                v3_f_otorga       ,
                v3_edo_procesar   ,
                v3_tpo_dscto --nuevo
           FROM cre_acreditado cre,
                cat_maq_credito edo
          WHERE cre.id_derechohabiente = p_id_derechohabiente
            AND cre.estado = edo.estado
            AND edo.entidad = 3
          ORDER BY cre.f_otorga DESC, cre.estado

         LET v_entidad = 3;
      END FOREACH;

      IF v_entidad = 3 THEN
         IF v_resultado = 1 THEN
            LET v_bnd = 1;
         ELSE
            IF v3_f_otorga > v_f_liquida THEN
               LET v_bnd = 1;
            END IF
         END IF
      END IF

      IF v_bnd = 1 THEN
         LET v_tpo_originacion = v3_tpo_originacion;
         LET v_tpo_credito     = v3_tpo_credito;
         LET v_num_credito     = v3_num_credito;
         LET v_f_otorga        = v3_f_otorga;
         LET v_f_liquida       = "";
         LET v_resultado       = 3; -- CREDITO EN TRÁMITE
         LET v_tpo_dscto       = v3_tpo_dscto;
     END IF
   END IF

   RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;

END FUNCTION;


