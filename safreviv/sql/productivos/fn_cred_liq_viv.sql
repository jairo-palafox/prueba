






CREATE FUNCTION "safreviv".fn_cred_liq_viv (p_id_derechohabiente DECIMAL(9,0), p_valida SMALLINT)

   RETURNING SMALLINT, SMALLINT, SMALLINT, DECIMAL(10,0), DATE, DATE, SMALLINT;
   DEFINE v_resultado         SMALLINT;
   DEFINE v_tpo_originacion   SMALLINT;
   DEFINE v_tpo_credito       SMALLINT;
   DEFINE v_num_credito       DECIMAL(10,0);
   DEFINE v_f_otorga          DATE;
   DEFINE v_f_liquida         DATE;
   DEFINE v_nss               CHAR(11);
   DEFINE v_id_cre_acreditado DECIMAL(9,0);
   DEFINE v_entidad           SMALLINT;
   DEFINE v_tpo_dscto         SMALLINT; -- NUEVO

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto;   END EXCEPTION

   ---SET DEBUG FILE TO 'credviv.trace';
   ---TRACE ON;

   LET v_tpo_originacion = NULL;
   LET v_tpo_credito     = NULL;
   LET v_num_credito     = NULL;
   LET v_f_otorga        = "";
   LET v_f_liquida       = "";
   LET v_nss             = NULL;
   LET v_tpo_dscto       = NULL; --NUEVO

   LET v_resultado = 1; -- No tiene crédito vigente

   FOREACH
         SELECT FIRST 1 ccr.tpo_originacion,
                cta.tpo_credito            ,
                cta.num_credito            ,
                cta.f_credito              ,
                cta.f_actualiza            ,
                cmq.entidad                ,
                cre.tpo_dscto  --nuevo
           INTO v_tpo_originacion,
                v_tpo_credito    ,
                v_num_credito    ,
                v_f_otorga       ,
                v_f_liquida      ,
                v_resultado      ,
                v_tpo_dscto --nuevo
           FROM cta_his_credito cta,
                cre_acreditado  cre,
                cat_maq_credito cmq,
          OUTER cat_tipo_credito ccr
          WHERE cta.id_derechohabiente = p_id_derechohabiente
            AND cta.tpo_credito        = ccr.tpo_credito
            AND cre.id_derechohabiente = cta.id_derechohabiente
            AND cre.tpo_credito        = cta.tpo_credito
            AND cre.tpo_originacion    = ccr.tpo_originacion
            AND cre.num_credito        = cta.num_credito
            AND cre.estado             = cmq.estado
          ORDER BY f_actualiza DESC

      --LET v_resultado = 2 ;
   END FOREACH;

   IF v_resultado = 1 THEN
      FOREACH
         SELECT cre.tpo_originacion  ,
                cre.tpo_credito      ,
                cre.num_credito      ,
                cre.f_otorga         ,
                cre.id_cre_acreditado,
                edo.entidad          ,
                cre.tpo_dscto  --nuevo
           INTO v_tpo_originacion,
                v_tpo_credito      ,
                v_num_credito      ,
                v_f_otorga         ,
                v_id_cre_acreditado,
                v_resultado        ,
                v_tpo_dscto  --nuevo
           FROM cre_acreditado cre,
                cat_maq_credito edo
          WHERE cre.id_derechohabiente = p_id_derechohabiente
            AND cre.estado = edo.estado
            AND edo.entidad IN(2,5)
          ORDER BY cre.f_otorga DESC

         SELECT MIN(his.f_proceso)
           INTO v_f_liquida
           FROM cre_his_acreditado his, cat_maq_credito ent
          WHERE his.id_cre_acreditado = v_id_cre_acreditado
            AND his.estado = ent.estado
            AND ent.entidad = 2;

         IF v_f_liquida IS NULL THEN
            SELECT MAX(f_fin)
              INTO v_f_liquida
              FROM sfr_marca_historica mrh,
                   cat_tipo_credito ctc
             WHERE mrh.id_derechohabiente = p_id_derechohabiente
               AND mrh.marca = ctc.marca_inf;
         END IF

         IF v_f_liquida IS NULL THEN
            LET v_f_liquida = "01/01/0001";
         END IF

         --LET v_resultado = 2 ;
      END FOREACH
   END IF

   RETURN v_resultado, v_tpo_originacion, v_tpo_credito, v_num_credito, v_f_otorga, v_f_liquida, v_tpo_dscto; -- nuevo

END FUNCTION;


