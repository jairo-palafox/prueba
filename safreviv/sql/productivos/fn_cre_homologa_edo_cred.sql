






CREATE FUNCTION "safreviv".fn_cre_homologa_edo_cred(p_tpo         SMALLINT,
                                         p_usuario     CHAR(20),
                                         p_proceso_cod SMALLINT,
                                         p_folio       DECIMAL(9,0),
                                         p_id_cre_ctr_arh DECIMAL(9,0))
   RETURNING SMALLINT, CHAR(11), INTEGER

   DEFINE v_error                   SMALLINT;  -- en caso de error contiene el código
   DEFINE v_total_edo               INTEGER;

   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_nss                     CHAR(11); 
   DEFINE v_num_credito             DECIMAL(10,0);
   DEFINE v_tpo_credito             SMALLINT;
   DEFINE v_edo_credito             SMALLINT;
   DEFINE v_num_credito1            DECIMAL(10,0);
   DEFINE v_tpo_credito1            SMALLINT;
   DEFINE v_edo_credito1            SMALLINT;
   DEFINE v_marca_orig              SMALLINT;
   DEFINE v_marca                   SMALLINT;
   DEFINE v_tpo_orig                SMALLINT;
   DEFINE v_tpo_nvo                 SMALLINT;
   DEFINE v_f_otorga                DATE;
   DEFINE v_edo_orig                SMALLINT;
   DEFINE v_edo_nvo                 SMALLINT;

   DEFINE v_id_derechohabiente      DECIMAL(9,0);
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_codigo_rechazo          SMALLINT;
   DEFINE v_fecha_causa             DATE;
   DEFINE r_edo_retorno             SMALLINT;
   DEFINE v_fecha_act               DATE;
   DEFINE v_nvo_edo_liq             SMALLINT;

   DEFINE v_edo_procesar            SMALLINT;
   DEFINE v_diag                    CHAR(3);

   ON EXCEPTION SET v_error
      -- Devolverá el codigo de error cuando ocurra una excepción diferente a -239
      LET v_total_edo = 0;
      RETURN v_error, v_nss, v_total_edo;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/creHomologaEdo.trace';
   --TRACE ON;

   -- se establece la prioridad
   SET PDQPRIORITY HIGH;

   LET v_error          = 0;
   LET v_total_edo      = 0;
   LET v_marca_orig     = 0;
   LET v_marca          = 0;
   LET v_tpo_orig       = 0;
   LET v_tpo_nvo        = 0;
   LET v_estado_marca   = 0;
   LET v_marca_causa    = 0;
   LET v_codigo_rechazo = 0;
   LET v_edo_orig       = 0;
   LET v_edo_nvo        = 0;
   LET v_fecha_causa    = "";
   LET v_fecha_act      = TODAY;
   LET v_nss            = "0";
   LET v_diag           = "000";

   FOREACH
      SELECT t.nss,
             t.id_cre_acreditado,
             t.tpo_credito,
             t.num_credito,
             t.edo_credito,
             t.tpo_credito1,
             t.num_credito1,
             t.edo_credito1
        INTO v_nss,
             v_id_cre_acreditado,
             v_tpo_credito,
             v_num_credito,
             v_edo_credito,
             v_tpo_credito1,
             v_num_credito1,
             v_edo_credito1
        FROM safre_tmp:tmp_reg_homologa t
       WHERE t.edo_homologa = p_tpo

      FOREACH
         SELECT c.tpo_originacion,
                c.id_derechohabiente,
                c.f_otorga,
                m.entidad,
                DECODE(d.id_deudor,1,140,0,20),
                c.edo_procesar
           INTO v_tpo_orig,
                v_id_derechohabiente,
                v_f_otorga,
                v_edo_orig,
                v_edo_nvo,
                v_edo_procesar
           FROM cre_acreditado c,
                cat_maq_credito m,
                cat_tipo_credito d
          WHERE c.id_cre_acreditado = v_id_cre_acreditado
            AND c.estado            = m.estado
            AND c.tpo_originacion   = d.tpo_originacion
            AND c.tpo_credito       = d.tpo_credito
      END FOREACH;

      IF v_edo_credito = 1 AND v_edo_orig = 2 THEN
         ---desmarcar marca original
         FOREACH
            SELECT s.marca
              INTO v_marca
              FROM sfr_marca_historica s,
                   cat_tipo_credito c1
             WHERE s.id_derechohabiente = v_id_derechohabiente
               AND s.marca              = c1.marca_inf
               AND s.n_referencia       = v_id_cre_acreditado
               AND c1.tpo_credito       = v_tpo_credito
               AND c1.tpo_originacion   = v_tpo_orig

            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                v_marca,
                                                v_id_cre_acreditado,
                                                v_estado_marca,
                                                v_marca_causa,
                                                p_usuario,
                                                p_proceso_cod)
                                           INTO r_edo_retorno;
         END FOREACH;

         ---marcar nuevo tipo
         LET v_marca = 0;

         FOREACH
            SELECT c2.marca_inf
              INTO v_marca
              FROM cat_tipo_credito c2
             WHERE c2.tpo_originacion = v_tpo_orig
               AND c2.tpo_credito     = v_tpo_credito
         END FOREACH;

         IF v_marca > 0 THEN
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente,
                                             v_marca,
                                             v_id_cre_acreditado,
                                             p_folio,
                                             v_estado_marca,
                                             v_codigo_rechazo,
                                             v_marca_causa,
                                             v_fecha_causa,
                                             p_usuario,
                                             p_proceso_cod)
                                       INTO  r_edo_retorno;
         END IF

         DELETE
           FROM cta_his_credito
          WHERE id_derechohabiente = v_id_derechohabiente
            AND tpo_credito        = v_tpo_credito
            AND num_credito        = v_num_credito;

         DELETE
           FROM cta_credito
          WHERE id_derechohabiente = v_id_derechohabiente
            AND tpo_credito        = v_tpo_credito
            AND num_credito        = v_num_credito;

         INSERT INTO cta_credito
         SELECT id_derechohabiente, p_proceso_cod, tpo_credito, num_credito, f_otorga
           FROM cre_acreditado
          WHERE id_cre_acreditado = v_id_cre_acreditado;
      END IF

      IF (v_edo_credito = 2 OR v_edo_credito = 5) AND v_edo_orig = 1 THEN
         ---desmarcar marca original
         FOREACH
            SELECT s.marca
              INTO v_marca
              FROM sfr_marca_historica s,
                   cat_tipo_credito c1
             WHERE s.id_derechohabiente  = v_id_derechohabiente
               AND s.marca               = c1.marca_inf
               AND s.n_referencia        = v_id_cre_acreditado
               AND c1.tpo_credito        = v_tpo_credito
               AND c1.tpo_originacion    = v_tpo_orig

            EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente,
                                                v_marca,
                                                v_id_cre_acreditado,
                                                v_estado_marca,
                                                v_marca_causa,
                                                p_usuario,
                                                p_proceso_cod)
                                           INTO r_edo_retorno;
         END FOREACH;

         INSERT INTO cta_his_credito
         SELECT *, p_proceso_cod, v_fecha_act
           FROM cta_credito
          WHERE id_derechohabiente = v_id_derechohabiente
            AND tpo_credito        = v_tpo_credito
            AND num_credito        = v_num_credito;

         DELETE
           FROM cta_credito
          WHERE id_derechohabiente = v_id_derechohabiente
            AND tpo_credito        = v_tpo_credito
            AND num_credito        = v_num_credito;

         IF v_edo_credito = 2 THEN
            LET v_edo_nvo = 170;
         ELSE
            LET v_edo_nvo = 174;
         END IF
      END IF

      ---actualizar estado
      UPDATE cre_acreditado
         SET estado            = v_edo_nvo
       WHERE id_cre_acreditado = v_id_cre_acreditado;

      UPDATE cre_homologa_trm
         SET ind_homologa       = 1,
             f_homologa         = TODAY
       WHERE id_cre_acreditado  = v_id_cre_acreditado
         AND id_cre_ctr_archivo = p_id_cre_ctr_arh;

      INSERT INTO cre_his_acreditado(
                  id_cre_acreditado ,
                  id_cre_ctr_archivo,
                  tpo_transferencia ,
                  edo_procesar      ,
                  diagnostico       ,
                  estado            ,
                  nss_afore         ,
                  rfc_afore         ,
                  paterno_afore     ,
                  materno_afore     ,
                  nombre_afore      ,
                  nom_imss          ,
                  f_proceso         )
           VALUES(
                  v_id_cre_acreditado,
                  p_id_cre_ctr_arh   ,
                  ""                 ,
                  v_edo_procesar     ,
                  v_diag             ,
                  v_edo_nvo          ,
                  ""                 ,
                  ""                 ,
                  ""                 ,
                  ""                 ,
                  ""                 ,
                  ""                 ,
                  v_fecha_act        );

      LET v_total_edo = v_total_edo + 1;
   END FOREACH;

   -- actualiza estadisticas a la tabla histórica
   UPDATE STATISTICS FOR TABLE cre_homologa_trm;

   RETURN v_error, v_nss, v_total_edo;

END FUNCTION
;


