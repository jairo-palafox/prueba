






CREATE FUNCTION "safreviv".fn_homologa_marca_prcr_nss(p_usuario CHAR(40),
                                       p_proceso SMALLINT,
                                       p_folio   DECIMAL(9,0))
   RETURNING SMALLINT;

   DEFINE v_error                SMALLINT;
   DEFINE v_id_dh                DECIMAL(9,0);
   DEFINE v_nss                  CHAR(11);
   DEFINE v_tpo_credito          SMALLINT;
   DEFINE v_num_credito          DECIMAL(10,0);
   DEFINE v_tpo_originacion      SMALLINT;
   DEFINE v_estado               SMALLINT;
   DEFINE v_edo_procesar         SMALLINT;
   DEFINE v_marca_inf            SMALLINT;
   DEFINE v_marca_prc            SMALLINT;
   DEFINE v_id_deudor            DECIMAL(9,0);
   DEFINE v_updt_estado          SMALLINT;
   DEFINE bnd_cta_credito        SMALLINT;
   DEFINE v_f_credito            DATE;
   DEFINE v_id_cre_acreditado    DECIMAL(9,0);
   DEFINE bnd_mca_prcr           SMALLINT;
   DEFINE v_rch_marca            SMALLINT;
   DEFINE bnd_reg_homologado     SMALLINT;
   DEFINE v_n_referencia         DECIMAL(9,0);
   DEFINE v_id_dh_dm             DECIMAL(9,0);
   DEFINE v_marca_inf_dm         SMALLINT;
   DEFINE v_n_ref_dm             DECIMAL(9,0);

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_homologa_marca_prcr.trace';
   TRACE ON;

   LET v_error = 0;
   LET v_id_dh            = "";
   LET v_nss              = "";
   LET v_tpo_credito      = 0;
   LET v_num_credito      = 0;
   LET v_tpo_originacion  = 0;
   LET bnd_reg_homologado = 0;
   LET v_id_dh_dm         = "";
   LET v_marca_inf_dm     = "";
   LET v_n_ref_dm         = "";

   FOREACH
      SELECT af.id_derechohabiente,
             tf.nss,
             tf.tpo_credito,
             tf.num_credito,
             tw.tpo_originacion
        INTO v_id_dh,
             v_nss,
             v_tpo_credito,
             v_num_credito,
             v_tpo_originacion
        FROM afi_derechohabiente af,
             safre_tmp:tmp_hom_mca_prcr tf,
             safre_tmp:tmp_hmp tw
       WHERE af.nss       = tf.nss
         AND tf.nss       = tw.nss
         AND tf.situacion = 2
and  af.nss = "01695090306"

         LET v_id_cre_acreditado = NULL;
         LET bnd_cta_credito     = NULL;

    --  FOREACH

         IF NOT EXISTS (SELECT id_cre_acreditado
                      FROM cre_acreditado
                     WHERE id_derechohabiente = v_id_dh
                       AND tpo_credito = v_tpo_credito
                       AND num_credito = v_num_credito) THEN

             INSERT INTO safre_tmp:tmp_reg_homologado
                  VALUES ("01",
                          v_nss,
                          v_tpo_credito,
                          v_num_credito,
                          "1",
                          "0");

            CONTINUE FOREACH;
         ELSE
            FOREACH
               SELECT id_cre_acreditado
                 INTO v_id_cre_acreditado
                 FROM cre_acreditado
                WHERE id_derechohabiente = v_id_dh
                  AND tpo_credito = v_tpo_credito
                  AND num_credito = v_num_credito
               ORDER BY f_otorga DESC

               IF v_id_cre_acreditado IS NOT NULL THEN
                  EXIT FOREACH;
               END IF
            END FOREACH;
         END IF

     -- END FOREACH;

      IF v_id_cre_acreditado IS NULL THEN
         CONTINUE FOREACH;
      ELSE
         SELECT c.estado,
                c.edo_procesar,
                m.marca_inf,
                m.marca_prc,
                m.id_deudor
           INTO v_estado,
                v_edo_procesar,
                v_marca_inf,
                v_marca_prc,
                v_id_deudor
           FROM cre_acreditado c, cat_tipo_credito m
          WHERE c.id_cre_acreditado  = v_id_cre_acreditado
            AND c.tpo_originacion    = m.tpo_originacion
            AND c.tpo_credito        = m.tpo_credito;

         IF (v_tpo_originacion IS NULL) OR
            (v_tpo_originacion = 1)     OR
            (v_tpo_originacion = 2)     OR
            (v_tpo_originacion = 4)     THEN
{
            IF EXISTS (SELECT id_derechohabiente
                         FROM sfr_marca_activa
                        WHERE id_derechohabiente = v_id_dh
                          AND marca              = v_marca_inf) THEN
}
               LET v_n_referencia = NULL;

               SELECT n_referencia
                 INTO v_n_referencia
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = v_id_dh
                  AND marca              = v_marca_inf;

            IF v_n_referencia IS NOT NULL THEN
                  --llama función para desmarcar
--TRACE'Punto de control 1 : '||v_id_dh;

               EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                   v_marca_inf,
                                                   v_n_referencia,
                                                   0,
                                                   0,
                                                   p_usuario,
                                                   p_proceso) INTO v_rch_marca;
               IF v_rch_marca = 0 THEN
                  LET bnd_reg_homologado = 1;
               END IF
            END IF
{
            IF EXISTS (SELECT id_derechohabiente
                         FROM sfr_marca_activa
                        WHERE id_derechohabiente = v_id_dh
                          AND marca              = v_marca_prc) THEN
}
            LET v_n_referencia = NULL;

               SELECT n_referencia
                 INTO v_n_referencia
                 FROM sfr_marca_activa
                WHERE id_derechohabiente = v_id_dh
                  AND marca              = v_marca_prc;

            IF v_n_referencia IS NOT NULL THEN

                       --llama función para desmarcar
--TRACE'Punto de control 2 : '||v_id_dh;

                EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                    v_marca_prc,
                                                    v_n_referencia,
                                                    0,
                                                    0,
                                                    p_usuario,
                                                    p_proceso) INTO v_rch_marca;
               IF v_rch_marca = 0 THEN
                  LET bnd_reg_homologado = 1;
               END IF
            END IF

            IF v_n_referencia IS NULL THEN
               LET bnd_reg_homologado = 1;
            END IF

            IF bnd_reg_homologado  = 1 THEN
               INSERT INTO cre_his_acreditado
                    VALUES (v_id_cre_acreditado,
                            0,
                            '',
                            v_edo_procesar,
                            0,
                            v_estado,
                            v_nss,
                            '',
                            '',
                            '',
                            '',
                            '',
                            TODAY);
            END IF

            IF EXISTS (SELECT unique id_derechohabiente
                         FROM cta_credito
                        WHERE id_derechohabiente = v_id_dh
                          AND tpo_credito        = v_tpo_credito
                          AND num_credito        = v_num_credito) THEN
                       LET bnd_cta_credito = 1;
            END IF

                LET v_f_credito = "";
            FOREACH
               -- si existe en cta_credito se guarda la historia y se borra registro
          --     IF bnd_cta_credito = 1 THEN

            --      LET v_f_credito = "";

                  SELECT f_credito
                    INTO v_f_credito
                    FROM cta_credito
                   WHERE id_derechohabiente = v_id_dh
                     AND tpo_credito        = v_tpo_credito
                     AND num_credito        = v_num_credito

               IF bnd_cta_credito = 1 THEN
                  INSERT INTO cta_his_credito
                       VALUES (v_id_dh,
                               p_proceso,
                               v_tpo_credito,
                               v_num_credito,
                               v_f_credito,
                               '', --Validar el valor del estado
                               TODAY);

--TRACE'Punto de control 2-1 : '||v_id_dh;

                  DELETE FROM cta_credito
                        WHERE id_derechohabiente = v_id_dh
                          AND tpo_credito        = v_tpo_credito
                          AND num_credito        = v_num_credito;

                   LET v_f_credito = "";
               END IF
               -- se actualiza registro
               UPDATE cre_acreditado
                  SET estado       = 170,
                      edo_procesar = 210
                WHERE id_derechohabiente = v_id_dh
                  AND tpo_credito        = v_tpo_credito
                  AND num_credito        = v_num_credito;

            END FOREACH;

       END IF

       IF (v_tpo_originacion = 1)     OR
          (v_tpo_originacion = 2)     OR
          (v_tpo_originacion = 4)     AND
          (bnd_reg_homologado = 1)    THEN

            --insertar en tabla para solicitar desmarca a procesar
--TRACE'Punto de control 2.2 : '||v_id_dh;
--TRACE'Punto de control 2.2.1 : '||v_id_cre_acreditado;

          INSERT INTO cta_marca_ws
               SELECT c.id_derechohabiente,
                      c.id_cre_acreditado,
                      DECODE(c.tpo_originacion,1,"03",2, "16",4,"43"),
                      c.tpo_credito,
                      m.marca_prc,
                      today,
                      1,
                      "",
                      "",
                      0,
                      c.num_credito,
                      c.f_otorga,
                      DECODE(c.tpo_originacion,1,"01",2, "02",4,"04"),
                      p_folio,
                      p_usuario
                 FROM cre_acreditado c, cat_tipo_credito m
                WHERE c.id_cre_acreditado = v_id_cre_acreditadO
                  AND c.tpo_originacion = m.tpo_originacion
                  AND c.tpo_credito = m.tpo_credito;

         END IF

      END IF

   IF bnd_reg_homologado = 1 THEN
      INSERT INTO safre_tmp:tmp_reg_homologado
           VALUES ("01",
                   v_nss,
                   v_tpo_credito,
                   v_num_credito,
                   "2",
                   "1");
   ELSE
      INSERT INTO safre_tmp:tmp_reg_homologado
           VALUES ("01",
                   v_nss,
                   v_tpo_credito,
                   v_num_credito,
                   "2",
                   "0");
   END IF

   LET v_id_dh           = "";
   LET v_nss             = "";
   LET v_tpo_credito     = "";
   LET v_num_credito     = "";
   LET v_tpo_originacion = "";
   LET bnd_reg_homologado= 0;


   END FOREACH;

   LET v_id_dh             = "";
   LET v_nss               = "";
   LET v_tpo_credito       = "";
   LET v_num_credito       = "";
   LET v_tpo_originacion   = "";
   LET bnd_reg_homologado  = 0 ;
   LET v_id_cre_acreditado = "";

   FOREACH
      SELECT af.id_derechohabiente,
             tf.nss,
             tf.tpo_credito,
             tf.num_credito,
             tw.tpo_originacion
        INTO v_id_dh,
             v_nss,
             v_tpo_credito,
             v_num_credito,
             v_tpo_originacion
        FROM afi_derechohabiente af,
             safre_tmp:tmp_hom_mca_prcr tf,
             safre_tmp:tmp_hmp tw
       WHERE af.nss       = tf.nss
         AND tf.nss       = tw.nss
         AND tf.situacion = 1

      FOREACH
         SELECT c.id_cre_acreditado
           INTO v_id_cre_acreditado
           FROM cre_acreditado c, cat_maq_credito m
          WHERE c.id_derechohabiente = v_id_dh
            AND c.tpo_credito        = v_tpo_credito
            AND c.num_credito        = v_num_credito
            AND c.estado             = m.estado
         ORDER BY m.entidad, c.f_otorga DESC

         IF v_id_cre_acreditado IS NOT NULL THEN
           EXIT FOREACH;
         END IF
      END FOREACH;

      IF v_id_cre_acreditado IS NULL THEN
         CONTINUE FOREACH;
      ELSE
         SELECT c.estado,
                c.edo_procesar,
                m.marca_inf,
                m.marca_prc,
                m.id_deudor,
                c.f_otorga
           INTO v_estado,
                v_edo_procesar,
                v_marca_inf,
                v_marca_prc,
                v_id_deudor,
                v_f_credito
           FROM cre_acreditado c, cat_tipo_credito m
          WHERE c.id_cre_acreditado  = v_id_cre_acreditado
            AND c.tpo_originacion    = m.tpo_originacion
            AND c.tpo_credito        = m.tpo_credito;

         IF NOT EXISTS (SELECT id_derechohabiente
                          FROM cta_credito
                         WHERE tpo_credito   =  v_tpo_credito
                           AND num_credito   = v_num_credito) THEN

            ---LET v_f_credito = "";

                { SELECT f_credito
                    INTO v_f_credito
                    FROM cta_credito
                   WHERE id_derechohabiente = v_id_dh
                     AND tpo_credito        = v_tpo_credito 
                     AND num_credito        = v_num_credito; }

            INSERT INTO cta_credito
                 VALUES (v_id_dh,
                         p_proceso,
                         v_tpo_credito,
                         v_num_credito,
                         v_f_credito);
         END IF

         IF v_tpo_originacion IS NOT NULL THEN
            IF NOT EXISTS (SELECT id_derechohabiente
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_dh
                              AND marca              = v_marca_inf) THEN

               --ejecuta funcion de marca cuenta con marca v_marca_prcr
--TRACE'Punto de control 3 : '||v_id_dh;

               FOREACH
                  SELECT id_derechohabiente, marca, n_referencia
                    INTO v_id_dh_dm, v_marca_inf_dm, v_n_ref_dm
                    FROM sfr_marca_activa, cat_tipo_credito
                   WHERE id_derechohabiente = v_id_dh
                     AND marca = marca_inf
                     AND marca <> v_marca_inf

                   IF v_marca_inf_dm IS NOT NULL THEN
                      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                          v_marca_inf_dm,
                                                          v_n_ref_dm,
                                                          0,
                                                          0,
                                                          p_usuario,
                                                          p_proceso) INTO v_rch_marca;
                   END IF
               END FOREACH;

               EXECUTE FUNCTION fn_marca_cuenta(v_id_dh,
                                                v_marca_inf,
                                                v_id_cre_acreditado,
                                                0,
                                                0,
                                                0,
                                                0,
                                                TODAY,
                                                p_usuario,
                                                p_proceso) INTO v_rch_marca;
               IF v_rch_marca = 0 THEN
                  LET bnd_reg_homologado = 1;

                  IF (v_id_deudor = 1) THEN
                     LET v_updt_estado = 140;
                  END IF
                  IF (v_id_deudor = 0) THEN
                     LET v_updt_estado = 20;
                  END IF

                  UPDATE cre_acreditado
                     SET estado       = v_updt_estado,
                         edo_procesar = 120
                   WHERE id_derechohabiente = v_id_dh
                     AND id_cre_acreditado   = v_id_cre_acreditado;
               END IF
            END IF

            IF NOT EXISTS (SELECT id_derechohabiente
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_dh
                              AND marca              = v_marca_prc) THEN

               EXECUTE FUNCTION fn_marca_cuenta(v_id_dh,
                                                v_marca_prc,
                                                v_id_cre_acreditado,
                                                0,
                                                0,
                                                0,
                                                0,
                                                TODAY,
                                                p_usuario,
                                                p_proceso) INTO v_rch_marca;
            END IF
         END IF-- fin de if de tipo de originación no nulo

         IF v_tpo_originacion IS NULL THEN
            LET bnd_mca_prcr       = 1;

            IF NOT EXISTS (SELECT id_derechohabiente
                             FROM sfr_marca_activa
                            WHERE id_derechohabiente = v_id_dh
                              AND marca              = v_marca_inf) THEN
               IF EXISTS (SELECT c.id_derechohabiente
                            FROM cre_acreditado c, cat_tipo_credito m
                           WHERE c.id_cre_acreditado  = v_id_cre_acreditado
                             AND c.tpo_originacion    = m.tpo_originacion
                             AND c.tpo_credito        = m.tpo_credito
                             AND c.edo_procesar       IN (70,80,85)
                             AND c.estado             IN (140,145)
                             AND m.id_deudor          = 1) THEN
                  CONTINUE FOREACH;
               ELSE
                     -- ejecuta fn_marca cuenta con v_marca_prc y marca v_marca_inf
--TRACE'Punto de control 7 : '||v_id_dh;

                  FOREACH
                     SELECT id_derechohabiente, marca, n_referencia
                       INTO v_id_dh_dm, v_marca_inf_dm, v_n_ref_dm
                       FROM sfr_marca_activa, cat_tipo_credito
                      WHERE id_derechohabiente = v_id_dh
                        AND marca = marca_inf
                        AND marca <> v_marca_inf

                      IF v_marca_inf_dm IS NOT NULL THEN
                         EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                             v_marca_inf_dm,
                                                             v_n_ref_dm,
                                                             0,
                                                             0,
                                                             p_usuario,
                                                             p_proceso) INTO v_rch_marca;
                      END IF
                  END FOREACH;

                  EXECUTE FUNCTION fn_marca_cuenta(v_id_dh,
                                                   v_marca_inf,
                                                   v_id_cre_acreditado,
                                                   0,
                                                   0,
                                                   0,
                                                   0,
                                                   TODAY,
                                                   p_usuario,
                                                   p_proceso) INTO v_rch_marca;
                  IF v_rch_marca = 0 THEN
                     LET bnd_reg_homologado = 1;

                     IF (v_id_deudor = 1) THEN
                        LET v_updt_estado = 140;
                     END IF
                     IF (v_id_deudor = 0) THEN
                        LET v_updt_estado = 20;
                     END IF

                     UPDATE cre_acreditado
                        SET estado              = v_updt_estado
                      WHERE id_derechohabiente = v_id_dh
                        AND id_cre_acreditado   = v_id_cre_acreditado;
                  END IF
               END IF
            END IF

            IF bnd_mca_prcr = 1 THEN
               --insert para solicitar la marca a procesar
--TRACE'Punto de control 9 : '||v_id_dh;

               IF NOT EXISTS (SELECT w.id_derechohabiente
                                FROM cta_marca_ws w, cre_acreditado c, cat_tipo_credito t
                               WHERE c.id_cre_acreditado  = v_id_cre_acreditado
                                 AND c.tpo_originacion    = t.tpo_originacion
                                 AND c.tpo_credito        = t.tpo_credito
                                 AND c.id_derechohabiente = w.id_derechohabiente
                                 AND c.id_cre_acreditado  = w.id_origen
                                 AND c.tpo_credito        = w.tpo_credito) THEN

                  INSERT INTO cta_marca_ws
                     SELECT c.id_derechohabiente,
                            c.id_cre_acreditado,
                            DECODE(c.tpo_originacion,1,"03",2, "16",4,"43"),
                            c.tpo_credito,
                            m.marca_prc,
                            today,
                            1,
                            "",
                            "",
                            2,
                            c.num_credito,
                            c.f_otorga,
                            DECODE(c.tpo_originacion,1,"01",2, "02",4,"04"),
                            p_folio,
                            p_usuario
                       FROM cre_acreditado c, cat_tipo_credito m
                      WHERE c.id_cre_acreditado = v_id_cre_acreditado
                        AND c.tpo_originacion = m.tpo_originacion
                        AND c.tpo_credito = m.tpo_credito;
               END IF
            END IF
         END IF -- end if de v_tipo de originación nulo
      END IF

   IF bnd_reg_homologado = 1 THEN
      INSERT INTO safre_tmp:tmp_reg_homologado
           VALUES ("01",
                   v_nss,
                   v_tpo_credito,
                   v_num_credito,
                   "1",
                   "1");
   ELSE
      INSERT INTO safre_tmp:tmp_reg_homologado
           VALUES ("01",
                   v_nss,
                   v_tpo_credito,
                   v_num_credito,
                   "1",
                   "0");
   END IF

   LET bnd_mca_prcr = 0;
   LET v_id_dh           = "";
   LET v_nss             = "";
   LET v_tpo_credito     = "";
   LET v_num_credito     = "";
   LET v_tpo_originacion = "";
   LET bnd_reg_homologado= 0;
   LET v_id_dh_dm         = "";
   LET v_marca_inf_dm     = "";
   LET v_n_ref_dm         = "";

   END FOREACH;

   RETURN v_error;

END FUNCTION
;


