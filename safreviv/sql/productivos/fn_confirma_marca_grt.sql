






CREATE FUNCTION "safreviv".fn_confirma_marca_grt(p_fecha DATE, p_usuario CHAR(20))

   RETURNING SMALLINT

   DEFINE v_error                   SMALLINT;
   DEFINE v_sts_marcaje             SMALLINT;
   DEFINE v_marca_entra             SMALLINT;
   DEFINE v_desmarca                SMALLINT;
   DEFINE v_estado_marca            SMALLINT;
   DEFINE v_codigo_rechazo          SMALLINT;
   DEFINE v_marca_causa             SMALLINT;
   DEFINE v_fecha_causa             SMALLINT;
   DEFINE v_proceso_cod             SMALLINT;	
   DEFINE v_folio                   DECIMAL(10,0);
   DEFINE v_n_referencia            DECIMAL(9,0);
   DEFINE v_id_dh                   DECIMAL(9,0);
   DEFINE v_id_ocg_fz               DECIMAL(9,0);
   DEFINE v_id_ocg_dt               DECIMAL(9,0);
   DEFINE v_id_ocg_tmt              DECIMAL(9,0);
   DEFINE v_id_cre_acred            DECIMAL(9,0);
   DEFINE v_fecha_prcr              DATE;
   DEFINE v_fecha_inf               DATE;

   ON EXCEPTION SET v_error
      RETURN v_error;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/fn_confirma_marca_grt.trace';
   ---TRACE ON;

   -- Inicializa variables
   LET v_error          = 0;
   LET v_folio          = 1;
   LET v_marca_entra    = 202;
   LET v_id_dh          = NULL;
   LET v_id_ocg_fz      = NULL;
   LET v_id_ocg_dt      = NULL;
   LET v_id_ocg_tmt     = NULL;
   LET v_id_cre_acred   = NULL;
   LET v_estado_marca   = 0; 
   LET v_codigo_rechazo = 0; 
   LET v_marca_causa    = NULL;
   LET v_fecha_causa    = "";
   LET v_fecha_inf      = NULL;
   LET v_proceso_cod    = 1229;

   ---Cursor para registros marcados en Procesar
   FOREACH
      SELECT g.id_derechohabiente, g.f_inicio
        INTO v_id_dh, v_fecha_prcr
        FROM sfr_marca_activa g
       WHERE g.id_derechohabiente > 0
         AND g.marca = 232
         AND g.f_inicio > p_fecha

      IF (v_id_dh IS NOT NULL) THEN
         -- Busca en ocg_formalizacion con situación 60 y 70
         IF EXISTS (SELECT f.id_derechohabiente
                      FROM ocg_formalizacion f
                     WHERE f.id_derechohabiente = v_id_dh
                       AND f.situacion IN (60,70)) THEN

            -- Recupera ID de formalización
            SELECT fr.id_ocg_formalizacion,
                   fr.id_ocg_detalle,
                   fr.id_ocg_tramite
              INTO v_id_ocg_fz,
                   v_id_ocg_dt,
                   v_id_ocg_tmt
              FROM ocg_formalizacion fr
             WHERE fr.id_derechohabiente = v_id_dh
               AND fr.situacion IN (60,70);

            -- En caso de contar con el trámite 
            IF (v_id_ocg_tmt IS NOT NULL) THEN
               UPDATE ocg_tramite
                  SET situacion = 80
                WHERE id_ocg_tramite     = v_id_ocg_tmt
                  AND id_derechohabiente = v_id_dh;
            END IF

            -- Actualiza en ocg_acreditado y ocg_formalizacion
            IF (v_id_ocg_fz IS NOT NULL) THEN
                -- Valida registro de crédito en RF
               IF EXISTS (SELECT c1.id_derechohabiente
                            FROM cre_acreditado c1, cat_maq_credito m1
                           WHERE c1.id_derechohabiente = v_id_dh
                             AND c1.estado = m1.estado
                             AND m1.entidad = 1
                          GROUP BY 1) THEN
                  SELECT MAX(c2.id_cre_acreditado)
                    INTO v_id_cre_acred
                    FROM cre_acreditado c2, cat_maq_credito m2
                   WHERE c2.id_derechohabiente = v_id_dh
                     AND c2.estado = m2.estado
                     AND m2.entidad = 1;
               ELSE
                  IF EXISTS (SELECT c3.id_derechohabiente
                               FROM cre_acreditado c3, cat_maq_credito m3
                              WHERE c3.id_derechohabiente = v_id_dh
                                AND c3.estado = m3.estado
                                AND m3.entidad = 0
                             GROUP BY 1) THEN
                     SELECT MAX(c4.id_cre_acreditado)
                       INTO v_id_cre_acred
                       FROM cre_acreditado c4, cat_maq_credito m4
                      WHERE c4.id_derechohabiente = v_id_dh
                        AND c4.estado = m4.estado
                        AND m4.entidad = 0;

                     UPDATE cre_acreditado
                        SET estado = 20
                      WHERE id_cre_acreditado = v_id_cre_acred;
                  END IF
               END IF

               IF v_id_cre_acred IS NOT NULL OR v_id_cre_acred <> "" THEN
                  UPDATE cre_acreditado
                     SET estado = 240
                   WHERE id_derechohabiente = v_id_dh
                     AND tpo_credito        = 2
                     AND estado IN(SELECT t1.estado
                                     FROM cat_maq_credito t1
                                    WHERE t1.entidad IN(1,0))
                     AND id_cre_acreditado <> v_id_cre_acred;

                  ---Desmarcas de créditos diferentes a apoyo
                  IF EXISTS (SELECT d.id_derechohabiente
                               FROM sfr_marca_activa d
                              WHERE d.id_derechohabiente = v_id_dh
                                AND d.marca IN(SELECT tc.marca_inf
                                                 FROM cat_tipo_credito tc
                                                WHERE tc.marca_inf <> 202)
                             GROUP BY 1) THEN
                     FOREACH
                        SELECT d1.marca, d1.n_referencia
                          INTO v_desmarca, v_n_referencia
                          FROM sfr_marca_activa d1
                         WHERE d1.id_derechohabiente = v_id_dh
                           AND d1.marca IN(SELECT t2.marca_inf
                                             FROM cat_tipo_credito t2
                                            WHERE t2.marca_inf <> 202)

                        EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh,
                                                            v_desmarca,
                                                            v_n_referencia,
                                                            v_estado_marca,
                                                            v_marca_causa,
                                                            p_usuario,
                                                            v_proceso_cod)
                                                       INTO v_sts_marcaje;
                     END FOREACH;
                  END IF

                  --- Verifica marca 202
                  IF NOT EXISTS (SELECT s.id_derechohabiente
                                   FROM sfr_marca_activa s
                                  WHERE s.id_derechohabiente = v_id_dh
                                    AND s.marca = v_marca_entra) THEN
                     SELECT r.folio_archivo
                       INTO v_folio
                       FROM cre_acreditado a, cre_ctr_archivo r
                      WHERE a.id_cre_acreditado  = v_id_cre_acred
                        AND a.id_cre_ctr_archivo = r.id_cre_ctr_archivo;

                     EXECUTE FUNCTION fn_marca_cuenta(v_id_dh,
                                                      v_marca_entra,
                                                      v_id_cre_acred,
                                                      v_folio,
                                                      v_estado_marca,
                                                      v_codigo_rechazo,
                                                      v_marca_causa,
                                                      v_fecha_causa,
                                                      p_usuario,
                                                      v_proceso_cod)
                                                 INTO v_sts_marcaje;
                  END IF

                  SELECT MAX(s3.f_inicio)
                    INTO v_fecha_inf
                    FROM sfr_marca_activa s3
                   WHERE s3.id_derechohabiente = v_id_dh
                     AND s3.marca = v_marca_entra;
               END IF

               IF EXISTS (SELECT oa.id_ocg_formalizacion
                            FROM ocg_acreditado oa
                           WHERE oa.id_ocg_formalizacion = v_id_ocg_fz
                             AND oa.f_marca_infonavit IS NULL) AND
                  (v_fecha_inf IS NOT NULL) THEN
                  UPDATE ocg_acreditado
                     SET situacion            = 80,
                         f_marca_infonavit    = v_fecha_inf,
                         f_conf_marca_prcr    = v_fecha_prcr
                   WHERE id_ocg_formalizacion = v_id_ocg_fz;
               ELSE
                  UPDATE ocg_acreditado
                     SET situacion            = 80,
                         f_conf_marca_prcr    = v_fecha_prcr
                   WHERE id_ocg_formalizacion = v_id_ocg_fz;
               END IF

               UPDATE ocg_formalizacion
                  SET situacion = 80
                WHERE id_ocg_formalizacion = v_id_ocg_fz
                  AND id_derechohabiente   = v_id_dh;
            END IF
         END IF
      END IF
   END FOREACH;

   RETURN v_error;

END FUNCTION
;


