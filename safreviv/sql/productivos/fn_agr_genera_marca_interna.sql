






CREATE FUNCTION "safreviv".fn_agr_genera_marca_interna()

   RETURNING SMALLINT, INTEGER, VARCHAR(250), DECIMAL(9,0), DECIMAL(9,0), DECIMAL(9,0)

   DEFINE v_error                        SMALLINT;
   DEFINE v_isam_err                     INTEGER;
   DEFINE v_c_msj                        VARCHAR(250);
   DEFINE v_id_cre_acreditado            DECIMAL(9,0);
   DEFINE v_id_derechohabiente           DECIMAL(9,0);
   DEFINE v_id_cre_ctr_archivo           DECIMAL(9,0);
   DEFINE v_tpo_originacion              SMALLINT;
   DEFINE v_tpo_credito                  SMALLINT;
   DEFINE v_estado                       SMALLINT;
   DEFINE v_edo_procesar                 SMALLINT;
   DEFINE v_marca_ifv                    SMALLINT;
   DEFINE v_folio                        DECIMAL(9,0);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj,v_id_cre_acreditado,v_id_derechohabiente,v_id_cre_ctr_archivo;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_int/archivos/agr_gen_marca_interna.trace';
   --TRACE ON;

   LET v_error               = 0;
   LET v_isam_err            = 0;
   LET v_c_msj               = 'El proceso finalizó correctamente';
   LET v_id_cre_acreditado   = NULL;
   LET v_id_cre_ctr_archivo  = 0;
   LET v_id_derechohabiente  = NULL;
   LET v_tpo_originacion     = NULL;
   LET v_tpo_credito         = NULL;
   LET v_estado              = NULL;
   LET v_edo_procesar        = NULL;
   LET v_marca_ifv           = NULL;
   LET v_folio               = 0;

   FOREACH
      -- Registros que se recibieron en la OP.09 AGR
      SELECT cre.id_cre_acreditado ,
             cre.id_derechohabiente,
             his.id_cre_ctr_archivo,
             cre.tpo_originacion   ,
             cre.tpo_credito       ,
             cre.estado            ,
             cre.edo_procesar
        INTO v_id_cre_acreditado ,
             v_id_derechohabiente,
             v_id_cre_ctr_archivo,
             v_tpo_originacion   ,
             v_tpo_credito       ,
             v_estado            ,
             v_edo_procesar
        FROM cre_his_acreditado his,
             cre_acreditado cre,
             cat_maq_credito maq
       WHERE his.id_cre_acreditado = cre.id_cre_acreditado
         AND his.id_cre_ctr_archivo IN (15280,15419,15607,15782)
         AND his.edo_procesar = 120  -- Saldo transferido
         AND cre.estado = maq.estado
         AND maq.entidad = 1

      -- Obtiene marca interna ifv
      FOREACH
         SELECT FIRST 1 marca_inf
           INTO v_marca_ifv
           FROM cat_tipo_credito
          WHERE tpo_credito = v_tpo_credito
            AND tpo_originacion = v_tpo_originacion
          ORDER BY f_actualiza DESC
      END FOREACH;

      -- Verifica marca activa del crédito
      IF NOT EXISTS(SELECT id_derechohabiente
                      FROM sfr_marca_activa
                     WHERE id_derechohabiente = v_id_derechohabiente
                       AND marca        = v_marca_ifv
                       AND n_referencia = v_id_cre_acreditado) THEN

         -- Verifica histórico si fué desmarcado por la 305 - Recepción saldos transferidos AG
         IF EXISTS(SELECT id_derechohabiente
                     FROM sfr_marca_historica
                    WHERE id_derechohabiente = v_id_derechohabiente
                      AND marca        = v_marca_ifv
                      AND n_referencia = v_id_cre_acreditado
                      AND f_inicio IS NOT NULL
                      AND f_fin IS NOT NULL
                      AND proceso_desmarca = 305) THEN

            -- Recupera folio
            FOREACH
               SELECT FIRST 1 folio
                 INTO v_folio
                 FROM sfr_marca_historica
                WHERE id_derechohabiente = v_id_derechohabiente
                  AND marca        = v_marca_ifv
                  AND n_referencia = v_id_cre_acreditado
                  AND f_inicio IS NOT NULL
                  AND f_fin IS NOT NULL
                  AND proceso_desmarca = 305
               ORDER BY f_fin DESC
            END FOREACH;

            -- Reversa desmarca
            EXECUTE PROCEDURE sp_reversa_desmarca(v_id_derechohabiente,
                                                  v_marca_ifv,
                                                  v_id_cre_acreditado,
                                                  v_folio);

            -- Diagnóstico 100 indica que se reversó la desmarca
            INSERT INTO tmp_diag_marca
               VALUES(v_id_cre_acreditado ,
                      v_id_derechohabiente,
                      v_id_cre_ctr_archivo,
                      v_tpo_originacion   ,
                      v_tpo_credito       ,
                      v_estado            ,
                      v_edo_procesar      ,
                      v_marca_ifv         ,
                      v_folio             ,
                      100);
         ELSE
            -- Diagnóstico 0 indica que no se desmarcó en la Op.09
            INSERT INTO tmp_diag_marca
               VALUES(v_id_cre_acreditado ,
                      v_id_derechohabiente,
                      v_id_cre_ctr_archivo,
                      v_tpo_originacion   ,
                      v_tpo_credito       ,
                      v_estado            ,
                      v_edo_procesar      ,
                      v_marca_ifv         ,
                      NULL                ,
                      0);
         END IF
      ELSE
         -- Diagnóstico 1 indica que si tiene marca activa
         INSERT INTO tmp_diag_marca
            VALUES(v_id_cre_acreditado ,
                   v_id_derechohabiente,
                   v_id_cre_ctr_archivo,
                   v_tpo_originacion   ,
                   v_tpo_credito       ,
                   v_estado            ,
                   v_edo_procesar      ,
                   v_marca_ifv         ,
                   NULL                ,
                   1);
      END IF

      -- Setea variables
      LET v_marca_ifv = NULL;
      LET v_folio     = NULL;

   END FOREACH

   RETURN v_error, v_isam_err, v_c_msj,v_id_cre_acreditado,v_id_derechohabiente,v_id_cre_ctr_archivo;

END FUNCTION

/*

   DROP TABLE IF EXISTS tmp_diag_marca;
   CREATE TABLE tmp_diag_marca(
                   id_cre_acreditado  DECIMAL(9,0),
                   id_derechohabiente DECIMAL(9,0),
                   id_cre_ctr_archivo DECIMAL(9,0),
                   tpo_originacion    SMALLINT,
                   tpo_credito        SMALLINT,
                   estado             SMALLINT,
                   edo_procesar       SMALLINT,
                   marca_ifv          SMALLINT,
                   folio              DECIMAL(9,0),
                   diagnostico        SMALLINT
                ) IN cre_dbs:
*/
;


