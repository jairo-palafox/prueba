






CREATE PROCEDURE "safreviv".sp_inserta_cta_credito(p_i_proceso_cod SMALLINT,
                                        p_d_id_cre_ctr_archivo DECIMAL(9,0))
   -- Registro de cta credito
   DEFINE cta_id_derechohabiente DECIMAL(9,0);
   DEFINE cta_proceso_cod        SMALLINT;
   DEFINE cta_tpo_credito        SMALLINT;
   DEFINE cta_num_credito        DECIMAL(10,0);
   DEFINE cta_f_credito          DATE;

   -- Registro de cta credito auxiliar
   DEFINE aux_id_derechohabiente DECIMAL(9,0);
   DEFINE aux_proceso_cod        SMALLINT;
   DEFINE aux_tpo_credito        SMALLINT;
   DEFINE aux_num_credito        DECIMAL(10,0);
   DEFINE aux_f_credito          DATE;

   -- Registro de cta his credito
   DEFINE his_id_derechohabiente DECIMAL(9,0);
   DEFINE his_proceso_cod        SMALLINT;
   DEFINE his_tpo_credito        SMALLINT;
   DEFINE his_num_credito        DECIMAL(10,0);
   DEFINE his_f_credito          DATE;
   DEFINE his_estado             SMALLINT;
   DEFINE his_f_actualiza        DATE;

   -- Variables auxiliares
   DEFINE v_d_id_derechohab      DECIMAL(9,0);
   DEFINE v_i_tpo_credito        SMALLINT;
   DEFINE v_d_num_credito        DECIMAL(10,0);

   -- verifica si el proceso corresponde a RECEPCIÓN SALDOS TRANSFERIDOS 43BIS
   IF p_i_proceso_cod = 1205 THEN
      -- obtienen los registros de cre his acreditado
      FOREACH
      SELECT UNIQUE id_derechohabiente, tpo_credito, num_credito, f_otorga
        INTO v_d_id_derechohab, v_i_tpo_credito, v_d_num_credito, cta_f_credito
        FROM cre_acreditado
       WHERE id_cre_acreditado IN (
             SELECT id_cre_acreditado
               FROM cre_his_acreditado
              WHERE edo_procesar = 120
                AND id_cre_ctr_archivo = p_d_id_cre_ctr_archivo)

         -- se asignan los valores del registro a insertar
         LET cta_id_derechohabiente = v_d_id_derechohab;
         LET cta_proceso_cod        = p_i_proceso_cod;
         LET cta_tpo_credito        = v_i_tpo_credito;
         LET cta_num_credito        = v_d_num_credito;

         IF cta_f_credito IS NULL THEN
            LET cta_f_credito = TODAY;
         END IF

         -- se consulta la tabla para saber si ya existe el registro a insertar
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM cta_credito
          WHERE id_derechohabiente = v_d_id_derechohab) THEN

            -- se inserta el registro
            INSERT INTO cta_credito (
                        id_derechohabiente,
                        proceso_cod,
                        tpo_credito,
                        num_credito,
                        f_credito)
                VALUES (cta_id_derechohabiente,
                        cta_proceso_cod,
                        cta_tpo_credito,
                        cta_num_credito,
                        cta_f_credito);
         ELSE
            -- se consulta la tabla para saber si ya existe el registro a insertar
            IF EXISTS (
               SELECT id_derechohabiente
                 FROM cta_credito
                WHERE id_derechohabiente = v_d_id_derechohab
                  AND tpo_credito = v_i_tpo_credito
                  AND num_credito = v_d_num_credito) THEN

               CONTINUE FOREACH;
            ELSE
               -- se selecciona el registro de cta credito
               SELECT id_derechohabiente,
                      proceso_cod,
                      tpo_credito,
                      num_credito,
                      f_credito
                 INTO aux_id_derechohabiente,
                      aux_proceso_cod,
                      aux_tpo_credito,
                      aux_num_credito,
                      aux_f_credito
                 FROM cta_credito
                WHERE id_derechohabiente = v_d_id_derechohab;

               -- se elimina el registro de cta credito
               DELETE
                 FROM cta_credito
                WHERE id_derechohabiente = v_d_id_derechohab;

               -- se inserta el registro
               INSERT INTO cta_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito)
                   VALUES (cta_id_derechohabiente,
                           cta_proceso_cod,
                           cta_tpo_credito,
                           cta_num_credito,
                           cta_f_credito);

               -- elimina el registro de cta historico en caso de haber
               DELETE
                 FROM cta_his_credito
                WHERE id_derechohabiente = v_d_id_derechohab
                  AND num_credito = aux_num_credito;

               -- se asignan los valores del registro a insertar
               LET his_id_derechohabiente = aux_id_derechohabiente;
               LET his_proceso_cod        = aux_proceso_cod;
               LET his_tpo_credito        = aux_tpo_credito;
               LET his_num_credito        = aux_num_credito;
               LET his_f_credito          = aux_f_credito;
               LET his_estado             = 4;
               LET his_f_actualiza        = TODAY;

               -- se inserta el registro
               INSERT INTO cta_his_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito,
                           estado,
                           f_actualiza)
                   VALUES (his_id_derechohabiente,
                           his_proceso_cod,
                           his_tpo_credito,
                           his_num_credito,
                           his_f_credito,
                           his_estado,
                           his_f_actualiza);
            END IF
         END IF
      END FOREACH;
   ELSE
      -- obtienen los registros de acr transferencia para el folio dado
      FOREACH
      SELECT UNIQUE id_derechohabiente, tpo_credito, num_credito, f_otorga
        INTO v_d_id_derechohab, v_i_tpo_credito, v_d_num_credito, cta_f_credito
        FROM cre_acreditado
       WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_archivo
         AND edo_credito = 1

         -- se asignan los valores del registro a insertar
         LET cta_id_derechohabiente = v_d_id_derechohab;
         LET cta_proceso_cod        = p_i_proceso_cod;
         LET cta_tpo_credito        = v_i_tpo_credito;
         LET cta_num_credito        = v_d_num_credito;
         
         IF cta_f_credito IS NULL THEN
            LET cta_f_credito = TODAY;
         END IF

         -- se consulta la tabla para saber si ya existe el registro a insertar
         IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM cta_credito
          WHERE id_derechohabiente = v_d_id_derechohab) THEN
            -- se inserta el registro
            INSERT INTO cta_credito (
                        id_derechohabiente,
                        proceso_cod,
                        tpo_credito,
                        num_credito,
                        f_credito)
                VALUES (cta_id_derechohabiente,
                        cta_proceso_cod,
                        cta_tpo_credito,
                        cta_num_credito,
                        cta_f_credito);
         ELSE
            -- se consulta la tabla para saber si ya existe el registro a insertar
            IF EXISTS (
            SELECT id_derechohabiente
              FROM cta_credito
             WHERE id_derechohabiente = v_d_id_derechohab
               AND tpo_credito = v_i_tpo_credito
               AND num_credito = v_d_num_credito) THEN
               CONTINUE FOREACH;
            ELSE
               -- se selecciona el registro de cta credito
               SELECT id_derechohabiente,
                      proceso_cod,
                      tpo_credito,
                      num_credito,
                      f_credito
                 INTO aux_id_derechohabiente,
                      aux_proceso_cod,
                      aux_tpo_credito,
                      aux_num_credito,
                      aux_f_credito
                 FROM cta_credito
                WHERE id_derechohabiente = v_d_id_derechohab;

               -- se elimina el registro de cta credito
               DELETE
                 FROM cta_credito
                WHERE id_derechohabiente = v_d_id_derechohab;

               -- se inserta el registro
               INSERT INTO cta_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito)
                   VALUES (cta_id_derechohabiente,
                           cta_proceso_cod,
                           cta_tpo_credito,
                           cta_num_credito,
                           cta_f_credito);

               -- elimina el registro de cta historico en caso de haber
               DELETE
                 FROM cta_his_credito
                WHERE id_derechohabiente = v_d_id_derechohab
                  AND num_credito = aux_num_credito;

               -- se asignan los valores del registro a insertar
               LET his_id_derechohabiente = aux_id_derechohabiente;
               LET his_proceso_cod        = aux_proceso_cod;
               LET his_tpo_credito        = aux_tpo_credito;
               LET his_num_credito        = aux_num_credito;
               LET his_f_credito          = aux_f_credito;
               LET his_estado             = 4;
               LET his_f_actualiza        = TODAY;

               -- se inserta el registro
               INSERT INTO cta_his_credito (
                           id_derechohabiente,
                           proceso_cod,
                           tpo_credito,
                           num_credito,
                           f_credito,
                           estado,
                           f_actualiza)
                   VALUES (his_id_derechohabiente,
                           his_proceso_cod,
                           his_tpo_credito,
                           his_num_credito,
                           his_f_credito,
                           his_estado,
                           his_f_actualiza);
            END IF
         END IF
      END FOREACH;
   END IF

   -- actualiza estadisticas a la tabla de crédito
   UPDATE STATISTICS FOR TABLE cta_credito;

END PROCEDURE
;


