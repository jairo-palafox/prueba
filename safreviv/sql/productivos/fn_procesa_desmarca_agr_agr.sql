






CREATE PROCEDURE "safreviv".fn_procesa_desmarca_agr_agr( )
   -- REGISTRO de cta credito
   DEFINE cta_id_derechohabiente  DECIMAL(9,0);
   DEFINE cta_proceso_cod         SMALLINT;
   DEFINE cta_tpo_credito         SMALLINT;
   DEFINE cta_num_credito         DECIMAL(10,0);
   DEFINE cta_f_credito           DATE;
   -- REGISTRO de cta his credito
   DEFINE ctah_id_derechohabiente DECIMAL(9,0);
   DEFINE ctah_proceso_cod        SMALLINT;
   DEFINE ctah_tpo_credito        SMALLINT;
   DEFINE ctah_num_credito        DECIMAL(10,0);
   DEFINE ctah_f_credito          DATE;

   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_ax_tpo_credito        SMALLINT;
   DEFINE v_id_cre_ctr_archivo    VARCHAR(18);
   DEFINE v_ax_folio_archivo      DECIMAL(9,0);
   DEFINE v_ax_marca_inf          SMALLINT;
      FOREACH
         -- se procesan los registros de his acreditado para el folio dado
         SELECT id_cre_acreditado
           INTO v_ax_id_cre_acreditado
           FROM safre_viv:cre_his_acreditado
          WHERE estado IN (160, 170)
            AND id_cre_ctr_archivo = 632

         LET v_ax_id_derechohabiente = NULL;
         
         SELECT id_derechohabiente, tpo_credito, id_cre_ctr_archivo
           INTO v_ax_id_derechohabiente, v_ax_tpo_credito, v_id_cre_ctr_archivo
           FROM safre_viv:cre_acreditado
          WHERE estado IN (160, 170)
            AND id_cre_acreditado = v_ax_id_cre_acreditado;

         IF v_ax_id_derechohabiente IS NULL THEN
            CONTINUE FOREACH;
         END IF
         
         LET v_ax_folio_archivo = 10314;

         -- se obtiene la marca para el reverso
         SELECT marca_inf
           INTO v_ax_marca_inf
           FROM safre_viv:cat_tipo_credito
          WHERE tpo_credito = v_ax_tpo_credito;

         -- ejecuta función de reversa desmarca
         EXECUTE PROCEDURE safre_viv:sp_reversa_desmarca(v_ax_id_derechohabiente,
                                                         v_ax_marca_inf,
                                                         v_ax_id_cre_acreditado,
                                                         v_ax_folio_archivo);

         FOREACH
         -- se leen el registro de cta credito
         SELECT UNIQUE id_derechohabiente,
                proceso_cod,
                tpo_credito,
                num_credito,
                f_credito
           INTO ctah_id_derechohabiente,
                ctah_proceso_cod,
                ctah_tpo_credito,
                ctah_num_credito,
                ctah_f_credito
           FROM safre_viv:cta_his_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente

         -- verifica si se encontró registro en cta credito
         IF ctah_id_derechohabiente IS NOT NULL THEN
            -- se asignan los valores en el registro cta his credito
            LET cta_id_derechohabiente = ctah_id_derechohabiente;
            LET cta_proceso_cod        = ctah_proceso_cod;
            LET cta_tpo_credito        = ctah_tpo_credito;
            LET cta_num_credito        = ctah_num_credito;
            LET cta_f_credito          = ctah_f_credito;

            -- se inserta el registro en cta his credito
            INSERT INTO safre_viv:cta_credito (
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

            -- se elimina el registro de cta credito
            DELETE
              FROM safre_viv:cta_his_credito
             WHERE id_derechohabiente = v_ax_id_cre_acreditado
               AND proceso_cod = ctah_proceso_cod
               AND tpo_credito = ctah_tpo_credito
               AND num_credito = ctah_num_credito
               AND f_credito = ctah_f_credito;
         END IF
         END FOREACH
{
         -- se actualiza el edo_procesar de his acreditado
         DELETE FROM safre_viv:cre_his_acreditado
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado
            AND id_cre_ctr_archivo = 632;
}
         -- se actualiza el edo_procesar de his acreditado
         UPDATE safre_viv:cre_acreditado
            SET estado = 140
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
      END FOREACH
END PROCEDURE;


