






CREATE PROCEDURE "safreviv".sp_tmp_trad_liq()
   DEFINE v_ax_nss                VARCHAR(11);
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0);
   DEFINE v_ax_cuenta_regs        SMALLINT;
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_ax_estado             SMALLINT;
   DEFINE v_ax_edo_procesar       SMALLINT;

   FOREACH
   SELECT nss
   INTO v_ax_nss
   FROM safre_tmp:tmp_trad_liq
   WHERE id_cre_acreditado = 0
      -- se obtiene el id derechohabiente del catalogo
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM safre_viv:afi_derechohabiente
        WHERE nss = v_ax_nss;

      SELECT COUNT(*)
        INTO v_ax_cuenta_regs
        FROM safre_viv:cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND tpo_originacion IN (1,4);

      IF v_ax_cuenta_regs = 1 THEN
         SELECT id_cre_acreditado, estado, edo_procesar
           INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar
           FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND tpo_originacion IN (1,4);

         UPDATE safre_tmp:tmp_trad_liq
            SET id_cre_acreditado = v_ax_id_cre_acreditado,
                estado = v_ax_estado,
                edo_procesar = v_ax_edo_procesar
          WHERE nss = v_ax_nss;

         UPDATE safre_viv:cre_acreditado
            SET estado = 20,
                edo_procesar = 60
          WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
      ELSE
         -- se inicializa el identificador
         LET v_ax_id_cre_acreditado = 0;

         FOREACH
         SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar
           INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar
           FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND tpo_originacion IN (1,4)
            AND edo_procesar = 120
         END FOREACH

         IF v_ax_id_cre_acreditado IS NOT NULL AND v_ax_id_cre_acreditado <> 0 THEN
            UPDATE safre_tmp:tmp_trad_liq
               SET id_cre_acreditado = v_ax_id_cre_acreditado,
                   estado = v_ax_estado,
                   edo_procesar = v_ax_edo_procesar
             WHERE nss = v_ax_nss;

            UPDATE safre_viv:cre_acreditado
               SET estado = 20,
                   edo_procesar = 60
             WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
         ELSE
            FOREACH
            SELECT COUNT(*)
              INTO v_ax_cuenta_regs
              FROM safre_viv:cre_acreditado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_originacion IN (1,4)
               AND estado <> 920
            END FOREACH

            IF v_ax_cuenta_regs = 1 THEN
               SELECT id_cre_acreditado, estado, edo_procesar
                 INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar
                 FROM safre_viv:cre_acreditado
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_originacion IN (1,4)
                  AND estado <> 920;

               UPDATE safre_tmp:tmp_trad_liq
                  SET id_cre_acreditado = v_ax_id_cre_acreditado,
                      estado = v_ax_estado,
                      edo_procesar = v_ax_edo_procesar
                WHERE nss = v_ax_nss;

               UPDATE safre_viv:cre_acreditado
                  SET estado = 20,
                      edo_procesar = 60
                WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
            ELSE
               FOREACH
               SELECT COUNT(*)
                 INTO v_ax_cuenta_regs
                 FROM safre_viv:cre_acreditado
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_originacion = 1
                  AND estado <> 920
               END FOREACH

               IF v_ax_cuenta_regs = 1 THEN
                  SELECT id_cre_acreditado, estado, edo_procesar
                    INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar
                    FROM safre_viv:cre_acreditado
                   WHERE id_derechohabiente = v_ax_id_derechohabiente
                     AND tpo_originacion = 1
                     AND estado <> 920;

                  UPDATE safre_tmp:tmp_trad_liq
                     SET id_cre_acreditado = v_ax_id_cre_acreditado,
                         estado = v_ax_estado,
                         edo_procesar = v_ax_edo_procesar
                   WHERE nss = v_ax_nss;

                  UPDATE safre_viv:cre_acreditado
                     SET estado = 20,
                         edo_procesar = 60
                   WHERE id_cre_acreditado = v_ax_id_cre_acreditado;
{
               ELSE
                  UPDATE safre_tmp:tmp_trad_liq
                     SET id_cre_acreditado = 0
                   WHERE nss = v_ax_nss;
}
               END IF
            END IF
         END IF
      END IF
   END FOREACH
END PROCEDURE
;


