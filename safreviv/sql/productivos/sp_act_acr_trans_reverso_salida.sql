






CREATE PROCEDURE "safreviv".sp_act_acr_trans_reverso_salida(p_id_cre_ctr_archivo DECIMAL(9,0))
   -- Registro de tmp agr_solic_sdo
   DEFINE tmp_nss                CHAR(11);
   DEFINE tmp_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_id_cre_acreditado  DECIMAL(9,0);
   DEFINE tmp_modulo_cod         CHAR(2);
   DEFINE tmp_f_proceso          DATE;

   -- se procesan los registros de cre acreditado insertados en el archivo de Solic. Sdos.
   FOREACH
   SELECT nss,
          id_derechohabiente,
          id_cre_acreditado,
          modulo_cod,
          f_proceso
     INTO tmp_nss,
          tmp_id_derechohabiente,
          tmp_id_cre_acreditado,
          tmp_modulo_cod,
          tmp_f_proceso
     FROM safre_tmp:tmp_acr_solic_sdo
      -- se actualiza registros de cre acreditado
      UPDATE safre_viv:cre_acreditado
         SET edo_procesar = 60
       WHERE edo_procesar = 80
         AND estado IN(140,900)
         AND tpo_originacion = 1
         AND id_derechohabiente = tmp_id_derechohabiente
         AND id_cre_acreditado = tmp_id_cre_acreditado;

      -- se actualiza registros de cre acreditado
      UPDATE safre_viv:cre_acreditado
         SET edo_procesar = 70
       WHERE edo_procesar = 85
         AND estado IN(140,900)
         AND tpo_originacion = 1
         AND id_derechohabiente = tmp_id_derechohabiente
         AND id_cre_acreditado = tmp_id_cre_acreditado;

      -- se actualiza registros de his acreditado
      UPDATE safre_viv:cre_his_acreditado
         SET edo_procesar = 60
       WHERE edo_procesar = 80
         AND estado IN(140,900)
         AND tpo_transferencia = "03"
         AND id_cre_acreditado = tmp_id_cre_acreditado;

      -- se actualiza registros de his acreditado
      UPDATE safre_viv:cre_his_acreditado
         SET edo_procesar = 70
       WHERE edo_procesar = 85
         AND estado IN(140,900)
         AND tpo_transferencia = "03"
         AND id_cre_acreditado = tmp_id_cre_acreditado;
  END FOREACH

   -- se elimina el registro de la tabla de control
   DELETE safre_viv:cre_ctr_archivo
   WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
     AND id_proceso = 201;
END PROCEDURE
;


