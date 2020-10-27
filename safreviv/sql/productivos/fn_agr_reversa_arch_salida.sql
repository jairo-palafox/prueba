






CREATE FUNCTION "safreviv".fn_agr_reversa_arch_salida(p_id_cre_ctr_archivo DECIMAL(9,0))
   RETURNING SMALLINT;
   -- Registro de tmp agr_solic_sdo
   DEFINE tmp_id_derechohabiente DECIMAL(9,0);
   -- Registro de cre his acreditado
   DEFINE his_id_cre_acreditado  DECIMAL(9,0);
   DEFINE his_edo_procesar       SMALLINT;
   -- Registro de cre acreditado
   DEFINE cre_id_cre_acreditado  DECIMAL(9,0);
   -- Variables Auxiliares
   DEFINE v_ax_edo_procesar      SMALLINT;
   DEFINE v_ax_sql_error         SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_ax_sql_error
      -- Imprime el codigo de error
      RETURN v_ax_sql_error;
   END EXCEPTION

   -- Indica el archivo de errores
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrReversoArchSal.trace';
   --TRACE ON;
   
   SET PDQPRIORITY HIGH;

   -- se asume que no ocurrirá ningún error
   LET v_ax_sql_error = 0;

   -- se procesan los registros de cre acreditado insertados en el archivo de Solic. Sdos.
   FOREACH
   SELECT id_cre_acreditado, edo_procesar
     INTO his_id_cre_acreditado, his_edo_procesar
     FROM safre_viv:cre_his_acreditado
    WHERE edo_procesar IN (80, 85)
      AND id_cre_ctr_archivo = p_id_cre_ctr_archivo
      -- se valida el estado procesar
      IF his_edo_procesar = 80 THEN
         LET v_ax_edo_procesar = 60;
      ELSE
         LET v_ax_edo_procesar = 70;
      END IF

      -- se actualiza registros de cre acreditado
      UPDATE safre_viv:cre_acreditado
         SET edo_procesar = v_ax_edo_procesar
       WHERE id_cre_acreditado = his_id_cre_acreditado
         AND estado = 140
         AND edo_procesar IN (80, 85)
         AND tpo_originacion = 4;
   END FOREACH

   -- se eliminan los registros de historicos
   DELETE safre_viv:cre_his_acreditado
   WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo;

   -- se procesan los registros de uso garantia insertados en el archivo de Solic. Sdos.
   FOREACH
   SELECT id_derechohabiente
     INTO tmp_id_derechohabiente
     FROM safre_tmp:tmp_agr_solic_sdo
    WHERE modulo_cod = "UA"
      -- se actualiza registros de cre acreditado
      UPDATE safre_viv:cre_uso_garantia
         SET edo_procesar = 10
       WHERE estado = 140
         AND edo_procesar = 80
         AND id_derechohabiente = tmp_id_derechohabiente
         AND tpo_transferencia = "43";

      -- se actualiza registros de cre acreditado
      UPDATE safre_viv:cre_uso_garantia
         SET edo_procesar = 70
       WHERE estado = 140
         AND edo_procesar = 85
         AND id_derechohabiente = tmp_id_derechohabiente
         AND tpo_transferencia = "43";
   END FOREACH

   -- se elimina el registro insertado en la tabla de control
   DELETE safre_viv:cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_id_cre_ctr_archivo
      AND id_proceso = 301;

   -- se pasa los registros de la tabla temporal de No Atendidas a la tabla global de Solic. Sdo.
   INSERT INTO safre_tmp:tmp_agr_solic_sdo SELECT * FROM safre_tmp:tmp_agr_solic_sdo2;
   INSERT INTO safre_tmp:tmp_agr_solic_sdo_ua SELECT * FROM safre_tmp:tmp_agr_solic_sdo_ua2;

   SET PDQPRIORITY DEFAULT;

   RETURN v_ax_sql_error;
END FUNCTION

;


