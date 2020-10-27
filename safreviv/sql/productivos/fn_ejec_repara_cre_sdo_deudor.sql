






CREATE FUNCTION "safreviv".fn_ejec_repara_cre_sdo_deudor()
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- Registro de la tabla temporal
   DEFINE tmp_tpo_registro       CHAR(2);
   DEFINE tmp_nss                CHAR(11);
   DEFINE tmp_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_id_cre_acreditado  DECIMAL(9,0);
   DEFINE tmp_num_credito        DECIMAL(10,0);
   DEFINE tmp_saldo_archivo      DECIMAL(12,0);
   DEFINE tmp_saldo_calculado    DECIMAL(12,2);
   DEFINE tmp_saldo_anterior     DECIMAL(12,2);
   DEFINE tmp_fec_otorga         DATE;
   DEFINE tmp_tpo_credito        SMALLINT;
   DEFINE tmp_estado             SMALLINT;
   -- Variables auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_ax_sdo_deudor         DECIMAL(12,2);
   DEFINE v_ax_diferencia         DECIMAL(12,2);
   DEFINE v_ax_edo_registro       SMALLINT;
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_c_nss                 CHAR(11);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegDevol.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_edo_registro       = 0;
   LET v_ax_id_derechohabiente = 0;
   LET v_ax_id_cre_acreditado  = 0;
   LET v_ax_sdo_deudor         = 0;
   LET v_ax_edo_registro       = 0;
   LET v_error                 = 0;
   LET v_isam_err              = 0;
   LET v_c_msj                 = 'El proceso finalizó correctamente';
   LET v_c_nss                 = "0"; -- valor del NSS antes de entrar al ciclo

   -- Se obtienen los registros de la temporal
   FOREACH
   SELECT tpo_registro,
          nss,
          id_derechohabiente,
          id_cre_acreditado,
          num_credito,
          saldo_archivo,
          saldo_calculado,
          saldo_anterior,
          fec_otorga,
          tpo_credito,
          estado
     INTO tmp_tpo_registro,
          tmp_nss,
          tmp_id_derechohabiente,
          tmp_id_cre_acreditado,
          tmp_num_credito,
          tmp_saldo_archivo,
          tmp_saldo_calculado,
          tmp_saldo_anterior,
          tmp_fec_otorga,
          tmp_tpo_credito,
          tmp_estado
     FROM safre_tmp:tmp_repara_cre_sdo_deudor
    WHERE tpo_registro IN ("01", "20")
      AND estado = 7 --IN (1,7)

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss;

      -- se actualiza el registro temporal
      UPDATE safre_viv:cre_saldo_deudor
         SET monto_pesos = tmp_saldo_calculado
        WHERE id_cre_acreditado = tmp_id_cre_acreditado
          AND movimiento = 181;
   END FOREACH

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION;


