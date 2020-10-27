






CREATE FUNCTION "safreviv".fn_repara_cre_sdo_deudor()
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- Registro de la tabla temporal
   DEFINE tmp_tpo_registro        CHAR(2);
   DEFINE tmp_nss                 CHAR(11);
   DEFINE tmp_id_derechohabiente  DECIMAL(9,0);
   DEFINE tmp_id_cre_acreditado   DECIMAL(9,0);
   DEFINE tmp_num_credito         DECIMAL(10,0);
   DEFINE tmp_saldo_archivo       DECIMAL(12,0);
   DEFINE tmp_saldo_calculado     DECIMAL(12,2);
   DEFINE tmp_saldo_anterior      DECIMAL(12,2);
   DEFINE tmp_fec_otorga          DATE;
   DEFINE tmp_tpo_credito         SMALLINT;
   DEFINE tmp_estado              SMALLINT;
   -- Variables auxiliares
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
      AND estado IS NULL

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss;

      IF tmp_id_derechohabiente IS NOT NULL THEN
         SELECT id_cre_acreditado, monto_pesos
           INTO v_ax_id_cre_acreditado, v_ax_sdo_deudor
           FROM safre_viv:cre_saldo_deudor
          WHERE id_cre_acreditado = tmp_id_cre_acreditado
            AND movimiento = 181;

         IF v_ax_id_cre_acreditado IS NOT NULL THEN
            IF v_ax_sdo_deudor > tmp_saldo_calculado THEN
               -- se asigna que el registro en cre acreditado es mayor que en la temporal
               LET v_ax_edo_registro = 2;
               LET v_ax_diferencia = v_ax_sdo_deudor - tmp_saldo_calculado;
            ELSE
               -- se asigna que el registro en la temporal es mayor que en cre acreditado
               LET v_ax_edo_registro = 3;
               LET v_ax_diferencia = tmp_saldo_calculado - v_ax_sdo_deudor;
            END IF

            IF v_ax_diferencia < 1 AND v_ax_diferencia >= 0 THEN
               -- se asigna que el registro no se actualizaría
               LET v_ax_edo_registro = 0;
            ELSE
               -- se asigna que el registro se actualizaría
               LET v_ax_edo_registro = 1;
            END IF
         ELSE
            -- se asigna que el registro no existe en cre_acreditado pero sí en rechazo
            LET v_ax_edo_registro = 4;
         END IF
      ELSE
         -- se asigna que el NSS no existe en el catalogo de afiliados
         LET v_ax_edo_registro = 6;
      END IF

      -- se actualiza el registro temporal
      UPDATE safre_tmp:tmp_repara_cre_sdo_deudor
         SET saldo_anterior     = v_ax_sdo_deudor,
             estado             = v_ax_edo_registro
        WHERE nss = tmp_nss
          AND num_credito = tmp_num_credito
          AND fec_otorga = tmp_fec_otorga
          AND tpo_credito = tmp_tpo_credito
          AND tpo_registro IN ("01", "20")
          AND estado IS NULL;

       --EXIT FOREACH;
   END FOREACH

   -- valor del nss después de finalizar el ciclo
   --LET v_c_nss = 1;

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION;


