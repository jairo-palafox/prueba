






CREATE FUNCTION "safreviv".fn_acr_integra_recurr_his_deudor(p_d_folio DECIMAL(9,0),
                                                 p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), DECIMAL(9,0)

   -- Registro de cre his acreditado
   DEFINE v_his_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_his_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE v_his_tpo_transferencia  CHAR(2);
   DEFINE v_his_edo_procesar       SMALLINT;
   DEFINE v_his_diagnostico        CHAR(3);
   DEFINE v_his_estado             SMALLINT;
   DEFINE v_his_nss_afore          CHAR(11);
   DEFINE v_his_rfc_afore          CHAR(13);
   DEFINE v_his_paterno_afore      CHAR(40);
   DEFINE v_his_materno_afore      CHAR(40);
   DEFINE v_his_nombre_afore       CHAR(40);
   DEFINE v_his_nom_imss           CHAR(50);
   DEFINE v_his_f_proceso          DATE;

   -- Registro de cre saldo deudor
   DEFINE v_sdo_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_sdo_folio_referencia   DECIMAL(9,0);
   DEFINE v_sdo_f_movimiento       DATE;
   DEFINE v_sdo_movimiento         SMALLINT;
   DEFINE v_sdo_id_referencia      DECIMAL(9,0);
   DEFINE v_sdo_monto_aivs         DECIMAL(16,6);
   DEFINE v_sdo_monto_pesos        DECIMAL(12,2);
   DEFINE v_sdo_f_proceso          DATE;

   -- Variables auxiliares
   DEFINE v_ax_movimiento          SMALLINT;
   DEFINE v_ax_precio_accion       DECIMAL(16,6);
   DEFINE v_ax_id_cre_acreditado   DECIMAL(9,0); 
   DEFINE v_ax_sdo_deudor          DECIMAL(12,2);
   DEFINE v_ax_edo_procesar        SMALLINT;
   DEFINE v_ax_tpo_credito         SMALLINT;
   DEFINE v_ax_tpo_originacion     SMALLINT;
   DEFINE v_ax_edo_credito         SMALLINT;
   DEFINE v_ax_tpo_transferencia   CHAR(2);
   DEFINE v_ax_id_deudor           SMALLINT;
   DEFINE v_error                  SMALLINT; -- codigo de error en caso de excepción
   DEFINE v_isam_err               INTEGER;
   DEFINE v_c_msj                  VARCHAR(250);
   DEFINE v_c_id_cre_acred         DECIMAL(9,0);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_id_cre_acred;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegRecurrHisDeu.trace';
   --SET DEBUG FILE TO '/safreviv_req/PRODINFXV-106/acrIntegRecurrHisDeu.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_error          = 0;
   LET v_isam_err       = 0;
   LET v_c_msj          = 'El proceso finalizó correctamente';
   LET v_c_id_cre_acred = 0; -- valor del NSS antes de entrar al ciclo

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_ax_precio_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   FOREACH
    SELECT id_cre_acreditado,
           sdo_deudor,
           edo_procesar,
           tpo_credito,
           tpo_originacion,
           edo_credito
      INTO v_ax_id_cre_acreditado,
           v_ax_sdo_deudor,
           v_ax_edo_procesar,
           v_ax_tpo_credito,
           v_ax_tpo_originacion,
           v_ax_edo_credito
      FROM cre_acreditado
     WHERE id_cre_ctr_archivo = p_ax_id_cre_ctr_arch

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_id_cre_acred = v_ax_id_cre_acreditado;

      -- se valida el saldo deudor obtenido
      IF v_ax_sdo_deudor IS NULL THEN
         LET v_ax_sdo_deudor = 0;
      END IF

      -- se hace la consulta del tipo de transferencia y el id deudor
       SELECT FIRST 1 DECODE(id_proceso,201,"03",301,"43"), id_deudor
         INTO v_ax_tpo_transferencia, v_ax_id_deudor
         FROM cat_tipo_credito
        WHERE tpo_credito = v_ax_tpo_credito
          AND tpo_originacion = v_ax_tpo_originacion;

      --se asignan valores al registro a insertar en his acreditados
      LET v_his_id_cre_acreditado  = v_ax_id_cre_acreditado;
      LET v_his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET v_his_tpo_transferencia  = v_ax_tpo_transferencia;
      LET v_his_edo_procesar       = v_ax_edo_procesar;
      LET v_his_diagnostico        = 0;
      LET v_his_estado             = 10;
      LET v_his_nss_afore          = "";
      LET v_his_rfc_afore          = "";
      LET v_his_paterno_afore      = "";
      LET v_his_materno_afore      = "";
      LET v_his_nombre_afore       = "";
      LET v_his_nom_imss           = "";
      LET v_his_f_proceso          = TODAY;

      --se inserta en la tabla his acreditado
      INSERT INTO cre_his_acreditado(
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  tpo_transferencia,
                  edo_procesar,
                  diagnostico,
                  estado,
                  nss_afore,
                  rfc_afore,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  f_proceso)
          VALUES (v_his_id_cre_acreditado,
                  v_his_id_cre_ctr_archivo,
                  v_his_tpo_transferencia,
                  v_his_edo_procesar,
                  v_his_diagnostico,
                  v_his_estado,
                  v_his_nss_afore,
                  v_his_rfc_afore,
                  v_his_paterno_afore,
                  v_his_materno_afore,
                  v_his_nombre_afore,
                  v_his_nom_imss,
                  v_his_f_proceso);

      -- si el id deudor es 1 indica que se inserta en la tabla de saldo deudor
      IF v_ax_id_deudor = 1  AND v_ax_edo_credito = 1 THEN
         -- se asigna el movimiento correspindiente
         LET v_ax_movimiento = 181;

         -- se asignan valores al registro a insertar en saldo deudor
         LET v_sdo_id_cre_acreditado = v_ax_id_cre_acreditado;
         LET v_sdo_folio_referencia  = p_d_folio;
         LET v_sdo_f_movimiento      = TODAY; -- VPD: antes se consultaba la f_actualiza de trasnf
         LET v_sdo_movimiento        = v_ax_movimiento;
         LET v_sdo_id_referencia     = v_ax_id_cre_acreditado;
         LET v_sdo_monto_aivs        = 0; --v_ax_sdo_deudor/v_ax_precio_accion;
         LET v_sdo_monto_pesos       = v_ax_sdo_deudor;
         LET v_sdo_f_proceso         = TODAY;

         -- se inserta en la tabla cre deudor
         INSERT INTO cre_saldo_deudor(
                     id_cre_acreditado,
                     folio_referencia,
                     f_movimiento,
                     movimiento,
                     id_referencia,
                     monto_aivs,
                     monto_pesos,
                     f_proceso)
             VALUES (v_sdo_id_cre_acreditado,
                     v_sdo_folio_referencia,
                     v_sdo_f_movimiento,
                     v_sdo_movimiento,
                     v_sdo_id_referencia,
                     v_sdo_monto_aivs,
                     v_sdo_monto_pesos,
                     v_sdo_f_proceso);
      END IF
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_id_cre_acred = 1;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

   RETURN v_error, v_isam_err, v_c_msj, v_c_id_cre_acred;

END FUNCTION;


