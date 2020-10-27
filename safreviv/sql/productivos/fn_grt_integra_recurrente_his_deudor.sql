






CREATE FUNCTION "safreviv".fn_grt_integra_recurrente_his_deudor(p_d_folio DECIMAL(9,0),
                                                     p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT;
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
   DEFINE v_sdo_monto_aivs         DECIMAL(22,2);
   DEFINE v_sdo_monto_pesos        DECIMAL(22,2);
   DEFINE v_sdo_f_proceso          DATE;
   -- Variables auxiliares
   DEFINE v_ax_movimiento          SMALLINT;
   DEFINE v_ax_precio_accion       DECIMAL(22,14);
   DEFINE v_ax_id_cre_acreditado   DECIMAL(9,0); 
   DEFINE v_ax_sdo_deudor          DECIMAL(22,2);
   DEFINE v_ax_edo_procesar        SMALLINT;
   DEFINE v_ax_tpo_credito         SMALLINT;
   DEFINE v_ax_tpo_transferencia   CHAR(2);
   DEFINE v_ax_id_deudor           SMALLINT;
   DEFINE v_error                  SMALLINT;

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtIntegRecurrHisDeudor.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_error = 0;

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_ax_precio_accion
     FROM safre_viv:glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   FOREACH
    SELECT id_cre_acreditado, sdo_deudor, edo_procesar, tpo_credito
      INTO v_ax_id_cre_acreditado, v_ax_sdo_deudor, v_ax_edo_procesar, v_ax_tpo_credito
      FROM safre_viv:cre_acreditado
     WHERE id_cre_ctr_archivo = p_ax_id_cre_ctr_arch
      -- se hace la consulta del tipo de transferencia y el id deudor
      FOREACH
       SELECT FIRST 1 DECODE(id_proceso,201,"03",1201,"16",301,"43"), id_deudor
         INTO v_ax_tpo_transferencia, v_ax_id_deudor
         FROM safre_viv:cat_tipo_credito
        WHERE tpo_credito = v_ax_tpo_credito
      END FOREACH;

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
      INSERT INTO safre_viv:cre_his_acreditado(
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
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cre_his_acreditado;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cre_saldo_deudor;

   RETURN v_error;
END FUNCTION;


