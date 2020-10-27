






CREATE FUNCTION "safreviv".fn_agr_integra_uso_anualidad(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                             p_d_folio           DECIMAL(9,0),
                                             p_v_usuario         CHAR(20))
   RETURNING SMALLINT;
   -- REGISTRO de la temporal
   DEFINE tmp_tpo_registro         CHAR(2);
   DEFINE tmp_nss                  CHAR(11);
   DEFINE tmp_num_cred             CHAR(10);
   DEFINE tmp_f_envio              DATE;
   DEFINE tmp_monto_solicitado     DECIMAL(15,0); --13,2
   DEFINE tmp_tipo_peticion        CHAR(1);
   -- REGISTRO uso garantia
   DEFINE uso_id_cre_uso_garantia  DECIMAL(9,0);
   DEFINE uso_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE uso_folio_liquida        DECIMAL(9,0);
   DEFINE uso_id_derechohabiente   DECIMAL(9,0);
   DEFINE uso_tpo_transferencia    CHAR(2);
   DEFINE uso_tpo_uso              SMALLINT;
   DEFINE uso_num_credito          DECIMAL(10,0);
   DEFINE uso_f_presentacion       DATE;
   DEFINE uso_f_movimiento         DATE;
   DEFINE uso_periodo_pago         CHAR(6);
   DEFINE uso_importe_v97          DECIMAL(22,2);
   DEFINE uso_nss_afore            CHAR(11);
   DEFINE uso_rfc_afore            CHAR(13);
   DEFINE uso_paterno_afore        CHAR(40);
   DEFINE uso_materno_afore        CHAR(40);
   DEFINE uso_nombre_afore         CHAR(40);
   DEFINE uso_nom_imss             CHAR(50);
   DEFINE uso_edo_procesar         SMALLINT;
   DEFINE uso_diagnostico          CHAR(3);
   DEFINE uso_estado               SMALLINT;
   DEFINE uso_f_proceso            DATE;
   -- Campos auxiliares
   DEFINE v_ax_nom_archivo         CHAR(40); -- nombre del archivo
   DEFINE v_ax_tpo_transferencia   CHAR(2); -- tipo de transferencia
   DEFINE v_ax_tpo_originacion     SMALLINT; -- tipo de originacion
   DEFINE v_ax_mto_recuperad       DECIMAL(13,2); -- saldo vivienda 97
   DEFINE v_ax_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador     CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt         INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech         INTEGER; -- contador de registros rechazados
   DEFINE v_i_estado               SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_sts_registro        SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_diagnostico         CHAR(3); -- diagnóstico del proceso
   DEFINE v_ax_edo_procesar        SMALLINT; -- estado procesar
   DEFINE r_ax_bandera             SMALLINT; -- valor de regreso de la actualización
   DEFINE v_error                  SMALLINT; -- codigo de error en caso de excepción

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegUsoAnual.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt = 0;
   LET v_ax_cuenta_rech = 0;
   LET v_error = 0;
   LET v_i_estado = 2; -- estado Integrado en la tabla glo ctr archivo
   LET v_ax_tpo_transferencia = "43"; -- 43-Anualidades Garantizadas
   LET v_ax_tpo_originacion = 4; -- 4-Anualidades Garantizadas

   SELECT nom_archivo
     INTO v_ax_nom_archivo
     FROM safre_viv:cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;

   --------------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE USO DE ANUALIDAD O GARANTIA --
   --------------------------------------------------------------
   FOREACH
      -- se obtienen los datos de la tabla temporal
      SELECT *
      INTO tmp_tpo_registro,
           tmp_nss,
           tmp_num_cred,
           tmp_f_envio,
           tmp_monto_solicitado,
           tmp_tipo_peticion
      FROM safre_tmp:tmp_uso_det_agr

      -- se inicializan variables
      LET v_ax_sts_registro = 10;
      LET v_ax_id_derechohabiente = NULL;
      LET v_ax_diagnostico = NULL;

      -- se obtiene el id del derechohabiente y el tipo trabajador para el nss
      SELECT id_derechohabiente, tipo_trabajador
      INTO v_ax_id_derechohabiente, v_ax_tipo_trabajador
      FROM safre_viv:afi_derechohabiente
      WHERE nss = tmp_nss;

      -- se valida el id_derechohabiente
      IF v_ax_id_derechohabiente IS NULL THEN
         LET v_ax_sts_registro = 240;
         LET v_ax_diagnostico = "11";
      END IF

      -- se valida el tipo de trabajador
      IF v_ax_tipo_trabajador = "I" THEN
         LET v_ax_edo_procesar = 10;
      ELSE
         LET v_ax_edo_procesar = 7;
      END IF

      -- se verifica que el registro no ha sido rechazado
      IF v_ax_sts_registro <> 240 THEN
         -- se comprueba que derechohabiente se encuentre en la tabla maestro
         IF NOT EXISTS (
         SELECT id_derechohabiente
         FROM safre_viv:cre_acreditado
         WHERE id_derechohabiente = v_ax_id_derechohabiente
           AND tpo_originacion = v_ax_tpo_originacion) THEN
            LET v_ax_sts_registro = 240;
            LET v_ax_diagnostico = "13";
         END IF
      END IF

      -- se verifica que el registro no ha sido rechazado
      IF v_ax_sts_registro = 240 THEN
         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;
      ELSE
         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;
      END IF

      -- se calculan los valores
      LET v_ax_mto_recuperad = tmp_monto_solicitado / 100;

      -- se valida el número de crédito. Este no puede ser nulo
      IF tmp_num_cred IS NULL OR
         tmp_num_cred = "" THEN
         -- se asigna cero al número de credíto
         LET tmp_num_cred = 0;
      END IF

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
      LET uso_id_cre_ctr_archivo  = p_d_id_cre_ctr_arch;
      LET uso_folio_liquida       = 0;
      LET uso_id_derechohabiente  = v_ax_id_derechohabiente;
      LET uso_tpo_transferencia   = v_ax_tpo_transferencia;
      LET uso_tpo_uso             = tmp_tipo_peticion;
      LET uso_num_credito         = tmp_num_cred;
      LET uso_f_presentacion      = tmp_f_envio;
      LET uso_f_movimiento        = TODAY;
      LET uso_periodo_pago        = NULL;
      LET uso_importe_v97         = v_ax_mto_recuperad;
      LET uso_nss_afore           = NULL;
      LET uso_rfc_afore           = NULL;
      LET uso_paterno_afore       = NULL;
      LET uso_materno_afore       = NULL;
      LET uso_nombre_afore        = NULL;
      LET uso_nom_imss            = NULL;
      LET uso_edo_procesar        = v_ax_edo_procesar;
      LET uso_diagnostico         = v_ax_diagnostico;
      LET uso_estado              = v_ax_sts_registro;
      LET uso_f_proceso           = TODAY;

      -- se inserta registro en la tabla maestro
      INSERT INTO safre_viv:cre_uso_garantia(
                  id_cre_uso_garantia,
                  id_cre_ctr_archivo,
                  folio_liquida,
                  id_derechohabiente,
                  tpo_transferencia,
                  tpo_uso,
                  num_credito,
                  f_presentacion,
                  f_movimiento,
                  periodo_pago,
                  importe_v97,
                  nss_afore,
                  rfc_afore,
                  paterno_afore,
                  materno_afore,
                  nombre_afore,
                  nom_imss,
                  edo_procesar,
                  diagnostico,
                  estado,
                  f_proceso)
          VALUES (uso_id_cre_uso_garantia,
                  uso_id_cre_ctr_archivo,
                  uso_folio_liquida,
                  uso_id_derechohabiente,
                  uso_tpo_transferencia,
                  uso_tpo_uso,
                  uso_num_credito,
                  uso_f_presentacion,
                  uso_f_movimiento,
                  uso_periodo_pago,
                  uso_importe_v97,
                  uso_nss_afore,
                  uso_rfc_afore,
                  uso_paterno_afore,
                  uso_materno_afore,
                  uso_nombre_afore,
                  uso_nom_imss,
                  uso_edo_procesar,
                  uso_diagnostico,
                  uso_estado,
                  uso_f_proceso);
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cre_uso_garantia;

   -- se ejecuta el sp que actualiza el registro de la tabla de control de archivos a estatus "Integrado"
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(v_ax_nom_archivo, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, v_ax_cuenta_acpt, v_ax_cuenta_rech, 0, p_d_id_cre_ctr_arch);

   RETURN v_error;
END FUNCTION
;


