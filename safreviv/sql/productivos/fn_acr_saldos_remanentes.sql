






CREATE FUNCTION "safreviv".fn_acr_saldos_remanentes(p_v_usuario CHAR(20),
                                         p_d_monto_validar DECIMAL(7,2))
   RETURNING SMALLINT, INTEGER, VARCHAR(250)

   -- registro de cre acreditado
   DEFINE v_cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE v_cre_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE v_cre_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_cre_estado               SMALLINT;
   DEFINE v_cre_edo_procesar         SMALLINT;

   -- registro temporal de saldos remanentes
   DEFINE v_tmp_id_cre_acreditado    DECIMAL(9,0);
   DEFINE v_tmp_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE v_tmp_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_tmp_estado               SMALLINT;
   DEFINE v_tmp_edo_procesar         SMALLINT;

   -- registro de cre his acreditado
   DEFINE v_his_id_cre_acreditado    DECIMAL(9,0);
   DEFINE v_his_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE v_his_tpo_transferencia    CHAR(2);
   DEFINE v_his_edo_procesar         SMALLINT;
   DEFINE v_his_diagnostico          CHAR(3);
   DEFINE v_his_estado               SMALLINT;
   DEFINE v_his_nss_afore            CHAR(11);
   DEFINE v_his_rfc_afore            CHAR(13);
   DEFINE v_his_paterno_afore        CHAR(40);
   DEFINE v_his_materno_afore        CHAR(40);
   DEFINE v_his_nombre_afore         CHAR(40);
   DEFINE v_his_nom_imss             CHAR(50);
   DEFINE v_his_f_proceso            DATE;

   -- variables auxiliares
   DEFINE v_ax_precio_fondo          DECIMAL(19,14); -- precio de la acción
   DEFINE v_ax_acc_validar           DECIMAL(9,4); -- precio de la acción del monto a validar
   DEFINE v_ax_acc_sdo_diario        decimal(20,2); -- saldo en acciones de la tabla saldo diario
   DEFINE v_ax_tpo_originacion       SMALLINT;

   --DEFINE v_ax_monto_acciones        DECIMAL(28,6);
   DEFINE v_error                    SMALLINT; -- codigo de error en caso de excepción
   DEFINE v_isam_err                 INTEGER;
   DEFINE v_c_msj                    VARCHAR(250);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrSaldoReman.trace';
   --TRACE ON;

   SET PDQPRIORITY HIGH;

   -- se inicializa las variables
   LET v_ax_tpo_originacion = 1;
   LET v_error              = 0;
   LET v_isam_err           = 0;
   LET v_c_msj              = 'El proceso finalizó correctamente';

   -- se obtiene el precio de accion para la fecha de hoy
   SELECT precio_fondo
     INTO v_ax_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   -- se obtienen las acciones del monto a validar
   LET v_ax_acc_validar = p_d_monto_validar / v_ax_precio_fondo;

   -- se obtienen los registros de cre acreditado con estado Liquidado (140)
   FOREACH
   SELECT UNIQUE id_derechohabiente
     INTO v_cre_id_derechohabiente
     FROM safre_viv:cre_acreditado
    WHERE tpo_originacion = v_ax_tpo_originacion
      AND estado IN (140,900,220)
      AND edo_procesar IN (120,5)

      -- se obtiene la información del derechohabiente seleccionado
      FOREACH
       SELECT FIRST 1
              id_cre_acreditado,
              id_cre_ctr_archivo,
              estado,
              edo_procesar
         INTO v_cre_id_cre_acreditado,
              v_cre_id_cre_ctr_archivo,
              v_cre_estado,
              v_cre_edo_procesar
         FROM safre_viv:cre_acreditado
        WHERE id_derechohabiente = v_cre_id_derechohabiente
          AND tpo_originacion = v_ax_tpo_originacion
          AND estado IN (140,900,220)
          AND edo_procesar IN (120,5)
        ORDER BY f_otorga DESC, estado
         -- se asignan los valores en el registro a insertar
         LET v_tmp_id_cre_acreditado  = v_cre_id_cre_acreditado;
         LET v_tmp_id_cre_ctr_archivo = v_cre_id_cre_ctr_archivo;
         LET v_tmp_id_derechohabiente = v_cre_id_derechohabiente;
         LET v_tmp_estado             = v_cre_estado;
         LET v_tmp_edo_procesar       = v_cre_edo_procesar;

         -- se inserta el registro en la temporal
         INSERT INTO safre_tmp:tmp_saldo_rem(
                     id_cre_acreditado,
                     id_cre_ctr_archivo,
                     id_derechohabiente,
                     estado,
                     edo_procesar)
             VALUES (v_tmp_id_cre_acreditado,
                     v_tmp_id_cre_ctr_archivo,
                     v_tmp_id_derechohabiente,
                     v_tmp_estado,
                     v_tmp_edo_procesar);
      END FOREACH
   END FOREACH

   -- se obtienen los registros de cre acreditado con estado Liquidado (140)
   FOREACH
   SELECT tmp.id_cre_acreditado,
          tmp.id_cre_ctr_archivo,
          tmp.id_derechohabiente,
          tmp.edo_procesar,
          SUM(cta.monto_acciones)
     INTO v_tmp_id_cre_acreditado,
          v_tmp_id_cre_ctr_archivo,
          v_tmp_id_derechohabiente,
          v_tmp_edo_procesar,
          v_ax_acc_sdo_diario
     FROM safre_tmp:tmp_saldo_rem tmp, safre_viv:cta_movimiento cta
    WHERE tmp.id_derechohabiente = cta.id_derechohabiente
      AND cta.subcuenta > 0
    GROUP BY 1,2,3,4

      -- se valida que el importe seleccionado del derechohabiente sea mayor igual al ingresado por
      -- el usuario, de lo contrario continua con el siguiente registro
      IF v_ax_acc_sdo_diario < v_ax_acc_validar THEN
         CONTINUE FOREACH;
      END IF

      -- se verifica si el registro es de SI
      IF v_tmp_edo_procesar = 5 THEN
         LET v_his_edo_procesar = v_tmp_edo_procesar;
      ELSE
         LET v_his_edo_procesar = 70;
      END IF

      -- se asignan los valores del registro a insertar
      LET v_his_id_cre_acreditado  = v_tmp_id_cre_acreditado;
      LET v_his_id_cre_ctr_archivo = v_tmp_id_cre_ctr_archivo;
      LET v_his_tpo_transferencia  = NULL;
      LET v_his_diagnostico        = NULL;
      LET v_his_estado             = 25;
      LET v_his_nss_afore          = NULL;
      LET v_his_rfc_afore          = NULL;
      LET v_his_paterno_afore      = NULL;
      LET v_his_materno_afore      = NULL;
      LET v_his_nombre_afore       = NULL;
      LET v_his_nom_imss           = NULL;
      LET v_his_f_proceso          = TODAY;

      -- se inserta en la tabla cre his acreditado
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

      -- se actualiza el registro leído de cre acreditado a estado 70-Por Reenviar
      IF v_tmp_edo_procesar = 5 THEN
         -- se actualiza unicamente el estado a 25
         UPDATE safre_viv:cre_acreditado
            SET estado = 25
          WHERE id_cre_acreditado = v_tmp_id_cre_acreditado
            AND tpo_originacion = v_ax_tpo_originacion
            AND estado IN(140,900,220)
            AND edo_procesar = 5;
      ELSE
         -- se actualiza estado a 25 y estado procesar a 70
         UPDATE safre_viv:cre_acreditado
            SET estado = 25,
                edo_procesar = 70
          WHERE id_cre_acreditado = v_tmp_id_cre_acreditado
            AND tpo_originacion = v_ax_tpo_originacion
            AND estado IN(140,900,220)
            AND edo_procesar = 120;
      END IF
   END FOREACH

   SET PDQPRIORITY DEFAULT;

   RETURN v_error, v_isam_err, v_c_msj;
END FUNCTION
;


