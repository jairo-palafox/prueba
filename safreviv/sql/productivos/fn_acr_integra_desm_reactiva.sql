






CREATE FUNCTION "safreviv".fn_acr_integra_desm_reactiva(p_v_usuario          CHAR(20),
                                             p_d_folio            DECIMAL(9,0),
                                             p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                             p_si_proceso_cod     SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250)
   -- Registro tmp acr desmarca
   DEFINE tmp_tpo_registro          CHAR(2);
   DEFINE tmp_nss                   CHAR(11);
   DEFINE tmp_num_credito           DECIMAL(10,0);
   DEFINE tmp_ssv_92_97             DECIMAL(8,0);
   DEFINE tmp_fec_otorgamiento      DATE;
   DEFINE tmp_fec_culminacion       DATE;
   DEFINE tmp_tpo_credito           DECIMAL(3,0);
   DEFINE tmp_sts_credito           DECIMAL(3,0);
   DEFINE tmp_tpo_descuento         DECIMAL(1,0);
   DEFINE tmp_val_descuento         DECIMAL(8,0);
   DEFINE tmp_nrp                   CHAR(11);
   DEFINE tmp_fec_ini_oblig_patron  DATE;
   DEFINE tmp_nss_liberado          CHAR(11);
   DEFINE tmp_fec_proceso           DATE;
   DEFINE tmp_sdo_credito           DECIMAL(8,0);
   DEFINE tmp_fec_prox_liquidar     DATE;
   DEFINE tmp_fec_dsd_avis_desast   DATE;
   DEFINE tmp_fec_hst_avis_desast   DATE;
   DEFINE tmp_tpo_rechazo           DECIMAL(2,0);
   -- Registro cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE rch_nss                   CHAR(11);
   DEFINE rch_tpo_originacion       SMALLINT;
   DEFINE rch_tpo_credito           SMALLINT;
   DEFINE rch_tpo_registro          CHAR(2);
   DEFINE rch_num_credito           DECIMAL(10,0);
   DEFINE rch_sdo_deudor            DECIMAL(12,2);
   DEFINE rch_f_otorga              DATE;
   DEFINE rch_f_culmina             DATE;
   DEFINE rch_edo_credito           SMALLINT;
   DEFINE rch_tpo_dscto             SMALLINT;
   DEFINE rch_valor_dscto           DECIMAL(8,4);
   DEFINE rch_nrp                   CHAR(11);
   DEFINE rch_f_ini_dscto           DATE;
   DEFINE rch_nss_liberado          CHAR(11);
   DEFINE rch_f_gen_arh             DATE;
   DEFINE rch_sdo_credito           DECIMAL(12,2);
   DEFINE rch_f_prox_liq            DATE;
   DEFINE rch_f_desde               DATE;
   DEFINE rch_f_hasta               DATE;
   DEFINE rch_tpo_rch               SMALLINT;
   DEFINE rch_estado                SMALLINT;
   -- Registro leido de cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);
   DEFINE cre_estado                SMALLINT; -- estado en la tabla maestro
   -- Registros de his acreditado
   DEFINE his_id_cre_acreditado     DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE his_tpo_transferencia     CHAR(2);
   DEFINE his_edo_procesar          SMALLINT;
   DEFINE his_diagnostico           CHAR(3);
   DEFINE his_estado                SMALLINT;
   DEFINE his_nss_afore             CHAR(11);
   DEFINE his_rfc_afore             CHAR(13);
   DEFINE his_paterno_afore         CHAR(40);
   DEFINE his_materno_afore         CHAR(40);
   DEFINE his_nombre_afore          CHAR(40);
   DEFINE his_nom_imss              CHAR(50);
   DEFINE his_f_proceso             DATE;
   -- Registro de cta credito
   DEFINE cta_id_derechohabiente    DECIMAL(9,0);
   DEFINE cta_proceso_cod           SMALLINT;
   DEFINE cta_tpo_credito           SMALLINT;
   DEFINE cta_num_credito           DECIMAL(10,0);
   DEFINE cta_f_credito             DATE;
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar a insertar
   DEFINE v_ax_estado               SMALLINT; -- estado a insertar
   DEFINE v_ax_id_lote_acpt         INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech         INTEGER; -- total de registros rechazados
   DEFINE v_ax_id_lote_sin_orig     INTEGER; -- total de registros sin originacion
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_id_deudor            SMALLINT; -- variable que indica si el tipo de crédito genera deudor
   DEFINE v_ax_precio_accion        DECIMAL(22,14); -- precio de acción para el día de hoy
   DEFINE v_ax_sum_monto_acc        DECIMAL(18,6); -- monto en acciones
   DEFINE v_ax_sum_monto_pss        DECIMAL(14,2); -- monto en pesos
   DEFINE v_ax_marca_inf            SMALLINT; -- marca infonavit
   DEFINE v_ax_sts_retorno          SMALLINT; -- status, retorno de alguna funcion o procedimiento
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_ax_isam_err             INTEGER;
   DEFINE v_ax_msj_err              VARCHAR(250);

   ON EXCEPTION SET v_ax_error, v_ax_isam_err, v_ax_msj_err
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error, v_ax_isam_err, v_ax_msj_err;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegDesm.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_ax_id_lote_sin_orig = 0;
   LET v_ax_error = 0;
   LET v_ax_isam_err = 0;
   LET v_ax_msj_err = 'El proceso finalizó correctamente';

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_ax_precio_accion
     FROM  glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   -- se procesan los registros para reactivación (Tipo de registro 04 y 08)
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_nss,
           tmp_num_credito,
           tmp_ssv_92_97,
           tmp_fec_otorgamiento,
           tmp_fec_culminacion,
           tmp_tpo_credito,
           tmp_sts_credito,
           tmp_tpo_descuento,
           tmp_val_descuento,
           tmp_nrp,
           tmp_fec_ini_oblig_patron,
           tmp_nss_liberado,
           tmp_fec_proceso,
           tmp_sdo_credito,
           tmp_fec_prox_liquidar,
           tmp_fec_dsd_avis_desast,
           tmp_fec_hst_avis_desast,
           tmp_tpo_rechazo
      FROM safre_tmp:tmp_acr_desmarca_04

      -- se inicializa la variable del derechohabiente
      LET v_ax_id_derechohabiente = NULL;
      LET cre_id_cre_acreditado = NULL;
      LET cre_estado = NULL;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM  afi_derechohabiente
       WHERE nss = tmp_nss;
      
      -- se valida que el NSS exista en catalogo
      IF v_ax_id_derechohabiente IS NULL THEN
         -- se asigna el estado de rechazo por Trabajador Inexistente
         LET v_ax_sts_registro = 11;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = tmp_ssv_92_97/100;
         LET rch_f_otorga           = tmp_fec_otorgamiento;
         LET rch_f_culmina          = tmp_fec_culminacion;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = tmp_val_descuento/10000;
         LET rch_nrp                = tmp_nrp;
         LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET rch_nss_liberado       = tmp_nss_liberado;
         LET rch_f_gen_arh          = tmp_fec_proceso;
         LET rch_sdo_credito        = tmp_sdo_credito/100;
         LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
         LET rch_f_desde            = tmp_fec_dsd_avis_desast;
         LET rch_f_hasta            = tmp_fec_hst_avis_desast;
         LET rch_tpo_rch            = tmp_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta en la tabla de rechazos
         INSERT INTO cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_f_otorga,
                     rch_f_culmina,
                     rch_edo_credito,
                     rch_tpo_dscto,
                     rch_valor_dscto,
                     rch_nrp,
                     rch_f_ini_dscto,
                     rch_nss_liberado,
                     rch_f_gen_arh,
                     rch_sdo_credito,
                     rch_f_prox_liq,
                     rch_f_desde,
                     rch_f_hasta,
                     rch_tpo_rch,
                     rch_estado);

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF
{
      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      FOREACH
       SELECT FIRST 1 tpo_originacion
         INTO v_ax_tpo_originacion
         FROM  cat_tipo_credito
        WHERE tpo_credito = tmp_tpo_credito
          AND tpo_originacion IN (1,4)
          AND f_actualiza <= tmp_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;
}
      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente      
      SELECT id_cre_acreditado, estado
        INTO cre_id_cre_acreditado, cre_estado
        FROM  cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND tpo_originacion IN (1,4)
         AND tpo_credito = tmp_tpo_credito
         AND num_credito = tmp_num_credito;

      -- se valida si fue posible identificar el registro correspondiente en la tabla maestro
      IF cre_id_cre_acreditado IS NULL THEN
         -- se asigna el estado de rechazo por 25-NÚMERO DE CRÉDITO NO EXISTE
         LET v_ax_sts_registro = 25;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = tmp_ssv_92_97/100;
         LET rch_f_otorga           = tmp_fec_otorgamiento;
         LET rch_f_culmina          = tmp_fec_culminacion;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = tmp_val_descuento/10000;
         LET rch_nrp                = tmp_nrp;
         LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET rch_nss_liberado       = tmp_nss_liberado;
         LET rch_f_gen_arh          = tmp_fec_proceso;
         LET rch_sdo_credito        = tmp_sdo_credito/100;
         LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
         LET rch_f_desde            = tmp_fec_dsd_avis_desast;
         LET rch_f_hasta            = tmp_fec_hst_avis_desast;
         LET rch_tpo_rch            = tmp_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta en la tabla de rechazos
         INSERT INTO cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_f_otorga,
                     rch_f_culmina,
                     rch_edo_credito,
                     rch_tpo_dscto,
                     rch_valor_dscto,
                     rch_nrp,
                     rch_f_ini_dscto,
                     rch_nss_liberado,
                     rch_f_gen_arh,
                     rch_sdo_credito,
                     rch_f_prox_liq,
                     rch_f_desde,
                     rch_f_hasta,
                     rch_tpo_rch,
                     rch_estado);

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se valida que el registro en la tabla maestro se encuentre con estado Desmarcado Crédito Liquidado (170)
      IF cre_estado <> 170 THEN
         -- se asigna el estado de rechazo por 15-ESTADO DEL CREDITO NO VALIDO
         LET v_ax_sts_registro = 15;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = tmp_ssv_92_97/100;
         LET rch_f_otorga           = tmp_fec_otorgamiento;
         LET rch_f_culmina          = tmp_fec_culminacion;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = tmp_val_descuento/10000;
         LET rch_nrp                = tmp_nrp;
         LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET rch_nss_liberado       = tmp_nss_liberado;
         LET rch_f_gen_arh          = tmp_fec_proceso;
         LET rch_sdo_credito        = tmp_sdo_credito/100;
         LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
         LET rch_f_desde            = tmp_fec_dsd_avis_desast;
         LET rch_f_hasta            = tmp_fec_hst_avis_desast;
         LET rch_tpo_rch            = tmp_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta en la tabla de rechazos
         INSERT INTO cre_rch_acreditado (
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_credito,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     f_otorga,
                     f_culmina,
                     edo_credito,
                     tpo_dscto,
                     valor_dscto,
                     nrp,
                     f_ini_dscto,
                     nss_liberado,
                     f_gen_arh,
                     sdo_credito,
                     f_prox_liq,
                     f_desde,
                     f_hasta,
                     tpo_rch,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_credito,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_f_otorga,
                     rch_f_culmina,
                     rch_edo_credito,
                     rch_tpo_dscto,
                     rch_valor_dscto,
                     rch_nrp,
                     rch_f_ini_dscto,
                     rch_nss_liberado,
                     rch_f_gen_arh,
                     rch_sdo_credito,
                     rch_f_prox_liq,
                     rch_f_desde,
                     rch_f_hasta,
                     rch_tpo_rch,
                     rch_estado);

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se el id deudor para el tipo de crédito en proceso
      FOREACH
       SELECT FIRST 1 id_deudor, marca_inf
         INTO v_ax_id_deudor, v_ax_marca_inf
         FROM  cat_tipo_credito
        WHERE tpo_credito = tmp_tpo_credito
          AND tpo_originacion IN (1,4)
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se valida el id deudor
      IF v_ax_id_deudor = 1 THEN
         -- se consulta el monto en acciones de cta movimiento para el derechohabiente en proceso
         SELECT SUM(monto_acciones)
           INTO v_ax_sum_monto_acc
           FROM  cta_movimiento
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND subcuenta IN (4,8,42,44)
            AND fondo_inversion = 11;

         -- se calculan los pesos
         LET v_ax_sum_monto_pss = v_ax_sum_monto_acc * v_ax_precio_accion;

         -- se valida que el monto calculado sea mayor igual a $1000.00
         IF v_ax_sum_monto_pss >= 1000 THEN
            -- se asignan los estados a insertar
            LET v_ax_estado       = 25;
            LET v_ax_edo_procesar = 70;
         ELSE
            -- se asignan los estados a insertar
            LET v_ax_estado       = 140;
            LET v_ax_edo_procesar = 10;
         END IF
      ELSE
         -- se asignan los estados a insertar
         LET v_ax_estado       = 140;
         LET v_ax_edo_procesar = 10;
      END IF

      -- se reactiva el credito en la tabla maestro
      UPDATE  cre_acreditado
         SET estado = v_ax_estado,
             edo_procesar = v_ax_edo_procesar
       WHERE id_cre_acreditado = cre_id_cre_acreditado
         AND id_derechohabiente = v_ax_id_derechohabiente
         AND tpo_originacion IN (1,4)
         AND tpo_credito = tmp_tpo_credito
         AND num_credito = tmp_num_credito;

      -- se asgina los valores del registro de his acreditado
      LET his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = "03"; -- 03.- Transferencia de Acreditados
      LET his_edo_procesar       = v_ax_edo_procesar;
      LET his_diagnostico        = NULL;
      LET his_estado             = v_ax_estado;
      LET his_nss_afore          = NULL;
      LET his_rfc_afore          = NULL;
      LET his_paterno_afore      = NULL;
      LET his_materno_afore      = NULL;
      LET his_nombre_afore       = NULL;
      LET his_nom_imss           = NULL;
      LET his_f_proceso          = TODAY;

      -- se inserta en cre his acreditado
      INSERT INTO cre_his_acreditado (
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
          VALUES (his_id_cre_acreditado,
                  his_id_cre_ctr_archivo,
                  his_tpo_transferencia,
                  his_edo_procesar,
                  his_diagnostico,
                  his_estado,
                  his_nss_afore,
                  his_rfc_afore,
                  his_paterno_afore,
                  his_materno_afore,
                  his_nombre_afore,
                  his_nom_imss,
                  his_f_proceso);

      -- se valida que no exista el registro en cta credito
      IF NOT EXISTS (
         SELECT id_derechohabiente
           FROM  cta_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND tpo_credito = tmp_tpo_credito
            AND num_credito = tmp_num_credito) THEN
         -- se inserta registro nuevo en cta credito
         LET cta_id_derechohabiente = v_ax_id_derechohabiente;
         LET cta_proceso_cod        = p_si_proceso_cod;
         LET cta_tpo_credito        = tmp_tpo_credito;
         LET cta_num_credito        = tmp_num_credito;
         LET cta_f_credito          = tmp_fec_otorgamiento;

         IF cta_f_credito IS NULL THEN
            LET cta_f_credito = TODAY;
         END IF

         -- se inserta el registro en cta credito
         INSERT INTO  cta_credito (
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

         -- se elimina el registro de cta his credito
         DELETE
           FROM  cta_his_credito
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND num_credito = tmp_num_credito;
      END IF

      -- se valida que no exista la marca
      IF NOT EXISTS (
      SELECT id_derechohabiente
        FROM  sfr_marca_activa
       WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN

         -- se ejecuta la función de marcaje
         EXECUTE FUNCTION  fn_marca_cuenta(
                          v_ax_id_derechohabiente,
                          v_ax_marca_inf,
                          cre_id_cre_acreditado,
                          p_d_folio,
                          0, -- estado marca
                          0, -- código de rechazo
                          NULL, -- marca causa
                          "", -- fecha causa
                          p_v_usuario,
                          p_si_proceso_cod)
                     INTO v_ax_sts_retorno;
      END IF

      -- se incrementa el contador de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
   END FOREACH;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE  cre_acreditado;
   UPDATE STATISTICS FOR TABLE  cre_his_acreditado;

   RETURN v_ax_error, v_ax_isam_err, v_ax_msj_err;
END FUNCTION;


