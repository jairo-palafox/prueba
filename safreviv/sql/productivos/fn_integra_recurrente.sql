






CREATE FUNCTION "safreviv".fn_integra_recurrente(p_d_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, INTEGER;
   -- REGISTRO tmp acr transferencia
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10,0);
   DEFINE v_rt_ssv_92_97            DECIMAL(8,0);
   DEFINE v_rt_fec_otorgamiento     DATE;
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_credito          CHAR(3);
   DEFINE v_rt_sts_credito          CHAR(3);
   DEFINE v_rt_tpo_descuento        DECIMAL(1,0);
   DEFINE v_rt_val_descuento        DECIMAL(8,0);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(10,0);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2,0);
   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE cre_folio_liquida        DECIMAL(9,0);
   DEFINE cre_id_derechohabiente   DECIMAL(9,0);
   DEFINE cre_tpo_originacion      SMALLINT;
   DEFINE cre_tpo_credito          SMALLINT;
   DEFINE cre_tpo_registro         CHAR(2);
   DEFINE cre_num_credito          DECIMAL(10,0);
   DEFINE cre_sdo_deudor           DECIMAL(12,2);
   DEFINE cre_f_otorga             DATE;
   DEFINE cre_f_culmina            DATE;
   DEFINE cre_edo_credito          SMALLINT;
   DEFINE cre_tpo_dscto            SMALLINT;
   DEFINE cre_valor_dscto          DECIMAL(8,4);
   DEFINE cre_nrp                  CHAR(11);
   DEFINE cre_f_ini_dscto          DATE;
   DEFINE cre_nss_liberado         CHAR(11);
   DEFINE cre_f_gen_arh            DATE;
   DEFINE cre_sdo_credito          DECIMAL(12,2);
   DEFINE cre_f_prox_liq           DATE;
   DEFINE cre_f_desde              DATE;
   DEFINE cre_f_hasta              DATE;
   DEFINE cre_tpo_rch              SMALLINT;
   DEFINE cre_edo_procesar         SMALLINT;
   DEFINE cre_estado               SMALLINT;
   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE rch_nss                  CHAR(11);
   DEFINE rch_tpo_originacion      SMALLINT;
   DEFINE rch_tpo_credito          SMALLINT;
   DEFINE rch_tpo_registro         CHAR(2);
   DEFINE rch_num_credito          DECIMAL(10,0);
   DEFINE rch_sdo_deudor           DECIMAL(12,2);
   DEFINE rch_f_otorga             DATE;
   DEFINE rch_f_culmina            DATE;
   DEFINE rch_edo_credito          SMALLINT;
   DEFINE rch_tpo_dscto            SMALLINT;
   DEFINE rch_valor_dscto          DECIMAL(8,4);
   DEFINE rch_nrp                  CHAR(11);
   DEFINE rch_f_ini_dscto          DATE;
   DEFINE rch_nss_liberado         CHAR(11);
   DEFINE rch_f_gen_arh            DATE;
   DEFINE rch_sdo_credito          DECIMAL(12,2);
   DEFINE rch_f_prox_liq           DATE;
   DEFINE rch_f_desde              DATE;
   DEFINE rch_f_hasta              DATE;
   DEFINE rch_tpo_rch              SMALLINT;
   DEFINE rch_estado               SMALLINT;
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador    CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt        INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech        INTEGER; -- contador de registros rechazados
   DEFINE v_existe_derech_vig     SMALLINT; -- contador de registros con id derechohabiente vigentes
   DEFINE v_existe_numcred        SMALLINT; -- contador de registros con numero de crédito existente
   DEFINE v_ax_sts_registro       SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_sdo_cred_aux       DECIMAL(12,2); -- saldo del credito
   DEFINE v_ax_ssv_92_97          DECIMAL(12,2); -- saldo deudor
   DEFINE v_ax_valor_dscto        DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion    SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_credito        SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito         SMALLINT; -- identificador del crédito
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código

   ON EXCEPTION SET v_error
      -- verifica si el error se debe a:
      -- 239 Could not insert new row - duplicate value in a UNIQUE INDEX column
      IF v_error = -239 THEN
         -- se inserta el registro en la tabla de rechazos con estado = 17
         LET v_ax_sts_registro = 17;

         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO safre_viv:cre_rch_acreditado (
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
      ELSE
        -- Devolvera el codigo de error cuando ocurra una excepción diferente a -239
        RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
      END IF
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegRecurr.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt    = 0;
   LET v_ax_cuenta_rech    = 0;
   LET v_error             = 0;
   LET v_existe_derech_vig = 0;
   LET v_existe_numcred    = 0;

   --------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS INEXISTENTES EN CATÁLOGO --
   --------------------------------------------------------
   FOREACH
    SELECT *
      INTO v_rt_tpo_registro,
           v_rt_nss,
           v_rt_num_credito,
           v_rt_ssv_92_97,
           v_rt_fec_otorgamiento,
           v_rt_fec_culminacion,
           v_rt_tpo_credito,
           v_rt_sts_credito,
           v_rt_tpo_descuento,
           v_rt_val_descuento,
           v_rt_nrp,
           v_rt_fec_ini_oblig_patron,
           v_rt_nss_liberado,
           v_rt_fec_proceso,
           v_rt_sdo_credito,
           v_rt_fec_prox_liquidar,
           v_rt_fec_dsd_avis_desast,
           v_rt_fec_hst_avis_desast,
           v_rt_tpo_rechazo
      FROM safre_tmp:tmp_acr_transferencia
     WHERE nss NOT IN (
           SELECT nss
             FROM safre_viv:afi_derechohabiente)
      -- se asigna el estatud de error
      LET v_ax_sts_registro = 11; -- 11 Derechohabiente no existe en maestro

      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      FOREACH
       SELECT FIRST 1 tpo_originacion
         INTO v_ax_tpo_originacion
         FROM safre_viv:cat_tipo_credito
        WHERE tpo_credito = v_rt_tpo_credito
          AND f_actualiza <= v_rt_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se incrementa el numero de registros rechazados
      LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = v_rt_sdo_credito / 100;
      LET v_ax_ssv_92_97 = v_rt_ssv_92_97 / 100;
      LET v_ax_valor_dscto = v_rt_val_descuento / 10000;

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
      LET rch_nss                = v_rt_nss;
      LET rch_tpo_originacion    = v_ax_tpo_originacion;
      LET rch_tpo_credito        = v_rt_tpo_credito;
      LET rch_tpo_registro       = v_rt_tpo_registro;
      LET rch_num_credito        = v_rt_num_credito;
      LET rch_sdo_deudor         = v_ax_ssv_92_97;
      LET rch_f_otorga           = v_rt_fec_otorgamiento;
      LET rch_f_culmina          = v_rt_fec_culminacion;
      LET rch_edo_credito        = v_rt_sts_credito;
      LET rch_tpo_dscto          = v_rt_tpo_descuento;
      LET rch_valor_dscto        = v_ax_valor_dscto;
      LET rch_nrp                = v_rt_nrp;
      LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
      LET rch_nss_liberado       = v_rt_nss_liberado;
      LET rch_f_gen_arh          = v_rt_fec_proceso;
      LET rch_sdo_credito        = v_ax_sdo_cred_aux;
      LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
      LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
      LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
      LET rch_tpo_rch            = v_rt_tpo_rechazo;
      LET rch_estado             = v_ax_sts_registro;

      -- se inserta registro
      INSERT INTO safre_viv:cre_rch_acreditado (
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
   END FOREACH;

   -----------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE NUEVOS ACREDITADOS --
   -----------------------------------------------------
   -- se obtienen los datos de la tabla temporal del proceso de Recurrente tipo de registro (01 y 20)
   FOREACH
    SELECT t.*,
           a.id_derechohabiente,
           a.tipo_trabajador
      INTO v_rt_tpo_registro,
           v_rt_nss,
           v_rt_num_credito,
           v_rt_ssv_92_97,
           v_rt_fec_otorgamiento,
           v_rt_fec_culminacion,
           v_rt_tpo_credito,
           v_rt_sts_credito,
           v_rt_tpo_descuento,
           v_rt_val_descuento,
           v_rt_nrp,
           v_rt_fec_ini_oblig_patron,
           v_rt_nss_liberado,
           v_rt_fec_proceso,
           v_rt_sdo_credito,
           v_rt_fec_prox_liquidar,
           v_rt_fec_dsd_avis_desast,
           v_rt_fec_hst_avis_desast,
           v_rt_tpo_rechazo,
           v_ax_id_derechohabiente,
           v_ax_tipo_trabajador
      FROM safre_tmp:tmp_acr_transferencia t,
           safre_viv:afi_derechohabiente a
     WHERE t.nss = a.nss
{
      -- se obtiene el id del derechohabiente y el tipo trabajador para el nss
      SELECT id_derechohabiente, tipo_trabajador
      INTO v_ax_id_derechohabiente, v_ax_tipo_trabajador
      FROM safre_viv:afi_derechohabiente
      WHERE nss = v_rt_nss;
}
      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      FOREACH
       SELECT FIRST 1 tpo_credito, tpo_originacion
         INTO v_ax_tpo_credito, v_ax_tpo_originacion
         FROM safre_viv:cat_tipo_credito
        WHERE tpo_credito = v_rt_tpo_credito
          AND f_actualiza <= v_rt_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
      IF EXISTS (
      SELECT id_derechohabiente
        FROM cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND estado < 170) THEN
         -- se prende la bandera
         LET v_existe_derech_vig = 1;
      ELSE
         -- se mantiene la bandera apagada
         LET v_existe_derech_vig = 0;
      END IF

      -- se valida que el num crédito no exista en la tabla maestro para el mismo tipo de originación
      IF EXISTS (
      SELECT id_derechohabiente
        FROM cre_acreditado
       WHERE edo_credito = 1
         AND tpo_originacion = v_ax_tpo_originacion
         AND num_credito = v_rt_num_credito) THEN
         -- se prende la bandera
         LET v_existe_numcred = 1;
      ELSE
         -- se prende la bandera
         LET v_existe_numcred = 0;
      END IF

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = v_rt_sdo_credito / 100;
      LET v_ax_ssv_92_97 = v_rt_ssv_92_97 / 100;
      LET v_ax_valor_dscto = v_rt_val_descuento / 10000;

      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente no debe estar en catalogo (no debe ser nulo)
      -- * el estatus del credito debe ser igual a 1
      -- * el tipo de credito debe ser de originación 1 o 4 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL AND
         v_rt_sts_credito = 1 AND
         v_ax_tpo_credito IS NOT NULL AND
         v_existe_derech_vig = 0 AND
         v_existe_numcred = 0 THEN
         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;

         -- se valida el tipo de trabajador
         IF v_ax_tipo_trabajador = "I" THEN
            LET v_ax_edo_procesar = 10;
         ELSE
            LET v_ax_edo_procesar = 5;
         END IF

         -- se asigna que el registro no fue rechazado
         LET v_ax_sts_registro = 10;
         
         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
         LET cre_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET cre_folio_liquida      = 0;
         LET cre_id_derechohabiente = v_ax_id_derechohabiente;
         LET cre_tpo_originacion    = v_ax_tpo_originacion;
         LET cre_tpo_credito        = v_rt_tpo_credito;
         LET cre_tpo_registro       = v_rt_tpo_registro;
         LET cre_num_credito        = v_rt_num_credito;
         LET cre_sdo_deudor         = v_ax_ssv_92_97;
         LET cre_f_otorga           = v_rt_fec_otorgamiento;
         LET cre_f_culmina          = v_rt_fec_culminacion;
         LET cre_edo_credito        = v_rt_sts_credito;
         LET cre_tpo_dscto          = v_rt_tpo_descuento;
         LET cre_valor_dscto        = v_ax_valor_dscto;
         LET cre_nrp                = v_rt_nrp;
         LET cre_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET cre_nss_liberado       = v_rt_nss_liberado;
         LET cre_f_gen_arh          = v_rt_fec_proceso;
         LET cre_sdo_credito        = v_ax_sdo_cred_aux;
         LET cre_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET cre_f_desde            = v_rt_fec_dsd_avis_desast;
         LET cre_f_hasta            = v_rt_fec_hst_avis_desast;
         LET cre_tpo_rch            = v_rt_tpo_rechazo;
         LET cre_edo_procesar       = v_ax_edo_procesar;
         LET cre_estado             = v_ax_sts_registro;

         -- se inserta registro en la tabla maestro
         INSERT INTO safre_viv:cre_acreditado (
                     id_cre_acreditado,
                     id_cre_ctr_archivo,
                     folio_liquida,
                     id_derechohabiente,
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
                     edo_procesar,
                     estado)
             VALUES (cre_id_cre_acreditado,
                     cre_id_cre_ctr_archivo,
                     cre_folio_liquida,
                     cre_id_derechohabiente,
                     cre_tpo_originacion,
                     cre_tpo_credito,
                     cre_tpo_registro,
                     cre_num_credito,
                     cre_sdo_deudor,
                     cre_f_otorga,
                     cre_f_culmina,
                     cre_edo_credito,
                     cre_tpo_dscto,
                     cre_valor_dscto,
                     cre_nrp,
                     cre_f_ini_dscto,
                     cre_nss_liberado,
                     cre_f_gen_arh,
                     cre_sdo_credito,
                     cre_f_prox_liq,
                     cre_f_desde,
                     cre_f_hasta,
                     cre_tpo_rch,
                     cre_edo_procesar,
                     cre_estado);
{
         -- verifica si el tipo de originación es de Transferencia de Acreditados (1)
         IF cre_tpo_originacion = 1 THEN
            LET v_ax_id_credito = 1;
         ELSE
            -- se asume que el tipo de originación es de Anualidades Garantizadas (4)
            LET v_ax_id_credito = 4;
         END IF
}
         -- 06/07/2012 12:31:50 p.m. Todos se actualizarán con id credito igual a 1
         LET v_ax_id_credito = 1;

         -- se actualiza el registro de afi derechohabiente
         UPDATE safre_viv:afi_derechohabiente
            SET id_credito = v_ax_id_credito,
                f_credito = cre_f_otorga 
          WHERE id_derechohabiente = cre_id_derechohabiente;
      ELSE
         -- se valida la razón del rechazo
         IF v_ax_id_derechohabiente IS NULL THEN
            -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
            LET v_ax_sts_registro = 11;
         ELIF v_rt_sts_credito <> 1 THEN
            -- se asigna status 15 en el registro "Estatus del crédito no valido"
            LET v_ax_sts_registro = 15;
         ELIF v_ax_tpo_credito IS NULL THEN
            -- se asigna status 16 en el registro "Tipo credito no valido"
            LET v_ax_sts_registro = 16;
         ELIF v_existe_derech_vig = 1 THEN
            -- se asigna status 20 en el registro "Derechohabiente ya vigente"
            LET v_ax_sts_registro = 20;
         ELSE
            -- se asigna status 21 en el registro "Número de crédito vigente"
            LET v_ax_sts_registro = 21;
         END IF

         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO safre_viv:cre_rch_acreditado (
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
      END IF;

      -- se inicializan variables
      LET v_ax_ssv_92_97 = 0;
      LET v_rt_ssv_92_97 = 0;
   END FOREACH;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:cre_acreditado;
   UPDATE STATISTICS FOR TABLE safre_viv:cre_rch_acreditado;

   RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
END FUNCTION
;


