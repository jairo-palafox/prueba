






CREATE FUNCTION "safreviv".fn_grt_integra_recurrente(p_d_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, INTEGER;

   -- REGISTRO tmp cre acred agr 01
   DEFINE tmp_tpo_registro         CHAR(2);
   DEFINE tmp_nss                  CHAR(11);
   DEFINE tmp_num_credito          DECIMAL(10);
   DEFINE tmp_ssv_92_97            DECIMAL(8);
   DEFINE tmp_fec_otorgamiento     DATE;
   DEFINE tmp_fec_culminacion      DATE;
   DEFINE tmp_tpo_credito          CHAR(3);
   DEFINE tmp_sts_credito          CHAR(3);
   DEFINE tmp_tpo_descuento        DECIMAL(1);
   DEFINE tmp_val_descuento        DECIMAL(8);
   DEFINE tmp_nrp                  CHAR(11);
   DEFINE tmp_fec_ini_oblig_patron DATE;
   DEFINE tmp_nss_liberado         CHAR(11);
   DEFINE tmp_fec_proceso          DATE;
   DEFINE tmp_sdo_credito          DECIMAL(10);
   DEFINE tmp_fec_prox_liquidar    DATE;
   DEFINE tmp_fec_dsd_avis_desast  DATE;
   DEFINE tmp_fec_hst_avis_desast  DATE;
   DEFINE tmp_tpo_rechazo          DECIMAL(2);
   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE cre_folio_liquida        DECIMAL(9,0);
   DEFINE cre_id_derechohabiente   DECIMAL(9,0);
   DEFINE cre_tpo_originacion      SMALLINT;
   DEFINE cre_tpo_credito          SMALLINT;
   DEFINE cre_tpo_registro         CHAR(2);
   DEFINE cre_num_credito          DECIMAL(10,0);
   DEFINE cre_sdo_deudor           DECIMAL(22,2);
   DEFINE cre_f_otorga             DATE;
   DEFINE cre_f_culmina            DATE;
   DEFINE cre_edo_credito          SMALLINT;
   DEFINE cre_tpo_dscto            SMALLINT;
   DEFINE cre_valor_dscto          DECIMAL(8,4);
   DEFINE cre_nrp                  CHAR(11);
   DEFINE cre_f_ini_dscto          DATE;
   DEFINE cre_nss_liberado         CHAR(11);
   DEFINE cre_f_gen_arh            DATE;
   DEFINE cre_sdo_credito          DECIMAL(22,2);
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
   DEFINE rch_sdo_deudor           DECIMAL(22,2);
   DEFINE rch_f_otorga             DATE;
   DEFINE rch_f_culmina            DATE;
   DEFINE rch_edo_credito          SMALLINT;
   DEFINE rch_tpo_dscto            SMALLINT;
   DEFINE rch_valor_dscto          DECIMAL(8,4);
   DEFINE rch_nrp                  CHAR(11);
   DEFINE rch_f_ini_dscto          DATE;
   DEFINE rch_nss_liberado         CHAR(11);
   DEFINE rch_f_gen_arh            DATE;
   DEFINE rch_sdo_credito          DECIMAL(22,2);
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
   DEFINE v_ax_exist_derech_vig   SMALLINT; -- indica si un id derechohabiente ya existe (vigente)
   --DEFINE v_cuenta_numcred_exist  INTEGER; -- contador de registros con numero de cr�dito existente
   DEFINE v_ax_sts_registro       SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_sdo_cred_aux       DECIMAL(22,2); -- saldo del credito
   DEFINE v_ax_ssv_92_97          DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto        DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion    SMALLINT; -- tipo de originaci�n
   DEFINE v_ax_tpo_credito        SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito         SMALLINT; -- identificador del cr�dito
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el c�digo

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
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = tmp_fec_otorgamiento;
         LET rch_f_culmina          = tmp_fec_culminacion;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = tmp_nrp;
         LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET rch_nss_liberado       = tmp_nss_liberado;
         LET rch_f_gen_arh          = tmp_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
         LET rch_f_desde            = tmp_fec_dsd_avis_desast;
         LET rch_f_hasta            = tmp_fec_hst_avis_desast;
         LET rch_tpo_rch            = tmp_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
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
      ELSE
        -- Devolvera el codigo de error cuando ocurra una excepci�n diferente a -239
        RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
      END IF
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtIntegRecurr.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt       = 0;
   LET v_ax_cuenta_rech       = 0;
   LET v_error                = 0;
   LET v_ax_exist_derech_vig  = 0; -- se asume que el derechohabiente no existe
   --LET v_cuenta_numcred_exist = 0;

   --------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS INEXISTENTES EN CAT�LOGO --
   --------------------------------------------------------
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
      FROM safre_tmp:tmp_cre_acred_grt_01
     WHERE nss NOT IN (
           SELECT afi.nss
           FROM afi_derechohabiente afi
           INNER JOIN safre_tmp:tmp_cre_acred_grt_01 t
           ON afi.nss = t.nss)

      -- se asigna el estatud de error
      LET v_ax_sts_registro = 11; -- 11 Derechohabiente no existe en maestro

      -- se consulta el tipo de cr�dito y el tipo originaci�n para el tipo credito
      FOREACH
       SELECT FIRST 1 tpo_originacion
         INTO v_ax_tpo_originacion
         FROM cat_tipo_credito
        WHERE tpo_credito = tmp_tpo_credito
      END FOREACH;

      -- se incrementa el numero de registros rechazados
      LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = tmp_sdo_credito / 100;
      LET v_ax_ssv_92_97 = tmp_ssv_92_97 / 100;
      LET v_ax_valor_dscto = tmp_val_descuento / 10000;

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
      LET rch_nss                = tmp_nss;
      LET rch_tpo_originacion    = v_ax_tpo_originacion;
      LET rch_tpo_credito        = tmp_tpo_credito;
      LET rch_tpo_registro       = tmp_tpo_registro;
      LET rch_num_credito        = tmp_num_credito;
      LET rch_sdo_deudor         = v_ax_ssv_92_97;
      LET rch_f_otorga           = tmp_fec_otorgamiento;
      LET rch_f_culmina          = tmp_fec_culminacion;
      LET rch_edo_credito        = tmp_sts_credito;
      LET rch_tpo_dscto          = tmp_tpo_descuento;
      LET rch_valor_dscto        = v_ax_valor_dscto;
      LET rch_nrp                = tmp_nrp;
      LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
      LET rch_nss_liberado       = tmp_nss_liberado;
      LET rch_f_gen_arh          = tmp_fec_proceso;
      LET rch_sdo_credito        = v_ax_sdo_cred_aux;
      LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
      LET rch_f_desde            = tmp_fec_dsd_avis_desast;
      LET rch_f_hasta            = tmp_fec_hst_avis_desast;
      LET rch_tpo_rch            = tmp_tpo_rechazo;
      LET rch_estado             = v_ax_sts_registro;

      -- se inserta registro
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
   END FOREACH;

   -----------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE NUEVOS ACREDITADOS --
   -----------------------------------------------------
   -- se obtienen los datos de la tabla temporal del proceso de Recurrente tipo de registro (01 y 20)
   FOREACH
    SELECT t.*,
           a.id_derechohabiente,
           a.tipo_trabajador
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
           tmp_tpo_rechazo,
           v_ax_id_derechohabiente,
           v_ax_tipo_trabajador
      FROM safre_tmp:tmp_cre_acred_grt_01 t,
           afi_derechohabiente a
     WHERE t.nss = a.nss

     -- se consulta el tipo de cr�dito y el tipo originaci�n para el tipo credito
      SELECT tpo_credito, tpo_originacion
        INTO v_ax_tpo_credito, v_ax_tpo_originacion
        FROM cat_tipo_credito
       WHERE tpo_credito = tmp_tpo_credito
         AND tpo_originacion = 2;

      -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
      IF EXISTS (
         SELECT c.id_derechohabiente
           FROM cre_acreditado c, cat_maq_credito m
          WHERE c.id_derechohabiente = v_ax_id_derechohabiente
            AND c.estado             = m.estado
            AND c.tpo_credito        = tmp_tpo_credito
            AND m.entidad            = 1) THEN

            -- se prende la bandera
            LET v_ax_exist_derech_vig = 1;
      ELSE
         -- se mantiene la bandera apagada
         LET v_ax_exist_derech_vig = 0;
      END IF

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = tmp_sdo_credito / 100;
      LET v_ax_ssv_92_97    = tmp_ssv_92_97 / 100;
      LET v_ax_valor_dscto  = tmp_val_descuento / 10000;

      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente debe estar en catalogo (no debe ser nulo)
      -- * el estatus del credito debe ser igual a 1
      -- * el tipo de credito debe ser de originaci�n 2 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL AND
         tmp_sts_credito = 1 AND
         v_ax_tpo_credito IS NOT NULL AND
         v_ax_exist_derech_vig = 0 THEN
         --v_cuenta_numcred_exist = 0 THEN
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
         LET cre_tpo_credito        = tmp_tpo_credito;
         LET cre_tpo_registro       = tmp_tpo_registro;
         LET cre_num_credito        = tmp_num_credito;
         LET cre_sdo_deudor         = v_ax_ssv_92_97;
         LET cre_f_otorga           = tmp_fec_otorgamiento;
         LET cre_f_culmina          = tmp_fec_culminacion;
         LET cre_edo_credito        = tmp_sts_credito;
         LET cre_tpo_dscto          = tmp_tpo_descuento;
         LET cre_valor_dscto        = v_ax_valor_dscto;
         LET cre_nrp                = tmp_nrp;
         LET cre_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET cre_nss_liberado       = tmp_nss_liberado;
         LET cre_f_gen_arh          = tmp_fec_proceso;
         LET cre_sdo_credito        = v_ax_sdo_cred_aux;
         LET cre_f_prox_liq         = tmp_fec_prox_liquidar;
         LET cre_f_desde            = tmp_fec_dsd_avis_desast;
         LET cre_f_hasta            = tmp_fec_hst_avis_desast;
         LET cre_tpo_rch            = tmp_tpo_rechazo;
         LET cre_edo_procesar       = v_ax_edo_procesar;
         LET cre_estado             = v_ax_sts_registro;

         -- se inserta registro en la tabla maestro
         INSERT INTO cre_acreditado (
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
         -- se asigna el id credito correspondiente a Cr�ditos en Garant�a 34 Bis
         LET v_ax_id_credito = 2;
}
         -- 06/07/2012 12:35:08 p.m. Todos se actualizar�n con id credito igual a 1
         LET v_ax_id_credito = 1;

         -- se actualiza el registro de afi derechohabiente
         UPDATE afi_derechohabiente
            SET id_credito = v_ax_id_credito,
                f_credito = cre_f_otorga 
          WHERE id_derechohabiente = cre_id_derechohabiente;
      ELSE
         -- se verfica la raz�n del rechazo
         IF v_ax_id_derechohabiente IS NULL THEN
            -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
            LET v_ax_sts_registro = 11;
         ELIF tmp_sts_credito <> 1 THEN
            -- se asigna status 15 en el registro "Estatus del cr�dito no valido"
            LET v_ax_sts_registro = 15;
         ELIF v_ax_tpo_credito IS NULL THEN
            -- se asigna status 16 en el registro "Tipo credito no valido"
            LET v_ax_sts_registro = 16;
         ELSE -- v_ax_exist_derech_vig > 0 THEN
            -- se asigna status 20 en el registro "Derechohabiente vigente"
            LET v_ax_sts_registro = 20;
         --ELSE
         --   -- se asigna status 21 en el registro "N�mero de cr�dito vigente"
         --   LET v_ax_sts_registro = 21;
         END IF

         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = v_ax_ssv_92_97;
         LET rch_f_otorga           = tmp_fec_otorgamiento;
         LET rch_f_culmina          = tmp_fec_culminacion;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = tmp_nrp;
         LET rch_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET rch_nss_liberado       = tmp_nss_liberado;
         LET rch_f_gen_arh          = tmp_fec_proceso;
         LET rch_sdo_credito        = v_ax_sdo_cred_aux;
         LET rch_f_prox_liq         = tmp_fec_prox_liquidar;
         LET rch_f_desde            = tmp_fec_dsd_avis_desast;
         LET rch_f_hasta            = tmp_fec_hst_avis_desast;
         LET rch_tpo_rch            = tmp_tpo_rechazo;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
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
      END IF;
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_acreditado;
   UPDATE STATISTICS FOR TABLE cre_rch_acreditado;

   RETURN v_error, v_ax_cuenta_acpt, v_ax_cuenta_rech;
END FUNCTION
;


