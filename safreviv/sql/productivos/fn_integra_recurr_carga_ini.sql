






CREATE FUNCTION "safreviv".fn_integra_recurr_carga_ini(p_d_folio DECIMAL(9,0),
                                            p_d_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT;
   -- REGISTRO de la temporal
   DEFINE tmp_nss                  CHAR(11);
   DEFINE tmp_num_cred_0           CHAR(16);
   DEFINE tmp_num_credito          DECIMAL(10,0);
   DEFINE tmp_fec_ini_cred         DATE;
   DEFINE tmp_fec_cul_cred         DATE;
   DEFINE tmp_tpo_credito          SMALLINT;
   DEFINE tmp_sts_credito          SMALLINT;
   DEFINE tmp_tpo_descuento        SMALLINT;
   DEFINE tmp_por_descuento        DECIMAL(8,0);
   DEFINE tmp_fac_descuento        DECIMAL(8,0);
   DEFINE tmp_rel_laboral          CHAR(1);
   DEFINE tmp_fec_rel_laboral      DATE;

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado    DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo   DECIMAL(9,0);
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
   --DEFINE v_ax_tipo_trabajador    CHAR(1); -- tipo trabajador
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_cuenta_acpt        INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech        INTEGER; -- contador de registros rechazados
   DEFINE v_ax_sts_registro       SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_valor_dscto        DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion    SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_credito        SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito         SMALLINT; -- identificador del crédito
   DEFINE v_error                 SMALLINT; -- codigo de error en caso de excepción

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/iniIntegRecurrCarIni.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt = 0;
   LET v_ax_cuenta_rech = 0;
   LET v_error = 0;

   -------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE CARGA INICIAL) --
   -------------------------------------------------
   FOREACH
      -- se obtienen los datos de la tabla temporal del proceso de Recurrente
      -- en esta tabla ya incluye los tipo de registro (01 y 20)
      SELECT *
      INTO tmp_nss,
           tmp_num_cred_0,
           tmp_num_credito,
           tmp_fec_ini_cred,
           tmp_fec_cul_cred,
           tmp_tpo_credito,
           tmp_sts_credito,
           tmp_tpo_descuento,
           tmp_por_descuento,
           tmp_fac_descuento,
           tmp_rel_laboral,
           tmp_fec_rel_laboral
      FROM safre_tmp:tmp_cre_acreditado_carg_ini

      -- se obtiene el id del derechohabiente y el tipo trabajador para el nss
      SELECT UNIQUE id_derechohabiente--, tipo_trabajador
      INTO v_ax_id_derechohabiente--, v_ax_tipo_trabajador
      FROM safre_viv:afi_derechohabiente
      WHERE nss = tmp_nss;

      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      SELECT tpo_credito, tpo_originacion
        INTO v_ax_tpo_credito, v_ax_tpo_originacion
        FROM safre_viv:cat_tipo_credito
       WHERE tpo_originacion IN (1,2,4)
         AND tpo_credito = tmp_tpo_credito;

      -- se verifica el tipo de descuento
      IF tmp_tpo_descuento = 1 THEN
         -- se asigna el porcentaje de descuento
         LET v_ax_valor_dscto = tmp_por_descuento/10000;
      ELSE
         -- se asume que el tipo de descuento es 1 ó 2. Se asigna el factor de descuento
         LET v_ax_valor_dscto = tmp_fac_descuento/10000;
      END IF

      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente no debe estar en catalogo (no debe ser nulo)
      -- * el tipo de credito debe ser de originación 1 o 4 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL AND
         v_ax_tpo_credito IS NOT NULL THEN
         -- se inicializa el id credito
         LET v_ax_id_credito = NULL;

         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;

         ---- se valida el tipo de trabajador
         IF tmp_nss[1,2] = "77" THEN
            LET v_ax_edo_procesar = 5;
         ELSE
            LET v_ax_edo_procesar = 120;
         END IF

         -- se verifica si el estatus del crédito está Liquidado (2)
         IF tmp_sts_credito = 2 THEN
            LET v_ax_sts_registro = 910;
         ELSE
            LET v_ax_sts_registro = 900;
         END IF

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
         LET cre_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET cre_id_derechohabiente = v_ax_id_derechohabiente;
         LET cre_tpo_originacion    = v_ax_tpo_originacion;
         LET cre_tpo_credito        = tmp_tpo_credito;
         LET cre_tpo_registro       = "01";
         LET cre_num_credito        = tmp_num_credito;
         LET cre_sdo_deudor         = NULL;
         LET cre_f_otorga           = tmp_fec_ini_cred;
         LET cre_f_culmina          = tmp_fec_cul_cred;
         LET cre_edo_credito        = tmp_sts_credito;
         LET cre_tpo_dscto          = tmp_tpo_descuento;
         LET cre_valor_dscto        = v_ax_valor_dscto;
         LET cre_nrp                = NULL;
         LET cre_f_ini_dscto        = NULL;
         LET cre_nss_liberado       = NULL;
         LET cre_f_gen_arh          = NULL;
         LET cre_sdo_credito        = NULL;
         LET cre_f_prox_liq         = NULL;
         LET cre_f_desde            = NULL;
         LET cre_f_hasta            = NULL;
         LET cre_tpo_rch            = NULL;
         LET cre_edo_procesar       = v_ax_edo_procesar;
         LET cre_estado             = v_ax_sts_registro;

         -- se inserta registro en la tabla maestro
         INSERT INTO safre_viv:cre_acreditado (
                     id_cre_acreditado,
                     id_cre_ctr_archivo,
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

         -- verifica si se trata de un crédito vigente
         IF cre_edo_credito = 1 THEN
            -- verifica si el tipo de originación es de Transferencia de Acreditados (1)
            IF cre_tpo_originacion = 1 THEN
               LET v_ax_id_credito = 1;
            ELIF cre_tpo_originacion = 2 THEN
               LET v_ax_id_credito = 2;
            ELSE
               -- se asume que el tipo de originación es de Anualidades Garantizadas (4)
               LET v_ax_id_credito = 3;
            END IF
         ELIF cre_edo_credito = 2 THEN
            -- se trata de un crédito liquidado
            LET v_ax_id_credito = 0;
         END IF

         -- si se trata de un crédito vigente o liquidado se actualiza afi derechohabiente
         IF v_ax_id_credito IS NOT NULL THEN
            -- se actualiza el registro de afi derechohabiente
            UPDATE safre_viv:afi_derechohabiente
               SET id_credito = v_ax_id_credito,
                   f_credito = cre_f_otorga 
             WHERE id_derechohabiente = cre_id_derechohabiente;
         END IF
      ELSE
         -- se valida la razón del rechazo
         IF v_ax_id_derechohabiente IS NULL THEN
            -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
            LET v_ax_sts_registro = 11;
         ELSE
            -- se asigna status 16 en el registro "Tipo credito no valido"
            LET v_ax_sts_registro = 16;
         END IF

         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET rch_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_credito        = tmp_tpo_credito;
         LET rch_tpo_registro       = "01";
         LET rch_num_credito        = tmp_num_credito;
         LET rch_sdo_deudor         = NULL;
         LET rch_f_otorga           = tmp_fec_ini_cred;
         LET rch_f_culmina          = tmp_fec_cul_cred;
         LET rch_edo_credito        = tmp_sts_credito;
         LET rch_tpo_dscto          = tmp_tpo_descuento;
         LET rch_valor_dscto        = v_ax_valor_dscto;
         LET rch_nrp                = NULL;
         LET rch_f_ini_dscto        = NULL;
         LET rch_nss_liberado       = NULL;
         LET rch_f_gen_arh          = NULL;
         LET rch_sdo_credito        = NULL;
         LET rch_f_prox_liq         = NULL;
         LET rch_f_desde            = NULL;
         LET rch_f_hasta            = NULL;
         LET rch_tpo_rch            = NULL;
         LET rch_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO safre_tmp:tmp_cre_rch_acreditado (
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

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, v_ax_cuenta_acpt, v_ax_cuenta_rech, 0, p_d_id_cre_ctr_arch);

   RETURN v_error;
END FUNCTION;


