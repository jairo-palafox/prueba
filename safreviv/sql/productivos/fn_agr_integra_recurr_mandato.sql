






CREATE FUNCTION "safreviv".fn_agr_integra_recurr_mandato(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                              p_d_usuario CHAR(20))
   RETURNING SMALLINT;
   -- REGISTRO de la temporal de mandatos
   DEFINE v_rt_tpo_registro           CHAR(2);
   DEFINE v_rt_nss                    CHAR(11);
   DEFINE v_rt_num_credito            DECIMAL(10);
   DEFINE v_rt_ssv_92_97              DECIMAL(8);
   DEFINE v_rt_fec_otorgamiento       DATE;
   DEFINE v_rt_fec_culminacion        DATE;
   DEFINE v_rt_tpo_credito            CHAR(3);
   DEFINE v_rt_sts_credito            CHAR(3);
   DEFINE v_rt_tpo_descuento          DECIMAL(1);
   DEFINE v_rt_val_descuento          DECIMAL(8);
   DEFINE v_rt_nrp                    CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron   DATE;
   DEFINE v_rt_nss_liberado           CHAR(11);
   DEFINE v_rt_fec_proceso            DATE;
   DEFINE v_rt_sdo_credito            DECIMAL(8);
   DEFINE v_rt_fec_prox_liquidar      DATE;
   DEFINE v_rt_fec_dsd_avis_desast    DATE;
   DEFINE v_rt_fec_hst_avis_desast    DATE;
   DEFINE v_rt_tpo_rechazo            DECIMAL(2);
   DEFINE v_rt_fec_ini_mandato        DATE;
   DEFINE v_rt_fec_cul_mandato        DATE;
   DEFINE v_rt_id_mandato             CHAR(7);
   DEFINE v_rt_tpo_desc_mandato       DECIMAL(1,0);
   DEFINE v_rt_val_desc_mandato       CHAR(8);
   DEFINE v_rt_ref_mandato            CHAR(40);
   DEFINE v_rt_cve_mandato            CHAR(7);
   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo      DECIMAL(9,0);
   DEFINE rch_nss                     CHAR(11);
   DEFINE rch_tpo_originacion         SMALLINT;
   DEFINE rch_tpo_credito             SMALLINT;
   DEFINE rch_tpo_registro            CHAR(2);
   DEFINE rch_num_credito             DECIMAL(10,0);
   DEFINE rch_sdo_deudor              DECIMAL(22,2);
   DEFINE rch_f_otorga                DATE;
   DEFINE rch_f_culmina               DATE;
   DEFINE rch_edo_credito             SMALLINT;
   DEFINE rch_tpo_dscto               SMALLINT;
   DEFINE rch_valor_dscto             DECIMAL(8,4);
   DEFINE rch_nrp                     CHAR(11);
   DEFINE rch_f_ini_dscto             DATE;
   DEFINE rch_nss_liberado            CHAR(11);
   DEFINE rch_f_gen_arh               DATE;
   DEFINE rch_sdo_credito             DECIMAL(22,2);
   DEFINE rch_f_prox_liq              DATE;
   DEFINE rch_f_desde                 DATE;
   DEFINE rch_f_hasta                 DATE;
   DEFINE rch_tpo_rch                 SMALLINT;
   DEFINE rch_estado                  SMALLINT;
   -- PARÁMETROS para la función de mandatos
   DEFINE man_tipo_registro           CHAR(2);
   DEFINE man_id_derechohabiente      DECIMAL(9,0);
   DEFINE man_nss                     CHAR(11);
   DEFINE man_id_credito              DECIMAL(10,0);
   DEFINE man_tipo_credito            CHAR(3);
   DEFINE man_estado_credito          CHAR(3); --SMALLINT
   DEFINE man_f_inicio_mandato        DATE; -- CHAR(8);
   DEFINE man_f_culmina_mandato       DATE; -- CHAR(8);
   DEFINE man_id_mdt                  CHAR(7);
   DEFINE man_tpo_descuento_mandato   SMALLINT;
   DEFINE man_valor_descuento_mandato CHAR(8); --DECIMAL(8,0);
   DEFINE man_referencia_mandato      CHAR(40);
   DEFINE man_cve_mandato             CHAR(7);
   DEFINE man_usuario                 CHAR(20);
   DEFINE man_lote                    SMALLINT;
   DEFINE man_fecha_lote              DATE;
   DEFINE man_fecha_carga_acr         DATE;
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente     DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_cuenta_acpt            INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech            INTEGER; -- contador de registros rechazados
   DEFINE v_ax_sts_registro           SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_sdo_cred_aux           DECIMAL(22,2); -- saldo del credito
   DEFINE v_ax_ssv_92_97              DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto            DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion        SMALLINT; -- tipo de originación
   DEFINE v_ax_lote                   SMALLINT; -- lote
   DEFINE v_ax_f_lote                 DATE; -- fecha de lote
   DEFINE v_ax_error                  SMALLINT; -- cotiene el código de error en caso de ocurrir

   ON EXCEPTION SET v_ax_error
      --Ocurrio un error al realizar el proceso
      --Se regresa el número de error que ocurrio
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRecurrMand.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt = 0;
   LET v_ax_cuenta_rech = 0;
   LET v_ax_error = 0; --Se inicia el error en 0 para indicar que por default no ocurrio un error

   ---------------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE MANDATO SERVICIOS (30) --
   ---------------------------------------------------------
   FOREACH
      -- se obtienen los datos de mandato servicios de la tabla temporal
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
           v_rt_tpo_rechazo,
           v_rt_fec_ini_mandato,
           v_rt_fec_cul_mandato,
           v_rt_id_mandato,
           v_rt_tpo_desc_mandato,
           v_rt_val_desc_mandato,
           v_rt_ref_mandato,
           v_rt_cve_mandato
      FROM safre_tmp:tmp_cre_acred_agr_30

      -- se obtiene el id del derechohabiente y el tipo trabajador para el nss
      SELECT UNIQUE id_derechohabiente
      INTO v_ax_id_derechohabiente
      FROM safre_viv:afi_derechohabiente
      WHERE nss = v_rt_nss;

      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      SELECT tpo_originacion
        INTO v_ax_tpo_originacion
        FROM safre_viv:cat_tipo_credito
       WHERE tpo_originacion IN (1,4)
         AND tpo_credito = v_rt_tpo_credito;

      -- se consulta la información del archivo para la clave que entra como parámetro
      SELECT lote, f_lote
        INTO v_ax_lote, v_ax_f_lote
        FROM safre_viv:cre_ctr_archivo
       WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = v_rt_sdo_credito / 100;
      LET v_ax_ssv_92_97 = v_rt_ssv_92_97 / 100;
      LET v_ax_valor_dscto = v_rt_val_descuento / 10000;

      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente no debe estar en catalogo (no debe ser nulo)
      -- * el estatus del credito debe ser igual a 1
      -- * el tipo de credito debe ser de originación 1 o 4 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL THEN
         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;

         -- se asignan los valores de la función de mandatos
         LET man_tipo_registro           = v_rt_tpo_registro;
         LET man_id_derechohabiente      = v_ax_id_derechohabiente;
         LET man_nss                     = v_rt_nss;
         LET man_id_credito              = v_rt_num_credito;
         LET man_tipo_credito            = v_rt_tpo_credito;
         LET man_estado_credito          = v_rt_sts_credito;
         LET man_f_inicio_mandato        = v_rt_fec_ini_mandato;
         LET man_f_culmina_mandato       = v_rt_fec_cul_mandato;
         LET man_id_mdt                  = v_rt_id_mandato;
         LET man_tpo_descuento_mandato   = v_rt_tpo_desc_mandato;
         LET man_valor_descuento_mandato = v_rt_val_desc_mandato;
         LET man_referencia_mandato      = v_rt_ref_mandato;
         LET man_cve_mandato             = v_rt_cve_mandato;
         LET man_usuario                 = p_d_usuario;
         LET man_lote                    = v_ax_lote;
         LET man_fecha_lote              = v_ax_f_lote;
         LET man_fecha_carga_acr         = TODAY;

         -- se invoca la función de mandatos
         EXECUTE PROCEDURE safre_viv:sp_mdt_inserta_inst_recurrente(
                           man_tipo_registro,
                           man_id_derechohabiente,
                           man_nss,
                           man_id_credito,
                           man_tipo_credito,
                           man_estado_credito,
                           man_f_inicio_mandato,
                           man_f_culmina_mandato,
                           man_id_mdt,
                           man_tpo_descuento_mandato,
                           man_valor_descuento_mandato,
                           man_referencia_mandato,
                           man_cve_mandato,
                           man_usuario,
                           man_lote,
                           man_fecha_lote,
                           man_fecha_carga_acr);
      ELSE
         -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
         LET v_ax_sts_registro = 11;

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
   END FOREACH;

   RETURN v_ax_error;
END FUNCTION
;


