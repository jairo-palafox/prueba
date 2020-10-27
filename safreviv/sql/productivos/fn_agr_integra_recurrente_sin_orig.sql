






CREATE FUNCTION "safreviv".fn_agr_integra_recurrente_sin_orig(p_v_usuario CHAR(20),
                                                   p_v_arch_proceso CHAR(100),
                                                   p_d_folio DECIMAL(10),
                                                   p_i_id_lote_acpt INTEGER,
                                                   p_i_id_lote_rech INTEGER,
                                                   p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT;
   -- REGISTRO tmp acr transferencia
   DEFINE tmp_tpo_registro         CHAR(2);
   DEFINE tmp_nss                  CHAR(11);
   DEFINE tmp_num_credito          DECIMAL(10,0);
   DEFINE tmp_ssv_92_97            DECIMAL(8,0);
   DEFINE tmp_fec_otorgamiento     DATE;
   DEFINE tmp_fec_culminacion      DATE;
   DEFINE tmp_tpo_credito          DECIMAL(3,0);
   DEFINE tmp_sts_credito          DECIMAL(3,0);
   DEFINE tmp_tpo_descuento        DECIMAL(1,0);
   DEFINE tmp_val_descuento        DECIMAL(8,0);
   DEFINE tmp_nrp                  CHAR(11);
   DEFINE tmp_fec_ini_oblig_patron DATE;
   DEFINE tmp_nss_liberado         CHAR(11);
   DEFINE tmp_fec_proceso          DATE;
   DEFINE tmp_sdo_credito          DECIMAL(8,0);
   DEFINE tmp_fec_prox_liquidar    DATE;
   DEFINE tmp_fec_dsd_avis_desast  DATE;
   DEFINE tmp_fec_hst_avis_desast  DATE;
   DEFINE tmp_tpo_rechazo          DECIMAL(2,0);
   -- REGISTRO cre sin originación
   DEFINE v_sin_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE v_sin_nss                CHAR(11);
   DEFINE v_sin_tpo_originacion    SMALLINT;
   DEFINE v_sin_tpo_credito        SMALLINT;
   DEFINE v_sin_tpo_registro       CHAR(2);
   DEFINE v_sin_num_credito        DECIMAL(10);
   DEFINE v_sin_sdo_deudor         DECIMAL(22,2);
   DEFINE v_sin_f_otorga           DATE;
   DEFINE v_sin_f_culmina          DATE;
   DEFINE v_sin_edo_credito        SMALLINT;
   DEFINE v_sin_tpo_dscto          SMALLINT;
   DEFINE v_sin_valor_dscto        DECIMAL(8,4);
   DEFINE v_sin_nrp                CHAR(11);
   DEFINE v_sin_f_ini_dscto        DATE;
   DEFINE v_sin_nss_liberado       CHAR(11);
   DEFINE v_sin_f_gen_arh          DATE;
   DEFINE v_sin_sdo_credito        DECIMAL(22,2);
   DEFINE v_sin_f_prox_liq         DATE;
   DEFINE v_sin_f_desde            DATE;
   DEFINE v_sin_f_hasta            DATE;
   DEFINE v_sin_tpo_rch            SMALLINT;
   DEFINE v_sin_estado             SMALLINT;
   -- Campos auxiliares
   DEFINE v_ax_sts_registro    SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_sdo_cred_aux    DECIMAL(22,2); -- saldo del credito
   DEFINE v_ax_ssv_92_97       DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto     DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion SMALLINT; -- tipo de originación
   DEFINE v_i_estado           SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_i_id_lote_sin      INTEGER; -- mantiene el conteo de los registros sin originación
   DEFINE r_ax_bandera         SMALLINT; -- valor de regreso de la actualización
   DEFINE v_ax_excep_error     SMALLINT; -- contiene código error (ocurrido en el proceso)

   ON EXCEPTION SET v_ax_excep_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_excep_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRecurrSinOrig.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_i_estado = 2; -- estado Integrado en la tabla glo ctr archivo
   LET v_i_id_lote_sin = 0;
   LET v_ax_excep_error = 0;

   -------------------------------------------------
   -- SE PROCESA LA TABLA CON TIPO DE REGISTRO 02 --
   -------------------------------------------------
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
      FROM safre_tmp:tmp_cre_acred_agr_02
         -- se inicializan variables
         LET v_ax_sts_registro = 0;
         LET v_ax_tpo_originacion = NULL;

         -- se incrementa el numero de registros rechazados y el id transferencia
         LET v_i_id_lote_sin = v_i_id_lote_sin + 1;

         -- se consulta el tipo de crédito y el tipo originación para el tipo credito
         FOREACH
          SELECT FIRST 1 tpo_originacion
            INTO v_ax_tpo_originacion
            FROM safre_viv:cat_tipo_credito
           WHERE tpo_credito = tmp_tpo_credito
             AND f_actualiza <= tmp_fec_otorgamiento
           ORDER BY f_actualiza DESC
         END FOREACH;

         IF v_ax_tpo_originacion IS NULL THEN
            LET v_ax_tpo_originacion = 4; -- Anualidades Garantizadas
         END IF;

         -- se calculan los valores
         LET v_ax_sdo_cred_aux = tmp_sdo_credito   / 100;
         LET v_ax_ssv_92_97    = tmp_ssv_92_97     / 100;
         LET v_ax_valor_dscto  = tmp_val_descuento / 10000;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET v_sin_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET v_sin_nss                = tmp_nss;
         LET v_sin_tpo_originacion    = v_ax_tpo_originacion;
         LET v_sin_tpo_credito        = tmp_tpo_credito;
         LET v_sin_tpo_registro       = tmp_tpo_registro;
         LET v_sin_num_credito        = tmp_num_credito;
         LET v_sin_sdo_deudor         = v_ax_ssv_92_97;
         LET v_sin_f_otorga           = tmp_fec_otorgamiento;
         LET v_sin_f_culmina          = tmp_fec_culminacion;
         LET v_sin_edo_credito        = tmp_sts_credito;
         LET v_sin_tpo_dscto          = tmp_tpo_descuento;
         LET v_sin_valor_dscto        = v_ax_valor_dscto;
         LET v_sin_nrp                = tmp_nrp;
         LET v_sin_f_ini_dscto        = tmp_fec_ini_oblig_patron;
         LET v_sin_nss_liberado       = tmp_nss_liberado;
         LET v_sin_f_gen_arh          = tmp_fec_proceso;
         LET v_sin_sdo_credito        = v_ax_sdo_cred_aux;
         LET v_sin_f_prox_liq         = tmp_fec_prox_liquidar;
         LET v_sin_f_desde            = tmp_fec_dsd_avis_desast;
         LET v_sin_f_hasta            = tmp_fec_hst_avis_desast;
         LET v_sin_tpo_rch            = tmp_tpo_rechazo;
         LET v_sin_estado             = v_ax_sts_registro;

         -- se inserta registro
         INSERT INTO safre_viv:cre_sin_originacion (
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
             VALUES (v_sin_id_cre_ctr_archivo,
                     v_sin_nss,
                     v_sin_tpo_originacion,
                     v_sin_tpo_credito,
                     v_sin_tpo_registro,
                     v_sin_num_credito,
                     v_sin_sdo_deudor,
                     v_sin_f_otorga,
                     v_sin_f_culmina,
                     v_sin_edo_credito,
                     v_sin_tpo_dscto,
                     v_sin_valor_dscto,
                     v_sin_nrp,
                     v_sin_f_ini_dscto,
                     v_sin_nss_liberado,
                     v_sin_f_gen_arh,
                     v_sin_sdo_credito,
                     v_sin_f_prox_liq,
                     v_sin_f_desde,
                     v_sin_f_hasta,
                     v_sin_tpo_rch,
                     v_sin_estado);
   END FOREACH

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:cre_sin_originacion;

   -- se ejecuta el sp que actualiza el registro de la tabla de control de archivos a estatus "Integrado"
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, p_i_id_lote_acpt, p_i_id_lote_rech, v_i_id_lote_sin, p_ax_id_cre_ctr_arch);

   RETURN v_ax_excep_error;

END FUNCTION;


