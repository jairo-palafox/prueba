






CREATE FUNCTION  "safreviv".fn_ocg_originacion(p_nss                  CHAR(11),
                                    p_id_derechohabiente   DECIMAL(9,0),
                                    p_id_ocg_formalizacion DECIMAL(9,0))

   RETURNING SMALLINT, CHAR(11), DECIMAL(9,0);

   -- REGISTRO de la temporal
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10);
   DEFINE v_rt_ssv_92_97            DECIMAL(8);
   DEFINE v_rt_fec_otorgamiento     DATE;
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_credito          DECIMAL(3);
   DEFINE v_rt_sts_credito          DECIMAL(3);
   DEFINE v_rt_tpo_descuento        DECIMAL(1);
   DEFINE v_rt_val_descuento        DECIMAL(8);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(8);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2);

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);
   DEFINE cre_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE cre_folio_liquida         DECIMAL(9,0);
   DEFINE cre_id_derechohabiente    DECIMAL(9,0);
   DEFINE cre_tpo_originacion       SMALLINT;
   DEFINE cre_tpo_credito           SMALLINT;
   DEFINE cre_tpo_registro          CHAR(2);
   DEFINE cre_num_credito           DECIMAL(10,0);
   DEFINE cre_sdo_deudor            DECIMAL(22,2);
   DEFINE cre_f_otorga              DATE;
   DEFINE cre_f_culmina             DATE;
   DEFINE cre_edo_credito           SMALLINT;
   DEFINE cre_tpo_dscto             SMALLINT;
   DEFINE cre_valor_dscto           DECIMAL(8,4);
   DEFINE cre_nrp                   CHAR(11);
   DEFINE cre_f_ini_dscto           DATE;
   DEFINE cre_nss_liberado          CHAR(11);
   DEFINE cre_f_gen_arh             DATE;
   DEFINE cre_sdo_credito           DECIMAL(22,2);
   DEFINE cre_f_prox_liq            DATE;
   DEFINE cre_f_desde               DATE;
   DEFINE cre_f_hasta               DATE;
   DEFINE cre_tpo_rch               SMALLINT;
   DEFINE cre_edo_procesar          SMALLINT;
   DEFINE cre_estado                SMALLINT;

   -- REGISTRO cre rch acreditado
   DEFINE rch_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE rch_nss                   CHAR(11);
   DEFINE rch_tpo_originacion       SMALLINT;
   DEFINE rch_tpo_credito           SMALLINT;
   DEFINE rch_tpo_registro          CHAR(2);
   DEFINE rch_num_credito           DECIMAL(10,0);
   DEFINE rch_sdo_deudor            DECIMAL(22,2);
   DEFINE rch_f_otorga              DATE;
   DEFINE rch_f_culmina             DATE;
   DEFINE rch_edo_credito           SMALLINT;
   DEFINE rch_tpo_dscto             SMALLINT;
   DEFINE rch_valor_dscto           DECIMAL(8,4);
   DEFINE rch_nrp                   CHAR(11);
   DEFINE rch_f_ini_dscto           DATE;
   DEFINE rch_nss_liberado          CHAR(11);
   DEFINE rch_f_gen_arh             DATE;
   DEFINE rch_sdo_credito           DECIMAL(22,2);
   DEFINE rch_f_prox_liq            DATE;
   DEFINE rch_f_desde               DATE;
   DEFINE rch_f_hasta               DATE;
   DEFINE rch_tpo_rch               SMALLINT;
   DEFINE rch_estado                SMALLINT;

   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador      CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt          INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech          INTEGER; -- contador de registros rechazados
   DEFINE v_ax_exist_derech_vig     SMALLINT; -- indica si el derechohabiente existe como vigente
   DEFINE v_ax_exist_numcred        INTEGER; -- contador de registros auxiliar
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar
   DEFINE v_ax_sdo_cred_aux         DECIMAL(22,2); -- saldo del crédito
   DEFINE v_ax_ssv_92_97            DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto          DECIMAL(8,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion      SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_credito          SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito           SMALLINT; -- identificador del crédito
   DEFINE v_error                   SMALLINT; -- en caso de error contiene el código
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_edo_nci                 SMALLINT;
   DEFINE v_entidad                 SMALLINT;
   DEFINE v_ax_nss                  CHAR(11);
   DEFINE vcodResp                  CHAR(4);
   DEFINE vdescResp                 VARCHAR(140);
   DEFINE v_f_proceso               DATE;
   DEFINE v_tpo_operacion           CHAR(3);
   DEFINE verror                    SMALLINT; -- código de error en caso de excepción
   DEFINE v_folio_archivo           DECIMAL(10,0);
   DEFINE v_subproc                 SMALLINT;

   -- parámetros de la función de desmarca
   DEFINE des_id_derechohabiente    DECIMAL(9,0);
   DEFINE des_marca_entra           SMALLINT;
   DEFINE des_n_referencia          INTEGER;
   DEFINE des_estado_marca          SMALLINT;
   DEFINE des_marca_causa           SMALLINT;
   DEFINE des_usuario               CHAR(20);
   DEFINE v_ax_marca_inf            SMALLINT; -- marca infonavit
   DEFINE v_ax_marca_prc            SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_transferencia    CHAR(2);  -- tipo de transferencia
   DEFINE v_usuario                 CHAR(20);
   DEFINE v_ax_cod_error            SMALLINT;

    ON EXCEPTION SET v_error
      -- verifica si el error se debe a:
      -- 239 Could not insert new row - duplicate value in a UNIQUE INDEX column
      --Inicializa valor de la variable auxiliar
      --LET v_ax_cuenta_rech = 0;

      IF v_error = -239 THEN
         -- se inserta el registro en la tabla de rechazos con estado = 17
         LET v_ax_sts_registro = 17;

         -- se incrementa el número de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;

         -- se asignan los valores en las variables que se usarán para insertar el registro
         LET rch_id_cre_ctr_archivo = cre_id_cre_ctr_archivo;
         LET rch_nss                = p_nss;
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

         --Se integra tabla de rechazos
         INSERT INTO safre_tmp:tmp_marca_rechazo(nss,
                     tpo_credito,
                     estado)
             VALUES (rch_nss,
                     rch_tpo_credito,
                     3);
      ELSE
         -- Devolverá el código de error cuando ocurra una excepción diferente a -239
         RETURN v_error, p_nss, cre_id_cre_ctr_archivo;
      END IF
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegMF.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt           = 0;
   LET v_ax_cuenta_rech           = 0;
   LET v_error                    = 0;
   LET v_ax_exist_derech_vig      = 0;
   LET v_ax_exist_numcred         = 0;
   LET rch_id_cre_ctr_archivo     = NULL;
   LET rch_nss                    = NULL;
   LET rch_tpo_originacion        = NULL;
   LET rch_tpo_credito            = NULL;
   LET rch_tpo_registro           = NULL;
   LET rch_num_credito            = NULL;
   LET rch_sdo_deudor             = NULL;
   LET rch_f_otorga               = NULL;
   LET rch_f_culmina              = NULL;
   LET rch_edo_credito            = NULL;
   LET rch_tpo_dscto              = NULL;
   LET rch_valor_dscto            = NULL;
   LET rch_nrp                    = NULL;
   LET rch_f_ini_dscto            = NULL;
   LET rch_nss_liberado           = NULL;
   LET rch_f_gen_arh              = NULL;
   LET rch_sdo_credito            = NULL;
   LET rch_f_prox_liq             = NULL;
   LET rch_f_desde                = NULL;
   LET rch_f_hasta                = NULL;
   LET rch_tpo_rch                = NULL;
   LET rch_estado                 = NULL;
   LET v_rt_nss                   = NULL;
   LET v_ax_tpo_originacion       = NULL;
   LET v_rt_tpo_credito           = NULL;
   LET v_rt_tpo_registro          = NULL;
   LET v_rt_num_credito           = 0;
   LET v_ax_ssv_92_97             = NULL;
   LET v_rt_fec_otorgamiento      = NULL;
   LET v_rt_fec_culminacion       = NULL;
   LET v_rt_sts_credito           = NULL;
   LET v_rt_tpo_descuento         = NULL;
   LET v_ax_valor_dscto           = NULL;
   LET v_rt_nrp                   = NULL;
   LET v_rt_fec_ini_oblig_patron  = NULL;
   LET v_rt_nss_liberado          = NULL;
   LET v_rt_fec_proceso           = NULL;
   LET v_ax_sdo_cred_aux          = NULL;
   LET v_rt_fec_prox_liquidar     = NULL;
   LET v_rt_fec_dsd_avis_desast   = NULL;
   LET v_rt_fec_hst_avis_desast   = NULL;
   LET v_rt_tpo_rechazo           = NULL;
   LET des_id_derechohabiente     = NULL;
   LET des_marca_entra            = NULL;
   LET des_n_referencia           = NULL;
   LET des_estado_marca           = NULL;
   LET des_marca_causa            = NULL;
   LET des_usuario                = NULL;
   LET v_ax_marca_inf             = NULL;
   LET v_ax_marca_prc             = NULL;
   LET v_ax_tpo_transferencia     = NULL;
   LET v_edo_nci                  = 0;
   LET v_entidad                  = 0;
   LET v_f_proceso                = TODAY;
   LET v_ax_tpo_originacion       = 2;
   LET v_rt_tpo_credito           = 2;
   LET v_rt_tpo_registro          = "01";
   LET v_ax_edo_procesar          = 10;
   LET v_ax_sts_registro          = 10;
   LET cre_id_cre_ctr_archivo     = 0;
   LET v_subproc                  = 2;

   CALL fn_verifica_id_archivo_ocg(v_subproc)
   RETURNING cre_id_cre_ctr_archivo, v_folio_archivo;

   -- se calculan los valores
   LET v_ax_sdo_cred_aux = 0;
   LET v_ax_ssv_92_97    = 0;
   LET v_ax_valor_dscto  = 0;

   -- se asignan los valores en las variables que se usarán para insertar el registro
   LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
   LET cre_id_cre_ctr_archivo = cre_id_cre_ctr_archivo;
   LET cre_folio_liquida      = 0;
   LET cre_id_derechohabiente = p_id_derechohabiente;
   LET cre_tpo_originacion    = v_ax_tpo_originacion;
   LET cre_tpo_credito        = v_rt_tpo_credito;
   LET cre_tpo_registro       = v_rt_tpo_registro;
   LET cre_num_credito        = v_rt_num_credito;
   LET cre_sdo_deudor         = v_ax_ssv_92_97;
   LET cre_f_otorga           = TODAY;
   LET cre_f_culmina          = "";
   LET cre_edo_credito        = 1;
   LET cre_tpo_dscto          = 1;
   LET cre_valor_dscto        = 0;
   LET cre_nrp                = "";
   LET cre_f_ini_dscto        = "";
   LET cre_nss_liberado       = "";
   LET cre_f_gen_arh          = "";
   LET cre_sdo_credito        = 0;
   LET cre_f_prox_liq         = "";
   LET cre_f_desde            = "";
   LET cre_f_hasta            = "";
   LET cre_tpo_rch            = "";
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

   INSERT INTO ocg_transaccion_cre
   VALUES( v_subproc,
           p_id_ocg_formalizacion,
           cre_id_cre_acreditado,
           "",
           v_f_proceso );

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_acreditado;
   UPDATE STATISTICS FOR TABLE ocg_transaccion_cre;

   RETURN v_error, p_nss, cre_id_cre_acreditado;

END FUNCTION;


