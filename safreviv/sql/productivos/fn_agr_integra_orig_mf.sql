






CREATE FUNCTION "safreviv".fn_agr_integra_orig_mf(v_rt_tpo_registro       CHAR(2),
                                       v_rt_nss                CHAR(11),
                                       v_rt_num_credito        DECIMAL(10,0),
                                       v_rt_ssv_92_97          DECIMAL(15,0),
                                       v_rt_fec_otorgamiento   DATE,
                                       v_rt_fec_culminacion    DATE,
                                       v_rt_tpo_credito        CHAR(3),
                                       v_rt_sts_credito        SMALLINT,
                                       v_rt_tpo_descuento      SMALLINT,
                                       v_rt_val_descuento      DECIMAL(15,0),
                                       v_rt_nrp                CHAR(11),
                                       p_d_id_cre_ctr_arch     DECIMAL(9,0),
                                       p_folio_arh             DECIMAL(9,0),
                                       v_ax_id_derechohabiente DECIMAL(9,0),
                                       v_ax_tipo_trabajador    CHAR(1))

   RETURNING SMALLINT, CHAR(4), VARCHAR(140)

   -- Parámetros de respuesta
   DEFINE vcodResp                 CHAR(4);
   DEFINE vdescOrig                VARCHAR(140);

   -- Registro de la temporal
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(8,0);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2);

   -- Registro cre acreditado
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
   DEFINE cre_valor_dscto           DECIMAL(12,4);
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
   DEFINE cre_edo_mn                SMALLINT;

   -- Registro cre rch acreditado
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
   DEFINE rch_valor_dscto           DECIMAL(12,4);
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
   DEFINE v_sdo_monto_aivs         DECIMAL(16,6);
   DEFINE v_sdo_monto_pesos        DECIMAL(12,2);
   DEFINE v_sdo_f_proceso          DATE;

   -- Campos auxiliares
   DEFINE v_ax_cuenta_acpt          INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech          INTEGER; -- contador de registros rechazados
   DEFINE v_ax_exist_derech_vig     SMALLINT; -- indica si el derechohabiente existe como vigente
   DEFINE v_ax_exist_numcred        INTEGER; -- contador de registros auxiliar
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar
   DEFINE v_ax_sdo_cred_aux         DECIMAL(22,2); -- saldo del crédito
   DEFINE v_ax_ssv_92_97            DECIMAL(22,2); -- saldo deudor
   DEFINE v_ax_valor_dscto          DECIMAL(12,4); -- valor del descuento
   DEFINE v_ax_tpo_originacion      SMALLINT; -- tipo de originación
   DEFINE v_ax_tpo_credito          SMALLINT; -- tipo de credito del registro
   DEFINE v_ax_id_credito           SMALLINT; -- identificador del crédito
   DEFINE v_error                   SMALLINT; -- en caso de error contiene el código
   DEFINE v_id_cre_acreditado       DECIMAL(9,0);
   DEFINE v_edo_nci                 SMALLINT;
   DEFINE v_entidad                 SMALLINT;
   DEFINE v_ax_tpo_transferencia    CHAR(2);
   DEFINE v_ax_id_deudor            SMALLINT;
   DEFINE v_ax_movimiento           SMALLINT;
   DEFINE v_ax_historico            SMALLINT;
   DEFINE v_ax_ins_hist             SMALLINT;
   DEFINE v_ax_estado               SMALLINT;
   DEFINE v_ax_cre_acreditado       DECIMAL(9,0);
   DEFINE v_ax_edo_prcsr            SMALLINT;

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
      END IF

      LET vcodResp  = "2999";
      LET vdescOrig = "ERROR EN PROCESO, excepcion: "||v_error;
      LET v_error   = 2999;

      -- Devolverá el código de error cuando ocurra una excepción diferente a -239
      RETURN v_error, vcodResp, vdescOrig;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRecurr.trace';
   --SET DEBUG FILE TO '/safreviv_int/archivos/agrIntegRecurr.trace';
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
   LET v_ax_tpo_originacion       = NULL;
   LET v_ax_ssv_92_97             = 0;
   LET v_ax_valor_dscto           = NULL;
   LET v_rt_fec_ini_oblig_patron  = NULL;
   LET v_rt_nss_liberado          = NULL;
   LET v_rt_fec_proceso           = NULL;
   LET v_ax_sdo_cred_aux          = NULL;
   LET v_rt_fec_prox_liquidar     = NULL;
   LET v_rt_fec_dsd_avis_desast   = NULL;
   LET v_rt_fec_hst_avis_desast   = NULL;
   LET v_rt_tpo_rechazo           = NULL;
   LET v_ax_sts_registro          = NULL;
   LET v_ax_tpo_transferencia     = NULL;
   LET v_edo_nci                  = 0;
   LET v_entidad                  = 0;
   LET v_ax_tpo_credito           = 0;
   LET v_rt_sdo_credito           = 0;
   LET cre_edo_mn                 = 148;
   LET v_ax_historico             = 0;  --sin históricos
   LET v_ax_ins_hist              = 0;  --registro no originado
   LET cre_tpo_credito            = v_rt_tpo_credito;
   LET vdescOrig                  = "CRÉDITO ORIGINADO";
   LET vcodResp                   = "01";
   LET v_ax_estado                = 18;
   LET v_ax_edo_procesar          = 60;
   LET v_ax_edo_prcsr             = 60;

   -----------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE NUEVOS ACREDITADOS --
   -- ORIGINADOS VÍA MICROFLUJO                       --
   -----------------------------------------------------

   IF NOT EXISTS ( SELECT id_derechohabiente
                    FROM cre_acreditado
                   WHERE id_derechohabiente = v_ax_id_derechohabiente
                     AND tpo_credito        = cre_tpo_credito
                     AND num_credito        = v_rt_num_credito
                     AND estado             = cre_edo_mn
                ) THEN
      LET v_ax_ins_hist = 1;
   END IF

   FOREACH
      SELECT id_cre_acreditado, edo_procesar
        INTO v_ax_cre_acreditado, v_ax_edo_prcsr
        FROM cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND estado             = 18

      UPDATE cre_acreditado
         SET estado = 19
       WHERE id_cre_acreditado = v_ax_cre_acreditado;
   END FOREACH;

   IF v_rt_tpo_registro = 20 THEN
      LET v_ax_ins_hist = 1;
   END IF

   IF v_ax_edo_prcsr IS NULL OR v_ax_edo_prcsr = "" THEN
      LET v_ax_edo_prcsr = 60;
   END IF

   IF v_ax_ins_hist = 1 THEN
      -- se consulta el tipo de crédito y el tipo originación para el tipo crédito
      FOREACH
       SELECT FIRST 1 tpo_credito, tpo_originacion
         INTO v_ax_tpo_credito, v_ax_tpo_originacion
         FROM cat_tipo_credito
        WHERE tpo_credito  = cre_tpo_credito
          AND f_actualiza <= v_rt_fec_otorgamiento
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se valida que el id_derechohabiente obtenido no exista en la tabla maestro (vigente)
      IF EXISTS (SELECT UNIQUE c.id_derechohabiente
                   FROM cre_acreditado c,
                        cat_maq_credito m
                  WHERE c.id_derechohabiente = v_ax_id_derechohabiente
                    AND c.estado = m.estado
                    AND m.entidad = 1) THEN
         -- se prende la bandera
         LET v_ax_exist_derech_vig = 1;
      ELSE
         -- se mantiene la bandera apagada
         LET v_ax_exist_derech_vig = 0;
      END IF

      -- se valida que el num crédito no exista en la tabla maestro para el mismo tipo de originación
      FOREACH
         SELECT c1.estado, c2.entidad
           INTO v_edo_nci, v_entidad
           FROM cre_acreditado c1, cat_maq_credito c2
          WHERE c1.tpo_originacion = v_ax_tpo_originacion
            AND c1.num_credito     = v_rt_num_credito
            AND c1.edo_credito     = 1
            AND c1.estado          = c2.estado
      END FOREACH;

      IF v_entidad = 1 THEN
         -- se prende la bandera
         LET v_ax_exist_numcred = 1; --existe nci vigente
      END IF

      -- se calculan los valores
      LET v_ax_sdo_cred_aux = 0;
      LET v_ax_ssv_92_97    = v_rt_ssv_92_97 / 100;

      IF v_rt_tpo_descuento = 2 THEN
         LET v_ax_valor_dscto  = v_rt_val_descuento / 1000;
      ELSE
         LET v_ax_valor_dscto  = v_rt_val_descuento / 10000;
      END IF
      -- si no existe con las siguientes validaciones se rechaza el registro:
      -- * el derechohabiente no debe estar en catálogo de acreditados con crédito vigente
      -- * el tipo de crédito debe ser de originación 1 o 4 (no debe ser nulo)
      IF v_ax_id_derechohabiente IS NOT NULL AND
         v_ax_tpo_credito IS NOT NULL AND
         v_ax_exist_derech_vig = 0 AND
         v_ax_exist_numcred = 0 THEN

         LET v_ax_ins_hist = 1;

         -- se valida el tipo de trabajador
         IF v_ax_tipo_trabajador = "I" THEN
            IF v_ax_edo_prcsr <> 0 THEN
               LET v_ax_edo_procesar = v_ax_edo_prcsr;
            ELSE
               LET v_ax_edo_procesar = 60;
            END IF
         ELSE
            LET v_ax_edo_procesar = 7;
         END IF

         -- se asigna que el registro no fue rechazado
         IF v_rt_tpo_registro = 20 THEN
            LET v_ax_sts_registro = 20;
         ELSE
            LET v_ax_sts_registro = 18;
         END IF

         IF EXISTS ( SELECT id_derechohabiente
                       FROM cre_acreditado
                      WHERE id_derechohabiente = v_ax_id_derechohabiente
                        AND estado             = v_ax_estado
                   ) THEN

            SELECT id_cre_acreditado
              INTO cre_id_cre_acreditado
              FROM cre_acreditado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND estado             = v_ax_estado;

            -- se asignan los valores en las variables que se usarán para insertar el registro
            LET cre_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
            LET cre_folio_liquida      = 0;
            LET cre_id_derechohabiente = v_ax_id_derechohabiente;
            LET cre_tpo_originacion    = v_ax_tpo_originacion;
            --LET cre_tpo_credito        = v_rt_tpo_credito;
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
            UPDATE cre_acreditado
               SET id_cre_ctr_archivo = cre_id_cre_ctr_archivo,
                   tpo_credito        = cre_tpo_credito,
                   tpo_registro       = cre_tpo_registro,
                   num_credito        = cre_num_credito,
                   sdo_deudor         = cre_sdo_deudor,
                   f_otorga           = cre_f_otorga,
                   f_culmina          = cre_f_culmina,
                   edo_credito        = cre_edo_credito,
                   tpo_dscto          = cre_tpo_dscto,
                   valor_dscto        = cre_valor_dscto,
                   nrp                = cre_nrp,
                   f_ini_dscto        = cre_f_ini_dscto,
                   nss_liberado       = cre_nss_liberado,
                   f_gen_arh          = cre_f_gen_arh,
                   sdo_credito        = cre_sdo_credito,
                   f_prox_liq         = cre_f_prox_liq,
                   f_desde            = cre_f_desde,
                   f_hasta            = cre_f_hasta,
                   tpo_rch            = cre_tpo_rch,
                   edo_procesar       = cre_edo_procesar,
                   estado             = cre_estado
             WHERE id_cre_acreditado  = cre_id_cre_acreditado;
         ELSE
            -- se asignan los valores en las variables que se usarán para insertar el registro
            LET cre_id_cre_acreditado  = seq_cre_acred.NEXTVAL;
            LET cre_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
            LET cre_folio_liquida      = 0;
            LET cre_id_derechohabiente = v_ax_id_derechohabiente;
            LET cre_tpo_originacion    = v_ax_tpo_originacion;
            --LET cre_tpo_credito        = v_rt_tpo_credito;
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
         END IF

         LET v_ax_id_credito = 1;

         -- se actualiza el registro de afi derechohabiente
         UPDATE afi_derechohabiente
            SET id_credito         = v_ax_id_credito,
                f_credito          = cre_f_otorga
          WHERE id_derechohabiente = cre_id_derechohabiente;

         IF v_ax_sts_registro = 20 THEN
            -- cambio de estatus para los tipos 18
            IF EXISTS ( SELECT id_derechohabiente
                             FROM cre_acreditado
                            WHERE id_derechohabiente = v_ax_id_derechohabiente
                              AND tpo_credito        = cre_tpo_credito
                              AND num_credito        = v_rt_num_credito
                              AND estado             = cre_edo_mn
                         ) THEN

               FOREACH
                  SELECT id_cre_acreditado
                    INTO v_id_cre_acreditado
                    FROM cre_acreditado
                   WHERE id_derechohabiente =  v_ax_id_derechohabiente
                     AND tpo_credito        =  cre_tpo_credito
                     AND num_credito        =  v_rt_num_credito
                     AND estado             =  cre_edo_mn
                     AND id_cre_acreditado  <> cre_id_cre_acreditado

                  UPDATE cre_acreditado
                     SET estado            = 19
                   WHERE id_cre_acreditado = v_id_cre_acreditado;
               END FOREACH;

               LET v_ax_historico = 2;  ---ins cre_his y act cre_sdo
            ELSE
               LET v_ax_historico = 1;  ---ins cre_his e ins cre_sdo
            END IF
         END IF
      ELSE
         -- se valida la razón del rechazo
         IF v_ax_id_derechohabiente IS NULL THEN
            -- se asigna status 11 en el registro "Derechohabiente no existe en maestro"
            LET v_ax_sts_registro = 11;
         ELIF NOT v_rt_sts_credito IN (1,3) THEN
            -- se asigna status 15 en el registro "Estatus del crédito no válido"
            LET v_ax_sts_registro = 15;
         ELIF v_ax_tpo_credito IS NULL THEN
            -- se asigna status 16 en el registro "Tipo credito no válido"
            LET v_ax_sts_registro = 16;
         ELIF v_ax_exist_derech_vig > 0 THEN
            -- se asigna status 20 en el registro "Derechohabiente vigente"
            LET v_ax_sts_registro = 20;
         ELSE
            -- se asigna status 21 en el registro "Número de crédito vigente"
            LET v_ax_sts_registro = 21;
         END IF

         LET v_ax_ins_hist = 0;

         SELECT d1.desc_estado
           INTO vdescOrig
           FROM cat_rch_acreditado d1
          WHERE d1.estado = v_ax_sts_registro;

         LET vcodResp = "2"||v_ax_sts_registro;

         -- se asignan los valores en las variables que se usarán para insertar el registro
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
   ELSE
      SELECT id_cre_acreditado
        INTO cre_id_cre_acreditado
        FROM cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND tpo_credito        = cre_tpo_credito
         AND num_credito        = v_rt_num_credito
         AND estado             = cre_edo_mn;

      -- se valida el tipo de trabajador
      IF v_ax_tipo_trabajador = "I" THEN
         IF v_ax_edo_prcsr <> 0 THEN
            LET v_ax_edo_procesar = v_ax_edo_prcsr;
         ELSE
            LET v_ax_edo_procesar = 70;
         END IF
      ELSE
         LET v_ax_edo_procesar = 7;
      END IF

      UPDATE cre_acreditado
         SET estado            = 20,
             edo_procesar      = v_ax_edo_procesar
       WHERE id_cre_acreditado = cre_id_cre_acreditado;

      LET v_ax_ins_hist  = 1;
      LET v_ax_historico = 2;
   END IF;

   IF v_ax_ins_hist = 1 THEN
      SELECT FIRST 1 DECODE(c1.id_proceso,201,"03",301,"43"), c1.id_deudor
        INTO v_ax_tpo_transferencia, v_ax_id_deudor
        FROM cat_tipo_credito c1
       WHERE c1.tpo_credito     = v_ax_tpo_credito
         AND c1.tpo_originacion = v_ax_tpo_originacion;

      --se asignan valores al registro a insertar en his_acreditado
      LET v_his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET v_his_id_cre_ctr_archivo = p_d_id_cre_ctr_arch;
      LET v_his_tpo_transferencia  = v_ax_tpo_transferencia;
      LET v_his_edo_procesar       = v_ax_edo_procesar;
      LET v_his_diagnostico        = 0;
      LET v_his_estado             = cre_estado;
      LET v_his_nss_afore          = "";
      LET v_his_rfc_afore          = "";
      LET v_his_paterno_afore      = "";
      LET v_his_materno_afore      = "";
      LET v_his_nombre_afore       = "";
      LET v_his_nom_imss           = "";
      LET v_his_f_proceso          = TODAY;

      --se inserta en la tabla his acreditado
      INSERT INTO cre_his_acreditado(
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

      IF v_rt_tpo_registro = 20 THEN
         IF EXISTS( SELECT id_derechohabiente
                      FROM cta_credito
                     WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN

            DELETE
              FROM cta_credito
             WHERE id_derechohabiente = v_ax_id_derechohabiente;
         END IF

         INSERT INTO cta_credito(
                     id_derechohabiente,
                     proceso_cod,
                     tpo_credito,
                     num_credito,
                     f_credito)
             VALUES (v_ax_id_derechohabiente,
                     301,
                     v_rt_tpo_credito,
                     v_rt_num_credito,
                     v_rt_fec_otorgamiento);
      END IF

      LET v_ax_movimiento = 181;

      -- se asignan valores al registro a insertar en saldo deudor
      LET v_sdo_id_cre_acreditado = cre_id_cre_acreditado;
      LET v_sdo_folio_referencia  = p_folio_arh;
      LET v_sdo_f_movimiento      = TODAY;
      LET v_sdo_movimiento        = v_ax_movimiento;
      LET v_sdo_id_referencia     = cre_id_cre_acreditado;
      LET v_sdo_monto_aivs        = 0;
      LET v_sdo_monto_pesos       = v_ax_ssv_92_97;
      LET v_sdo_f_proceso         = TODAY;

      IF v_ax_historico = 1 THEN
         -- se inserta en la tabla cre deudor
         INSERT INTO cre_saldo_deudor(
                     id_cre_acreditado,
                     folio_referencia,
                     f_movimiento,
                     movimiento,
                     id_referencia,
                     monto_aivs,
                     monto_pesos,
                     f_proceso)
             VALUES (v_sdo_id_cre_acreditado,
                     v_sdo_folio_referencia,
                     v_sdo_f_movimiento,
                     v_sdo_movimiento,
                     v_sdo_id_referencia,
                     v_sdo_monto_aivs,
                     v_sdo_monto_pesos,
                     v_sdo_f_proceso);
      ELSE
         IF EXISTS (SELECT id_cre_acreditado
                      FROM cre_saldo_deudor
                     WHERE id_cre_acreditado = v_sdo_id_cre_acreditado
                       AND movimiento        = v_sdo_movimiento) THEN

            INSERT INTO cre_his_sdo_deudor
            SELECT *, TODAY
              FROM cre_saldo_deudor
             WHERE id_cre_acreditado = v_sdo_id_cre_acreditado
               AND movimiento        = v_sdo_movimiento;

            UPDATE cre_saldo_deudor
               SET folio_referencia  = v_sdo_folio_referencia,
                   f_movimiento      = f_movimiento,
                   monto_pesos       = v_sdo_monto_pesos,
                   f_proceso         = v_sdo_f_proceso
             WHERE id_cre_acreditado = v_sdo_id_cre_acreditado
               AND movimiento        = v_sdo_movimiento;
         ELSE
            INSERT INTO cre_saldo_deudor(
                        id_cre_acreditado,
                        folio_referencia,
                        f_movimiento,
                        movimiento,
                        id_referencia,
                        monto_aivs,
                        monto_pesos,
                        f_proceso)
                VALUES (v_sdo_id_cre_acreditado,
                        v_sdo_folio_referencia,
                        v_sdo_f_movimiento,
                        v_sdo_movimiento,
                        v_sdo_id_referencia,
                        v_sdo_monto_aivs,
                        v_sdo_monto_pesos,
                        v_sdo_f_proceso);
         END IF
      END IF

      -- actualiza estadísticas a la tabla historica acr
      UPDATE STATISTICS FOR TABLE cre_his_acreditado;

      -- actualiza estadísticas a la tabla deudoresa
      UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

      -- actualiza estadísticas a la tabla histórica sdo
      UPDATE STATISTICS FOR TABLE cre_his_sdo_deudor;
   END IF


   -- se inicializan variables
   LET v_ax_ssv_92_97          = 0;
   LET v_rt_ssv_92_97          = 0;
   LET v_ax_tpo_transferencia  = NULL;
   LET v_id_cre_acreditado     = NULL;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_acreditado;
   UPDATE STATISTICS FOR TABLE cre_rch_acreditado;

   RETURN v_error, vcodResp, vdescOrig;

END FUNCTION
;


