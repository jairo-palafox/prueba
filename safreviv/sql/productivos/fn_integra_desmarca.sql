






CREATE FUNCTION "safreviv".fn_integra_desmarca(p_v_usuario CHAR(20),
                                    p_v_arch_proceso CHAR(100),
                                    p_d_folio DECIMAL(9,0),
                                    p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT
   -- REGISTRO tmp acr desmarca
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10,0);
   DEFINE v_rt_ssv_92_97            DECIMAL(8,0);
   DEFINE v_rt_fec_otorgamiento     DATE;
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_credito          DECIMAL(3,0);
   DEFINE v_rt_sts_credito          DECIMAL(3,0);
   DEFINE v_rt_tpo_descuento        DECIMAL(1,0);
   DEFINE v_rt_val_descuento        DECIMAL(8,0);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(8,0);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          DECIMAL(2,0);
   -- REGISTRO cre rch acreditado
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
   -- REGISTRO cre acreditado
   DEFINE v_cre_id_cre_acreditado   DECIMAL(9,0);
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_operacion            SMALLINT; -- operacion del proceso
   DEFINE v_ax_id_lote_acpt         INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech         INTEGER; -- total de registros rechazados
   DEFINE v_ax_id_lote_sin_orig     INTEGER; -- total de registros sin originacion
   DEFINE v_ax_num_registros        INTEGER; -- total de registros con originacion
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_tpo_transferencia    CHAR(2); -- tipo de transferencia
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar         
   DEFINE r_ax_bandera              SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_estado                SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_ax_f_proceso            DATE; --fecha de proceso

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegDesm.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_ax_id_lote_sin_orig = 0;
   LET v_ax_operacion = 30; -- Desmarca
   LET v_i_estado = 2; -- estado Integrado para glo ctr archivo
   LET v_ax_error = 0;
   LET v_ax_f_proceso = TODAY;
   LET v_ax_tpo_transferencia = "03"; -- Transferencia de Acreditados

   FOREACH
      -- se obtienen los datos de tmp desmarca para el archivo en proceso
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
      FROM safre_tmp:tmp_acr_desmarca

      -- se inicializan variables
      LET v_ax_id_derechohabiente = NULL;
      LET v_cre_id_cre_acreditado = NULL;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = v_rt_nss;

      -- se valida que el NSS exista en catalogo
      IF v_ax_id_derechohabiente IS NULL THEN
         -- se asigna el estado de rechazo por Trabajador Inexistente
         LET v_ax_sts_registro = 11;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_rt_ssv_92_97/100;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_rt_val_descuento/10000;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_rt_sdo_credito/100;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
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

      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente      
      SELECT id_cre_acreditado, edo_procesar
        INTO v_cre_id_cre_acreditado, v_ax_edo_procesar
        FROM cre_acreditado
       WHERE id_derechohabiente = v_ax_id_derechohabiente
         AND tpo_credito = v_rt_tpo_credito
         AND num_credito = v_rt_num_credito;

      -- se valida si fue posible identificar el registro correspondiente en la tabla maestro
      IF v_cre_id_cre_acreditado IS NULL THEN
         -- se asigna el estado de rechazo por No Existe Marca Crédito Vigente
         LET v_ax_sts_registro = 13;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
         LET rch_tpo_credito        = v_rt_tpo_credito;
         LET rch_tpo_registro       = v_rt_tpo_registro;
         LET rch_num_credito        = v_rt_num_credito;
         LET rch_sdo_deudor         = v_rt_ssv_92_97/100;
         LET rch_f_otorga           = v_rt_fec_otorgamiento;
         LET rch_f_culmina          = v_rt_fec_culminacion;
         LET rch_edo_credito        = v_rt_sts_credito;
         LET rch_tpo_dscto          = v_rt_tpo_descuento;
         LET rch_valor_dscto        = v_rt_val_descuento/10000;
         LET rch_nrp                = v_rt_nrp;
         LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
         LET rch_nss_liberado       = v_rt_nss_liberado;
         LET rch_f_gen_arh          = v_rt_fec_proceso;
         LET rch_sdo_credito        = v_rt_sdo_credito/100;
         LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
         LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
         LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
         LET rch_tpo_rch            = v_rt_tpo_rechazo;
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

      -- se valida que el estado procesar sea mayor o igual a 120 - SALDO TRANSFERIDO
      IF v_ax_edo_procesar >= 120 OR
         v_ax_edo_procesar <= 60 THEN
         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET v_ax_sts_registro = 160;

         -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
         INSERT INTO safre_tmp:tmp_nss_desmarcados_acr (
                     nss,
                     tpo_credito)
             VALUES (v_rt_nss,
                     v_rt_tpo_credito);
      ELSE
         -- se asigna rechazo de desmarca
         LET v_ax_sts_registro = 270;
      END IF

      -- se inserta en cre his acreditado
      INSERT INTO cre_his_acreditado (
                  id_cre_acreditado,
                  id_cre_ctr_archivo,
                  tpo_transferencia,
                  edo_procesar,
                  estado,
                  f_proceso)
          VALUES (v_cre_id_cre_acreditado,
                  p_ax_id_cre_ctr_arch,
                  v_ax_tpo_transferencia,
                  v_ax_edo_procesar,
                  v_ax_sts_registro,
                  v_ax_f_proceso);

      -- se actualizan estados en  cre acreditado
      UPDATE cre_acreditado 
         SET estado = v_ax_sts_registro
       WHERE id_cre_acreditado = v_cre_id_cre_acreditado         
         AND id_derechohabiente = v_ax_id_derechohabiente;

      -- se incrementa el contador de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
   END FOREACH;

   -- se procesan los registros tipo "02" para guardarlos en la tabla de rechazos
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
     FROM safre_tmp:tmp_acr_desmarca_02
      -- se validan los registros que no pueden ir nulos
      IF v_rt_num_credito IS NULL THEN
         LET v_rt_num_credito = 0;
      END IF;
      IF v_rt_ssv_92_97 IS NULL THEN
         LET v_rt_ssv_92_97 = 0;
      END IF;

      -- se asigna el estado de rechazo por Tipo Registro no Valido
      LET v_ax_sts_registro = 14;

      -- se asigna el valor al registro de rechazo
      LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET rch_nss                = v_rt_nss;
      LET rch_tpo_originacion    = 1; -- Transferencia de Acreditados
      LET rch_tpo_credito        = v_rt_tpo_credito;
      LET rch_tpo_registro       = v_rt_tpo_registro;
      LET rch_num_credito        = v_rt_num_credito;
      LET rch_sdo_deudor         = v_rt_ssv_92_97/100;
      LET rch_f_otorga           = v_rt_fec_otorgamiento;
      LET rch_f_culmina          = v_rt_fec_culminacion;
      LET rch_edo_credito        = v_rt_sts_credito;
      LET rch_tpo_dscto          = v_rt_tpo_descuento;
      LET rch_valor_dscto        = v_rt_val_descuento/10000;
      LET rch_nrp                = v_rt_nrp;
      LET rch_f_ini_dscto        = v_rt_fec_ini_oblig_patron;
      LET rch_nss_liberado       = v_rt_nss_liberado;
      LET rch_f_gen_arh          = v_rt_fec_proceso;
      LET rch_sdo_credito        = v_rt_sdo_credito/100;
      LET rch_f_prox_liq         = v_rt_fec_prox_liquidar;
      LET rch_f_desde            = v_rt_fec_dsd_avis_desast;
      LET rch_f_hasta            = v_rt_fec_hst_avis_desast;
      LET rch_tpo_rch            = v_rt_tpo_rechazo;
      LET rch_estado             = v_ax_sts_registro;

      --se inserta en la tabla de rechazos
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

      -- se incrementa el contrador de sin originacion
      LET v_ax_id_lote_sin_orig = v_ax_id_lote_sin_orig + 1;
   END FOREACH;

   -- se cuenta el numero de registros con Originación
   SELECT COUNT(*)
     INTO v_ax_num_registros
     FROM safre_tmp:tmp_acr_desmarca_01;

   -- se suman los registros con originación con los aceptados
   LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + v_ax_num_registros;

   -- se cuenta el numero de registros con Reactivación
   SELECT COUNT(*)
     INTO v_ax_num_registros
     FROM safre_tmp:tmp_acr_desmarca_04;

   -- se suman los registros con reactivación con los aceptados
   LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + v_ax_num_registros;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;

   -- se ejecuta el sp que actualiza el registro de la tabla de control de archivos a estatus "Integrado"
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio,v_ax_id_lote_acpt,v_ax_id_lote_rech,v_ax_id_lote_sin_orig,p_ax_id_cre_ctr_arch);

   RETURN v_ax_error;

END FUNCTION
;


