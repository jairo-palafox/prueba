






CREATE FUNCTION "safreviv".fn_agr_integra_desmarca(p_v_usuario          CHAR(20),
                                        p_v_arch_proceso     CHAR(100),
                                        p_d_folio            DECIMAL(9),
                                        p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   -- REGISTRO tmp desmarca det agr
   DEFINE v_rt_tpo_registro         CHAR(2);
   DEFINE v_rt_nss                  CHAR(11);
   DEFINE v_rt_num_credito          DECIMAL(10,0);
   DEFINE v_rt_ssv_92_97            DECIMAL(12,2);
   DEFINE v_rt_fec_otorgamiento     DATE;
   DEFINE v_rt_fec_culminacion      DATE;
   DEFINE v_rt_tpo_originacion      SMALLINT;
   DEFINE v_rt_tpo_credito          SMALLINT;
   DEFINE v_rt_sts_credito          SMALLINT;
   DEFINE v_rt_tpo_descuento        SMALLINT;
   DEFINE v_rt_val_descuento        DECIMAL(8);
   DEFINE v_rt_nrp                  CHAR(11);
   DEFINE v_rt_fec_ini_oblig_patron DATE;
   DEFINE v_rt_nss_liberado         CHAR(11);
   DEFINE v_rt_fec_proceso          DATE;
   DEFINE v_rt_sdo_credito          DECIMAL(12,0);
   DEFINE v_rt_fec_prox_liquidar    DATE;
   DEFINE v_rt_fec_dsd_avis_desast  DATE;
   DEFINE v_rt_fec_hst_avis_desast  DATE;
   DEFINE v_rt_tpo_rechazo          SMALLINT;

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
   DEFINE rch_valor_dscto           DECIMAL(12,4);
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
   DEFINE v_estado_acred            SMALLINT;

   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_operacion            SMALLINT; -- operación del proceso
   DEFINE v_ax_id_lote_acpt         INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech         INTEGER; -- total de registros rechazados
   DEFINE v_ax_id_lote_sin_orig     INTEGER; -- total de registros sin originación
   DEFINE v_ax_num_registros        INTEGER; -- total de registros con originación
   DEFINE v_ax_sts_registro         SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_estado               SMALLINT; -- estado
   DEFINE v_ax_edo_procesar         SMALLINT; -- estado procesar
   DEFINE v_ax_tpo_transferencia    CHAR(2); -- tipo de transferencia
   DEFINE r_ax_bandera              SMALLINT; -- valor de regreso de la actualización
   DEFINE v_ax_id_deudor            SMALLINT; -- id deudor
   DEFINE v_i_glo_estado            SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_f_proceso            DATE; --fecha de proceso
   DEFINE v_ax_cont_regs            INTEGER; -- contador de registros
   DEFINE v_b_desmarcar             SMALLINT; -- booleana que indica si el registro se debe o no desmarcar
   DEFINE v_ax_error                SMALLINT; -- contiene el código de error en caso de ocurrir
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);
   DEFINE v_c_nss                   CHAR(11);

   -- Campos desmarca 225
   DEFINE v_dm_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_dm_marca_entra          SMALLINT;
   DEFINE v_dm_n_referencia         INTEGER;
   DEFINE v_dm_estado_marca         SMALLINT;
   DEFINE v_dm_marca_causa          SMALLINT;
   DEFINE v_dm_usuario              CHAR(12);
   DEFINE v_dm_proceso_cod          SMALLINT;
   DEFINE v_ax_cod_error            SMALLINT;

   ON EXCEPTION SET v_ax_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_ax_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/agrIntegDesm.trace';
   ---TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt      = 0;
   LET v_ax_id_lote_rech      = 0;
   LET v_ax_id_lote_sin_orig  = 0;
   LET v_ax_operacion         = 30; -- Desmarca
   LET v_i_glo_estado         = 2; -- estado Integrado para glo ctr archivo
   LET v_ax_f_proceso         = TODAY;
   LET v_ax_tpo_transferencia = "43";
   LET v_ax_error             = 0;
   LET v_isam_err             = 0;
   LET v_c_msj                = 'El proceso finalizó correctamente';
   LET v_c_nss                = "0"; -- valor del NSS antes de entrar al ciclo
   LET v_estado_acred         = "";

   LET v_dm_estado_marca      = 0;
   LET v_dm_marca_causa       = 0;
   LET v_dm_usuario           = p_v_usuario;
   LET v_dm_proceso_cod       = 308;
   LET v_ax_sts_registro      = 160;
   LET v_rt_tpo_originacion   = 4;

   -- se obtienen los datos de tmp desmarca para el archivo en proceso
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
      FROM safre_tmp:tmp_desmarca_det_agr
     WHERE tpo_registro = "11"

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = v_rt_nss;
      LET v_ax_id_derechohabiente = NULL;

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
         LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
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
                     
         --Se integran las inserciones de rechazo
         INSERT INTO safre_tmp:tmp_nss_desmarcados_agr(
                     nss         ,
                     tpo_credito ,
                     estado      )
          VALUES ( rch_nss,
                  rch_tpo_credito,
                  3);
                   
         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se consulta el tipo de crédito y el tipo originación para el tipo credito
      FOREACH
       SELECT FIRST 1 id_deudor
         INTO v_ax_id_deudor
         FROM cat_tipo_credito
        WHERE tpo_credito = v_rt_tpo_credito
          AND tpo_originacion IN (1,4)
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se inicializa el contador de registros
      LET v_ax_cont_regs = 0;

      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente en proceso
      FOREACH
       SELECT id_cre_acreditado, edo_procesar, estado
         INTO v_cre_id_cre_acreditado, v_ax_edo_procesar, v_estado_acred
         FROM cre_acreditado
        WHERE id_derechohabiente = v_ax_id_derechohabiente
          AND tpo_credito        = v_rt_tpo_credito
          AND num_credito        = v_rt_num_credito

         -- se valida que el estado procesar sea mayor o igual a 120 - SALDO TRANSFERIDO
         IF v_ax_edo_procesar >  120 OR
            v_ax_edo_procesar <= 70 THEN
            -- se asignan los valores en las variables que se usarán para insertar el registro
            LET v_ax_sts_registro = 160;

            -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
            INSERT INTO safre_tmp:tmp_nss_desmarcados_agr (
                        nss,
                        tpo_credito,
                        estado)
                VALUES (v_rt_nss,
                        v_rt_tpo_credito,
                        1); --Se agrega nuevo valor de 1 en la columna para indicar que la desmarca fué aceptada
         ELIF v_ax_edo_procesar = 120 THEN
            -- se inicializa la variable
            LET v_ax_estado = NULL;
            LET v_b_desmarcar = 1;

            -- se obtienen los estados de la tabla de Uso de Garantía
            FOREACH
               SELECT estado, edo_procesar
                 INTO v_ax_estado, v_ax_edo_procesar
                 FROM cre_uso_garantia
                WHERE id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_transferencia  = v_ax_tpo_transferencia
                  AND id_cre_ctr_archivo IN (
                      SELECT id_cre_ctr_archivo
                        FROM cre_ctr_archivo
                       WHERE operacion = 43)

               IF v_ax_estado IS NULL OR
                  v_ax_estado < 20    OR
                  v_ax_estado > 142   OR
                  ((v_ax_estado >= 130 AND v_ax_estado <= 142) AND v_ax_edo_procesar = 120) THEN
                  CONTINUE FOREACH;
               ELSE
                  -- existen registro(s) en Uso de Garantía que no cumplen con la condición. No desmarcar
                  LET v_b_desmarcar = 0;

                  EXIT FOREACH;
               END IF
            END FOREACH

            IF v_b_desmarcar = 1 THEN
               -- se asignan los valores en las variables que se usarán para insertar el registro
               LET v_ax_sts_registro = 160;

               -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
               INSERT INTO safre_tmp:tmp_nss_desmarcados_agr (
                           nss,
                           tpo_credito,
                           estado)
                   VALUES (v_rt_nss,
                           v_rt_tpo_credito,
                           1);            ELSE
               -- se asigna rechazo de desmarca
               LET v_ax_sts_registro = 280;

               -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
               INSERT INTO safre_tmp:tmp_nss_desmarcados_agr (
                           nss,
                           tpo_credito,
                           estado)
               VALUES (v_rt_nss,
                       v_rt_tpo_credito,
                       2);            END IF
         ELSE
            IF v_estado_acred = 150 THEN
               LET v_ax_sts_registro = 170;
            ELIF v_estado_acred < 140 THEN
               IF v_estado_acred < 25 THEN
                  -- se asigna rechazo de desmarca
                  LET v_ax_sts_registro = 270;
               ELSE
                  LET v_ax_sts_registro = 275;
               END IF
            ELSE
               IF v_estado_acred < 145 THEN
                  -- se asigna rechazo de desmarca
                  LET v_ax_sts_registro = 300;
               ELSE
                  LET v_ax_sts_registro = 310;
               END IF
            END IF

            -- se inserta en la tabla temporal, para generar archivo de tipos de crédito
            INSERT INTO safre_tmp:tmp_nss_desmarcados_agr (
                        nss,
                        tpo_credito,
                        estado)
                VALUES (v_rt_nss,
                        v_rt_tpo_credito,
                        2);         END IF

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
            SET estado            = v_ax_sts_registro
          WHERE id_cre_acreditado = v_cre_id_cre_acreditado;

         -- se incrementa el contador de registros
         LET v_ax_cont_regs = v_ax_cont_regs + 1;

         LET v_ax_estado       = "";
         LET v_ax_edo_procesar = "";
         LET v_estado_acred    = "";
         LET v_ax_sts_registro = 160;
         LET v_b_desmarcar     = 0;
      END FOREACH

      -- se verifica si se procesaron registro en la tabla maestro
      IF v_ax_cont_regs = 0 THEN
         -- se asigna el estado de rechazo por No Existe Marca Crédito Vigente
         LET v_ax_sts_registro = 13;

         -- se asigna el valor al registro de rechazo
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = v_rt_nss;
         LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
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

        --Se integran las inserciones de rechazo
         INSERT INTO safre_tmp:tmp_nss_desmarcados_agr
                     (nss,
                      tpo_credito,
                      estado)
              VALUES (rch_nss,
                      rch_tpo_credito,
                      3);

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
      ELSE
         -- el contador de registros es mayor que cero por lo que se procesó satisfactoriamente el registro
         LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
      END IF

      -- desmarca 225 registros liquidados pendientes de conciliación
      FOREACH
         SELECT id_derechohabiente,
                marca,
                n_referencia
           INTO v_dm_id_derechohabiente,
                v_dm_marca_entra,
                v_dm_n_referencia
           FROM sfr_marca_activa
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND marca              = 225

         -- se invoca la función de desmarca
         EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                             v_dm_marca_entra,
                                             v_dm_n_referencia,
                                             v_dm_estado_marca,
                                             v_dm_marca_causa,
                                             v_dm_usuario,
                                             v_dm_proceso_cod)
                                        INTO v_ax_cod_error;
      END FOREACH;

      LET v_ax_sts_registro = 160;
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
     FROM safre_tmp:tmp_desmarca_det_agr_02

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = v_rt_nss;

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
      LET rch_tpo_originacion    = 4; -- Anualidades Garantizadas
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

         --Se integran las inserciones de rechazo
         INSERT INTO safre_tmp:tmp_nss_desmarcados_agr(
                     nss         ,
                     tpo_credito ,
                     estado      )
          VALUES ( rch_nss,
                  rch_tpo_credito,
                  3);

      -- se incrementa el contrador de rechazos
      LET v_ax_id_lote_sin_orig = v_ax_id_lote_sin_orig + 1;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- se cuenta el número de registros con Originación
   SELECT COUNT(*)
     INTO v_ax_num_registros
     FROM safre_tmp:tmp_desmarca_det_agr_01;

   -- se suman los registros con originación con los aceptados
   LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + v_ax_num_registros;

   -- se cuenta el número de registros con Reactivación
   SELECT COUNT(*)
     INTO v_ax_num_registros
     FROM safre_tmp:tmp_desmarca_det_agr_04;

   -- se suman los registros con reactivación con los aceptados
   LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + v_ax_num_registros;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;
   UPDATE STATISTICS FOR TABLE cre_rch_acreditado;

   -- se ejecuta el sp que actualiza el registro de la tabla de control de archivos a estatus "Integrado"
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio,v_ax_id_lote_acpt,v_ax_id_lote_rech,v_ax_id_lote_sin_orig,p_ax_id_cre_ctr_arch);

   RETURN v_ax_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION;


