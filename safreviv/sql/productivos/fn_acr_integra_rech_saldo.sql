






CREATE FUNCTION "safreviv".fn_acr_integra_rech_saldo(p_v_usuario CHAR(20),
                                          p_v_arch_proceso CHAR(100),
                                          p_d_folio DECIMAL(10,0),
                                          p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- REGISTRO tmp acr solic
   DEFINE tmp_tpo_registro        CHAR(2);
   DEFINE tmp_contador_srv        DECIMAL(10,0);
   DEFINE tmp_tpo_ent_rec         CHAR(2);
   DEFINE tmp_cve_ent_rec         CHAR(3);
   DEFINE tmp_tpo_ent_ced         CHAR(2);
   DEFINE tmp_cve_ent_ced         CHAR(3);
   DEFINE tmp_tpo_transferencia   CHAR(2);
   DEFINE tmp_fec_presentacion    DATE;
   DEFINE tmp_filler1             CHAR(8);
   DEFINE tmp_curp                CHAR(18);
   DEFINE tmp_nss_infonavit       CHAR(11);
   DEFINE tmp_filler2             CHAR(15);
   DEFINE tmp_rfc_infonavit       CHAR(13);
   DEFINE tmp_ape_pat_infonavit   CHAR(40);
   DEFINE tmp_ape_mat_infonavit   CHAR(40);
   DEFINE tmp_nombre_infonavit    CHAR(40);
   DEFINE tmp_filler3             CHAR(22);
   DEFINE tmp_id_lote_sol         CHAR(16);
   DEFINE tmp_filler4             CHAR(15);
   DEFINE tmp_nss_afore           CHAR(11);
   DEFINE tmp_rfc_afore           CHAR(13);
   DEFINE tmp_filler5             CHAR(30);
   DEFINE tmp_ape_pat_afore       CHAR(40);
   DEFINE tmp_ape_mat_afore       CHAR(40);
   DEFINE tmp_nombre_afore        CHAR(40);
   DEFINE tmp_filler6             CHAR(30);
   DEFINE tmp_num_aplic_intereses DECIMAL(15,0);
   DEFINE tmp_ult_aportacion      DECIMAL(15,0);
   DEFINE tmp_filler7             CHAR(78);
   DEFINE tmp_cod_resultado_ope   CHAR(2);
   DEFINE tmp_diag_proceso        CHAR(15);
   DEFINE tmp_nombre_imss         CHAR(50);
   DEFINE tmp_num_cred_infonavit  DECIMAL(10,0);
   DEFINE tmp_filler8             CHAR(53);
   DEFINE tmp_periodo_pago        CHAR(6);
   DEFINE tmp_filler9             CHAR(12);
   --REGISTRO rch acreditado
   DEFINE rch_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE rch_nss                         CHAR(11);
   DEFINE rch_tpo_originacion             SMALLINT;
   DEFINE rch_tpo_registro                CHAR(2);
   DEFINE rch_num_credito                 DECIMAL(10,0);
   DEFINE rch_sdo_deudor                  DECIMAL(12,2);
   DEFINE rch_valor_dscto                 DECIMAL(8,4);
   DEFINE rch_estado                      SMALLINT;
   -- REGISTRO cre his acreditado
   DEFINE his_id_cre_acreditado    DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE his_tpo_transferencia    CHAR(2);
   DEFINE his_edo_procesar         SMALLINT;
   DEFINE his_diagnostico          CHAR(3);
   DEFINE his_estado               SMALLINT;
   DEFINE his_nss_afore            CHAR(11);
   DEFINE his_rfc_afore            CHAR(13);
   DEFINE his_paterno_afore        CHAR(40);
   DEFINE his_materno_afore        CHAR(40);
   DEFINE his_nombre_afore         CHAR(40);
   DEFINE his_nom_imss             CHAR(50);
   DEFINE his_f_proceso            DATE;
   -- REGISTRO tmp deudor rechazo
   DEFINE tmp_deudor_id_cre_acreditado  DECIMAL(9,0);
   DEFINE tmp_deudor_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_deudor_nss                CHAR(11);
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0); -- identificador de cre acreditado
   DEFINE v_ax_estado             SMALLINT; -- estado
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_id_lote_acpt       INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech       INTEGER; -- total de registros rechazados
   DEFINE v_ax_operacion          SMALLINT; -- operacion del proceso
   DEFINE v_ax_marca_prc          SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_credito        SMALLINT; -- tipo de crédito
   DEFINE v_ax_proceso_cod        SMALLINT; -- código del proceso
   DEFINE v_i_estado              SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_c_nss                 CHAR(11);
   DEFINE v_b_existe_reg          SMALLINT; -- booleana que indica si existe o no la originación de crédito
   DEFINE r_ax_existe_marca_prc   SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera            SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_edo_retorno        SMALLINT; -- estado retorno de alguna funcion

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegRchSdo.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt    = 0; -- número de registros aceptados
   LET v_ax_id_lote_rech    = 0; -- número de registros rechazados
   LET v_ax_operacion       = 01; -- operación (01-Rechazo de saldos)
   LET v_ax_estado          = 0; -- estado del registro
   LET v_ax_edo_procesar    = 0; -- estado procesar
   LET v_i_estado           = 2; -- estado Integrado
   LET v_ax_proceso_cod     = 203; -- Rechazo de Saldos ACR
   LET v_error              = 0;
   LET v_isam_err           = 0;
   LET v_c_msj              = 'El proceso finalizó correctamente';
   LET v_c_nss              = "0"; -- valor del NSS antes de entrar al ciclo

   -- se obtienen los datos de la tabla temporal del proceso de rechazo de saldos
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_contador_srv,
           tmp_tpo_ent_rec,
           tmp_cve_ent_rec,
           tmp_tpo_ent_ced,
           tmp_cve_ent_ced,
           tmp_tpo_transferencia,
           tmp_fec_presentacion,
           tmp_filler1,
           tmp_curp,
           tmp_nss_infonavit,
           tmp_filler2,
           tmp_rfc_infonavit,
           tmp_ape_pat_infonavit,
           tmp_ape_mat_infonavit,
           tmp_nombre_infonavit,
           tmp_filler3,
           tmp_id_lote_sol,
           tmp_filler4,
           tmp_nss_afore,
           tmp_rfc_afore,
           tmp_filler5,
           tmp_ape_pat_afore,
           tmp_ape_mat_afore,
           tmp_nombre_afore,
           tmp_filler6,
           tmp_num_aplic_intereses,
           tmp_ult_aportacion,
           tmp_filler7,
           tmp_cod_resultado_ope,
           tmp_diag_proceso,
           tmp_nombre_imss,
           tmp_num_cred_infonavit,
           tmp_filler8,
           tmp_periodo_pago,
           tmp_filler9
      FROM safre_tmp:tmp_acr_solic
      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss_infonavit;

      -- se asume que no existirá la originación de crédito
      LET v_b_existe_reg = 0;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_nss_infonavit;

      -- se obtiene la información de la tabla de generación de archivo
      FOREACH
       SELECT FIRST 1 t1.id_cre_acreditado
         INTO v_ax_id_cre_acreditado
         FROM safre_tmp:tmp_acr_solic_sdo t1
        WHERE t1.nss = v_c_nss
      END FOREACH;

      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente
      FOREACH
       SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar, tpo_credito
         INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar, v_ax_tpo_credito
         FROM cre_acreditado
        WHERE id_cre_acreditado = v_ax_id_cre_acreditado

         -- se indica que existe la originación de crédito
         LET v_b_existe_reg = 1;
      END FOREACH;

      -- se verifica si no existió la originación de crédito
      IF v_b_existe_reg = 0 THEN
         -- No existió la originación de crédito. Se rechaza el registro
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss_infonavit;
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_cred_infonavit;
         LET rch_sdo_deudor         = 0;
         LET rch_valor_dscto        = 0;
         LET rch_estado             = 26; -- 26-REGISTRO SIN ORIGINACIÓN DE CRÉDITO

         -- se inserta el registro rechazado
         INSERT INTO cre_rch_acreditado(
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     valor_dscto,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_valor_dscto,
                     rch_estado);

         -- se incrementa el numero de registros aceptados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se valida el estado procesar. Si corresponde a sólo infonavit se rechaza el registro
      IF v_ax_edo_procesar = 5 THEN
         -- se rechaza el registro
         LET v_ax_estado = 240;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
         LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET his_tpo_transferencia  = tmp_tpo_transferencia;
         LET his_edo_procesar       = 90; -- Saldo Rechazado
         LET his_diagnostico        = tmp_diag_proceso[1,3];
         LET his_estado             = v_ax_estado;
         LET his_nss_afore          = tmp_nss_afore;
         LET his_rfc_afore          = tmp_rfc_afore;
         LET his_paterno_afore      = tmp_ape_pat_afore;
         LET his_materno_afore      = tmp_ape_mat_afore;
         LET his_nombre_afore       = tmp_nombre_afore;
         LET his_nom_imss           = tmp_nombre_imss;
         LET his_f_proceso          = TODAY;

         -- se inserta registro en tabla his acreditado
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

         -- El registro corresponde a Solo Infonavit. Se rechaza el registro
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss_infonavit;
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = tmp_num_cred_infonavit;
         LET rch_sdo_deudor         = 0;
         LET rch_valor_dscto        = 0;
         LET rch_estado             = 24; -- 24-REGISTRO CORRESPONDE A SOLO INFONAVIT

         -- se inserta el registro rechazado
         INSERT INTO cre_rch_acreditado(
                     id_cre_ctr_archivo,
                     nss,
                     tpo_originacion,
                     tpo_registro,
                     num_credito,
                     sdo_deudor,
                     valor_dscto,
                     estado)
             VALUES (rch_id_cre_ctr_archivo,
                     rch_nss,
                     rch_tpo_originacion,
                     rch_tpo_registro,
                     rch_num_credito,
                     rch_sdo_deudor,
                     rch_valor_dscto,
                     rch_estado);

         -- se incrementa el numero de registros aceptados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;

         CONTINUE FOREACH;
      END IF

      -- se obtiene la marca y tipo originacion para el tipo de credito en proceso
      FOREACH
       SELECT FIRST 1 marca_prc
         INTO v_ax_marca_prc
         FROM cat_tipo_credito
        WHERE tpo_credito = v_ax_tpo_credito
        ORDER BY f_actualiza DESC
      END FOREACH;

      -- se valida el estado procesar. Si es menor a 60 se intenta marcar
      IF v_ax_edo_procesar < 60 THEN
         -- se invoca la función que verifica si ya existe la marca de procesar
         EXECUTE FUNCTION fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                                  v_ax_marca_prc)
                                             INTO r_ax_existe_marca_prc;

         -- en caso de no existir la marca se ejecuta
         IF r_ax_existe_marca_prc = 0 THEN
            -- se ejecuta la función de marcaje
            EXECUTE FUNCTION fn_marca_cuenta(v_ax_id_derechohabiente,
                                             v_ax_marca_prc,
                                             v_ax_id_cre_acreditado, -- referencia
                                             p_d_folio,
                                             0, -- estado marca
                                             0, -- codigo rechazo
                                             NULL, -- marca causa
                                             "", -- fecha causa
                                             p_v_usuario,
                                             v_ax_proceso_cod)
                                        INTO r_ax_edo_retorno;
         END IF
      END IF

      -- se incrementa el numero de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = tmp_tpo_transferencia;
      LET his_edo_procesar       = 90; -- Saldo Rechazado
      LET his_diagnostico        = tmp_diag_proceso[1,3];
      LET his_estado             = v_ax_estado;
      LET his_nss_afore          = tmp_nss_afore;
      LET his_rfc_afore          = tmp_rfc_afore;
      LET his_paterno_afore      = tmp_ape_pat_afore;
      LET his_materno_afore      = tmp_ape_mat_afore;
      LET his_nombre_afore       = tmp_nombre_afore;
      LET his_nom_imss           = tmp_nombre_imss;
      LET his_f_proceso          = TODAY;

      -- se inserta registro
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

      -- si el registro fue rechazado continua con el siguiente registro
      IF v_ax_estado = 240 THEN
         CONTINUE FOREACH;
      END IF

      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla maestro a estado procesar 90-Saldo Rechazado
      EXECUTE PROCEDURE sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);

      -- se inserta registro duplicado con estado_procesar 70 y sin diagnostico
      LET his_edo_procesar = 70;
      LET his_diagnostico = NULL;

      -- se inserta registro
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

      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla maestro a estado procesar 70-Por reenviar
      EXECUTE PROCEDURE sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);

      -- se asignan los valores en las variables que se usaran para insertar el registro en deudor
      LET tmp_deudor_id_cre_acreditado  = his_id_cre_acreditado;
      LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
      LET tmp_deudor_nss                = tmp_nss_infonavit;

      -- se inserta registro
      INSERT INTO safre_tmp:tmp_deudor_rechazo (
                  id_cre_acreditado,
                  id_derechohabiente,
                  nss)
          VALUES (tmp_deudor_id_cre_acreditado,
                  tmp_deudor_id_derechohabiente,
                  tmp_deudor_nss);

      -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro
      IF tmp_diag_proceso[4,6] IS NOT NULL AND tmp_diag_proceso[4,6] <> "   " AND
         tmp_diag_proceso[4,6] <> "000" THEN
         -- se obtiene el numero de diagnosticos en el registro
         LET his_diagnostico = tmp_diag_proceso[4,6];
         LET his_edo_procesar = 90; -- Saldo Rechazado

         -- se inserta registro
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
      ELSE
         -- si el diagnostico fue nulo continua con el siguiente registro
         CONTINUE FOREACH;
      END IF;

      -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro
      IF tmp_diag_proceso[7,9] IS NOT NULL AND tmp_diag_proceso[7,9] <> "   " AND
         tmp_diag_proceso[7,9] <> "000" THEN
         -- se obtiene el numero de diagnosticos en el registro
         LET his_diagnostico = tmp_diag_proceso[7,9];
         LET his_edo_procesar = 90; -- Saldo Rechazado

         -- se inserta registro
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
      ELSE
         -- si el diagnostico fue nulo continua con el siguiente registro
         CONTINUE FOREACH;
      END IF;

      -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro
      IF tmp_diag_proceso[10,12] IS NOT NULL AND tmp_diag_proceso[10,12] <> "   " AND
         tmp_diag_proceso[10,12] <> "000" THEN
         -- se obtiene el numero de diagnosticos en el registro
         LET his_diagnostico = tmp_diag_proceso[10,12];
         LET his_edo_procesar = 90; -- Saldo Rechazado

         -- se inserta registro
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
      ELSE
         -- si el diagnostico fue nulo continua con el siguiente registro
         CONTINUE FOREACH;
      END IF;

      -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro
      IF tmp_diag_proceso[13,15] IS NOT NULL AND tmp_diag_proceso[13,15] <> "   " AND
         tmp_diag_proceso[13,15] <> "000" THEN
         -- se obtiene el numero de diagnosticos en el registro
         LET his_diagnostico = tmp_diag_proceso[13,15];
         LET his_edo_procesar = 90; -- Saldo Rechazado

         -- se inserta registro
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
      ELSE
         -- si el diagnostico fue nulo continua con el siguiente registro
         CONTINUE FOREACH;
      END IF;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = 1;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION
;


