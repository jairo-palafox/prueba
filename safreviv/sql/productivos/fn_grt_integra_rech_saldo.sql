






CREATE FUNCTION "safreviv".fn_grt_integra_rech_saldo(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_folio DECIMAL(10),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- REGISTRO tmp rech sdo det grt
   DEFINE tmp_tpo_registro         CHAR(2);
   DEFINE tmp_cont_servicio        DECIMAL(10,0);
   DEFINE tmp_tpo_ent_recep        CHAR(2);
   DEFINE tmp_cve_ent_recep        CHAR(3);
   DEFINE tmp_tpo_ent_cede         CHAR(2);
   DEFINE tmp_cve_ent_cede         CHAR(3);
   DEFINE tmp_tpo_transferencia    CHAR(2);
   DEFINE tmp_f_presentacion       DATE;
   DEFINE tmp_filler               CHAR(8);
   DEFINE tmp_curp                 CHAR(18);
   DEFINE tmp_nss                  CHAR(11);
   DEFINE tmp_filler1              CHAR(15);
   DEFINE tmp_rfc                  CHAR(13);
   DEFINE tmp_ap_paterno           CHAR(40);
   DEFINE tmp_ap_materno           CHAR(40);
   DEFINE tmp_nombre               CHAR(40);
   DEFINE tmp_filler2              CHAR(22);
   DEFINE tmp_id_lote              CHAR(16);
   DEFINE tmp_filler3              CHAR(15);
   DEFINE tmp_nss_afore            CHAR(11);
   DEFINE tmp_rfc_afore            CHAR(13);
   DEFINE tmp_filler4              CHAR(30);
   DEFINE tmp_ap_paterno_afore     CHAR(40);
   DEFINE tmp_ap_materno_afore     CHAR(40);
   DEFINE tmp_nombre_afore         CHAR(40);
   DEFINE tmp_filler5              CHAR(45);
   DEFINE tmp_saldo_viv97          DECIMAL(15,0);
   DEFINE tmp_filler6              CHAR(60);
   DEFINE tmp_saldo_viv92          DECIMAL(15,0);
   DEFINE tmp_filler7              CHAR(3);
   DEFINE tmp_cod_operacion        DECIMAL(2,0);
   DEFINE tmp_diagnostico          CHAR(3);
   DEFINE tmp_nombre_imss          CHAR(50);
   DEFINE tmp_num_credito          DECIMAL(10,0);
   DEFINE tmp_int_viv97            DECIMAL(15,0);
   DEFINE tmp_int_viv92            DECIMAL(15,2);
   DEFINE tmp_filler8              CHAR(23);
   DEFINE tmp_per_pago             DECIMAL(6,0);
   DEFINE tmp_filler9              CHAR(12);
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
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_cre_acreditado  DECIMAL(9,0); -- identificador de cre acreditado
   DEFINE v_ax_estado             SMALLINT; -- estado
   DEFINE v_ax_edo_procesar       SMALLINT; -- estado procesar
   DEFINE v_ax_id_lote_acpt       INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech       INTEGER; -- total de registros rechazados
   DEFINE v_ax_operacion          SMALLINT; -- operacion del proceso
   DEFINE r_ax_bandera            SMALLINT; -- valor de regreso de la actualización
   DEFINE v_ax_tpo_originacion    SMALLINT; -- tipo de originación
   DEFINE v_i_estado              SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_c_nss                 CHAR(11);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtIntegRchSdo.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt    = 0;
   LET v_ax_id_lote_rech    = 0;
   LET v_ax_operacion       = 01;
   LET v_ax_estado          = 0;
   LET v_i_estado           = 2; -- estado Integrado
   LET v_ax_tpo_originacion = 2; -- 2-Solicitud de Saldo en Garantía (Créditos en G. 43 bis)
   LET v_error              = 0;
   LET v_isam_err           = 0;
   LET v_c_msj              = 'El proceso finalizó correctamente';
   LET v_c_nss              = "0"; -- valor del NSS antes de entrar al ciclo

   FOREACH
      -- se obtienen los datos de la tabla temporal del proceso de rechazo de saldos
      SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servicio,
           tmp_tpo_ent_recep,
           tmp_cve_ent_recep,
           tmp_tpo_ent_cede,
           tmp_cve_ent_cede,
           tmp_tpo_transferencia,
           tmp_f_presentacion,
           tmp_filler,
           tmp_curp,
           tmp_nss,
           tmp_filler1,
           tmp_rfc,
           tmp_ap_paterno,
           tmp_ap_materno,
           tmp_nombre,
           tmp_filler2,
           tmp_id_lote,
           tmp_filler3,
           tmp_nss_afore,
           tmp_rfc_afore,
           tmp_filler4,
           tmp_ap_paterno_afore,
           tmp_ap_materno_afore,
           tmp_nombre_afore,
           tmp_filler5,
           tmp_saldo_viv97,
           tmp_filler6,
           tmp_saldo_viv92,
           tmp_filler7,
           tmp_cod_operacion,
           tmp_diagnostico,
           tmp_nombre_imss,
           tmp_num_credito,
           tmp_int_viv97,
           tmp_int_viv92,
           tmp_filler8,
           tmp_per_pago,
           tmp_filler9
      FROM safre_tmp:tmp_rech_sdo_det_grt

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
      INTO v_ax_id_derechohabiente
      FROM safre_viv:afi_derechohabiente
      WHERE nss = tmp_nss;

      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente
      FOREACH
         SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar
           INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar
           FROM safre_viv:cre_acreditado
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND estado >= 20 --IN (20,900,910,920)
            AND edo_procesar >= 55 --IN (5, 55, 60, 70, 120, 200, 210)
            AND tpo_originacion = v_ax_tpo_originacion
          ORDER BY f_otorga DESC
      END FOREACH;

      -- se valida el estado procesar
      IF v_ax_edo_procesar = 5 THEN
         -- se rechaza el registro
         LET v_ax_estado = 240;

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
      ELSE
         -- se incrementa el numero de registros aceptados
         LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
      END IF

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = tmp_tpo_transferencia;
      LET his_edo_procesar       = 90; -- Saldo Rechazado
      LET his_diagnostico        = tmp_diagnostico;
      LET his_estado             = v_ax_estado;
      LET his_nss_afore          = tmp_nss_afore;
      LET his_rfc_afore          = tmp_rfc_afore;
      LET his_paterno_afore      = tmp_ap_paterno_afore;
      LET his_materno_afore      = tmp_ap_materno_afore;
      LET his_nombre_afore       = tmp_nombre_afore;
      LET his_nom_imss           = tmp_nombre_imss;
      LET his_f_proceso          = TODAY;

      -- se inserta registro
      INSERT INTO safre_viv:cre_his_acreditado (
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
      EXECUTE PROCEDURE safre_viv:sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);

      -- se inserta registro duplicado con estado_procesar 70 y sin diagnostico
      LET his_edo_procesar = 70;
      LET his_diagnostico = NULL;

      -- se inserta registro
      INSERT INTO safre_viv:cre_his_acreditado (
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
      EXECUTE PROCEDURE safre_viv:sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE safre_viv:cre_his_acreditado;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);
   
   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION
;


