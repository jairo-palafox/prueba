






CREATE FUNCTION "safreviv".fn_uso_integra_rech_saldo(p_v_usuario          CHAR(20),
                                          p_v_arch_proceso     CHAR(100),
                                          p_d_folio            DECIMAL(10),
                                          p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   -- REGISTRO de la temporal
   DEFINE tmp_tpo_registro        CHAR(2);
   DEFINE tmp_cont_servicio       DECIMAL(10,0);
   DEFINE tmp_tpo_ent_recep       CHAR(2);
   DEFINE tmp_cve_ent_recep       CHAR(3);
   DEFINE tmp_tpo_ent_cede        CHAR(2);
   DEFINE tmp_cve_ent_cede        CHAR(3);
   DEFINE tmp_tpo_transferencia   CHAR(2);
   DEFINE tmp_f_presentacion      DATE;
   DEFINE tmp_f_movimiento        DATE;
   DEFINE tmp_curp                CHAR(18);
   DEFINE tmp_nss                 CHAR(11);
   DEFINE tmp_filler1             CHAR(15);
   DEFINE tmp_rfc                 CHAR(13);
   DEFINE tmp_ap_paterno          CHAR(40);
   DEFINE tmp_ap_materno          CHAR(40);
   DEFINE tmp_nombre              CHAR(40);
   DEFINE tmp_filler2             CHAR(22);
   DEFINE tmp_id_lote             CHAR(16);
   DEFINE tmp_filler3             CHAR(15);
   DEFINE tmp_nss_afore           CHAR(11);
   DEFINE tmp_rfc_afore           CHAR(13);
   DEFINE tmp_filler4             CHAR(30);
   DEFINE tmp_ap_paterno_afore    CHAR(40);
   DEFINE tmp_ap_materno_afore    CHAR(40);
   DEFINE tmp_nombre_afore        CHAR(40);
   DEFINE tmp_filler5             CHAR(30);
   DEFINE tmp_aporta_viv97        DECIMAL(15,0); -- 13,2
   DEFINE tmp_saldo_viv97         DECIMAL(15,0); -- 13,2
   DEFINE tmp_filler6             CHAR(78);
   DEFINE tmp_cod_operacion       DECIMAL(2,0);
   DEFINE tmp_diagnostico         CHAR(3);
   DEFINE tmp_nombre_imss         CHAR(50);
   DEFINE tmp_num_credito         DECIMAL(10,0);
   DEFINE tmp_filler7             CHAR(53);
   DEFINE tmp_per_pago            DECIMAL(6,0);
   DEFINE tmp_filler9             CHAR(12);

   -- REGISTRO cre uso garantia
   DEFINE uso_id_cre_uso_garantia DECIMAL(9,0);
   DEFINE uso_id_cre_ctr_archivo  DECIMAL(9,0);
   DEFINE uso_folio_liquida       DECIMAL(9,0);
   DEFINE uso_id_derechohabiente  DECIMAL(9,0);
   DEFINE uso_tpo_transferencia   CHAR(2);
   DEFINE uso_tpo_uso             SMALLINT;
   DEFINE uso_num_credito         DECIMAL(10,0);
   DEFINE uso_f_presentacion      DATE;
   DEFINE uso_f_movimiento        DATE;
   DEFINE uso_periodo_pago        CHAR(6);
   DEFINE uso_importe_v97         DECIMAL(22,2);
   DEFINE uso_nss_afore           CHAR(11);
   DEFINE uso_rfc_afore           CHAR(13);
   DEFINE uso_paterno_afore       CHAR(40);
   DEFINE uso_materno_afore       CHAR(40);
   DEFINE uso_nombre_afore        CHAR(40);
   DEFINE uso_nom_imss            CHAR(50);
   DEFINE uso_edo_procesar        SMALLINT;
   DEFINE uso_diagnostico         CHAR(3);
   DEFINE uso_estado              SMALLINT;
   DEFINE uso_f_proceso           DATE;

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
   DEFINE r_ax_bandera            SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_estado              SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_aporta_viv97       DECIMAL(13,2); -- aportación vivienda 97
   DEFINE v_ax_saldo_viv97        DECIMAL(13,2); -- saldo vivienda 97
   DEFINE v_ax_cre_uso_garant      DECIMAL(9,0); -- identificador de registro en cre uso garantia
   --DEFINE v_ax_folio_liquida      DECIMAL(9,0); -- folio de liquidación
   DEFINE v_error                 SMALLINT; -- codigo de error en caso de excepción
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);
   DEFINE v_c_nss                 CHAR(11);

   DEFINE v_nss_unificador          CHAR(11);
   DEFINE v_id_dh_unificador        DECIMAL(9,0);
   DEFINE v_id_ocg_ug_unificador    DECIMAL(9,0);
   DEFINE v_nss_unificado           CHAR(11);
   DEFINE v_id_dh_unificado         DECIMAL(9,0);
   DEFINE v_edo_procesar            SMALLINT;
   DEFINE v_folio_uni               DECIMAL(10,0);
   DEFINE v_f_proceso_uni           DATE;
   DEFINE v_id_ocg_ug_unificado     DECIMAL(9,0);
   DEFINE v_result_uni              SMALLINT;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/usoIntegRechSdos.trace';
   --TRACE ON;

   -- se inicializan variables
   LET v_ax_id_lote_acpt      = 0;
   LET v_ax_id_lote_rech      = 0;
   LET v_ax_operacion         = 01;
   LET v_ax_estado            = 0;
   LET v_i_estado             = 2; -- estado Integrado
   LET v_error                = 0;
   LET v_isam_err             = 0;
   LET v_c_msj                = 'El proceso finalizó correctamente';
   LET v_c_nss                = "0"; -- valor del NSS antes de entrar al ciclo
   LET v_id_ocg_ug_unificado  = "";

   -- se obtienen los datos de la tabla temporal del proceso de rechazo de saldos
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servicio,
           tmp_tpo_ent_recep,
           tmp_cve_ent_recep,
           tmp_tpo_ent_cede,
           tmp_cve_ent_cede,
           tmp_tpo_transferencia,
           tmp_f_presentacion,
           tmp_f_movimiento,
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
           tmp_aporta_viv97,
           tmp_saldo_viv97,
           tmp_filler6,
           tmp_cod_operacion,
           tmp_diagnostico,
           tmp_nombre_imss,
           tmp_num_credito,
           tmp_filler7,
           tmp_per_pago,
           tmp_filler9
      FROM safre_tmp:tmp_rech_sdo_det_uso

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss;

      -- se obtiene los id del derechohabiente para el nss
      FOREACH
         SELECT id_cre_uso_garantia, id_derechohabiente
           INTO v_ax_cre_uso_garant, v_ax_id_derechohabiente
           FROM safre_tmp:tmp_uso_solic_sdo
          WHERE nss          = v_c_nss
            AND periodo_pago = tmp_per_pago

         -- se obtiene el identificador de cre acreditado con para el id_derechohabiente
         SELECT estado, edo_procesar --, folio_liquida
           INTO v_ax_estado, v_ax_edo_procesar  --, v_ax_folio_liquida
           FROM cre_uso_garantia
          WHERE id_cre_uso_garantia = v_ax_cre_uso_garant;

         -- se valida el estado procesar
         IF v_ax_edo_procesar = 7 THEN
            -- se rechaza el registro
            LET v_ax_estado = 240;

            -- se incrementa el numero de registros aceptados y el id transferencia
            LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
         ELSE
            -- se incrementa el numero de registros aceptados y el id transferencia
            LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
         END IF

         -- se calculan los valores
         LET v_ax_aporta_viv97 = tmp_aporta_viv97 / 100;
         LET v_ax_saldo_viv97  = tmp_saldo_viv97 / 100;

         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
         LET uso_id_cre_ctr_archivo  = p_ax_id_cre_ctr_arch;
         LET uso_folio_liquida       = p_d_folio;
         LET uso_id_derechohabiente  = v_ax_id_derechohabiente;
         LET uso_tpo_transferencia   = tmp_tpo_transferencia;
         LET uso_tpo_uso             = 2;
         LET uso_num_credito         = tmp_num_credito;
         LET uso_f_presentacion      = tmp_f_presentacion;
         LET uso_f_movimiento        = tmp_f_movimiento;
         LET uso_periodo_pago        = tmp_per_pago;
         LET uso_importe_v97         = v_ax_saldo_viv97;
         LET uso_nss_afore           = tmp_nss_afore;
         LET uso_rfc_afore           = tmp_rfc_afore;
         LET uso_paterno_afore       = tmp_ap_paterno_afore;
         LET uso_materno_afore       = tmp_ap_materno_afore;
         LET uso_nombre_afore        = tmp_nombre_afore;
         LET uso_nom_imss            = tmp_nombre_imss;
         LET uso_edo_procesar        = 90; -- Saldo Rechazado
         LET uso_diagnostico         = tmp_diagnostico;
         LET uso_estado              = v_ax_estado;
         LET uso_f_proceso           = TODAY;

         -- se inserta registro
         INSERT INTO cre_uso_garantia (
                     id_cre_uso_garantia,
                     id_cre_ctr_archivo,
                     folio_liquida,
                     id_derechohabiente,
                     tpo_transferencia,
                     tpo_uso,
                     num_credito,
                     f_presentacion,
                     f_movimiento,
                     periodo_pago,
                     importe_v97,
                     nss_afore,
                     rfc_afore,
                     paterno_afore,
                     materno_afore,
                     nombre_afore,
                     nom_imss,
                     edo_procesar,
                     diagnostico,
                     estado,
                     f_proceso)
             VALUES (uso_id_cre_uso_garantia,
                     uso_id_cre_ctr_archivo,
                     uso_folio_liquida,
                     uso_id_derechohabiente,
                     uso_tpo_transferencia,
                     uso_tpo_uso,
                     uso_num_credito,
                     uso_f_presentacion,
                     uso_f_movimiento,
                     uso_periodo_pago,
                     uso_importe_v97,
                     uso_nss_afore,
                     uso_rfc_afore,
                     uso_paterno_afore,
                     uso_materno_afore,
                     uso_nombre_afore,
                     uso_nom_imss,
                     uso_edo_procesar,
                     uso_diagnostico,
                     uso_estado,
                     uso_f_proceso);

         -- si el registro fue rechazado continua con el siguiente registro
         IF v_ax_estado = 240 THEN
            CONTINUE FOREACH;
         END IF

         -- se actualiza el registro maestro a estado 70 - Por reenviar
         UPDATE cre_uso_garantia
            SET edo_procesar = 70
          WHERE id_cre_uso_garantia = v_ax_cre_uso_garant;

         -- se asignan los valores en las variables que se usaran para insertar el registro en deudor
         LET tmp_deudor_id_cre_acreditado = uso_id_cre_uso_garantia;
         LET tmp_deudor_id_derechohabiente = uso_id_derechohabiente;
         LET tmp_deudor_nss = tmp_nss;

         -- se inserta registro
         INSERT INTO safre_tmp:tmp_deudor_rechazo_grt (
                     id_cre_acreditado,
                     id_derechohabiente,
                     nss)
             VALUES (tmp_deudor_id_cre_acreditado,
                     tmp_deudor_id_derechohabiente,
                     tmp_deudor_nss);
      END FOREACH;

      FOREACH
         SELECT nss_unificador,
                id_dh_unificador,
                id_ocg_ug_unificador,
                nss_unificado,
                id_dh_unificado,
                edo_procesar,
                folio_uni,
                f_proceso_uni
           INTO v_nss_unificador,
                v_id_dh_unificador,
                v_id_ocg_ug_unificador,
                v_nss_unificado,
                v_id_dh_unificado,
                v_edo_procesar,
                v_folio_uni,
                v_f_proceso_uni
           FROM cre_uso_gtia_uni
          WHERE edo_procesar IN(80,85)

         EXECUTE FUNCTION fn_uso_actualiza_uni(v_nss_unificador,
                                               v_id_dh_unificador,
                                               v_id_ocg_ug_unificador,
                                               v_nss_unificado,
                                               v_id_dh_unificado,
                                               v_id_ocg_ug_unificado,
                                               v_folio_uni)
                                        INTO v_result_uni;
      END FOREACH;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION;


