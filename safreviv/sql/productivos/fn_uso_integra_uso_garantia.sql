






CREATE FUNCTION "safreviv".fn_uso_integra_uso_garantia(p_d_id_cre_ctr_arch DECIMAL(9,0),
                                            p_d_folio           DECIMAL(9,0),
                                            p_v_usuario         CHAR(20))
   RETURNING SMALLINT;

   -- REGISTRO de la temporal
   DEFINE tmp_tpo_registro         CHAR(2);
   DEFINE tmp_cont_servivio        DECIMAL(10,0);
   DEFINE tmp_tpo_ent_recep        CHAR(2);
   DEFINE tmp_cve_ent_recep        CHAR(3);
   DEFINE tmp_tpo_ent_cedente      CHAR(2);
   DEFINE tmp_cve_ent_cedente      CHAR(3);
   DEFINE tmp_origen_transf        CHAR(2);
   DEFINE tmp_fec_presentacion     DATE;
   DEFINE tmp_fec_movimiento       DATE;
   DEFINE tmp_curp_trab            CHAR(18);
   DEFINE tmp_nss_trab             CHAR(11);
   DEFINE tmp_filler_1             CHAR(15);
   DEFINE tmp_rfc_trab             CHAR(13);
   DEFINE tmp_ap_pat_trab          CHAR(40);
   DEFINE tmp_ap_mat_trab          CHAR(40);
   DEFINE tmp_nombre_trab          CHAR(40);
   DEFINE tmp_filler_2             CHAR(22);
   DEFINE tmp_id_lote              CHAR(16);
   DEFINE tmp_filler_3             CHAR(15);
   DEFINE tmp_nss_afore            CHAR(11);
   DEFINE tmp_rfc_trab_afore       CHAR(13);
   DEFINE tmp_filler_5             CHAR(30);
   DEFINE tmp_ap_pat_trab_afore    CHAR(40);
   DEFINE tmp_ap_mat_trab_afore    CHAR(40);
   DEFINE tmp_nombre_trab_afore    CHAR(40);
   DEFINE tmp_filler_6             CHAR(30);
   DEFINE tmp_num_apli_int_viv     DECIMAL(15,0);
   DEFINE tmp_saldo_viv            DECIMAL(15,0);
   DEFINE tmp_filler_7             CHAR(78);
   DEFINE tmp_cod_resul_opera      DECIMAL(2,0);
   DEFINE tmp_diagnostico_proceso  DECIMAL(3,0);
   DEFINE tmp_nombre_tab_imss      CHAR(50);
   DEFINE tmp_num_cred_infonavit   DECIMAL(10,0);
   DEFINE tmp_filler_8             CHAR(53);
   DEFINE tmp_periodo_pago         DECIMAL(6,0);
   DEFINE tmp_filler_9             CHAR(12);

   -- REGISTRO uso garantia
   DEFINE uso_id_cre_uso_garantia  DECIMAL(9,0);
   DEFINE uso_id_cre_ctr_archivo   DECIMAL(9,0);
   DEFINE uso_folio_liquida        DECIMAL(9,0);
   DEFINE uso_id_derechohabiente   DECIMAL(9,0);
   DEFINE uso_tpo_transferencia    CHAR(2);
   DEFINE uso_tpo_uso              SMALLINT;
   DEFINE uso_num_credito          DECIMAL(10,0);
   DEFINE uso_f_presentacion       DATE;
   DEFINE uso_f_movimiento         DATE;
   DEFINE uso_periodo_pago         CHAR(6);
   DEFINE uso_importe_v97          DECIMAL(22,2);
   DEFINE uso_nss_afore            CHAR(11);
   DEFINE uso_rfc_afore            CHAR(13);
   DEFINE uso_paterno_afore        CHAR(40);
   DEFINE uso_materno_afore        CHAR(40);
   DEFINE uso_nombre_afore         CHAR(40);
   DEFINE uso_nom_imss             CHAR(50);
   DEFINE uso_edo_procesar         SMALLINT;
   DEFINE uso_diagnostico          CHAR(3);
   DEFINE uso_estado               SMALLINT;
   DEFINE uso_f_proceso            DATE;

   -- Campos auxiliares
   DEFINE v_ax_nom_archivo         CHAR(40); -- nombre del archivo
   DEFINE v_ax_tpo_originacion     SMALLINT; -- tipo de originacion
   DEFINE v_ax_sdo_viv_97          DECIMAL(13,2); -- saldo vivienda 97
   DEFINE v_ax_num_apli_int_viv    DECIMAL(12,6); -- Numero de "Aplicaciones de Intereses de Vivienda" 97 solicitado
   DEFINE v_ax_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador     CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt         INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech         INTEGER; -- contador de registros rechazados
   DEFINE v_i_estado               SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_sts_registro        SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar        SMALLINT; -- estado procesar
   DEFINE r_ax_bandera             SMALLINT; -- valor de regreso de la actualización
   DEFINE v_error                  SMALLINT; -- código de error en caso de excepción
   DEFINE v_tot_solic_ug           SMALLINT; -- total de solicitudes de uso de garantía sin 09 de Procesar
   DEFINE v_id_derechohabiente_ant DECIMAL(9,0); -- Id derechohabiente anterior para validar periodo pago
   DEFINE v_periodo_pag_ant        DECIMAL(6,0);  -- Periodo pago anterior

   -- Campos ocg
   DEFINE v_subproceso             SMALLINT;
   DEFINE v_id_referencia_ocg      DECIMAL(9,0);
   DEFINE v_id_referencia_cre      DECIMAL(9,0);
   DEFINE v_periodo_pago           CHAR(6);
   DEFINE v_f_proceso              DATE;


   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/db/safreviv_int/BD/usoIntegUsoGarant.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt         = 0;
   LET v_ax_cuenta_rech         = 0;
   LET v_error                  = 0;
   LET v_i_estado               = 2; -- estado Integrado en la tabla glo ctr archivo
   LET v_ax_tpo_originacion     = 2; --  2-Créditos en Garantía 43 bis 
   LET v_tot_solic_ug           = 0;
   LET v_periodo_pag_ant        = 0;
   LET v_id_derechohabiente_ant = 0;
   LET v_subproceso             = 3;
   LET v_f_proceso              = TODAY;

   SELECT nom_archivo
     INTO v_ax_nom_archivo
     FROM cre_ctr_archivo
    WHERE id_cre_ctr_archivo = p_d_id_cre_ctr_arch;

   --------------------------------------------------
   -- SE PROCESAN LOS REGISTROS DE USO DE GARANTÍA --
   --------------------------------------------------
   -- se obtienen los datos de la tabla temporal
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servivio,
           tmp_tpo_ent_recep,
           tmp_cve_ent_recep,
           tmp_tpo_ent_cedente,
           tmp_cve_ent_cedente,
           tmp_origen_transf,
           tmp_fec_presentacion,
           tmp_fec_movimiento,
           tmp_curp_trab,
           tmp_nss_trab,
           tmp_filler_1,
           tmp_rfc_trab,
           tmp_ap_pat_trab,
           tmp_ap_mat_trab,
           tmp_nombre_trab,
           tmp_filler_2,
           tmp_id_lote,
           tmp_filler_3,
           tmp_nss_afore,
           tmp_rfc_trab_afore,
           tmp_filler_5,
           tmp_ap_pat_trab_afore,
           tmp_ap_mat_trab_afore,
           tmp_nombre_trab_afore,
           tmp_filler_6,
           tmp_num_apli_int_viv,
           tmp_saldo_viv,
           tmp_filler_7,
           tmp_cod_resul_opera,
           tmp_diagnostico_proceso,
           tmp_nombre_tab_imss,
           tmp_num_cred_infonavit,
           tmp_filler_8,
           tmp_periodo_pago,
           tmp_filler_9
      FROM safre_tmp:tmp_uso_garantia_det_uso
    ORDER BY nss_trab, periodo_pago

      -- se inicializan variables
      LET v_ax_sts_registro = 10;
      LET v_ax_id_derechohabiente = NULL;

      LET v_id_referencia_ocg = tmp_cont_servivio;
      LET v_periodo_pago      = tmp_periodo_pago;

      -- se obtiene el id del derechohabiente y el tipo trabajador para el nss
      SELECT id_derechohabiente, tipo_trabajador
        INTO v_ax_id_derechohabiente, v_ax_tipo_trabajador
        FROM afi_derechohabiente
       WHERE nss = tmp_nss_trab;

      -- se valida el id_derechohabiente
      IF v_ax_id_derechohabiente IS NULL THEN
         LET v_ax_sts_registro = 240;
         LET tmp_diagnostico_proceso = 11;
      END IF

      -- se valida el tipo de trabajador
      IF v_ax_tipo_trabajador  = "I" THEN
         LET v_ax_edo_procesar = 10;
      ELSE
         LET v_ax_edo_procesar = 7;
      END IF

      -- se verifica que el registro no ha sido rechazado
      IF v_ax_sts_registro <> 240 THEN
         -- se comprueba que derechohabiente se encuentre en la tabla maestro
         IF NOT EXISTS (
                        SELECT id_derechohabiente
                          FROM cre_acreditado
                         WHERE id_derechohabiente = v_ax_id_derechohabiente
                           AND tpo_originacion    = v_ax_tpo_originacion
                       ) THEN

               LET v_ax_sts_registro       = 240;
               LET tmp_diagnostico_proceso = 13;
         END IF

         -- se comprueba que no haya otra solicitud para el mismo periodo de pago
         {Se verificará si sigue esta validación
         IF EXISTS (
                    SELECT id_derechohabiente
                      FROM cre_uso_garantia
                     WHERE id_derechohabiente = v_ax_id_derechohabiente
                       AND tpo_transferencia  IN("18","48")
                       AND periodo_pago       = tmp_periodo_pago
                       AND estado             IN(10,20,130,140)
                    ) THEN
               LET v_ax_sts_registro       = 240;
               LET tmp_diagnostico_proceso = 17;
         END IF
         }

         IF v_id_derechohabiente_ant > 0 THEN
            IF v_id_derechohabiente_ant = v_ax_id_derechohabiente AND
               v_periodo_pag_ant = tmp_periodo_pago THEN
               LET v_ax_sts_registro       = 240;
               LET tmp_diagnostico_proceso = 17;
            END IF
         END IF
      END IF

      -- se verifica que el registro no ha sido rechazado
      IF v_ax_sts_registro = 240 THEN
         -- se incrementa el numero de registros rechazados
         LET v_ax_cuenta_rech = v_ax_cuenta_rech + 1;
      ELSE
         -- se incrementa el numero de registros aceptados
         LET v_ax_cuenta_acpt = v_ax_cuenta_acpt + 1;
      END IF

      -- se calculan los valores
      LET v_ax_sdo_viv_97 = tmp_saldo_viv / 100;
      LET v_ax_num_apli_int_viv = tmp_num_apli_int_viv / 1000000;

      -- se valida el número de crédito. Este no puede ser nulo
      IF tmp_num_cred_infonavit IS NULL OR
         tmp_num_cred_infonavit = "" THEN
         -- se asigna cero al número de credíto
         LET tmp_num_cred_infonavit = 0;
      END IF

      -- se asignan los valores en las variables que se usaran para insertar el registro
      LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
      LET uso_id_cre_ctr_archivo  = p_d_id_cre_ctr_arch;
      LET uso_folio_liquida       = 0;
      LET uso_id_derechohabiente  = v_ax_id_derechohabiente;
      LET uso_tpo_transferencia   = tmp_origen_transf;
      LET uso_tpo_uso             = tmp_tpo_ent_cedente;
      LET uso_num_credito         = tmp_num_cred_infonavit;
      LET uso_f_presentacion      = tmp_fec_presentacion;
      LET uso_f_movimiento        = tmp_fec_movimiento;
      LET uso_periodo_pago        = tmp_periodo_pago;
      LET uso_importe_v97         = v_ax_sdo_viv_97;
      LET uso_nss_afore           = tmp_nss_afore;
      LET uso_rfc_afore           = tmp_rfc_trab_afore;
      LET uso_paterno_afore       = tmp_ap_pat_trab_afore;
      LET uso_materno_afore       = tmp_ap_mat_trab_afore;
      LET uso_nombre_afore        = tmp_nombre_trab_afore;
      LET uso_nom_imss            = tmp_nombre_tab_imss;
      LET uso_edo_procesar        = v_ax_edo_procesar;
      LET uso_diagnostico         = tmp_diagnostico_proceso;
      LET uso_estado              = v_ax_sts_registro;
      LET uso_f_proceso           = TODAY;

      -- se inserta registro en la tabla maestro
      INSERT INTO cre_uso_garantia(
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

      LET v_id_derechohabiente_ant = v_ax_id_derechohabiente;
      LET v_periodo_pag_ant        = tmp_periodo_pago;
      LET v_id_referencia_cre      = uso_id_cre_uso_garantia;

      INSERT INTO ocg_transaccion_cre
      VALUES(v_subproceso        ,
             v_id_referencia_ocg ,
             v_id_referencia_cre ,
             v_periodo_pago      ,
             v_f_proceso); 

   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   -- se ejecuta el sp que actualiza el registro de la tabla de control de archivos a estatus "Integrado"
   EXECUTE FUNCTION fn_act_edo_archivo(v_ax_nom_archivo, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_cuenta_acpt, v_ax_cuenta_rech, 0, p_d_id_cre_ctr_arch);

   RETURN v_error;

END FUNCTION

;


