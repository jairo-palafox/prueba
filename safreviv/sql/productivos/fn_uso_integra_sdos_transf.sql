






CREATE FUNCTION "safreviv".fn_uso_integra_sdos_transf(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_folio DECIMAL(10),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   --REGISTRO tmp_sdo_transf_enc_uso
   DEFINE v_re_tpo_registro               CHAR(2);
   DEFINE v_re_id_servicio                CHAR(2);
   DEFINE v_re_id_operacion               CHAR(2);
   DEFINE v_re_tpo_entidad_origen         CHAR(2);
   DEFINE v_re_cve_entidad_origen         CHAR(3);
   DEFINE v_re_tpo_entidad_destino        CHAR(2);
   DEFINE v_re_cve_entidad_destino        CHAR(3);
   DEFINE v_re_ent_federativa             CHAR(3);
   DEFINE v_re_f_presentacion             DATE;
   DEFINE v_re_con_lote_dia               DECIMAL(3,0);
   DEFINE v_re_cod_res_operacion          CHAR(2);
   DEFINE v_re_mot_rechazo_lote           CHAR(9);

   -- REGISTRO tmp_sdo_transf_det_uso
   DEFINE v_rs_tpo_registro               CHAR(2);
   DEFINE v_rs_con_servicio               DECIMAL(10);
   DEFINE v_rs_tpo_entidad_recep          CHAR(2);
   DEFINE v_rs_cve_entidad_recep          CHAR(3);
   DEFINE v_rs_tpo_entidad_cede           CHAR(2);
   DEFINE v_rs_cve_entidad_cede           CHAR(3);
   DEFINE v_rs_tpo_transferencia          CHAR(2);
   DEFINE v_rs_f_presentacion             DATE;
   DEFINE v_rs_f_movimiento               DATE;
   DEFINE v_rs_curp                       CHAR(18);
   DEFINE v_rs_nss_infonavit              CHAR(11);
   DEFINE v_rs_rfc_infonavit              CHAR(13);
   DEFINE v_rs_ape_pat_trabajador         CHAR(40);
   DEFINE v_rs_ape_mat_trabajador         CHAR(40);
   DEFINE v_rs_nom_trabajador             CHAR(40);
   DEFINE v_rs_id_lote                    CHAR(16);
   DEFINE v_rs_nss_afore                  CHAR(11);
   DEFINE v_rs_rfc_afore                  CHAR(13);
   DEFINE v_rs_ape_pat_trabajador_afore   CHAR(40);
   DEFINE v_rs_ape_mat_trabajador_afore   CHAR(40);
   DEFINE v_rs_nom_trabajador_afore       CHAR(40);
   DEFINE v_rs_aivs_97                    DECIMAL(15);
   DEFINE v_rs_saldo_viv97                DECIMAL(15);
   DEFINE v_rs_cod_res_operacion          CHAR(2);
   DEFINE v_rs_diagnostico_proceso        CHAR(15);
   DEFINE v_rs_nom_trabajador_imss        CHAR(50);
   DEFINE v_rs_num_credito_infonavit      DECIMAL(10);
   DEFINE v_rs_periodo_pago               CHAR(6);

   --REGISTRO cre uso garantia
   DEFINE his_id_cre_uso_garantia         DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE his_folio_liquida               DECIMAL(9,0);
   DEFINE his_id_derechohabiente          DECIMAL(9,0);
   DEFINE his_tpo_transferencia           CHAR(2);
   DEFINE his_tpo_uso                     SMALLINT;
   DEFINE his_num_credito                 DECIMAL(10,0);
   DEFINE his_f_presentacion              DATE;
   DEFINE his_f_movimiento                DATE;
   DEFINE his_periodo_pago                CHAR(6);
   DEFINE his_importe_v97                 DECIMAL(22,2);
   DEFINE his_nss_afore                   CHAR(11);
   DEFINE his_rfc_afore                   CHAR(13);
   DEFINE his_paterno_afore               CHAR(40);
   DEFINE his_materno_afore               CHAR(40);
   DEFINE his_nombre_afore                CHAR(40);
   DEFINE his_nom_imss                    CHAR(50);
   DEFINE his_edo_procesar                SMALLINT;
   DEFINE his_diagnostico                 CHAR(3);
   DEFINE his_estado                      SMALLINT;
   DEFINE his_f_proceso                   DATE;

   --REGISTRO saldo deudor
   DEFINE sdo_id_cre_acreditado           DECIMAL(9,0);
   DEFINE sdo_folio_referencia            DECIMAL(9,0);
   DEFINE sdo_f_movimiento                DATE;
   DEFINE sdo_movimiento                  SMALLINT;
   DEFINE sdo_id_referencia               DECIMAL(9,0);
   DEFINE sdo_monto_aivs                  DECIMAL(22,2);
   DEFINE sdo_monto_pesos                 DECIMAL(22,2);
   DEFINE sdo_f_proceso                   DATE;

   -- REGISTRO tmp deudor rechazo
   DEFINE tmp_deudor_id_cre_acreditado    DECIMAL(9,0);
   DEFINE tmp_deudor_id_derechohabiente   DECIMAL(9,0);
   DEFINE tmp_deudor_nss                  CHAR(11);

   -- CAMPOS auxiliares
   DEFINE v_ax_estado                     SMALLINT; -- estado
   DEFINE v_ax_edo_procesar               SMALLINT; -- estado procesar
   DEFINE v_ax_id_derechohabiente         DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_lote_acpt               INTEGER; -- identificador del lote de acr transferencia   
   DEFINE v_ax_id_lote_rech               INTEGER; 
   DEFINE v_ax_proceso_cod                SMALLINT; -- código del proceso
   DEFINE r_ax_bandera                    SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_glo_estado                  SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_cre_uso_garant             DECIMAL(9,0); -- identificador de registro en cre uso garantia
   DEFINE v_error                         SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                      INTEGER;
   DEFINE v_c_msj                         VARCHAR(250);
   DEFINE v_c_nss                         CHAR(11);
   DEFINE v_sts_pend                      SMALLINT;
   DEFINE v_ax_folio_liq                  DECIMAL(9,0);
   DEFINE v_folio_liquida                 DECIMAL(9,0);
   DEFINE v_ax_tipo_trabajador            CHAR(1);
   DEFINE v_ax_f_liquida                  DATE;
   DEFINE v_criterio                      SMALLINT;
   DEFINE v_f_liquida                     DATE;
   DEFINE v_proceso_cod                   SMALLINT;

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado           DECIMAL(9,0);
   DEFINE cre_id_derechohabiente          DECIMAL(9,0);
   DEFINE cre_estado                      SMALLINT; -- estado
   DEFINE cre_edo_procesar                SMALLINT; -- estado procesar
   DEFINE cre_tpo_credito                 SMALLINT; -- tipo de crédito
   DEFINE cre_num_credito                 DECIMAL(10,0); -- número de crédito
   DEFINE cre_f_culmina                   DATE; -- fecha de culminación

   DEFINE v_dm_id_derechohabiente         DECIMAL(9,0);
   DEFINE v_dm_marca_entra                SMALLINT;
   DEFINE v_dm_n_referencia               DECIMAL(9,0);
   DEFINE v_dm_estado_marca               SMALLINT;
   DEFINE v_dm_marca_causa                SMALLINT;
   DEFINE v_dm_usuario                    CHAR(20);
   DEFINE v_dm_proceso_cod                SMALLINT;
   DEFINE v_ax_cod_error                  SMALLINT;

   -- REGISTRO de cta marca ws
   DEFINE ws_id_derechohabiente           DECIMAL(9,0);
   DEFINE ws_id_origen                    DECIMAL(9,0);
   DEFINE ws_modulo_cod                   CHAR(3);
   DEFINE ws_tpo_credito                  SMALLINT;
   DEFINE ws_marca                        SMALLINT;
   DEFINE ws_f_solicita                   DATE;
   DEFINE ws_intento                      SMALLINT;
   DEFINE ws_cod_result_op                SMALLINT;
   DEFINE ws_diagnostico                  SMALLINT;
   DEFINE ws_situacion                    SMALLINT;
   DEFINE ws_num_credito                  DECIMAL(10,0);
   DEFINE ws_f_infonavit                  DATE;
   DEFINE ws_marca_procesar               CHAR(2);
   DEFINE ws_folio_archivo                DECIMAL(9,0);
   DEFINE ws_usuario                      CHAR(20);

   DEFINE v_folio_sin_conciliar           DECIMAL(9,0);  --Folio del registro que aun no se concilia con procesar

   ---Variables para extractores
   DEFINE v_ex_nss                       CHAR(11);
   DEFINE v_ex_id_derechohabiente        DECIMAL(9,0);
   DEFINE v_ex_modulo_cod                CHAR(2);
   DEFINE v_ex_modulo_orig               CHAR(2);
   DEFINE v_ex_aivs92_09                 DECIMAL(12,2);
   DEFINE v_ex_pesos92_09                DECIMAL(12,2);
   DEFINE v_ex_aivs97_09                 DECIMAL(12,2);
   DEFINE v_ex_pesos97_09                DECIMAL(12,2);
   DEFINE v_ex_aivs92                    DECIMAL(12,2);
   DEFINE v_ex_pesos92                   DECIMAL(12,2);
   DEFINE v_ex_aivs97                    DECIMAL(12,2);
   DEFINE v_ex_pesos97                   DECIMAL(12,2);
   DEFINE v_ex_folio_liquida             DECIMAL(10,0);
   DEFINE v_ex_f_liquida                 DATE;
   DEFINE v_ex_porcentaje_var            DECIMAL(15,2);
   DEFINE v_ex_id_cre_ctr_archivo        DECIMAL(9,0);
   DEFINE v_ex_f_archivo                 CHAR(8);
   DEFINE v_ex_f_proceso                 DATE;

   -- REGISTRO tmp confirmado (conciliación general)
   DEFINE tmpconc_nss                    CHAR(11);
   DEFINE tmpconc_id_derechohabiente     DECIMAL(9,0);
   DEFINE tmpconc_f_liquida              DATE;
   DEFINE tmpconc_modulo_cod             CHAR(3);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/usoIntegSdosTrans.trace';
   ---TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_rech      = 0;
   LET v_ax_id_lote_acpt      = 0;   
   LET v_i_glo_estado         = 2; -- estado Integrado
   LET v_error                = 0;
   LET v_isam_err             = 0;
   LET v_c_msj                = 'El proceso finalizó correctamente';
   LET v_c_nss                = "0"; -- valor del NSS antes de entrar al ciclo
   LET v_folio_sin_conciliar  = 0;

   LET v_ex_nss                = "";
   LET v_ex_id_derechohabiente = "";
   LET v_ex_modulo_cod         = "";
   LET v_ex_modulo_orig        = "";
   LET v_ex_aivs92_09          = 0;
   LET v_ex_pesos92_09         = 0;
   LET v_ex_aivs97_09          = 0;
   LET v_ex_pesos97_09         = 0;
   LET v_ex_aivs92             = 0;
   LET v_ex_pesos92            = 0;
   LET v_ex_aivs97             = 0;
   LET v_ex_pesos97            = 0;
   LET v_ex_folio_liquida      = "";
   LET v_ex_f_liquida          = "";
   LET v_ex_porcentaje_var     = 0;
   LET v_ex_id_cre_ctr_archivo = "";
   LET v_ex_f_archivo          = "";
   LET v_ex_f_proceso          = "";
   LET v_criterio              = 0;
   LET v_f_liquida             = "";
   LET v_proceso_cod           = 1217;
   LET v_ax_folio_liq          = 69238;
   LET v_ax_tipo_trabajador    = "";
   LET v_folio_liquida         = 0;
   LET v_ax_proceso_cod       = 1205; -- Saldos Transferidos USOGRT

   LET cre_id_cre_acreditado  = "";
   LET cre_id_derechohabiente = "";
   LET cre_estado             = "";
   LET cre_edo_procesar       = "";
   LET cre_tpo_credito        = "";
   LET cre_num_credito        = "";
   LET cre_f_culmina          = "";

   LET v_dm_usuario          = p_v_usuario;
   LET v_dm_estado_marca     = 0;
   LET v_dm_marca_causa      = 0;
   LET v_dm_proceso_cod      = v_ax_proceso_cod;

   -- se obtiene la fecha de presentacion de la información a integrar
   FOREACH
    SELECT FIRST 1 f_movimiento
      INTO v_rs_f_movimiento
      FROM safre_tmp:tmp_sdo_transf_det_uso
     WHERE f_movimiento IS NOT NULL
   END FOREACH

   -- se asigna la fecha de liquidación, fecha del ultimo día de mes anterior de la fecha movimiento
   LET v_ax_f_liquida = v_rs_f_movimiento;

   -- se obtienen los datos de tmp_sdo_transf_det_uso para el archivo en proceso
   FOREACH
    SELECT tpo_registro,
           cont_servicio,
           tpo_ent_recep,
           cve_ent_recep,
           tpo_ent_cede,
           cve_ent_cede,
           tpo_transferencia,
           f_presentacion,
           f_movimiento,
           curp,
           nss,
           rfc,
           ap_paterno,
           ap_materno,
           nombre,
           id_lote,
           nss_afore,
           rfc_afore,
           ap_paterno_afore,
           ap_materno_afore,
           nombre_afore,
           interes_viv97,
           saldo_viv97,
           cod_operacion,
           diagnostico,
           nombre_imss,
           num_credito,
           per_pago
      INTO v_rs_tpo_registro,
           v_rs_con_servicio,
           v_rs_tpo_entidad_recep,
           v_rs_cve_entidad_recep,
           v_rs_tpo_entidad_cede,
           v_rs_cve_entidad_cede,
           v_rs_tpo_transferencia,
           v_rs_f_presentacion,
           v_rs_f_movimiento,
           v_rs_curp,
           v_rs_nss_infonavit,
           v_rs_rfc_infonavit,
           v_rs_ape_pat_trabajador,
           v_rs_ape_mat_trabajador,
           v_rs_nom_trabajador,
           v_rs_id_lote,
           v_rs_nss_afore,
           v_rs_rfc_afore,
           v_rs_ape_pat_trabajador_afore,
           v_rs_ape_mat_trabajador_afore,
           v_rs_nom_trabajador_afore,
           v_rs_aivs_97,
           v_rs_saldo_viv97,
           v_rs_cod_res_operacion,
           v_rs_diagnostico_proceso,
           v_rs_nom_trabajador_imss,
           v_rs_num_credito_infonavit,
           v_rs_periodo_pago
      FROM safre_tmp:tmp_sdo_transf_det_uso

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = v_rs_nss_afore;

      -- se obtiene el identificador de cre acreditado con para el id_derechohabiente
      FOREACH
         SELECT t.id_cre_uso_garantia, t.id_derechohabiente, a.tipo_trabajador
           INTO v_ax_cre_uso_garant, v_ax_id_derechohabiente, v_ax_tipo_trabajador
           FROM safre_tmp:tmp_uso_solic_sdo t, afi_derechohabiente a
          WHERE t.nss          = v_rs_nss_afore
            AND t.periodo_pago = v_rs_periodo_pago
            AND t.nss          = a.nss

         SELECT FIRST 1 estado, edo_procesar, id_cre_uso_garantia, folio_liquida
           INTO v_ax_estado, v_ax_edo_procesar, v_ax_cre_uso_garant, v_folio_liquida
           FROM cre_uso_garantia
          WHERE id_cre_uso_garantia = v_ax_cre_uso_garant;

         -- se asignan los valores en el registro de la temporal que se usa en la conciliación general
         LET tmpconc_nss                = v_rs_nss_infonavit;
         LET tmpconc_id_derechohabiente = v_ax_id_derechohabiente;
         LET tmpconc_f_liquida          = v_ax_f_liquida;
         LET tmpconc_modulo_cod         = "grt";

         IF v_ax_tipo_trabajador = "I" AND v_folio_liquida < v_ax_folio_liq THEN
            -- se inserta registro
            INSERT INTO safre_tmp:tmp_cre_confirmado (
                        nss,
                        id_derechohabiente,
                        f_liquida,
                        modulo_cod)
                VALUES (tmpconc_nss,
                        tmpconc_id_derechohabiente,
                        tmpconc_f_liquida,
                        tmpconc_modulo_cod);

            LET v_ex_nss                = v_rs_nss_infonavit;
            LET v_ex_id_derechohabiente = v_ax_id_derechohabiente;
            LET v_ex_modulo_cod         = "UG";
            LET v_ex_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
            LET v_ex_f_proceso          = TODAY;
            LET v_ex_f_archivo          = year(v_rs_f_presentacion)||lpad(month(v_rs_f_presentacion),2,0)||lpad(day(v_rs_f_presentacion),2,0);
            LET v_ex_aivs92_09          = 0;
            LET v_ex_pesos92_09         = 0;
            LET v_ex_aivs97_09          = v_rs_aivs_97 / 1000000;
            LET v_ex_pesos97_09         = v_rs_saldo_viv97 / 100;
            LET v_ex_folio_liquida      = v_folio_liquida;
            LET v_ex_aivs97             = 0;
            LET v_ex_pesos97            = 0;
            LET v_ex_aivs92             = 0;
            LET v_ex_pesos92            = 0;

            SELECT proceso_cod
              INTO v_proceso_cod
              FROM glo_folio
             WHERE folio = v_folio_liquida;

            IF v_proceso_cod <> 1217 THEN
               IF v_proceso_cod = 220 THEN
                  LET v_ex_modulo_orig = "TA";
               ELSE
                  LET v_ex_modulo_orig = "AG";
               END IF
            ELSE
               LET v_ex_modulo_orig = "UG";
            END IF

           {---Ya no se leerá el movimiento liquidado
            EXECUTE FUNCTION fn_cre_obtiene_montos_liq(v_ex_id_derechohabiente,v_ex_folio_liquida)
                                                  INTO v_ex_f_liquida,
                                                       v_ex_aivs97,
                                                       v_ex_pesos97,
                                                       v_ex_aivs92,
                                                       v_ex_pesos92;
           }

            ---Ahora se busca el monto pendiente de conciliar de la base de registros sin conciliar
            IF EXISTS (SELECT id_derechohabiente
                         FROM safre_tmp:tmp_adelanto
                        WHERE id_derechohabiente = v_ex_id_derechohabiente) THEN

               SELECT MAX(f_liquida)
                 INTO v_ex_f_liquida
                 FROM safre_tmp:tmp_adelanto
                WHERE id_derechohabiente = v_ex_id_derechohabiente;

               SELECT SUM(monto_acciones)
                 INTO v_ex_aivs97
                 FROM safre_tmp:tmp_adelanto
                WHERE id_derechohabiente = v_ex_id_derechohabiente
                  AND subcuenta = 4;

               IF v_ex_aivs97 IS NULL OR v_ex_aivs97 = "" THEN
                  LET v_ex_aivs97 = 0;
               END IF

               SELECT SUM(monto_acciones)
                 INTO v_ex_aivs92
                 FROM safre_tmp:tmp_adelanto
                WHERE id_derechohabiente = v_ex_id_derechohabiente
                  AND subcuenta = 8;

               IF v_ex_aivs92 IS NULL OR v_ex_aivs92 = "" THEN
                  LET v_ex_aivs92 = 0;
               END IF

               LET v_ex_porcentaje_var = ROUND(100 - (((v_ex_aivs97+v_ex_aivs92)*-1)/(v_ex_aivs97_09+v_ex_aivs92_09)*100),2);

               IF v_ex_porcentaje_var < -100.00 THEN
                  LET v_ex_porcentaje_var = -101.00;
               END IF

               IF v_ex_porcentaje_var > 100.00 THEN
                  LET v_ex_porcentaje_var = 101.00;
               END IF

               INSERT INTO cre_extr_adelanto(
                           nss,
                           id_derechohabiente,
                           modulo_cod,
                           modulo_orig,
                           aivs92_09,
                           pesos92_09,
                           aivs97_09,
                           pesos97_09,
                           aivs92,
                           pesos92,
                           aivs97,
                           pesos97,
                           folio_liquida,
                           f_liquida,
                           porcentaje_var,
                           id_cre_ctr_archivo,
                           f_archivo,
                           f_proceso)
                    VALUES(v_ex_nss,
                           v_ex_id_derechohabiente,
                           v_ex_modulo_cod,
                           v_ex_modulo_orig,
                           v_ex_aivs92_09,
                           v_ex_pesos92_09,
                           v_ex_aivs97_09,
                           v_ex_pesos97_09,
                           v_ex_aivs92,
                           v_ex_pesos92,
                           v_ex_aivs97,
                           v_ex_pesos97,
                           v_ex_folio_liquida,
                           v_ex_f_liquida,
                           v_ex_porcentaje_var,
                           v_ex_id_cre_ctr_archivo,
                           v_ex_f_archivo,
                           v_ex_f_proceso);
            END IF
         END IF

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

         -- se asignan los valores en las variables que se usaran para insertar el registro historico
         LET his_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
         LET his_id_cre_ctr_archivo  = p_ax_id_cre_ctr_arch;
         LET his_folio_liquida       = p_d_folio;
         LET his_id_derechohabiente  = v_ax_id_derechohabiente;
         LET his_tpo_transferencia   = v_rs_tpo_transferencia;
         LET his_tpo_uso             = 2; 
         LET his_num_credito         = v_rs_num_credito_infonavit;
         LET his_f_presentacion      = v_rs_f_presentacion;      
         LET his_f_movimiento        = v_rs_f_movimiento;
         LET his_periodo_pago        = v_rs_periodo_pago;
         LET his_importe_v97         = v_rs_saldo_viv97/100;
         LET his_nss_afore           = v_rs_nss_afore;
         LET his_rfc_afore           = v_rs_rfc_afore;
         LET his_paterno_afore       = v_rs_ape_pat_trabajador_afore;
         LET his_materno_afore       = v_rs_ape_mat_trabajador_afore;
         LET his_nombre_afore        = v_rs_nom_trabajador_afore;
         LET his_nom_imss            = v_rs_nom_trabajador_imss;
         LET his_edo_procesar        = 115; -- saldos transferidos Procesar
         LET his_diagnostico         = v_rs_diagnostico_proceso;
         LET his_estado              = v_ax_estado;
         LET his_f_proceso           = TODAY;  

         -- se inserta registro en tabla cre uso garantia
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
             VALUES (his_id_cre_uso_garantia,
                     his_id_cre_ctr_archivo,
                     his_folio_liquida,
                     his_id_derechohabiente,
                     his_tpo_transferencia,
                     his_tpo_uso,
                     his_num_credito,
                     his_f_presentacion,
                     his_f_movimiento,
                     his_periodo_pago,
                     his_importe_v97,
                     his_nss_afore,
                     his_rfc_afore,
                     his_paterno_afore,
                     his_materno_afore,
                     his_nombre_afore,
                     his_nom_imss,
                     his_edo_procesar,
                     his_diagnostico,
                     his_estado,
                     his_f_proceso);

         -- si el registro fue rechazado continua con el siguiente registro
         IF v_ax_estado = 240 THEN
            CONTINUE FOREACH;
         END IF

         -- se validan las acciones obtenidas
         IF v_rs_aivs_97 IS NULL THEN
            LET v_rs_aivs_97 = 0;
         END IF

         -- se validan los pesos obtenidos
         IF v_rs_saldo_viv97 IS NULL THEN
            LET v_rs_saldo_viv97 = 0;
         END IF

         -- se asignan valores a las variables que se usan para insertar en deudor
         LET sdo_id_cre_acreditado = his_id_cre_uso_garantia;
         LET sdo_folio_referencia  = p_d_folio;
         LET sdo_f_movimiento      = v_rs_f_movimiento;
         LET sdo_movimiento        = 292;
         LET sdo_id_referencia     = v_rs_periodo_pago;
         LET sdo_monto_aivs        = v_rs_aivs_97/1000000;
         LET sdo_monto_pesos       = v_rs_saldo_viv97/100;
         LET sdo_f_proceso         = TODAY;

         -- se actualiza el registro maestro a estado 120 - Saldos transferidos
         UPDATE cre_uso_garantia
            SET edo_procesar        = 120,
                importe_v97         = sdo_monto_pesos
          WHERE id_cre_uso_garantia = v_ax_cre_uso_garant;

         --inserta en acr deudor
         INSERT INTO cre_saldo_deudor(
                     id_cre_acreditado,
                     folio_referencia,
                     f_movimiento,
                     movimiento,
                     id_referencia,
                     monto_aivs,
                     monto_pesos,
                     f_proceso)
             VALUES (sdo_id_cre_acreditado,
                     sdo_folio_referencia,
                     sdo_f_movimiento,
                     sdo_movimiento,
                     sdo_id_referencia,
                     sdo_monto_aivs,
                     sdo_monto_pesos,
                     sdo_f_proceso);

         -- se asignan los valores en las variables que se usaran para insertar el registro en tmp
         LET tmp_deudor_id_cre_acreditado = his_id_cre_uso_garantia;
         LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
         LET tmp_deudor_nss = v_rs_nss_infonavit;

         -- se inserta registro
         INSERT INTO safre_tmp:tmp_deudor_saldo_grt (
                     id_cre_acreditado,
                     id_derechohabiente,
                     nss)
             VALUES (tmp_deudor_id_cre_acreditado,
                     tmp_deudor_id_derechohabiente,
                     tmp_deudor_nss);

         FOREACH
            SELECT id_cre_acreditado, estado, tpo_credito, num_credito
              INTO cre_id_cre_acreditado, cre_estado, cre_tpo_credito, cre_num_credito
              FROM cre_acreditado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_originacion    = 2
               AND estado             = 280

            IF cre_id_cre_acreditado IS NOT NULL THEN
               -- se actualiza el registro maestro a estado 120 - Saldos transferidos
               UPDATE cre_uso_garantia
                  SET estado              = 320,
                      importe_v97         = sdo_monto_pesos
                WHERE id_cre_uso_garantia = v_ax_cre_uso_garant;

               FOREACH
                  SELECT id_derechohabiente,
                         marca,
                         n_referencia
                    INTO v_dm_id_derechohabiente,
                         v_dm_marca_entra,
                         v_dm_n_referencia
                    FROM sfr_marca_activa
                   WHERE id_derechohabiente = v_ax_id_derechohabiente
                     ---AND n_referencia  = cre_id_cre_acreditado
                     AND marca IN(SELECT marca_inf
                                    FROM cat_tipo_credito)

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
            END IF
         END FOREACH;
      END FOREACH;
   END FOREACH;

   FOREACH
    SELECT id_cre_acreditado, id_derechohabiente, tpo_credito, num_credito, f_culmina, estado
      INTO cre_id_cre_acreditado, cre_id_derechohabiente, cre_tpo_credito, cre_num_credito, cre_f_culmina, cre_estado
      FROM cre_acreditado
     WHERE estado          = 280
       AND tpo_originacion = 2

       SELECT COUNT(*)
         INTO v_folio_sin_conciliar
         FROM cre_uso_garantia c, cre_ctr_archivo r
        WHERE c.id_derechohabiente = cre_id_derechohabiente
          AND c.estado             = 140
          AND c.edo_procesar       < 120
          AND c.tpo_transferencia IN("18","48")
          AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo
          AND r.operacion          = 18;

       IF v_folio_sin_conciliar IS NULL OR
          v_folio_sin_conciliar = 0 THEN
          -- se valida si no existe el el id derechohabiente en proceso en la tabla del WS
          IF EXISTS (
             SELECT id_derechohabiente
               FROM cta_marca_ws
              WHERE id_derechohabiente = cre_id_derechohabiente) THEN
                -- Ya existe el derechohabiente en la tabla de WS. Se elimina
                DELETE
                  FROM cta_marca_ws
                 WHERE id_derechohabiente = cre_id_derechohabiente;
          END IF

          IF cre_f_culmina IS NULL THEN
             LET cre_f_culmina = TODAY;
          END IF

          -- se asignan los valores del registro a insertar en la tabla de WebService
          LET ws_id_derechohabiente = cre_id_derechohabiente;
          LET ws_id_origen          = cre_id_cre_acreditado;
          LET ws_modulo_cod         = v_rs_tpo_transferencia;
          LET ws_tpo_credito        = cre_tpo_credito;
          LET ws_marca              = 232;
          LET ws_f_solicita         = TODAY;
          LET ws_intento            = 1;
          LET ws_cod_result_op      = NULL;
          LET ws_diagnostico        = NULL;
          LET ws_situacion          = 0;
          LET ws_num_credito        = cre_num_credito;
          LET ws_f_infonavit        = cre_f_culmina;
          LET ws_folio_archivo      = p_d_folio;
          LET ws_usuario            = p_v_usuario;
          LET ws_marca_procesar     = "02";

          -- se inserta el registro en la tabla del WebService
          INSERT INTO cta_marca_ws (
                      id_derechohabiente,
                      id_origen,
                      modulo_cod,
                      tpo_credito,
                      marca,
                      f_solicita,
                      intento,
                      cod_result_op,
                      diagnostico,
                      situacion,
                      num_credito,
                      f_infonavit,
                      marca_procesar,
                      folio_archivo,
                      usuario)
              VALUES (ws_id_derechohabiente,
                      ws_id_origen,
                      ws_modulo_cod,
                      ws_tpo_credito,
                      ws_marca,
                      ws_f_solicita,
                      ws_intento,
                      ws_cod_result_op,
                      ws_diagnostico,
                      ws_situacion,
                      ws_num_credito,
                      ws_f_infonavit,
                      ws_marca_procesar,
                      ws_folio_archivo,
                      ws_usuario);

          UPDATE cre_acreditado
             SET estado = 170
           WHERE id_cre_acreditado = cre_id_cre_acreditado;
       END IF

       LET v_folio_sin_conciliar  = 0;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   -- actualiza estadisticas a la tabla de saldo deudor
   UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);
   
   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION;


