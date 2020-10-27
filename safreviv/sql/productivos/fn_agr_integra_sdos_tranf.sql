






CREATE FUNCTION "safreviv".fn_agr_integra_sdos_tranf(p_v_usuario          CHAR(20),
                                          p_v_arch_proceso     CHAR(100),
                                          p_d_folio            DECIMAL(10),
                                          p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                          p_ax_f_movimiento    DATE)

   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11), DECIMAL(10,0)
   --REGISTRO tmp sdo transf enc agr

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
   DEFINE v_re_cve_mod_recepcion          CHAR(2);
   DEFINE v_re_cod_res_operacion          CHAR(2);
   DEFINE v_re_mot_rechazo_lote           CHAR(9);

   -- REGISTRO tmp sdo transf det agr
   DEFINE v_rs_tpo_registro               CHAR(2);
   DEFINE v_rs_con_servicio               DECIMAL(10);
   DEFINE v_rs_tpo_entidad_recep          CHAR(2);
   DEFINE v_rs_cve_entidad_recep          CHAR(3);
   DEFINE v_rs_tpo_entidad_cede           CHAR(2);
   DEFINE v_rs_cve_entidad_cede           CHAR(3);
   DEFINE v_rs_tpo_transferencia          CHAR(2);
   DEFINE v_rs_f_presentacion             DATE;
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
   DEFINE v_rs_indicador_viv92            DECIMAL(15);
   DEFINE v_rs_aivs_92                    DECIMAL(15);
   DEFINE v_rs_indicador_viv97            DECIMAL(15);
   DEFINE v_rs_cod_res_operacion          CHAR(2);
   DEFINE v_rs_diagnostico_proceso        CHAR(15);
   DEFINE v_rs_nom_trabajador_imss        CHAR(50);
   DEFINE v_rs_num_credito_infonavit      DECIMAL(10);

   --REGISTRO rch acreditado
   DEFINE rch_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE rch_nss                         CHAR(11);
   DEFINE rch_tpo_originacion             SMALLINT;
   DEFINE rch_tpo_registro                CHAR(2);
   DEFINE rch_num_credito                 DECIMAL(10,0);
   DEFINE rch_sdo_deudor                  DECIMAL(12,2);
   DEFINE rch_valor_dscto                 DECIMAL(8,4);
   DEFINE rch_estado                      SMALLINT;

   --REGISTRO his acreditado
   DEFINE his_id_cre_acreditado           DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE his_tpo_transferencia           CHAR(2);
   DEFINE his_edo_procesar                SMALLINT;
   DEFINE his_diagnostico                 CHAR(3);
   DEFINE his_estado                      SMALLINT;
   DEFINE his_nss_afore                   CHAR(11);
   DEFINE his_rfc_afore                   CHAR(13);
   DEFINE his_paterno_afore               CHAR(40);
   DEFINE his_materno_afore               CHAR(40);
   DEFINE his_nombre_afore                CHAR(40);
   DEFINE his_nom_imss                    CHAR(50);
   DEFINE his_f_proceso                   DATE;

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado           DECIMAL(9,0);
   DEFINE cre_estado                      SMALLINT;      -- estado
   DEFINE cre_edo_procesar                SMALLINT;      -- estado procesar
   DEFINE cre_tpo_credito                 SMALLINT;      -- tipo de crédito
   DEFINE cre_num_credito                 DECIMAL(10,0); -- número de crédito
   DEFINE cre_f_culmina                   DATE;          -- fecha de culminación

   -- registro de cta tipo credito
   DEFINE cta_marca_inf                   SMALLINT; -- marca infonavit
   DEFINE cta_marca_prc                   SMALLINT; -- marca procesar

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

   -- REGISTRO cre uso garantia
   DEFINE uso_id_cre_uso_garantia         DECIMAL(9,0);
   DEFINE uso_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE uso_id_derechohabiente          DECIMAL(9,0);
   DEFINE uso_tpo_transferencia           CHAR(2);
   DEFINE uso_tpo_uso                     SMALLINT;
   DEFINE uso_num_credito                 DECIMAL(10,0);
   DEFINE uso_f_presentacion              DATE;
   DEFINE uso_f_movimiento                DATE;
   DEFINE uso_periodo_pago                CHAR(6);
   DEFINE uso_importe_v97                 DECIMAL(22,2);
   DEFINE uso_nss_afore                   CHAR(11);
   DEFINE uso_rfc_afore                   CHAR(13);
   DEFINE uso_paterno_afore               CHAR(40);
   DEFINE uso_materno_afore               CHAR(40);
   DEFINE uso_nombre_afore                CHAR(40);
   DEFINE uso_nom_imss                    CHAR(50);
   DEFINE uso_edo_procesar                SMALLINT;
   DEFINE uso_diagnostico                 CHAR(3);
   DEFINE uso_estado                      SMALLINT;
   DEFINE uso_f_proceso                   DATE;
   DEFINE uso_folio_liquida               DECIMAL(9,0);

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
   DEFINE tmp_deudor_id_cre_acreditado   DECIMAL(9,0);
   DEFINE tmp_deudor_id_derechohabiente  DECIMAL(9,0);
   DEFINE tmp_deudor_nss                 CHAR(11);

   -- REGISTRO tmp confirmado (conciliación general)
   DEFINE tmpconc_nss                    CHAR(11);
   DEFINE tmpconc_id_derechohabiente     DECIMAL(9,0);
   DEFINE tmpconc_f_liquida              DATE;
   DEFINE tmpconc_modulo_cod             CHAR(3);

   -- parámetros de la función de desmarca
   DEFINE des_id_derechohabiente         DECIMAL(9,0);
   DEFINE des_marca_entra                SMALLINT;
   DEFINE des_n_referencia               INTEGER;
   DEFINE des_estado_marca               SMALLINT;
   DEFINE des_marca_causa                SMALLINT;
   DEFINE des_usuario                    CHAR(20);
   DEFINE des_proceso_cod                SMALLINT;

   -- CAMPOS auxiliares
   DEFINE v_ax_id_referencia             DECIMAL(9,0); -- referencia a acreditados o uso
   DEFINE v_ax_id_derechohabiente        DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_cre_uso_garantia       DECIMAL(9,0); -- identificador de uso garantia
   DEFINE v_ax_estado_uso                SMALLINT; -- estado de uso garantia
   DEFINE v_ax_edo_proc_uso              SMALLINT; -- estado procesar de uso de garantia
   DEFINE v_ax_id_lote_acpt              INTEGER; -- identificador del lote de acr transferencia
   DEFINE v_ax_id_lote_rech              INTEGER;
   DEFINE v_ax_proceso_cod               SMALLINT; -- código del proceso
   DEFINE v_ax_modulo_cod                CHAR(2); -- código del modulo
   DEFINE v_i_glo_estado                 SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_aporta_viv97              DECIMAL(13,2); -- aportación vivienda 97
   DEFINE v_ax_d_monto_aivs              DECIMAL(22,2); -- monto en aivs
   DEFINE v_ax_monto_pesos               DECIMAL(22,2); -- monto en pesos
   DEFINE v_ax_marca_entra               SMALLINT; -- marca de procesar
   DEFINE v_ax_f_presentacion            DATE; -- fecha de presentación
   DEFINE v_ax_f_liquida                 DATE; -- fecha de liquidación
   DEFINE v_error                        SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                     INTEGER;
   DEFINE v_c_msj                        VARCHAR(250);
   DEFINE v_c_nss                        CHAR(11);
   DEFINE v_b_existe_reg                 SMALLINT; -- booleana que indica si existe o no la originación de crédito
   DEFINE v_ax_ban_reg_rech              SMALLINT; -- bandera que indica si el registro fue o no rechazado
   DEFINE r_ax_existe_marca_prc          SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera                   SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_edo_desmarca              SMALLINT; --guarda el valor retornado por la funcion de desmarca
   DEFINE r_ax_edo_marca                 SMALLINT; -- estado retorno de alguna funcion

   DEFINE v_folio_sin_conciliar          DECIMAL(9,0);  --Total de folios del registro que aun no se concilian con Procesar
   DEFINE v_sts_pend                     SMALLINT;

   DEFINE v_importe_v92                  DECIMAL(22,2);
   DEFINE v_importe_v97                  DECIMAL(22,2);
   DEFINE v_importe_viv                  DECIMAL(22,2);
   DEFINE v_f_valua                      DATE;
   DEFINE v_valor_fondo                  DECIMAL(15,6);
   DEFINE v_tot_liq                      DECIMAL(10,0);

   DEFINE v_dm_id_derechohabiente        DECIMAL(9,0);
   DEFINE v_dm_marca_entra               SMALLINT;
   DEFINE v_dm_n_referencia              DECIMAL(9,0);
   DEFINE v_dm_estado_marca              SMALLINT;
   DEFINE v_dm_marca_causa               SMALLINT;
   DEFINE v_dm_usuario                   CHAR(20);
   DEFINE v_dm_proceso_cod               SMALLINT;
   DEFINE v_ax_cod_error                 SMALLINT;

   DEFINE v_criterio                     SMALLINT;
   DEFINE v_f_liquida                    DATE;
   DEFINE v_ax_tipo_trabajador           CHAR(1);
   DEFINE v_ax_folio_liq                 DECIMAL(9,0);
   DEFINE v_folio_liquida                DECIMAL(9,0);
   DEFINE v_proceso_cod                  SMALLINT;

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

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      LET v_tot_liq = 0;
      LET v_c_nss   = "";

      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss, v_tot_liq;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/agrIntegraSaldosTrans.trace';
   ---TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_rech     = 0;
   LET v_ax_id_lote_acpt     = 0;
   LET v_ax_ban_reg_rech     = 0;
   LET v_i_glo_estado        = 2;   -- estado Integrado
   LET v_ax_marca_entra      = 225; --marca para anualidad
   LET v_ax_proceso_cod      = 305; -- Saldos Transferidos AGR
   LET v_error               = 0;
   LET v_isam_err            = 0;
   LET v_c_msj               = 'El proceso finalizó correctamente';
   LET v_c_nss               = "0"; -- valor del NSS antes de entrar al ciclo
   LET v_folio_sin_conciliar = 0;
   LET v_tot_liq             = 0;
   LET cre_f_culmina         = TODAY;
   LET cre_tpo_credito       = 10;
   LET cta_marca_prc         = 234;
   LET cre_num_credito       = 1;
   LET v_dm_usuario          = p_v_usuario;
   LET v_dm_estado_marca     = 0;
   LET v_dm_marca_causa      = 0;
   LET v_dm_proceso_cod      = v_ax_proceso_cod;
   LET v_ax_folio_liq        = 48858;
   LET v_folio_liquida       = 0;
   LET v_ax_tipo_trabajador  = "";

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
   LET v_proceso_cod           = 312;

   -- se obtiene la fecha de presentacion de la información a integrar
   FOREACH
      SELECT FIRST 1 f_presentacion
        INTO v_ax_f_presentacion
        FROM safre_tmp:tmp_sdo_transf_det_agr
       WHERE f_presentacion IS NOT NULL
   END FOREACH

   LET v_f_valua = v_ax_f_presentacion - DAY(v_ax_f_presentacion) + 1 UNITS DAY;
   LET v_f_valua = v_f_valua + 1 UNITS MONTH;

   SELECT precio_fondo
     INTO v_valor_fondo
     FROM glo_valor_fondo
    WHERE f_valuacion = v_f_valua
      AND fondo = 11;

   -- se calcula la fecha de liquidación, fecha del ultimo día del mes de la fecha de presentación
   LET v_ax_f_liquida = v_ax_f_presentacion - DAY(v_ax_f_presentacion) + 1;
   LET v_ax_f_liquida = v_ax_f_liquida + 1 UNITS MONTH;

   -- se obtienen los datos de acr transferencia para el archivo en proceso
   FOREACH
      SELECT tpo_registro,
             con_servicio,
             tpo_entidad_recep,
             cve_entidad_recep,
             tpo_entidad_cede,
             cve_entidad_cede,
             tpo_transferencia,
             f_presentacion,
             curp,
             nss_infonavit,
             rfc_infonavit,
             ape_pat_trabajador,
             ape_mat_trabajador,
             nom_trabajador,
             id_lote,
             nss_afore,
             rfc_afore,
             ape_pat_trabajador_afore,
             ape_mat_trabajador_afore,
             nom_trabajador_afore,
             aplicaciones_interes_viv92,
             indicador_viv92,
             aplicaciones_interes_viv97,
             indicador_viv97,
             cod_res_operacion,
             diagnostico_proceso,
             nom_trabajador_imss,
             num_credito_infonavit
        INTO v_rs_tpo_registro,
             v_rs_con_servicio,
             v_rs_tpo_entidad_recep,
             v_rs_cve_entidad_recep,
             v_rs_tpo_entidad_cede,
             v_rs_cve_entidad_cede,
             v_rs_tpo_transferencia,
             v_rs_f_presentacion,
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
             v_rs_aivs_92,
             v_rs_indicador_viv92,
             v_rs_aivs_97,
             v_rs_indicador_viv97,
             v_rs_cod_res_operacion,
             v_rs_diagnostico_proceso,
             v_rs_nom_trabajador_imss,
             v_rs_num_credito_infonavit
        FROM safre_tmp:tmp_sdo_transf_det_agr

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = v_rs_nss_infonavit;

      LET v_importe_v92 = v_rs_aivs_92 / 1000000 * v_valor_fondo;
      LET v_importe_v97 = v_rs_aivs_97 / 1000000 * v_valor_fondo;
      LET v_importe_viv = v_importe_v92 + v_importe_v97;

      -- se valida si el registro fue o no rechazado
      IF v_ax_ban_reg_rech = 1 THEN
         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
      ELSE
         -- se incrementa el numero de registros aceptados
         LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
      END IF

      -- se asume que el registro será aceptado
      LET v_ax_ban_reg_rech = 0;

      -- se obtiene el id del derechohabiente para el nss
      FOREACH
         SELECT t.id_derechohabiente, t.id_referencia, t.modulo_cod, a.tipo_trabajador
           INTO v_ax_id_derechohabiente, v_ax_id_referencia, v_ax_modulo_cod, v_ax_tipo_trabajador
           FROM safre_tmp:tmp_agr_solic_sdo t, afi_derechohabiente a
          WHERE t.nss = v_rs_nss_infonavit
            AND t.nss = a.nss

         -- se valida el módulo
         IF v_ax_modulo_cod = "AG" THEN
            -- se asume que no existirá la originación de crédito
            LET v_b_existe_reg = 0;

            -- se obtiene el identificador del acreditado para el id_derechohabiente en proceso
            FOREACH
               SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar, tpo_credito, num_credito, f_culmina, folio_liquida
                 INTO cre_id_cre_acreditado, cre_estado, cre_edo_procesar, cre_tpo_credito, cre_num_credito, cre_f_culmina, v_folio_liquida
                 FROM cre_acreditado
                WHERE id_cre_acreditado = v_ax_id_referencia
            END FOREACH;

            -- se indica que existe la originación de crédito
            LET v_b_existe_reg = 1;

            -- se verifica si no existió la originación de crédito
            IF v_b_existe_reg = 0 THEN
               -- No existió la originación de crédito. Se rechaza el registro
               LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
               LET rch_nss                = v_rs_nss_infonavit;
               LET rch_tpo_originacion    = 4;
               LET rch_tpo_registro       = v_rs_tpo_registro;
               LET rch_num_credito        = v_rs_num_credito_infonavit;
               LET rch_sdo_deudor         = v_rs_aivs_92 + v_rs_aivs_97;
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

               -- se indica que el registro fue rechazado
               LET v_ax_ban_reg_rech = 1;

               CONTINUE FOREACH;
            END IF

            IF cre_id_cre_acreditado IS NULL THEN
               CONTINUE FOREACH;
            END IF

            -- se asignan los valores en el registro de la temporal que se usa en la conciliación general
            LET tmpconc_nss                = v_rs_nss_infonavit;
            LET tmpconc_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmpconc_f_liquida          = v_ax_f_liquida;
            LET tmpconc_modulo_cod         = "agr";

            -- se inserta registro
            IF v_ax_tipo_trabajador = "I" AND v_folio_liquida < v_ax_folio_liq THEN
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
               LET v_ex_modulo_cod         = "AG";
               LET v_ex_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
               LET v_ex_f_proceso          = TODAY;
               LET v_ex_f_archivo          = year(v_rs_f_presentacion)||lpad(month(v_rs_f_presentacion),2,0)||lpad(day(v_rs_f_presentacion),2,0);
               LET v_ex_aivs92_09          = v_rs_aivs_92 / 1000000;
               LET v_ex_pesos92_09         = 0;
               LET v_ex_aivs97_09          = v_rs_aivs_97 / 1000000;
               LET v_ex_pesos97_09         = 0;
               LET v_ex_folio_liquida      = v_folio_liquida;
               LET v_ex_aivs97             = 0;
               LET v_ex_pesos97            = 0;
               LET v_ex_aivs92             = 0;
               LET v_ex_pesos92            = 0;

               SELECT proceso_cod
                 INTO v_proceso_cod
                 FROM glo_folio
                WHERE folio = v_folio_liquida;

               IF v_proceso_cod <> 312 THEN
                  IF v_proceso_cod = 1217 THEN
                     LET v_ex_modulo_orig = "UG";
                  ELSE
                     LET v_ex_modulo_orig = "TA";
                  END IF
               ELSE
                  LET v_ex_modulo_orig = "AG";
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

                  INSERT INTO safre_tmp:tmp_adelanto_conc
                  SELECT *
                    FROM safre_tmp:tmp_adelanto
                   WHERE id_derechohabiente = v_ex_id_derechohabiente;

                  DELETE
                    FROM safre_tmp:tmp_adelanto
                   WHERE id_derechohabiente = v_ex_id_derechohabiente;
               END IF
            END IF

            -- se valida el estado procesar
            IF cre_edo_procesar = 7 THEN
               -- se rechaza el registro
               LET cre_estado = 240;

               -- se asignan los valores en las variables que se usaran para insertar el registro historico
               LET his_id_cre_acreditado  = cre_id_cre_acreditado;
               LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
               LET his_tpo_transferencia  = v_rs_tpo_transferencia;
               LET his_edo_procesar       = 120; -- saldos transferidos
               LET his_diagnostico        = 0;
               LET his_estado             = cre_estado;
               LET his_nss_afore          = v_rs_nss_afore;
               LET his_rfc_afore          = v_rs_rfc_afore;
               LET his_paterno_afore      = v_rs_ape_pat_trabajador_afore;
               LET his_materno_afore      = v_rs_ape_mat_trabajador_afore;
               LET his_nombre_afore       = v_rs_nom_trabajador_afore;
               LET his_nom_imss           = v_rs_nom_trabajador_imss;
               LET his_f_proceso          = TODAY;

               -- se inserta registro en tabla cre his acreditado
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

               -- No existió la originación de crédito. Se rechaza el registro
               LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
               LET rch_nss                = v_rs_nss_infonavit;
               LET rch_tpo_originacion    = 4;
               LET rch_tpo_registro       = v_rs_tpo_registro;
               LET rch_num_credito        = v_rs_num_credito_infonavit;
               LET rch_sdo_deudor         = v_rs_aivs_92 + v_rs_aivs_97;
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
               LET v_ax_ban_reg_rech = 1;

               CONTINUE FOREACH;
            END IF

            -- se asignan los valores para la función de desmarca
            LET des_id_derechohabiente = v_ax_id_derechohabiente;
            LET des_marca_entra        = v_ax_marca_entra;
            LET des_n_referencia       = cre_id_cre_acreditado;
            LET des_estado_marca       = 0;
            LET des_marca_causa        = 0;
            LET des_usuario            = p_v_usuario;
            LET des_proceso_cod        = v_ax_proceso_cod;

            -- se ejecuta la función de desmarcaje
            EXECUTE FUNCTION fn_desmarca_cuenta(des_id_derechohabiente,
                                                des_marca_entra,
                                                des_n_referencia,
                                                des_estado_marca,
                                                des_marca_causa,
                                                des_usuario,
                                                des_proceso_cod)
                                           INTO r_ax_edo_desmarca;

            -- se obtiene la marca y tipo originacion para el tipo de credito en proceso
            FOREACH
               SELECT FIRST 1 marca_inf, marca_prc
                 INTO cta_marca_inf, cta_marca_prc
                 FROM cat_tipo_credito
                WHERE tpo_credito = cre_tpo_credito
                ORDER BY f_actualiza DESC
            END FOREACH;

            IF cre_estado = 270 OR
               cre_estado = 275 OR
               cre_estado = 280 OR
               cre_estado = 300 OR
               cre_estado = 310 THEN
               LET v_tot_liq = v_tot_liq + 1;

               -- se asignan los valores para la función de desmarca
               LET des_id_derechohabiente = v_ax_id_derechohabiente;
               LET des_marca_entra        = cta_marca_inf;
               LET des_n_referencia       = cre_id_cre_acreditado;
               LET des_estado_marca       = 0;
               LET des_marca_causa        = 0;
               LET des_usuario            = p_v_usuario;
               LET des_proceso_cod        = v_ax_proceso_cod;

               -- se invoca la función de desmarca
               EXECUTE FUNCTION fn_desmarca_cuenta(des_id_derechohabiente,
                                                   des_marca_entra,
                                                   des_n_referencia,
                                                   des_estado_marca,
                                                   des_marca_causa,
                                                   des_usuario,
                                                   des_proceso_cod)
                                              INTO r_ax_edo_desmarca;

               -- se valida si no existe el el id derechohabiente en proceso en la tabla del WS
               IF EXISTS (
                  SELECT id_derechohabiente
                    FROM cta_marca_ws
                   WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
                  -- Ya existe el derechohabiente en la tabla de WS. Se elimina
                  DELETE
                    FROM cta_marca_ws
                   WHERE id_derechohabiente = v_ax_id_derechohabiente;
               END IF

               IF cre_f_culmina IS NULL OR cre_f_culmina = "" THEN
                  LET cre_f_culmina = TODAY;
               END IF

               -- se asignan los valores del registro a insertar en la tabla de WebService
               LET ws_id_derechohabiente = v_ax_id_derechohabiente;
               LET ws_id_origen          = cre_id_cre_acreditado;
               LET ws_modulo_cod         = v_rs_tpo_transferencia;
               LET ws_tpo_credito        = cre_tpo_credito;
               LET ws_marca              = cta_marca_prc;
               LET ws_f_solicita         = TODAY;
               LET ws_intento            = 1;
               LET ws_cod_result_op      = NULL;
               LET ws_diagnostico        = NULL;
               LET ws_situacion          = 0;
               LET ws_num_credito        = cre_num_credito;
               LET ws_f_infonavit        = TODAY;
               LET ws_folio_archivo      = p_d_folio;
               LET ws_usuario            = p_v_usuario;

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
            END IF

            -- se valida el estado procesar. Si es menor a 60 se intenta marcar
            IF cre_edo_procesar < 60 AND cre_estado < 170 THEN
               -- se invoca la función que verifica si ya existe la marca de procesar
               EXECUTE FUNCTION fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                                        cta_marca_prc)
                                                   INTO r_ax_existe_marca_prc;

               -- en caso de no existir la marca se ejecuta
               IF r_ax_existe_marca_prc = 0 THEN
                  -- se ejecuta la función de marcaje
                  EXECUTE FUNCTION fn_marca_cuenta(v_ax_id_derechohabiente,
                                                   cta_marca_prc,
                                                   cre_id_cre_acreditado, -- referencia
                                                   p_d_folio,
                                                   0, -- estado marca
                                                   0, -- codigo rechazo
                                                   NULL, -- marca causa
                                                   "", -- fecha causa
                                                   p_v_usuario,
                                                   v_ax_proceso_cod)
                                              INTO r_ax_edo_marca;
               END IF
            END IF

            -- se asignan los valores en las variables que se usarán para insertar el registro histórico
            LET his_id_cre_acreditado  = cre_id_cre_acreditado;
            LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
            LET his_tpo_transferencia  = v_rs_tpo_transferencia;
            LET his_edo_procesar       = 120; -- saldos transferidos
            LET his_diagnostico        = 0;
            LET his_estado             = cre_estado;
            LET his_nss_afore          = v_rs_nss_afore;
            LET his_rfc_afore          = v_rs_rfc_afore;
            LET his_paterno_afore      = v_rs_ape_pat_trabajador_afore;
            LET his_materno_afore      = v_rs_ape_mat_trabajador_afore;
            LET his_nombre_afore       = v_rs_nom_trabajador_afore;
            LET his_nom_imss           = v_rs_nom_trabajador_imss;
            LET his_f_proceso          = TODAY;

            -- se inserta registro en tabla cre his acreditado
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
            IF cre_estado = 240 THEN
               -- se indica que el registro fue rechazado
               LET v_ax_ban_reg_rech = 1;

               CONTINUE FOREACH;
            END IF

            -- se desmarcan todos los registros
            -- se asigna el estado a actualiza en la tabla maestro
            LET his_edo_procesar = 120;

            -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
            IF v_rs_tpo_transferencia = "03" THEN
               -- corresponde a Transferencia de Acreditados
               LET ws_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
            ELIF v_rs_tpo_transferencia = "16" THEN
               -- corresponde a Anualidades Garantizadas
               LET ws_marca_procesar = "02"; -- 'grt' => 02 (Créditos en Garantía)
            ELSE
               -- corresponde a Anualidades Garantizadas
               LET ws_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
            END IF

            SELECT COUNT(*)
              INTO v_folio_sin_conciliar
              FROM cre_uso_garantia
             WHERE id_derechohabiente = v_ax_id_derechohabiente
               AND estado             IN(20,140,320)
               AND edo_procesar       < 120
               AND tpo_transferencia  = "43"
               AND id_cre_ctr_archivo NOT IN(SELECT id_cre_ctr_archivo
                                               FROM cre_ctr_archivo
                                              WHERE operacion = 43);

            IF v_folio_sin_conciliar IS NULL OR
               v_folio_sin_conciliar = 0 THEN
               IF cre_estado = 280 THEN
                  UPDATE cre_acreditado
                     SET estado            = 170
                   WHERE id_cre_acreditado = cre_id_cre_acreditado;
               END IF
            END IF

            -- se ejecuta el store procedure que actualiza el registro correspondiente de la
            -- tabla acr transferencia a estado procesar 120-Saldo transferridos
            EXECUTE PROCEDURE sp_act_cre_transf(cre_id_cre_acreditado, his_edo_procesar);

            -- se calculan los importes
            LET v_ax_d_monto_aivs = (v_rs_aivs_97/1000000) + (v_rs_aivs_92/1000000);
            LET v_ax_monto_pesos  = (v_rs_indicador_viv92/100) + (v_rs_indicador_viv97/100);

            -- se validan las aivs calculadas
            IF v_ax_d_monto_aivs IS NULL THEN
               LET v_ax_d_monto_aivs = 0;
            END IF

            -- se validan los pesos calculados
            IF v_ax_monto_pesos IS NULL THEN
               LET v_ax_monto_pesos = 0;
            END IF

            --se asignan valores a las variables que se usan para insertar en deudor
            LET sdo_id_cre_acreditado = cre_id_cre_acreditado;
            LET sdo_folio_referencia  = p_d_folio;
            LET sdo_movimiento        = 282;
            LET sdo_f_movimiento      = p_ax_f_movimiento;
            LET sdo_id_referencia     = cre_id_cre_acreditado;
            LET sdo_monto_aivs        = v_ax_d_monto_aivs;
            LET sdo_monto_pesos       = v_ax_monto_pesos;
            LET sdo_f_proceso         = TODAY;

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
            LET tmp_deudor_id_cre_acreditado  = cre_id_cre_acreditado;
            LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmp_deudor_nss                = v_rs_nss_infonavit;

            -- se inserta registro
            INSERT INTO safre_tmp:tmp_deudor_saldo_agr (
                        id_cre_acreditado,
                        id_derechohabiente,
                        nss)
                VALUES (tmp_deudor_id_cre_acreditado,
                        tmp_deudor_id_derechohabiente,
                        tmp_deudor_nss);

            LET v_folio_sin_conciliar = 0;
         ELSE
            -- Se asume que el modulo es "UA". Se obtiene el identificador de uso garantia para el id_derechohabiente en proceso
            FOREACH
               SELECT t.id_derechohabiente, t.id_referencia, t.modulo_cod, a.tipo_trabajador
                 INTO v_ax_id_derechohabiente, v_ax_id_referencia, v_ax_modulo_cod, v_ax_tipo_trabajador
                 FROM safre_tmp:tmp_agr_solic_sdo_ua t, afi_derechohabiente a
                WHERE t.nss = v_rs_nss_infonavit
                  AND t.nss = a.nss

               SELECT id_cre_uso_garantia, estado, edo_procesar, folio_liquida
                 INTO v_ax_id_cre_uso_garantia, v_ax_estado_uso, v_ax_edo_proc_uso, v_folio_liquida
                 FROM cre_uso_garantia
                WHERE id_cre_uso_garantia = v_ax_id_referencia;

               -- se asignan los valores en el registro de la temporal que se usa en la conciliación general
               LET tmpconc_nss                = v_rs_nss_infonavit;
               LET tmpconc_id_derechohabiente = v_ax_id_derechohabiente;
               LET tmpconc_f_liquida          = v_ax_f_liquida;
               LET tmpconc_modulo_cod         = "agr";

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
               LET v_ex_modulo_cod         = "AG";
               LET v_ex_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
               LET v_ex_f_proceso          = TODAY;
               LET v_ex_f_archivo          = year(v_rs_f_presentacion)||lpad(month(v_rs_f_presentacion),2,0)||lpad(day(v_rs_f_presentacion),2,0);
               LET v_ex_aivs92_09          = v_rs_aivs_92 / 1000000;
               LET v_ex_pesos92_09         = 0;
               LET v_ex_aivs97_09          = v_rs_aivs_97 / 1000000;
               LET v_ex_pesos97_09         = 0;
               LET v_ex_folio_liquida      = v_folio_liquida;

               SELECT proceso_cod
                 INTO v_proceso_cod
                 FROM glo_folio
                WHERE folio = v_folio_liquida;

               IF v_proceso_cod <> 312 THEN
                  IF v_proceso_cod = 1217 THEN
                     LET v_ex_modulo_orig = "UG";
                  ELSE
                     LET v_ex_modulo_orig = "TA";
                  END IF
               ELSE
                  LET v_ex_modulo_orig = "AG";
               END IF

               EXECUTE FUNCTION fn_cre_obtiene_montos_liq(v_ex_id_derechohabiente,v_ex_folio_liquida)
                                                     INTO v_ex_f_liquida,
                                                          v_ex_aivs97,
                                                          v_ex_pesos97,
                                                          v_ex_aivs92,
                                                          v_ex_pesos92;

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

               -- se asignan los valores para la función de desmarca
               LET des_id_derechohabiente = v_ax_id_derechohabiente;
               LET des_marca_entra        = v_ax_marca_entra;
               LET des_n_referencia       = v_ax_id_cre_uso_garantia;
               LET des_estado_marca       = 0;
               LET des_marca_causa        = 0;
               LET des_usuario            = p_v_usuario;
               LET des_proceso_cod        = v_ax_proceso_cod;
               LET cre_id_cre_acreditado  = "";

               -- se ejecuta la función de desmarcaje
               EXECUTE FUNCTION fn_desmarca_cuenta(des_id_derechohabiente,
                                                             des_marca_entra,
                                                             des_n_referencia,
                                                             des_estado_marca,
                                                             des_marca_causa,
                                                             des_usuario,
                                                             des_proceso_cod)
                                                        INTO r_ax_edo_desmarca;

               -- se calculan los valores
               LET v_ax_aporta_viv97 = v_rs_indicador_viv97 / 100;

               -- se valida el estado procesar
               IF v_ax_edo_proc_uso = 5 THEN
                  -- se rechaza el registro
                  LET v_ax_estado_uso = 240;
               END IF

               -- se asignan los valores en las variables que se usaran para insertar el registro
               LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
               LET uso_id_cre_ctr_archivo  = p_ax_id_cre_ctr_arch;
               LET uso_folio_liquida       = p_d_folio;
               LET uso_id_derechohabiente  = v_ax_id_derechohabiente;
               LET uso_tpo_transferencia   = v_rs_tpo_transferencia;
               LET uso_tpo_uso             = 2;
               LET uso_num_credito         = v_rs_num_credito_infonavit;
               LET uso_f_presentacion      = v_rs_f_presentacion;
               LET uso_f_movimiento        = p_ax_f_movimiento;
               LET uso_periodo_pago        = NULL;
               LET uso_importe_v97         = v_ax_aporta_viv97;
               LET uso_nss_afore           = v_rs_nss_afore;
               LET uso_rfc_afore           = v_rs_rfc_afore;
               LET uso_paterno_afore       = v_rs_ape_pat_trabajador_afore;
               LET uso_materno_afore       = v_rs_ape_mat_trabajador_afore;
               LET uso_nombre_afore        = v_rs_nom_trabajador_afore;
               LET uso_nom_imss            = v_rs_nom_trabajador_imss;
               LET uso_edo_procesar        = 115; -- saldos transferidos
               LET uso_diagnostico         = v_rs_diagnostico_proceso;
               LET uso_estado              = v_ax_estado_uso;
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
               IF v_ax_estado_uso = 240 THEN
                  -- se indica que el registro fue rechazado
                  LET v_ax_ban_reg_rech = 1;

                  CONTINUE FOREACH;
               END IF

               -- se actualiza el registro maestro a estado 120 - Saldos transferidos
               UPDATE cre_uso_garantia
                  SET edo_procesar        = 120,
                      importe_v97         = v_importe_viv
                WHERE id_cre_uso_garantia = v_ax_id_cre_uso_garantia;

               UPDATE cre_saldo_deudor
                  SET monto_pesos       = v_importe_viv
                WHERE id_cre_acreditado = v_ax_id_cre_uso_garantia
                  AND movimiento        = 411;

               -- se calculan los importes
               LET v_ax_d_monto_aivs = (v_rs_aivs_97/1000000) + (v_rs_aivs_92/1000000);
               LET v_ax_monto_pesos  = (v_rs_indicador_viv92/100) + (v_rs_indicador_viv97/100);

               -- se validan las aivs calculadas
               IF v_ax_d_monto_aivs IS NULL THEN
                  LET v_ax_d_monto_aivs = 0;
               END IF

               -- se validan los pesos calculados
               IF v_ax_monto_pesos IS NULL THEN
                  LET v_ax_monto_pesos = 0;
               END IF

               --se asignan valores a las variables que se usan para insertar en deudor
               LET sdo_id_cre_acreditado = uso_id_cre_uso_garantia;
               LET sdo_folio_referencia  = p_d_folio;
               LET sdo_movimiento        = 282;
               LET sdo_f_movimiento      = p_ax_f_movimiento;
               LET sdo_id_referencia     = uso_id_cre_uso_garantia;
               LET sdo_monto_aivs        = v_ax_d_monto_aivs;
               LET sdo_monto_pesos       = v_ax_monto_pesos;
               LET sdo_f_proceso         = TODAY;

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
               LET tmp_deudor_id_cre_acreditado = uso_id_cre_uso_garantia;
               LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
               LET tmp_deudor_nss = v_rs_nss_infonavit;

               -- se inserta registro
               INSERT INTO safre_tmp:tmp_deudor_saldo_agr (
                           id_cre_acreditado,
                           id_derechohabiente,
                           nss)
                   VALUES (tmp_deudor_id_cre_acreditado,
                           tmp_deudor_id_derechohabiente,
                           tmp_deudor_nss);

               LET v_folio_sin_conciliar = 0;

               IF NOT EXISTS (SELECT c1.id_derechohabiente
                               FROM cre_acreditado c1, cat_maq_credito m1
                              WHERE c1.id_derechohabiente = v_ax_id_derechohabiente
                                AND c1.tpo_originacion    = 4 
                                AND c1.estado             = m1.estado
                                AND m1.entidad            = 1) THEN

                  FOREACH
                     SELECT FIRST 1 id_cre_acreditado, estado, tpo_credito, num_credito
                       INTO cre_id_cre_acreditado, cre_estado, cre_tpo_credito, cre_num_credito
                       FROM cre_acreditado
                      WHERE id_derechohabiente = v_ax_id_derechohabiente
                        AND tpo_originacion = 4
                        ---AND estado IN(270,275,280,300,310)
                        AND estado IN(280,300,310)
                     ORDER BY 2 DESC
                  END FOREACH;

                  IF cre_id_cre_acreditado IS NOT NULL THEN
                     -- se valida si no existe el el id derechohabiente en proceso en la tabla del WS
                     UPDATE cre_acreditado
                        SET estado            = 170
                      WHERE id_cre_acreditado = cre_id_cre_acreditado;

                     -- se actualiza el registro maestro a estado 120 - Saldos transferidos
                     UPDATE cre_uso_garantia
                        SET estado              = 320,
                            importe_v97         = v_importe_viv
                      WHERE id_cre_uso_garantia = v_ax_id_cre_uso_garantia;

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

                  IF EXISTS (SELECT id_derechohabiente
                               FROM sfr_marca_activa
                              WHERE id_derechohabiente = v_ax_id_derechohabiente
                                AND marca = 234) THEN
                     IF EXISTS (SELECT id_derechohabiente
                                FROM cta_marca_ws
                               WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN
                        -- Ya existe el derechohabiente en la tabla de WS. Se elimina
                        DELETE
                          FROM cta_marca_ws
                         WHERE id_derechohabiente = v_ax_id_derechohabiente;
                     END IF

                     IF cre_id_cre_acreditado IS NOT NULL THEN 
                        IF cre_f_culmina IS NULL OR cre_f_culmina = "" THEN
                           LET cre_f_culmina = TODAY;
                        END IF

                        IF cre_tpo_credito IS NULL OR cre_tpo_credito = "" THEN
                           LET cre_tpo_credito = 10;
                        END IF

                        IF cre_num_credito IS NULL OR cre_num_credito = "" THEN
                           LET cre_num_credito = 1;
                        END IF

                        -- se asignan los valores del registro a insertar en la tabla de WebService
                        LET ws_id_derechohabiente = v_ax_id_derechohabiente;
                        LET ws_id_origen          = cre_id_cre_acreditado;
                        LET ws_modulo_cod         = v_rs_tpo_transferencia;
                        LET ws_tpo_credito        = 10;
                        LET ws_marca              = cta_marca_prc;
                        LET ws_f_solicita         = TODAY;
                        LET ws_intento            = 1;
                        LET ws_cod_result_op      = NULL;
                        LET ws_diagnostico        = NULL;
                        LET ws_situacion          = 0;
                        LET ws_num_credito        = cre_num_credito;
                        LET ws_f_infonavit        = TODAY;
                        LET ws_folio_archivo      = p_d_folio;
                        LET ws_usuario            = p_v_usuario;
                        LET ws_marca_procesar     = "04";

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
                     END IF
                  END IF
               END IF
            END FOREACH;
         END IF;
      END FOREACH;
   END FOREACH;

   -- se valida si el último registro fue rechazado
   IF v_ax_ban_reg_rech = 1 THEN
      -- se incrementa el numero de registros rechazados
      LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt - 1;
   END IF

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   EXECUTE FUNCTION fn_procesa_desmarca_pend(p_v_usuario,
                                             p_d_folio,
                                             4)
                                        INTO v_sts_pend;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss, v_tot_liq;

END FUNCTION;


