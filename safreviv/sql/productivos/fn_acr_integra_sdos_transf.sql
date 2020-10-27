






CREATE FUNCTION "safreviv".fn_acr_integra_sdos_transf(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_folio DECIMAL(10),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   -- REGISTRO tmp acr saldo
   DEFINE v_rs_tpo_registro               CHAR(2);
   DEFINE v_rs_con_servicio               DECIMAL(10,0);
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
   DEFINE v_rs_paterno_inf                CHAR(40);
   DEFINE v_rs_materno_inf                CHAR(40);
   DEFINE v_rs_nom_trabajador             CHAR(40);
   DEFINE v_rs_id_lote                    CHAR(16);
   DEFINE v_rs_nss_afore                  CHAR(11);
   DEFINE v_rs_rfc_afore                  CHAR(13);
   DEFINE v_rs_paterno_afore              CHAR(40);
   DEFINE v_rs_materno_afore              CHAR(40);
   DEFINE v_rs_nom_trabajador_afore       CHAR(40);
   DEFINE v_rs_aivs97                     DECIMAL(15,0);
   DEFINE v_rs_saldo_viv97                DECIMAL(15,0);
   DEFINE v_rs_aivs92                     DECIMAL(15,0);
   DEFINE v_rs_saldo_viv92                DECIMAL(15,0);
   DEFINE v_rs_cod_res_operacion          CHAR(2);
   DEFINE v_rs_diagnostico_proceso        CHAR(15);
   DEFINE v_rs_nom_trabajador_imss        CHAR(50);  
   DEFINE v_rs_num_credito_infonavit      DECIMAL(10,0);
   DEFINE v_rs_interes_saldo_viv97        DECIMAL(15,0);
   DEFINE v_rs_interes_saldo_viv92        DECIMAL(15,0);
   DEFINE v_rs_periodo_pago               CHAR(6);

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

   --REGISTRO saldo deudor
   DEFINE sdo_id_cre_acreditado           DECIMAL(9,0);
   DEFINE sdo_folio_referencia            DECIMAL(9,0);
   DEFINE sdo_f_movimiento                DATE;
   DEFINE sdo_movimiento                  SMALLINT;
   DEFINE sdo_id_referencia               DECIMAL(9,0);
   DEFINE sdo_monto_aivs                  DECIMAL(16,6);
   DEFINE sdo_monto_pesos                 DECIMAL(12,2);
   DEFINE sdo_f_proceso                   DATE;

   -- REGISTRO tmp deudor rechazo
   DEFINE tmpdeu_id_cre_acreditado       DECIMAL(9,0);
   DEFINE tmpdeu_id_derechohabiente      DECIMAL(9,0);
   DEFINE tmpdeu_nss                     CHAR(11);

   -- REGISTRO tmp confirmado (conciliación general)
   DEFINE tmpconc_nss                    CHAR(11);
   DEFINE tmpconc_id_derechohabiente     DECIMAL(9,0);
   DEFINE tmpconc_f_liquida              DATE;
   DEFINE tmpconc_modulo_cod             CHAR(3);

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado          DECIMAL(9,0);
   DEFINE cre_estado                     SMALLINT; -- estado
   DEFINE cre_edo_procesar               SMALLINT; -- estado procesar
   DEFINE cre_tpo_credito                SMALLINT; -- tipo de crédito
   DEFINE cre_num_credito                DECIMAL(10,0); -- número de crédito
   DEFINE cre_f_culmina                  DATE; -- fecha de culminación

   -- registro de cta tipo credito
   DEFINE cta_marca_inf                  SMALLINT; -- marca infonavit
   DEFINE cta_marca_prc                  SMALLINT; -- marca procesar

   -- REGISTRO de cta marca ws
   DEFINE ws_id_derechohabiente          DECIMAL(9,0);
   DEFINE ws_id_origen                   DECIMAL(9,0);
   DEFINE ws_modulo_cod                  CHAR(3);
   DEFINE ws_tpo_credito                 SMALLINT;
   DEFINE ws_marca                       SMALLINT;
   DEFINE ws_f_solicita                  DATE;
   DEFINE ws_intento                     SMALLINT;
   DEFINE ws_cod_result_op               SMALLINT;
   DEFINE ws_diagnostico                 SMALLINT;
   DEFINE ws_situacion                   SMALLINT;
   DEFINE ws_num_credito                 DECIMAL(10,0);
   DEFINE ws_f_infonavit                 DATE;
   DEFINE ws_marca_procesar              CHAR(2);
   DEFINE ws_folio_archivo               DECIMAL(9,0);
   DEFINE ws_usuario                     CHAR(20);

   -- parametros de la función de desmarca
   DEFINE des_id_derechohabiente         DECIMAL(9,0);
   DEFINE des_marca_entra                SMALLINT;
   DEFINE des_n_referencia               INTEGER;
   DEFINE des_estado_marca               SMALLINT;
   DEFINE des_marca_causa                SMALLINT;
   DEFINE des_usuario                    CHAR(20);

   -- CAMPOS auxiliares
   DEFINE v_ax_id_derechohabiente        DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_lote_acpt              INTEGER; -- identificador del lote de acr transferencia   
   DEFINE v_ax_id_lote_rech              INTEGER; 
   DEFINE v_ax_proceso_cod               SMALLINT; -- código del proceso
   DEFINE v_i_glo_estado                 SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_d_monto_aivs              DECIMAL(16,6); -- monto en aivs
   DEFINE v_ax_monto_pesos               DECIMAL(12,2); -- monto en pesos
   DEFINE v_ax_f_movimiento              DATE; -- fecha de presentación
   DEFINE v_ax_f_liquida                 DATE; -- fecha de liquidación
   DEFINE v_error                        SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                     INTEGER;
   DEFINE v_c_msj                        VARCHAR(250);
   DEFINE v_c_nss                        CHAR(11);
   DEFINE v_b_existe_reg                 SMALLINT; -- booleana que indica si existe o no la originación de crédito
   DEFINE r_ax_existe_marca_prc          SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera                   SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_edo_retorno               SMALLINT; -- estado retorno de alguna funcion
   DEFINE v_ax_tipo_trabajador           CHAR(1);
   DEFINE v_ax_folio_liq                 DECIMAL(9,0);
   DEFINE v_folio_liquida                DECIMAL(9,0);

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

   DEFINE v_criterio                     SMALLINT;
   DEFINE v_f_liquida                    DATE;
   DEFINE v_tabla                        CHAR(20);
   DEFINE v_proceso_cod                  SMALLINT;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/acrIntegSdoTrasnf.trace';
   ---TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_rech       = 0;
   LET v_ax_id_lote_acpt       = 0;   
   LET v_i_glo_estado          = 2; -- estado Integrado
   LET v_ax_proceso_cod        = 204; -- Saldos Transferidos ACR
   LET v_error                 = 0;
   LET v_isam_err              = 0;
   LET v_c_msj                 = 'El proceso finalizó correctamente';
   LET v_c_nss                 = "0"; -- valor del NSS antes de entrar al ciclo
   LET cre_id_cre_acreditado   = "";
   LET v_ax_folio_liq          = 45890;
   LET v_ax_tipo_trabajador    = "";
   LET v_folio_liquida         = 0;

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
   LET v_proceso_cod           = 220;

   -- se obtiene la fecha de presentacion de la información a integrar
   FOREACH
    SELECT FIRST 1 f_movimiento
      INTO v_ax_f_movimiento
      FROM safre_tmp:tmp_acr_saldo
     WHERE f_movimiento IS NOT NULL
   END FOREACH;

   -- se asigna la fecha de liquidación, fecha del ultimo día de mes anterior de la fecha movimiento
   LET v_ax_f_liquida = v_ax_f_movimiento - 1;

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
           f_movimiento,
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
           aplicaciones_interes_viv97,
           saldo_viv97,
           aplicaciones_interes_viv92,
           saldo_viv92,
           cod_res_operacion,
           diagnostico_proceso,
           nom_trabajador_imss,
           num_credito_infonavit,
           interes_saldo_viv97,
           interes_saldo_viv92,
           periodo_pago
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
           v_rs_paterno_inf,
           v_rs_materno_inf,
           v_rs_nom_trabajador,
           v_rs_id_lote,
           v_rs_nss_afore,
           v_rs_rfc_afore,
           v_rs_paterno_afore,
           v_rs_materno_afore,
           v_rs_nom_trabajador_afore,
           v_rs_aivs97,
           v_rs_saldo_viv97,
           v_rs_aivs92,
           v_rs_saldo_viv92,
           v_rs_cod_res_operacion,
           v_rs_diagnostico_proceso,
           v_rs_nom_trabajador_imss,
           v_rs_num_credito_infonavit,
           v_rs_interes_saldo_viv97,
           v_rs_interes_saldo_viv92,
           v_rs_periodo_pago
      FROM safre_tmp:tmp_acr_saldo
    ORDER BY nss_infonavit

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss  = v_rs_nss_infonavit;

      -- se asume que no existirá la originación de crédito
      LET v_b_existe_reg = 0;

      -- se obtiene la información de la tabla de generación de archivo
      FOREACH
       SELECT FIRST 1 t1.id_cre_acreditado
         INTO cre_id_cre_acreditado
         FROM safre_tmp:tmp_acr_solic_sdo t1
        WHERE t1.nss = v_c_nss
      END FOREACH;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente, tipo_trabajador
        INTO v_ax_id_derechohabiente, v_ax_tipo_trabajador
        FROM afi_derechohabiente
       WHERE nss = v_rs_nss_infonavit;

      -- se obtiene la información de la tabla maestro
      FOREACH
       SELECT id_cre_acreditado, estado, edo_procesar, tpo_credito, num_credito, f_culmina, folio_liquida
         INTO cre_id_cre_acreditado, cre_estado, cre_edo_procesar, cre_tpo_credito, cre_num_credito, cre_f_culmina, v_folio_liquida
         FROM cre_acreditado
        WHERE id_cre_acreditado = cre_id_cre_acreditado

         -- se indica que existe la originación de crédito
         LET v_b_existe_reg = 1;
      END FOREACH;

      IF cre_id_cre_acreditado IS NULL THEN
         FOREACH
          SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar, tpo_credito, num_credito, f_culmina, folio_liquida
            INTO cre_id_cre_acreditado, cre_estado, cre_edo_procesar, cre_tpo_credito, cre_num_credito, cre_f_culmina, v_folio_liquida
            FROM cre_acreditado
           WHERE estado IN (20, 25, 140, 142, 270, 280) --,900,910,920)
             AND id_derechohabiente = v_ax_id_derechohabiente
             AND tpo_originacion = 1
           ORDER BY f_otorga DESC, estado

            -- se indica que existe la originación de crédito
            LET v_b_existe_reg = 1;
         END FOREACH;
      END IF

      -- se verifica si no existió la originación de crédito
      IF v_b_existe_reg = 0 THEN
         -- No existió la originación de crédito. Se rechaza el registro
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = v_rs_nss_infonavit;
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = v_rs_tpo_registro;
         LET rch_num_credito        = v_rs_num_credito_infonavit;
         LET rch_sdo_deudor         = v_rs_saldo_viv97 + v_rs_saldo_viv92;
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

      -- se asignan los valores en el registro de la temporal que se usa en la conciliación general
      LET tmpconc_nss                = v_rs_nss_infonavit;
      LET tmpconc_id_derechohabiente = v_ax_id_derechohabiente;
      LET tmpconc_f_liquida          = v_ax_f_liquida;
      LET tmpconc_modulo_cod         = "acr";

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
         LET v_ex_modulo_cod         = "TA";
         LET v_ex_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET v_ex_f_proceso          = TODAY;
         LET v_ex_f_archivo          = year(v_rs_f_presentacion)||lpad(month(v_rs_f_presentacion),2,0)||lpad(day(v_rs_f_presentacion),2,0);
         LET v_ex_aivs92_09          = v_rs_aivs92 / 1000000;
         LET v_ex_pesos92_09         = v_rs_saldo_viv92 / 100;
         LET v_ex_aivs97_09          = v_rs_aivs97 / 1000000;
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

         IF v_proceso_cod <> 220 THEN
            IF v_proceso_cod = 1217 THEN
               LET v_ex_modulo_orig = "UG";
            ELSE
               LET v_ex_modulo_orig = "AG";
            END IF
         ELSE
            LET v_ex_modulo_orig = "TA";
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

      -- se valida el estado procesar. Si corresponde a sólo infonavit se rechaza el registro
      IF cre_edo_procesar = 7 THEN
         -- se rechaza el registro
         LET cre_estado = 240;

         -- se asignan los valores en las variables que se usaran para insertar el registro histórico
         LET his_id_cre_acreditado  = cre_id_cre_acreditado;
         LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET his_tpo_transferencia  = v_rs_tpo_transferencia;
         LET his_edo_procesar       = 5; -- saldos transferidos
         LET his_diagnostico        = 0;
         LET his_estado             = cre_estado;
         LET his_nss_afore          = v_rs_nss_afore;
         LET his_rfc_afore          = v_rs_rfc_afore;
         LET his_paterno_afore      = v_rs_paterno_afore;
         LET his_materno_afore      = v_rs_materno_afore;
         LET his_nombre_afore       = v_rs_nom_trabajador_afore;
         LET his_nom_imss           = v_rs_nom_trabajador_imss;
         LET his_f_proceso          = TODAY;

         -- se inserta registro en tabla his acreditado
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
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = v_rs_tpo_registro;
         LET rch_num_credito        = v_rs_num_credito_infonavit;
         LET rch_sdo_deudor         = v_rs_saldo_viv97 + v_rs_saldo_viv92;
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

      -- se obtiene las marcas y el tipo de transferencia para el tipo de credito en proceso
      FOREACH
       SELECT FIRST 1 marca_inf, marca_prc
         INTO cta_marca_inf, cta_marca_prc
         FROM cat_tipo_credito
        WHERE tpo_credito = cre_tpo_credito
        ORDER BY f_actualiza DESC
      END FOREACH;

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
                                        INTO r_ax_edo_retorno;
         END IF
      END IF

      -- se incrementa el numero de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se asignan los valores en las variables que se usaran para insertar el registro historico
      LET his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = v_rs_tpo_transferencia;
      LET his_edo_procesar       = 120; -- saldos transferidos
      LET his_diagnostico        = 0;
      LET his_estado             = cre_estado;
      LET his_nss_afore          = v_rs_nss_afore;
      LET his_rfc_afore          = v_rs_rfc_afore;
      LET his_paterno_afore      = v_rs_paterno_afore;
      LET his_materno_afore      = v_rs_materno_afore;
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
         CONTINUE FOREACH;
      END IF

      -- si el registro ya está como desmarcado se ejecuta la rutina de desmarca
      IF cre_estado = 270 OR cre_estado = 280 THEN
         -- se asigna el estado a actualiza en la tabla maestro
         LET his_edo_procesar = 120;

         -- se asignan los valores para la función de desmarca
         LET des_id_derechohabiente = v_ax_id_derechohabiente;
         LET des_marca_entra        = cta_marca_inf;
         LET des_n_referencia       = cre_id_cre_acreditado;
         LET des_estado_marca       = 0;
         LET des_marca_causa        = 0;
         LET des_usuario            = p_v_usuario;

         -- se invoca la función de desmarca
         EXECUTE FUNCTION fn_desmarca_cuenta(des_id_derechohabiente,
                                             des_marca_entra,
                                             des_n_referencia,
                                             des_estado_marca,
                                             des_marca_causa,
                                             des_usuario,
                                             v_ax_proceso_cod)
                                        INTO r_ax_edo_retorno;

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

         IF cre_f_culmina IS NULL THEN
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
         LET ws_f_infonavit        = cre_f_culmina;
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

         UPDATE cre_acreditado
            SET estado = 170
          WHERE id_cre_acreditado = cre_id_cre_acreditado;
      END IF

      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla acr transferencia a estado procesar 120-Saldo transferridos
      EXECUTE PROCEDURE sp_act_cre_transf(cre_id_cre_acreditado, his_edo_procesar);

      -- se calculan los valores
      LET v_ax_d_monto_aivs = (v_rs_aivs97/1000000) + (v_rs_aivs92/1000000);
      LET v_ax_monto_pesos = (v_rs_saldo_viv97/100) + (v_rs_saldo_viv92/100);

      -- se validan las aivs calculadas
      IF v_ax_d_monto_aivs IS NULL THEN
         LET v_ax_d_monto_aivs = 0;
      END IF

      -- se validan los pesos calculados
      IF v_ax_monto_pesos IS NULL THEN
         LET v_ax_monto_pesos = 0;
      END IF

      -- se asignan valores a las variables que se usan para insertar en deudor
      LET sdo_id_cre_acreditado = cre_id_cre_acreditado;
      LET sdo_folio_referencia  = p_d_folio;
      LET sdo_f_movimiento      = v_rs_f_movimiento;
      LET sdo_movimiento        = 272; --230;
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

      -- se asignan los valores en las variables que se usarán para insertar el registro en tmp
      LET tmpdeu_id_cre_acreditado  = cre_id_cre_acreditado;
      LET tmpdeu_id_derechohabiente = v_ax_id_derechohabiente;
      LET tmpdeu_nss                = v_rs_nss_infonavit;

      -- se inserta registro
      INSERT INTO safre_tmp:tmp_deudor_saldo (
                  id_cre_acreditado,
                  id_derechohabiente,
                  nss)
          VALUES (tmpdeu_id_cre_acreditado,
                  tmpdeu_id_derechohabiente,
                  tmpdeu_nss);

      LET cre_id_cre_acreditado   = "";
      LET v_ax_tipo_trabajador    = "";
      LET v_folio_liquida         = 0;
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
      LET v_proceso_cod           = 220;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = 1;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;
   UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION;


