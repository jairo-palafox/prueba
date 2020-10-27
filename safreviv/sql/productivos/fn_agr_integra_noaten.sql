






CREATE FUNCTION "safreviv".fn_agr_integra_noaten(p_v_usuario CHAR(20),
                                      p_v_arch_proceso CHAR(100),
                                      p_d_folio DECIMAL(9),
                                      p_ax_id_cre_ctr_arch DECIMAL(9,0))
                                       
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- REGISTRO tmp no atend det agr
   DEFINE tmp_tpo_registro       CHAR(2);
   DEFINE tmp_cont_servicio      DECIMAL(10);
   DEFINE tmp_tpo_ent_recep      CHAR(2);
   DEFINE tmp_cve_ent_recep      CHAR(3);
   DEFINE tmp_tpo_ent_ceden      CHAR(2);
   DEFINE tmp_cve_ent_ceden      CHAR(3);
   DEFINE tmp_tpo_transf         CHAR(2);
   DEFINE tmp_f_presentacion     DATE;
   DEFINE tmp_filler1            CHAR(8);
   DEFINE tmp_curp_trabajador    CHAR(18);
   DEFINE tmp_nss_infonavit      CHAR(11);
   DEFINE tmp_filler2            CHAR(15);
   DEFINE tmp_rfc_infonavit      CHAR(13);
   DEFINE tmp_ape_pat_infonavit  CHAR(40);
   DEFINE tmp_ape_mat_infonavit  CHAR(40);
   DEFINE tmp_nom_trab_infonavit CHAR(40);
   DEFINE tmp_filler3            CHAR(22);
   DEFINE tmp_id_lote_solic      CHAR(16);
   DEFINE tmp_filler4            CHAR(15);
   DEFINE tmp_nss_afore          DECIMAL(11);
   DEFINE tmp_rfc_afore          CHAR(13);
   DEFINE tmp_filler5            CHAR(30);
   DEFINE tmp_ape_pat_afore      CHAR(40);
   DEFINE tmp_ape_mat_afore      CHAR(40);
   DEFINE tmp_nom_trab_afore     CHAR(40);
   DEFINE tmp_filler6            CHAR(45);
   DEFINE tmp_ult_aport_viv97    DECIMAL(15);
   DEFINE tmp_filler7            CHAR(95);
   DEFINE tmp_nom_trab_imss      CHAR(50);
   DEFINE tmp_num_cred_infonavit DECIMAL(10);
   DEFINE tmp_filler8            CHAR(53);
   DEFINE tmp_periodo_pago       CHAR(6);
   DEFINE tmp_filler9            CHAR(12);
   -- REGISTRO his acreditado
   DEFINE his_id_cre_acreditado  DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo DECIMAL(9,0);
   DEFINE his_tpo_transferencia  CHAR(2);
   DEFINE his_edo_procesar       SMALLINT;
   DEFINE his_diagnostico        CHAR(3);
   DEFINE his_estado             SMALLINT;
   DEFINE his_nss_afore          CHAR(11);
   DEFINE his_rfc_afore          CHAR(13);
   DEFINE his_paterno_afore      CHAR(40);
   DEFINE his_materno_afore      CHAR(40);
   DEFINE his_nombre_afore       CHAR(40);
   DEFINE his_nom_imss           CHAR(50);
   DEFINE his_f_proceso          DATE;
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
   -- CAMPOS auxiliares
   DEFINE v_ax_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_cre_acreditado   DECIMAL(9,0); -- identificador de acreditado
   DEFINE v_ax_id_cre_uso_garantia DECIMAL(9,0); -- identificador de uso garantia
   DEFINE v_ax_estado_cre          SMALLINT; -- estado
   DEFINE v_ax_estado_uso          SMALLINT; -- estado de uso garantia
   DEFINE v_ax_edo_proc_cre        SMALLINT; -- estado procesar de acreditado
   DEFINE v_ax_edo_proc_uso        SMALLINT; -- estado procesar de uso de garantia
   DEFINE v_ax_id_lote_acpt        INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech        INTEGER; -- total de registros rechazados
   DEFINE v_ax_marca_prc           SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_credito         SMALLINT; -- tipo de crédito
   DEFINE v_ax_proceso_cod         SMALLINT; -- código del proceso
   --DEFINE v_ax_folio_liquida       DECIMAL(9,0); --identificador del folio de liquidación en uso gtia
   DEFINE v_ax_modulo_cod          CHAR(2); -- código del modulo
   DEFINE v_ax_aporta_viv97        DECIMAL(13,2); -- aportación vivienda 97
   DEFINE v_i_estado               SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                  SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err               INTEGER;
   DEFINE v_c_msj                  VARCHAR(250);
   DEFINE v_c_nss                  CHAR(11);
   DEFINE r_ax_existe_marca_prc    SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera             SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_edo_marca           SMALLINT; -- estado retorno de alguna funcion

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegNoAten.trace';
   --TRACE ON;

   -- se inicializa las variables
   LET v_i_estado        = 2; -- estado Integrado
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_ax_estado_cre   = 0;
   LET v_ax_proceso_cod  = 307; -- Solicitudes No Atendidas AGR
   LET v_error           = 0;
   LET v_isam_err        = 0;
   LET v_c_msj           = 'El proceso finalizó correctamente';
   LET v_c_nss           = "0"; -- valor del NSS antes de entrar al ciclo

   -- se obtienen los datos de la temporal de No atendidas
   FOREACH
   SELECT *
   INTO tmp_tpo_registro,
        tmp_cont_servicio,
        tmp_tpo_ent_recep,
        tmp_cve_ent_recep,
        tmp_tpo_ent_ceden,
        tmp_cve_ent_ceden,
        tmp_tpo_transf,
        tmp_f_presentacion,
        tmp_filler1,
        tmp_curp_trabajador,
        tmp_nss_infonavit,
        tmp_filler2,
        tmp_rfc_infonavit,
        tmp_ape_pat_infonavit,
        tmp_ape_mat_infonavit,
        tmp_nom_trab_infonavit,
        tmp_filler3,
        tmp_id_lote_solic,
        tmp_filler4,
        tmp_nss_afore,
        tmp_rfc_afore,
        tmp_filler5,
        tmp_ape_pat_afore,
        tmp_ape_mat_afore,
        tmp_nom_trab_afore,
        tmp_filler6,
        tmp_ult_aport_viv97,
        tmp_filler7,
        tmp_nom_trab_imss,
        tmp_num_cred_infonavit,
        tmp_filler8,
        tmp_periodo_pago,
        tmp_filler9
   FROM safre_tmp:tmp_no_atend_det_agr
      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss_infonavit;

      -- se incrementa el numero de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se obtiene el id del derechohabiente y el módulo cod para el nss en proceso
      FOREACH
       SELECT UNIQUE id_derechohabiente, modulo_cod
         INTO v_ax_id_derechohabiente, v_ax_modulo_cod
         FROM safre_tmp:tmp_agr_solic_sdo2
        WHERE nss = tmp_nss_infonavit
         -- se valida el módulo
         IF v_ax_modulo_cod = "AG" THEN
            -- se obtiene el identificador del acreditado para el id_derechohabiente en proceso
            FOREACH
               SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar, tpo_credito
                 INTO v_ax_id_cre_acreditado, v_ax_estado_cre, v_ax_edo_proc_cre, v_ax_tpo_credito
                 FROM safre_viv:cre_acreditado
                WHERE estado >= 140 --IN (140,900,910,920)
                  AND id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_originacion = 4
                ORDER BY f_otorga DESC, estado
            END FOREACH;

            -- se obtiene la marca y tipo originacion para el tipo de credito en proceso
            FOREACH
             SELECT FIRST 1 marca_prc
               INTO v_ax_marca_prc
               FROM safre_viv:cat_tipo_credito
              WHERE tpo_credito = v_ax_tpo_credito
              ORDER BY f_actualiza DESC
            END FOREACH;

            -- se valida el estado procesar
            IF v_ax_edo_proc_cre = 5 THEN
               -- se rechaza el registro
               LET v_ax_estado_cre = 240;
            ELIF v_ax_edo_proc_cre < 60 THEN
               -- se invoca la función que verifica si ya existe la marca de procesar
               EXECUTE FUNCTION safre_viv:fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                                                  v_ax_marca_prc)
                                                             INTO r_ax_existe_marca_prc;

               -- en caso de no existir la marca se ejecuta
               IF r_ax_existe_marca_prc = 0 THEN
                  -- se ejecuta la función de marcaje
                  EXECUTE FUNCTION safre_viv:fn_marca_cuenta(v_ax_id_derechohabiente,
                                                             v_ax_marca_prc,
                                                             v_ax_id_cre_acreditado, -- referencia
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

            -- se asignan los valores en las variables que se usaran para insertar el registro
            LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
            LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
            LET his_tpo_transferencia  = tmp_tpo_transf;
            LET his_edo_procesar       = 110; -- No atendidas
            LET his_diagnostico        = 'NA1';
            LET his_estado             = v_ax_estado_cre;
            LET his_nss_afore          = tmp_nss_afore;
            LET his_rfc_afore          = tmp_rfc_afore;
            LET his_paterno_afore      = tmp_ape_pat_afore;
            LET his_materno_afore      = tmp_ape_mat_afore;
            LET his_nombre_afore       = tmp_nom_trab_afore;
            LET his_nom_imss           = tmp_nom_trab_imss;
            LET his_f_proceso          = TODAY;

            -- se inserta registro
            INSERT INTO safre_viv:cre_his_acreditado(
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
            IF v_ax_estado_cre = 240 THEN
               CONTINUE FOREACH;
            END IF

            -- se ejecuta el store procedure que actualiza el registro correspondiente de la
            -- tabla maestro a estado procesar 110-No atendidas
            EXECUTE PROCEDURE safre_viv:sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);

            -- se inserta registro duplicado con estado_procesar 70 y sin diagnostico
            LET his_edo_procesar = 70;
            LET his_diagnostico = NULL;

            -- se inserta registro
            INSERT INTO safre_viv:cre_his_acreditado(
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

            -- se asignan los valores en las variables que se usaran para insertar el registro en tmp
            LET tmp_deudor_id_cre_acreditado = his_id_cre_acreditado;
            LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmp_deudor_nss = tmp_nss_infonavit;

            -- se inserta registro
            INSERT INTO safre_tmp:tmp_deudor_no_aten_agr (
                        id_cre_acreditado,
                        id_derechohabiente,
                        nss)
                VALUES (tmp_deudor_id_cre_acreditado,
                        tmp_deudor_id_derechohabiente,
                        tmp_deudor_nss);
         ELSE
            -- se inicializa el folio de liquidación
            --LET v_ax_folio_liquida = 0;

            -- Se asume que el modulo es "UA". Se obtiene el identificador de uso garantia
            -- para el id_derechohabiente en proceso
            FOREACH
               SELECT FIRST 1 id_cre_uso_garantia, estado, edo_procesar --, folio_liquida
                 INTO v_ax_id_cre_uso_garantia, v_ax_estado_uso, v_ax_edo_proc_uso --, v_ax_folio_liquida
                 FROM safre_viv:cre_uso_garantia
                WHERE estado >= 140
                  AND edo_procesar IN (5, 60, 70, 75, 80, 85)
                  AND id_cre_ctr_archivo IN (
                      SELECT id_cre_ctr_archivo
                        FROM safre_viv:cre_ctr_archivo
                       WHERE operacion NOT IN (1,6,9,14))
                  AND id_derechohabiente = v_ax_id_derechohabiente
                  AND tpo_transferencia = '43'
                ORDER BY folio_liquida DESC
            END FOREACH;

            -- se calculan los valores
            LET v_ax_aporta_viv97 = tmp_ult_aport_viv97 / 100;

            -- se valida el estado procesar
            IF v_ax_edo_proc_uso = 5 THEN
               -- se rechaza el registro
               LET v_ax_estado_uso = 240;
{
               -- se incrementa el numero de registros aceptados y el id transferencia
               LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
            ELSE
               -- se incrementa el numero de registros aceptados y el id transferencia
               LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
}
            END IF

            -- se asignan los valores en las variables que se usaran para insertar el registro
            LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
            LET uso_id_cre_ctr_archivo  = p_ax_id_cre_ctr_arch;
            LET uso_folio_liquida       = p_d_folio;
            LET uso_id_derechohabiente  = v_ax_id_derechohabiente;
            LET uso_tpo_transferencia   = tmp_tpo_transf;
            LET uso_tpo_uso             = 2;
            LET uso_num_credito         = tmp_num_cred_infonavit;
            LET uso_f_presentacion      = tmp_f_presentacion;
            LET uso_f_movimiento        = NULL;
            LET uso_periodo_pago        = tmp_periodo_pago;
            LET uso_importe_v97         = v_ax_aporta_viv97;
            LET uso_nss_afore           = tmp_nss_afore;
            LET uso_rfc_afore           = tmp_rfc_afore;
            LET uso_paterno_afore       = tmp_ape_pat_afore;
            LET uso_materno_afore       = tmp_ape_mat_afore;
            LET uso_nombre_afore        = tmp_nom_trab_afore;
            LET uso_nom_imss            = tmp_nom_trab_imss;
            LET uso_edo_procesar        = 110; -- No atendidas
            LET uso_diagnostico         = 'NA1';
            LET uso_estado              = v_ax_estado_uso;
            LET uso_f_proceso           = TODAY;

            -- se inserta registro
            INSERT INTO safre_viv:cre_uso_garantia (
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
               CONTINUE FOREACH;
            END IF

{ Ya no se insertará registro duplicado con edo_procesar 70, mas bien se actualizará el registro maestro
  13/02/2013 05:38:32 p.m.
            -- se valida el folio de liqudiación
            IF v_ax_folio_liquida IS NULL OR v_ax_folio_liquida = 0 THEN
               LET v_ax_folio_liquida = 0;
            END IF

            -- se inserta registro duplicado con estado_procesar 70 y sin diagnostico
            LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
            LET uso_folio_liquida = v_ax_folio_liquida;
            LET uso_edo_procesar = 70;
            LET uso_diagnostico = NULL;

            -- se inserta registro duplicado con edo procesar 70 y sin diagnostico
            INSERT INTO safre_viv:cre_uso_garantia (
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
}

            -- se actualiza el registro maestro a estado 70 - Por reenviar
            UPDATE safre_viv:cre_uso_garantia
               SET edo_procesar = 70
             WHERE id_cre_uso_garantia = v_ax_id_cre_uso_garantia
               AND id_cre_ctr_archivo IN (
                   SELECT id_cre_ctr_archivo
                     FROM safre_viv:cre_ctr_archivo
                    WHERE operacion NOT IN (1,6,9,14))
               AND id_derechohabiente = v_ax_id_derechohabiente
               AND tpo_transferencia = '43'
               AND estado >= 140
               AND edo_procesar IN (5, 60, 70, 75, 80, 85);

            -- se asignan los valores en las variables que se usaran para insertar el registro en tmp
            LET tmp_deudor_id_cre_acreditado = uso_id_cre_uso_garantia;
            LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
            LET tmp_deudor_nss = tmp_nss_infonavit;

            -- se inserta registro
            INSERT INTO safre_tmp:tmp_deudor_no_aten_agr (
                        id_cre_acreditado,
                        id_derechohabiente,
                        nss)
                VALUES (tmp_deudor_id_cre_acreditado,
                        tmp_deudor_id_derechohabiente,
                        tmp_deudor_nss);
         END IF
      END FOREACH;
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:cre_his_acreditado;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE safre_viv:cre_uso_garantia;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION;


