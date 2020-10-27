






CREATE FUNCTION "safreviv".fn_dse_integra_rchdse_grt(p_d_pid DECIMAL(9,0),
                                          p_v_usuario CHAR(20),
                                          p_v_arch_proceso CHAR(100),
                                          p_d_folio DECIMAL(10))
   RETURNING SMALLINT
   -- REGISTRO tmp dse devolucion
   DEFINE tmp_tpo_registro            CHAR(2);
   DEFINE tmp_cont_servicio           DECIMAL(10,0);
   DEFINE tmp_tpo_entidad_recep       CHAR(2);
   DEFINE tmp_cve_entidad_recep       CHAR(3);
   DEFINE tmp_tpo_entidad_cede        CHAR(2);
   DEFINE tmp_cve_entidad_cede        CHAR(3);
   DEFINE tmp_tpo_transfeencia        CHAR(2);
   DEFINE tmp_f_presentacion          DATE;
   DEFINE tmp_f_movimiento            DATE;
   DEFINE tmp_curp                    CHAR(18);
   DEFINE tmp_nss                     CHAR(11);
   DEFINE tmp_filler1                 CHAR(28);
   DEFINE tmp_ap_paterno_af           CHAR(40);
   DEFINE tmp_ap_materno_af           CHAR(40);
   DEFINE tmp_nombre_af               CHAR(40);
   DEFINE tmp_filler2                 CHAR(22);
   DEFINE tmp_id_lote                 CHAR(16);
   DEFINE tmp_filler3                 CHAR(219);
   DEFINE tmp_num_aplic_interes97     DECIMAL(15,0); -- (9,6)
   DEFINE tmp_saldo_vivienda97        DECIMAL(15,0); -- (13,2)
   DEFINE tmp_filler4                 CHAR(78);
   DEFINE tmp_cod_res_operacion       CHAR(2);
   DEFINE tmp_diagnostico             CHAR(15);
   DEFINE tmp_nombre_imss             CHAR(50);
   DEFINE tmp_num_credito_infonavit   DECIMAL(10,0);
   DEFINE tmp_interes_saldo_viv97     DECIMAL(15,0); -- (13,2)
   DEFINE tmp_filler5                 CHAR(56);
   -- REGISTRO de la tabla his_devolucion
   DEFINE dseh_id_dse_grp_devolucion   DECIMAL(9,0);
   DEFINE dseh_folio                   DECIMAL(9,0);
   DEFINE dseh_tpo_transferencia       CHAR(2);
   DEFINE dseh_lote                    SMALLINT;
   DEFINE dseh_id_lote                 DECIMAL(9,0);
   DEFINE dseh_f_presentacion          DATE;
   DEFINE dseh_paterno_afore           CHAR(40);
   DEFINE dseh_materno_afore           CHAR(40);
   DEFINE dseh_nombre_afore            CHAR(40);
   DEFINE dseh_nom_imss                CHAR(50);
   DEFINE dseh_aivs97                  DECIMAL(22,2);
   DEFINE dseh_pesos97                 DECIMAL(22,2);
   DEFINE dseh_aivs92                  DECIMAL(22,2);
   DEFINE dseh_pesos92                 DECIMAL(22,2);
   DEFINE dseh_monto_aportacion        DECIMAL(22,2);
   DEFINE dseh_aivs_aportacion         DECIMAL(22,2);
   DEFINE dseh_nss_separacion          CHAR(11);
   DEFINE dseh_edo_procesar            SMALLINT;
   DEFINE dseh_diagnostico             CHAR(3);
   DEFINE dseh_estado                  SMALLINT;
   DEFINE dseh_f_proceso               DATE;
   ---- REGISTRO tmp deudor rech devol saldo
   DEFINE tmp_deudor_id_dse_devolucion DECIMAL(9,0);
   DEFINE tmp_deudor_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_deudor_nss CHAR(11);
   DEFINE tmp_deudor_folio_archivo      INTEGER;
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente    DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_diag_proceso          CHAR(3); -- dignostico en proceso
   DEFINE v_ax_id_dse_grp_devolucion DECIMAL(9,0); -- identificador de dse devolucion
   DEFINE v_ax_edo_proc_agrup        SMALLINT; -- estado procesar en agrupación
   DEFINE v_ax_estado                SMALLINT; -- estado a insertar en his
   DEFINE v_ax_tpo_transf_dse        CHAR(2); -- tipo de transferencia
   DEFINE v_ax_tpo_transf_rch_dse    CHAR(2); -- tipo de transferencia
   DEFINE v_ax_lote                  SMALLINT; -- numero de lotes cargados
   DEFINE v_ax_id_lote_acpt          INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech          INTEGER; -- total de registros rechazados
   DEFINE r_ax_bandera               SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_glo_estado             SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_error                 SMALLINT; -- contiene el código de error en caso de ocurrir

   ON EXCEPTION SET v_ax_error
      -- Devolvera el codigo de error que ocasione la excepcion
      RETURN v_ax_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseIntegRchDevsSdos.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_tpo_transf_dse = "19"; -- 19-DSE Créditos en Garantía 43 bis
   LET v_ax_tpo_transf_rch_dse = "96"; -- 96- Rechazos DSE GRT
   LET v_i_glo_estado = 2; -- estado Integrado
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_ax_error = 0;
   LET v_ax_lote = NULL;

   -- se busca el lote a procesar en la tabla de control de archivos del modulo, con estatus "validado"
   SELECT consecutivo_lote
     INTO v_ax_lote
     FROM safre_tmp:tmp_dse_rech_enc_grt;

   -- en caso de no encontrar ningun lote en la tabla se asigna o (VPD: que hacer en este caso)
   IF v_ax_lote IS NULL THEN
     LET v_ax_lote = 0;
   END IF;

   -- se obtienen los datos de la tabla temporal de rechazos de devolucion
   FOREACH
    SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servicio,
           tmp_tpo_entidad_recep,
           tmp_cve_entidad_recep,
           tmp_tpo_entidad_cede,
           tmp_cve_entidad_cede,
           tmp_tpo_transfeencia,
           tmp_f_presentacion,
           tmp_f_movimiento,
           tmp_curp,
           tmp_nss,
           tmp_filler1,
           tmp_ap_paterno_af,
           tmp_ap_materno_af,
           tmp_nombre_af,
           tmp_filler2,
           tmp_id_lote,
           tmp_filler3,
           tmp_num_aplic_interes97,
           tmp_saldo_vivienda97,
           tmp_filler4,
           tmp_cod_res_operacion,
           tmp_diagnostico,
           tmp_nombre_imss,
           tmp_num_credito_infonavit,
           tmp_interes_saldo_viv97,
           tmp_filler5
      FROM safre_tmp:tmp_dse_rech_det_grt
      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_nss;

      -- se obtiene el identificador de dse devolución con para el id_derechohabiente
      FOREACH

         SELECT id_dse_grp_devolucion,edo_procesar
           INTO v_ax_id_dse_grp_devolucion,v_ax_edo_proc_agrup
           FROM safre_tmp:tmp_ctr_devolucion_grt
          WHERE nss = tmp_nss
      
         -- se obtiene el numero de diagnosticos en el registro
         LET v_ax_diag_proceso = tmp_diagnostico[1,3];
         
         -- se valida que en el archivo de procesar no vengan nss "Solo infonavit"
         IF v_ax_edo_proc_agrup = "5" THEN
            -- "Solo infonavit" se rechaza el registro
            LET v_ax_estado = 240;
         
            -- se incrementa el numero de registros rechazados
            LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
         ELSE
            -- Se asigna estado procesar 
            LET v_ax_estado = 20;
         
            -- se incrementa el numero de registros aceptados
            LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
         END IF
         
         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET dseh_id_dse_grp_devolucion = v_ax_id_dse_grp_devolucion;
         LET dseh_folio                 = p_d_folio;
         LET dseh_tpo_transferencia     = v_ax_tpo_transf_rch_dse;
         LET dseh_lote                  = v_ax_lote;
         LET dseh_id_lote               = v_ax_id_lote_acpt;
         LET dseh_f_presentacion        = tmp_f_presentacion;
         LET dseh_paterno_afore         = tmp_ap_paterno_af;
         LET dseh_materno_afore         = tmp_ap_materno_af;
         LET dseh_nombre_afore          = tmp_nombre_af;
         LET dseh_nom_imss              = tmp_nombre_imss;
         LET dseh_aivs97                = tmp_num_aplic_interes97/1000000;
         LET dseh_pesos97               = tmp_saldo_vivienda97/100;
         LET dseh_aivs92                = 0;
         LET dseh_pesos92               = 0;
         LET dseh_monto_aportacion      = 0;
         LET dseh_aivs_aportacion       = 0;
         LET dseh_nss_separacion        = "";
         LET dseh_edo_procesar          = 90;
         LET dseh_diagnostico           = v_ax_diag_proceso;
         LET dseh_estado                = v_ax_estado;
         LET dseh_f_proceso             = TODAY;
         
         -- se inserta el registro en la tabla dse his devolucion
         INSERT INTO dse_his_devolucion (
                     id_dse_grp_devolucion,
                     folio,
                     tpo_transferencia,
                     lote,
                     id_lote,
                     f_presentacion,
                     paterno_afore,
                     materno_afore,
                     nombre_afore,
                     nom_imss,
                     aivs97,
                     pesos97,
                     aivs92,
                     pesos92,
                     monto_aportacion,
                     aivs_aportacion,
                     nss_separacion,
                     edo_procesar,
                     diagnostico,
                     estado,
                     f_proceso)
             VALUES (dseh_id_dse_grp_devolucion,
                     dseh_folio,
                     dseh_tpo_transferencia,
                     dseh_lote,
                     dseh_id_lote,
                     dseh_f_presentacion,
                     dseh_paterno_afore,
                     dseh_materno_afore,
                     dseh_nombre_afore,
                     dseh_nom_imss,
                     dseh_aivs97,
                     dseh_pesos97,
                     dseh_aivs92,
                     dseh_pesos92,
                     dseh_monto_aportacion,
                     dseh_aivs_aportacion,
                     dseh_nss_separacion,
                     dseh_edo_procesar,
                     dseh_diagnostico,
                     dseh_estado,
                     dseh_f_proceso);
         
         -- si el registro fue rechazado continua con el siguiente registro
         IF v_ax_estado = 240 THEN
            CONTINUE FOREACH;
         END IF
         
         -- se ejecuta el sp que actualiza el registro correspondiente de la tabla dse agrupa devolución
         -- a estado 90 - Saldo Rechazado
         EXECUTE PROCEDURE sp_act_dse_agrupa_devolucion(dseh_id_dse_grp_devolucion, dseh_edo_procesar);
         
         -- se asignan los valores faltantes
         LET dseh_edo_procesar = 70;
         LET dseh_diagnostico = NULL;
         
         -- se inserta el registro en la tabla dse his devolucion
         INSERT INTO dse_his_devolucion (
                     id_dse_grp_devolucion,
                     folio,
                     tpo_transferencia,
                     lote,
                     id_lote,
                     f_presentacion,
                     paterno_afore,
                     materno_afore,
                     nombre_afore,
                     nom_imss,
                     aivs97,
                     pesos97,
                     aivs92,
                     pesos92,
                     monto_aportacion,
                     aivs_aportacion,
                     nss_separacion,
                     edo_procesar,
                     diagnostico,
                     estado,
                     f_proceso)
             VALUES (dseh_id_dse_grp_devolucion,
                     dseh_folio,
                     dseh_tpo_transferencia,
                     dseh_lote,
                     dseh_id_lote,
                     dseh_f_presentacion,
                     dseh_paterno_afore,
                     dseh_materno_afore,
                     dseh_nombre_afore,
                     dseh_nom_imss,
                     dseh_aivs97,
                     dseh_pesos97,
                     dseh_aivs92,
                     dseh_pesos92,
                     dseh_monto_aportacion,
                     dseh_aivs_aportacion,
                     dseh_nss_separacion,
                     dseh_edo_procesar,
                     dseh_diagnostico,
                     dseh_estado,
                     dseh_f_proceso);
         
         -- se ejecuta el sp que actualiza el registro correspondiente de la tabla dse agrupa devolución
         -- a estado 70 - Por Reenviar
         EXECUTE PROCEDURE sp_act_dse_agrupa_devolucion(dseh_id_dse_grp_devolucion, dseh_edo_procesar);
         
         -- se asignan los valores en las variables que se usaran para insertar el registro en tmp
         LET tmp_deudor_id_dse_devolucion  = v_ax_id_dse_grp_devolucion;
         LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
         LET tmp_deudor_nss                = tmp_nss;
         LET tmp_deudor_folio_archivo      = p_d_folio;
         
         -- se inserta registro
         INSERT INTO safre_tmp:tmp_deudor_rech_devol_saldo_grt (
                     id_dse_devolucion,
                     id_derechohabiente,
                     nss,
                     folio_archivo)
             VALUES (tmp_deudor_id_dse_devolucion,
                     tmp_deudor_id_derechohabiente,
                     tmp_deudor_nss,
                     tmp_deudor_folio_archivo);
         
         -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro mas
         IF tmp_diagnostico[4,6] IS NOT NULL AND tmp_diagnostico[4,6] <> "   " AND
            tmp_diagnostico[4,6] <> "000" THEN
            -- se obtiene el numero de diagnosticos en el registro
            LET v_ax_diag_proceso = tmp_diagnostico[4,6];
         
            -- se asignan los valores faltantes
            LET dseh_edo_procesar = 90;
            LET dseh_diagnostico = v_ax_diag_proceso;
         
            -- se inserta el registro en la tabla dse his devolucion
            INSERT INTO dse_his_devolucion (
                        id_dse_grp_devolucion,
                        folio,
                        tpo_transferencia,
                        lote,
                        id_lote,
                        f_presentacion,
                        paterno_afore,
                        materno_afore,
                        nombre_afore,
                        nom_imss,
                        aivs97,
                        pesos97,
                        aivs92,
                        pesos92,
                        monto_aportacion,
                        aivs_aportacion,
                        nss_separacion,
                        edo_procesar,
                        diagnostico,
                        estado,
                        f_proceso)
                VALUES (dseh_id_dse_grp_devolucion,
                        dseh_folio,
                        dseh_tpo_transferencia,
                        dseh_lote,
                        dseh_id_lote,
                        dseh_f_presentacion,
                        dseh_paterno_afore,
                        dseh_materno_afore,
                        dseh_nombre_afore,
                        dseh_nom_imss,
                        dseh_aivs97,
                        dseh_pesos97,
                        dseh_aivs92,
                        dseh_pesos92,
                        dseh_monto_aportacion,
                        dseh_aivs_aportacion,
                        dseh_nss_separacion,
                        dseh_edo_procesar,
                        dseh_diagnostico,
                        dseh_estado,
                        dseh_f_proceso);
         ELSE
            -- si el diagnostico fue nulo continua con el siguiente registro
            CONTINUE FOREACH;
         END IF
         
         -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro mas
         IF tmp_diagnostico[7,9] IS NOT NULL AND tmp_diagnostico[7,9] <> "   " AND
            tmp_diagnostico[7,9] <> "000" THEN
            -- se obtiene el numero de diagnosticos en el registro
            LET v_ax_diag_proceso = tmp_diagnostico[7,9];
         
            -- se asignan los valores faltantes
            LET dseh_edo_procesar = 90;
            LET dseh_diagnostico = v_ax_diag_proceso;
         
            -- se inserta el registro en la tabla dse his devolucion
            INSERT INTO dse_his_devolucion (
                        id_dse_grp_devolucion,
                        folio,
                        tpo_transferencia,
                        lote,
                        id_lote,
                        f_presentacion,
                        paterno_afore,
                        materno_afore,
                        nombre_afore,
                        nom_imss,
                        aivs97,
                        pesos97,
                        aivs92,
                        pesos92,
                        monto_aportacion,
                        aivs_aportacion,
                        nss_separacion,
                        edo_procesar,
                        diagnostico,
                        estado,
                        f_proceso)
                VALUES (dseh_id_dse_grp_devolucion,
                        dseh_folio,
                        dseh_tpo_transferencia,
                        dseh_lote,
                        dseh_id_lote,
                        dseh_f_presentacion,
                        dseh_paterno_afore,
                        dseh_materno_afore,
                        dseh_nombre_afore,
                        dseh_nom_imss,
                        dseh_aivs97,
                        dseh_pesos97,
                        dseh_aivs92,
                        dseh_pesos92,
                        dseh_monto_aportacion,
                        dseh_aivs_aportacion,
                        dseh_nss_separacion,
                        dseh_edo_procesar,
                        dseh_diagnostico,
                        dseh_estado,
                        dseh_f_proceso);
         ELSE
            -- si el diagnostico fue nulo continua con el siguiente registro
            CONTINUE FOREACH;
         END IF
         
         -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro mas
         IF tmp_diagnostico[10,12] IS NOT NULL AND tmp_diagnostico[10,12] <> "   " AND
            tmp_diagnostico[10,12] <> "000" THEN
            -- se obtiene el numero de diagnosticos en el registro
            LET v_ax_diag_proceso = tmp_diagnostico[10,12];
         
            -- se asignan los valores faltantes
            LET dseh_edo_procesar = 90;
            LET dseh_diagnostico = v_ax_diag_proceso;
         
            -- se inserta el registro en la tabla dse his devolucion
            INSERT INTO dse_his_devolucion (
                        id_dse_grp_devolucion,
                        folio,
                        tpo_transferencia,
                        lote,
                        id_lote,
                        f_presentacion,
                        paterno_afore,
                        materno_afore,
                        nombre_afore,
                        nom_imss,
                        aivs97,
                        pesos97,
                        aivs92,
                        pesos92,
                        monto_aportacion,
                        aivs_aportacion,
                        nss_separacion,
                        edo_procesar,
                        diagnostico,
                        estado,
                        f_proceso)
                VALUES (dseh_id_dse_grp_devolucion,
                        dseh_folio,
                        dseh_tpo_transferencia,
                        dseh_lote,
                        dseh_id_lote,
                        dseh_f_presentacion,
                        dseh_paterno_afore,
                        dseh_materno_afore,
                        dseh_nombre_afore,
                        dseh_nom_imss,
                        dseh_aivs97,
                        dseh_pesos97,
                        dseh_aivs92,
                        dseh_pesos92,
                        dseh_monto_aportacion,
                        dseh_aivs_aportacion,
                        dseh_nss_separacion,
                        dseh_edo_procesar,
                        dseh_diagnostico,
                        dseh_estado,
                        dseh_f_proceso);
         ELSE
            -- si el diagnostico fue nulo continua con el siguiente registro
            CONTINUE FOREACH;
         END IF
         
         -- en caso de que el diagnostico sea diferente de nulo se inserta un regitro mas
         IF tmp_diagnostico[13,15] IS NOT NULL AND tmp_diagnostico[13,15] <> "   " AND
            tmp_diagnostico[13,15] <> "000" THEN
            -- se obtiene el numero de diagnosticos en el registro
            LET v_ax_diag_proceso = tmp_diagnostico[13,15];
         
            -- se asignan los valores faltantes
            LET dseh_edo_procesar = 90;
            LET dseh_diagnostico = v_ax_diag_proceso;
         
            -- se inserta el registro en la tabla dse his devolucion
            INSERT INTO dse_his_devolucion (
                        id_dse_grp_devolucion,
                        folio,
                        tpo_transferencia,
                        lote,
                        id_lote,
                        f_presentacion,
                        paterno_afore,
                        materno_afore,
                        nombre_afore,
                        nom_imss,
                        aivs97,
                        pesos97,
                        aivs92,
                        pesos92,
                        monto_aportacion,
                        aivs_aportacion,
                        nss_separacion,
                        edo_procesar,
                        diagnostico,
                        estado,
                        f_proceso)
                VALUES (dseh_id_dse_grp_devolucion,
                        dseh_folio,
                        dseh_tpo_transferencia,
                        dseh_lote,
                        dseh_id_lote,
                        dseh_f_presentacion,
                        dseh_paterno_afore,
                        dseh_materno_afore,
                        dseh_nombre_afore,
                        dseh_nom_imss,
                        dseh_aivs97,
                        dseh_pesos97,
                        dseh_aivs92,
                        dseh_pesos92,
                        dseh_monto_aportacion,
                        dseh_aivs_aportacion,
                        dseh_nss_separacion,
                        dseh_edo_procesar,
                        dseh_diagnostico,
                        dseh_estado,
                        dseh_f_proceso);
         ELSE
            -- si el diagnostico fue nulo continua con el siguiente registro
            CONTINUE FOREACH;
         END IF
      END FOREACH;
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE dse_his_devolucion;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE fn_act_dse_ctr_archivo(p_d_folio, p_d_pid, v_ax_tpo_transf_rch_dse, v_ax_id_lote_acpt, v_ax_id_lote_rech);

   RETURN v_ax_error;
END FUNCTION

;


