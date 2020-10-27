






CREATE FUNCTION "safreviv".fn_grt_integra_sdos_tranf(p_v_usuario          CHAR(20),
                                          p_v_arch_proceso     CHAR(100),
                                          p_d_folio            DECIMAL(10),
                                          p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                          p_si_proceso_cod     SMALLINT)
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)

   --REGISTRO tmp sdo transf enc grt
   DEFINE v_re_tpo_registro         CHAR(2);
   DEFINE v_re_id_servicio          CHAR(2);
   DEFINE v_re_id_operacion         CHAR(2);
   DEFINE v_re_tpo_entidad_origen   CHAR(2);
   DEFINE v_re_cve_entidad_origen   CHAR(3);
   DEFINE v_re_tpo_entidad_destin   CHAR(2);
   DEFINE v_re_cve_entidad_destino  CHAR(3);
   DEFINE v_re_ent_federativa       CHAR(3);
   DEFINE v_re_f_presentacion       DATE;
   DEFINE v_re_con_lote_dia         DECIMAL(3,0);
   DEFINE v_re_filler               CHAR(2);
   DEFINE v_re_cod_res_operacion    CHAR(2);
   DEFINE v_re_mot_rechazo_lote     CHAR(9);

   -- REGISTRO tmp sdo transf det grt
   DEFINE tmp_tpo_registro          CHAR(2);
   DEFINE tmp_cont_servicio         DECIMAL(10,0);
   DEFINE tmp_tpo_ent_recep         CHAR(2);
   DEFINE tmp_cve_ent_recep         CHAR(3);
   DEFINE tmp_tpo_ent_cede          CHAR(2);
   DEFINE tmp_cve_ent_cede          CHAR(3);
   DEFINE tmp_tpo_transferencia     CHAR(2);
   DEFINE tmp_f_presentacion        DATE;
   DEFINE tmp_filler                CHAR(8);
   DEFINE tmp_curp                  CHAR(18);
   DEFINE tmp_nss                   CHAR(11);
   DEFINE tmp_filler1               CHAR(15);
   DEFINE tmp_rfc                   CHAR(13);
   DEFINE tmp_ap_paterno            CHAR(40);
   DEFINE tmp_ap_materno            CHAR(40);
   DEFINE tmp_nombre                CHAR(40);
   DEFINE tmp_filler2               CHAR(22);
   DEFINE tmp_id_lote               CHAR(16);
   DEFINE tmp_filler3               CHAR(15);
   DEFINE tmp_nss_afore             CHAR(11);
   DEFINE tmp_rfc_afore             CHAR(13);
   DEFINE tmp_filler4               CHAR(30);
   DEFINE tmp_ap_paterno_afore      CHAR(40);
   DEFINE tmp_ap_materno_afore      CHAR(40);
   DEFINE tmp_nombre_afore          CHAR(40);
   DEFINE tmp_filler5               CHAR(30);
   DEFINE tmp_part_viv97            DECIMAL(14,0);
   DEFINE tmp_saldo_viv97           DECIMAL(15,0);
   DEFINE tmp_filler6               CHAR(78);
   DEFINE tmp_cod_operacion         CHAR(2);
   DEFINE tmp_diagnostico           DECIMAL(3,0);
   DEFINE tmp_nombre_imss           CHAR(50);  
   DEFINE tmp_num_credito           CHAR(10);
   DEFINE tmp_filler7               CHAR(54);
   DEFINE tmp_per_pago              DECIMAL(6,0);
   DEFINE tmp_filler8               CHAR(12);

   --REGISTRO rch acreditado
   DEFINE rch_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE rch_nss                   CHAR(11);
   DEFINE rch_tpo_originacion       SMALLINT;
   DEFINE rch_tpo_registro          CHAR(2);
   DEFINE rch_num_credito           DECIMAL(10,0);
   DEFINE rch_sdo_deudor            DECIMAL(12,2);
   DEFINE rch_valor_dscto           DECIMAL(8,4);
   DEFINE rch_estado                SMALLINT;

   --REGISTRO his acreditado
   DEFINE his_id_cre_acreditado     DECIMAL(9,0);
   DEFINE his_id_cre_ctr_archivo    DECIMAL(9,0);
   DEFINE his_tpo_transferencia     CHAR(2);
   DEFINE his_edo_procesar          SMALLINT;
   DEFINE his_diagnostico           CHAR(3);
   DEFINE his_estado                SMALLINT;
   DEFINE his_nss_afore             CHAR(11);
   DEFINE his_rfc_afore             CHAR(13);
   DEFINE his_paterno_afore         CHAR(40);
   DEFINE his_materno_afore         CHAR(40);
   DEFINE his_nombre_afore          CHAR(40);
   DEFINE his_nom_imss              CHAR(50);
   DEFINE his_f_proceso             DATE;

   -- REGISTRO cre acreditado
   DEFINE cre_id_cre_acreditado     DECIMAL(9,0);
   DEFINE cre_estado                SMALLINT; -- estado
   DEFINE cre_edo_procesar          SMALLINT; -- estado procesar
   DEFINE cre_tpo_credito           SMALLINT; -- tipo de crédito
   DEFINE cre_num_credito           DECIMAL(10,0); -- número de crédito
   DEFINE cre_f_culmina             DATE; -- fecha de culminación

   -- registro de cta tipo crédito
   DEFINE cta_marca_inf             SMALLINT; -- marca infonavit
   DEFINE cta_marca_prc             SMALLINT; -- marca procesar

   -- REGISTRO de cta marca ws
   DEFINE ws_id_derechohabiente     DECIMAL(9,0);
   DEFINE ws_id_origen              DECIMAL(9,0);
   DEFINE ws_modulo_cod             CHAR(3);
   DEFINE ws_tpo_credito            SMALLINT;
   DEFINE ws_marca                  SMALLINT;
   DEFINE ws_f_solicita             DATE;
   DEFINE ws_intento                SMALLINT;
   DEFINE ws_cod_result_op          SMALLINT;
   DEFINE ws_diagnostico            SMALLINT;
   DEFINE ws_situacion              SMALLINT;
   DEFINE ws_num_credito            DECIMAL(10,0);
   DEFINE ws_f_infonavit            DATE;
   DEFINE ws_marca_procesar         CHAR(2);
   DEFINE ws_folio_archivo          DECIMAL(9,0);
   DEFINE ws_usuario                CHAR(20);

   -- parametros de la función de desmarca
   DEFINE des_id_derechohabiente    DECIMAL(9,0);
   DEFINE des_marca_entra           SMALLINT;
   DEFINE des_n_referencia          INTEGER;
   DEFINE des_estado_marca          SMALLINT;
   DEFINE des_marca_causa           SMALLINT;
   DEFINE des_usuario               CHAR(20);

   -- CAMPOS auxiliares
   DEFINE v_ax_id_derechohabiente   DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_id_lote_acpt         INTEGER; -- identificador del lote de acr transferencia
   DEFINE v_ax_id_lote_rech         INTEGER; 
   DEFINE v_ax_proceso_cod          SMALLINT; -- código del proceso
   DEFINE v_i_glo_estado            SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_tpo_originacion      SMALLINT; -- tipo de originación
   DEFINE v_error                   SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);
   DEFINE v_c_nss                   CHAR(11);
   DEFINE v_b_existe_reg            SMALLINT; -- booleana que indica si existe o no la originación de crédito
   DEFINE r_ax_existe_marca_prc     SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera              SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_sts_marcaje          SMALLINT; -- estatus de retorno de función de marcaje

   -- variables para desmarcar marcas anteriores
   DEFINE v_dm_id_derechohabiente   DECIMAL(9,0);
   DEFINE v_dm_marca                SMALLINT;
   DEFINE v_dm_n_referencia         DECIMAL(9,0);
   DEFINE v_dm_folio                DECIMAL(10,0);
   
   --Variable cuando el registro es aceptado
   DEFINE v_ind_notificado          SMALLINT;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/grtIntegSdosTranf.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_rech    = 0;
   LET v_ax_id_lote_acpt    = 0;
   LET v_i_glo_estado       = 2; -- estado Integrado
   LET v_ax_tpo_originacion = 2; -- 2-Solicitud de Saldo en Garantía (Créditos en G. 43 bis)
   LET v_ax_proceso_cod     = 1205; -- Saldos Transferidos 43BIS
   LET v_error              = 0;
   LET v_isam_err           = 0;
   LET v_c_msj              = 'El proceso finalizó correctamente';
   LET v_c_nss              = "0"; -- valor del NSS antes de entrar al ciclo

   LET v_dm_id_derechohabiente = 0;
   LET v_dm_marca              = 0;
   LET v_dm_n_referencia       = 0;
   LET v_dm_folio              = 0;
	 LET v_ind_notificado = 0;
   -- se obtiene la marca de procesar para el tipo de originación en proceso
   SELECT FIRST 1 marca_inf, marca_prc
     INTO cta_marca_inf, cta_marca_prc
     FROM cat_tipo_credito
    WHERE tpo_originacion = v_ax_tpo_originacion;

   -- se obtienen los datos de tmp sdo transf det grt para el archivo en proceso
   FOREACH
    SELECT tmp.*, afi.id_derechohabiente
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
           tmp_part_viv97,
           tmp_saldo_viv97,
           tmp_filler6,  
           tmp_cod_operacion,
           tmp_diagnostico,
           tmp_nombre_imss,
           tmp_num_credito,
           tmp_filler7,
           tmp_per_pago,
           tmp_filler8,
           v_ax_id_derechohabiente
      FROM safre_tmp:tmp_sdo_transf_det_grt tmp, afi_derechohabiente afi
      WHERE tmp.nss = afi.nss

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_nss;

      -- se asume que no existirá la originación de crédito
      LET v_b_existe_reg = 0;

      -- se obtiene la información de la tabla maestro
      FOREACH
	       SELECT FIRST 1 c.id_cre_acreditado, c.estado, c.edo_procesar, c.tpo_credito, c.num_credito, c.f_culmina
	         INTO cre_id_cre_acreditado, cre_estado, cre_edo_procesar, cre_tpo_credito, cre_num_credito, cre_f_culmina
	         FROM cre_acreditado c, cat_maq_credito m
	        WHERE c.estado           = m.estado -->= 20 --IN (20,900,910,920)
	          AND id_derechohabiente = v_ax_id_derechohabiente
	          AND tpo_originacion    = v_ax_tpo_originacion
	          AND m.entidad          = 1
	        ORDER BY f_otorga DESC, estado

         -- se indica que existe la originación de crédito
         LET v_b_existe_reg = 1;
         LET cre_f_culmina  = TODAY;
      END FOREACH;

      -- se verifica si no existió la originación de crédito
      IF v_b_existe_reg = 0 THEN
         -- No existió la originación de crédito. Se rechaza el registro
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = 0;
         LET rch_sdo_deudor         = tmp_saldo_viv97;
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

      -- se valida si el registro corresponde a Solo INFONAVIT
      IF cre_edo_procesar = 5 THEN
         -- se rechaza el registro
         LET cre_estado = 240;

         -- se asignan los valores en las variables que se usaran para insertar el registro historico
         LET his_id_cre_acreditado  = cre_id_cre_acreditado;
         LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET his_tpo_transferencia  = tmp_tpo_transferencia;
         LET his_edo_procesar       = 120; -- saldos transferidos
         LET his_diagnostico        = 0;
         LET his_estado             = cre_estado;
         LET his_nss_afore          = tmp_nss;
         LET his_rfc_afore          = tmp_rfc_afore;
         LET his_paterno_afore      = tmp_ap_paterno_afore;
         LET his_materno_afore      = tmp_ap_materno_afore;
         LET his_nombre_afore       = tmp_nombre_afore;
         LET his_nom_imss           = tmp_nombre_imss;
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
         LET rch_nss                = tmp_nss;
         LET rch_tpo_originacion    = v_ax_tpo_originacion;
         LET rch_tpo_registro       = tmp_tpo_registro;
         LET rch_num_credito        = 0;
         LET rch_sdo_deudor         = tmp_saldo_viv97;
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

      FOREACH
         SELECT s.id_derechohabiente, s.marca, s.n_referencia, s.folio
           INTO v_dm_id_derechohabiente, v_dm_marca, v_dm_n_referencia, v_dm_folio
           FROM sfr_marca_activa s
          WHERE s.id_derechohabiente = v_ax_id_derechohabiente
            AND s.marca IN(231,234)

         IF v_dm_id_derechohabiente <> 0 AND v_dm_id_derechohabiente <> "" THEN
            LET des_estado_marca       = 0;
            LET des_marca_causa        = 0;
            LET des_usuario            = p_v_usuario;

            -- se invoca la función de desmarca
            EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                                v_dm_marca,
                                                v_dm_n_referencia,
                                                des_estado_marca,
                                                des_marca_causa,
                                                des_usuario,
                                                v_ax_proceso_cod)
                                           INTO r_ax_sts_marcaje;
         END IF
      END FOREACH;

      -- se invoca la función que verifica si ya existe la marca de procesar
      EXECUTE FUNCTION fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                               cta_marca_prc)
                                          INTO r_ax_existe_marca_prc;

      IF r_ax_existe_marca_prc = 1 THEN
         FOREACH
            SELECT s.id_derechohabiente, s.marca, s.n_referencia, s.folio
              INTO v_dm_id_derechohabiente, v_dm_marca, v_dm_n_referencia, v_dm_folio
              FROM sfr_marca_activa s
             WHERE s.id_derechohabiente = v_ax_id_derechohabiente
               AND s.marca              = cta_marca_prc
               AND s.n_referencia       <> cre_id_cre_acreditado

            IF v_dm_id_derechohabiente <> 0 AND v_dm_id_derechohabiente <> "" THEN
               LET des_estado_marca       = 0;
               LET des_marca_causa        = 0;
               LET des_usuario            = p_v_usuario;
         
               -- se invoca la función de desmarca
               EXECUTE FUNCTION fn_desmarca_cuenta(v_dm_id_derechohabiente,
                                                   v_dm_marca,
                                                   v_dm_n_referencia,
                                                   des_estado_marca,
                                                   des_marca_causa,
                                                   des_usuario,
                                                   v_ax_proceso_cod)
                                              INTO r_ax_sts_marcaje;
            END IF
         END FOREACH;
      END IF

      EXECUTE FUNCTION fn_cre_existe_marca_prc(v_ax_id_derechohabiente,
                                               cta_marca_prc)
                                          INTO r_ax_existe_marca_prc;

      -- en caso de no existir la marca se ejecuta
      IF r_ax_existe_marca_prc = 0 THEN
         -- se ejecuta la función de marcaje
         EXECUTE FUNCTION fn_marca_cuenta(v_ax_id_derechohabiente,
                                          cta_marca_prc,
                                          cre_id_cre_acreditado,
                                          p_d_folio,
                                          0, -- estado marca
                                          0, -- código de rechazo
                                          NULL, -- marca causa
                                          "", -- fecha causa
                                          p_v_usuario,
                                          p_si_proceso_cod)
                                     INTO r_ax_sts_marcaje;

         -- si el marcaje fue procedente se inserta registro en la tabla historica
         IF r_ax_sts_marcaje = 0 THEN
            -- se asignan los valores en las variables que se usaran para insertar el registro historico
            LET his_id_cre_acreditado  = cre_id_cre_acreditado;
            LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
            LET his_tpo_transferencia  = tmp_tpo_transferencia;
            LET his_edo_procesar       = 60; -- marcada por procesar
            LET his_diagnostico        = 0;
            LET his_estado             = cre_estado;
            LET his_nss_afore          = tmp_nss;
            LET his_rfc_afore          = tmp_rfc_afore;
            LET his_paterno_afore      = tmp_ap_paterno_afore;
            LET his_materno_afore      = tmp_ap_materno_afore;
            LET his_nombre_afore       = tmp_nombre_afore;
            LET his_nom_imss           = tmp_nombre_imss;
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
         END IF
      END IF

      -- se incrementa el numero de registros aceptados
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se asignan los valores en las variables que se usaran para insertar el registro historico
      LET his_id_cre_acreditado  = cre_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = tmp_tpo_transferencia;
      LET his_edo_procesar       = 120; -- saldos transferidos
      LET his_diagnostico        = 0;
      LET his_estado             = cre_estado;
      LET his_nss_afore          = tmp_nss;
      LET his_rfc_afore          = tmp_rfc_afore;
      LET his_paterno_afore      = tmp_ap_paterno_afore;
      LET his_materno_afore      = tmp_ap_materno_afore;
      LET his_nombre_afore       = tmp_nombre_afore;
      LET his_nom_imss           = tmp_nombre_imss;
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
      IF cre_estado = 280 THEN
         -- se asigna el estado a actualiza en la tabla maestro
         LET his_edo_procesar = 170;

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
                                        INTO r_ax_sts_marcaje;

         -- verifica si el tipo transferencia es de Transferencia de Acreditados ("03")
         IF tmp_tpo_transferencia = "03" THEN
            -- corresponde a Transferencia de Acreditados
            LET ws_marca_procesar = "01"; -- 'acr' => 01 (Crédito Tradicional)
         ELIF tmp_tpo_transferencia = "16" THEN
            -- corresponde a Anualidades Garantizadas
            LET ws_marca_procesar = "02"; -- 'grt' => 02 (Créditos en Garantía)
         ELSE
            -- corresponde a Anualidades Garantizadas
            LET ws_marca_procesar = "04"; -- 'agr' => 04 (Anualidades Garantizadas)
         END IF

         -- se valida si no existe el id derechohabiente en proceso en la tabla del WS
         IF EXISTS (
            SELECT id_derechohabiente
              FROM cta_marca_ws
             WHERE id_derechohabiente = v_ax_id_derechohabiente) THEN

             -- Ya existe el derechohabiente en la tabla de WS. Se elimina
             DELETE
               FROM cta_marca_ws
              WHERE id_derechohabiente = v_ax_id_derechohabiente;
         END IF

         -- se asignan los valores del registro a insertar en la tabla de WebService
         LET ws_id_derechohabiente = v_ax_id_derechohabiente;
         LET ws_id_origen          = cre_id_cre_acreditado;
         LET ws_modulo_cod         = tmp_tpo_transferencia;
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
      END IF

      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla acr transferencia a estado procesar 120-Saldo transferridos
      EXECUTE PROCEDURE sp_act_cre_transf(cre_id_cre_acreditado, his_edo_procesar);
      
      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla acr transferencia a estado procesar 120-Saldo transferidos
      EXECUTE PROCEDURE sp_act_cre_transf(cre_id_cre_acreditado, his_edo_procesar);

      IF his_edo_procesar = 120 THEN
          INSERT INTO cre_notifica_op16 (
                       id_cre_op16,
                       folio,
                       id_derechohabiente,
                       id_referencia,
                       ind_notificado)
               VALUES (seq_cre_notifica_op16.NEXTVAL,
                       p_d_folio,
                       v_ax_id_derechohabiente,
                       p_ax_id_cre_ctr_arch,
                       v_ind_notificado);
      END IF 
      
   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = "1";

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;
   
   --actualiza estadisticas a la tabla cuando el registro es aceptado
   UPDATE STATISTICS FOR TABLE cre_notifica_op16;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;

END FUNCTION;


