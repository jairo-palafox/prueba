






CREATE FUNCTION "safreviv".fn_acr_integra_devol_solic(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_folio DECIMAL(9,0),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(11)
   -- REGISTRO temporal detalle de devoluciones
   -- tmp_acr_devol
   DEFINE tmp_acr_devol_tpo_registro        char(2)    ;
   DEFINE tmp_acr_devol_cont_servicio       decimal(10,0);
   DEFINE tmp_acr_devol_tpo_entidad_recep   char(2)    ;
   DEFINE tmp_acr_devol_cve_entidad_recep   char(3)    ;
   DEFINE tmp_acr_devol_tpo_entidad_cedente char(2)    ;
   DEFINE tmp_acr_devol_cve_entidad_cedente char(3)    ;
   DEFINE tmp_acr_devol_orig_tpo_transf     char(2)    ;
   DEFINE tmp_acr_devol_f_presentacion      char(8)    ;
   DEFINE tmp_acr_devol_filler1             char(8)    ;
   DEFINE tmp_acr_devol_curp                char(18)   ;
   DEFINE tmp_acr_devol_nss                 char(11)   ;
   DEFINE tmp_acr_devol_filler2             char(15)   ;
   DEFINE tmp_acr_devol_rfc                 char(13)   ;
   DEFINE tmp_acr_devol_ap_paterno          char(40)   ;
   DEFINE tmp_acr_devol_ap_materno          char(40)   ;
   DEFINE tmp_acr_devol_nombre              char(40)   ;
   DEFINE tmp_acr_devol_filler3             char(22)   ;
   DEFINE tmp_acr_devol_id_lote_solicitud   char(16)   ; -----> VPD: es este el lote o se calcula?????
   DEFINE tmp_acr_devol_filler4             char(15)   ;
   DEFINE tmp_acr_devol_nss_afore           char(11)   ;
   DEFINE tmp_acr_devol_rfc_afore           char(13)   ;
   DEFINE tmp_acr_devol_filler5             char(30)   ;
   DEFINE tmp_acr_devol_ap_pat_afore_ced    char(40)   ;
   DEFINE tmp_acr_devol_ap_mat_afore_ced    char(40)   ;
   DEFINE tmp_acr_devol_nombre_afo_ced      char(40)   ;
   DEFINE tmp_acr_devol_filler6             char(30)   ;
   DEFINE tmp_acr_devol_num_aiv98_ult_aport decimal(9,0) ;
   DEFINE tmp_acr_devol_ult_aport_viv97     decimal(13,0);
   DEFINE tmp_acr_devol_filler7             char(78)   ;
   DEFINE tmp_acr_devol_result_operacion    char(2)    ;
   DEFINE tmp_acr_devol_diag_proceso        char(15)   ;
   DEFINE tmp_acr_devol_nombre_imss         char(50)   ;
   DEFINE tmp_acr_devol_num_credito         decimal(10,0);
   DEFINE tmp_acr_devol_filler8             char(43)   ;
   DEFINE tmp_acr_devol_mot_devolucion      char(2)    ;
   DEFINE tmp_acr_devol_filler9             char(8)    ;
   DEFINE tmp_acr_devol_periodo_pago        char(6)    ;
   DEFINE tmp_acr_devol_filler10            char(12)   ;
   --REGISTRO rch acreditado
   DEFINE rch_id_cre_ctr_archivo          DECIMAL(9,0);
   DEFINE rch_nss                         CHAR(11);
   DEFINE rch_tpo_originacion             SMALLINT;
   DEFINE rch_tpo_registro                CHAR(2);
   DEFINE rch_num_credito                 DECIMAL(10,0);
   DEFINE rch_sdo_deudor                  DECIMAL(12,2);
   DEFINE rch_valor_dscto                 DECIMAL(8,4);
   DEFINE rch_estado                      SMALLINT;
   -- Registro de his acreditado
   DEFINE his_id_cre_acreditado    decimal(9,0);
   DEFINE his_id_cre_ctr_archivo   decimal(9,0);
   DEFINE his_tpo_transferencia    char(2)     ;
   DEFINE his_edo_procesar         smallint    ;
   DEFINE his_diagnostico          CHAR(3)    ;
   DEFINE his_estado               smallint    ;
   DEFINE his_nss_afore            char(11)    ;
   DEFINE his_rfc_afore            char(13)    ;
   DEFINE his_paterno_afore        char(40)    ;
   DEFINE his_materno_afore        char(40)    ;
   DEFINE his_nombre_afore         char(40)    ;
   DEFINE his_nom_imss             char(50)    ;
   DEFINE his_f_proceso            date        ;
   -- REGISTRO tmp deudor rechazo
   DEFINE tmp_deudor_id_cre_acreditado DECIMAL(9,0);
   DEFINE tmp_deudor_id_derechohabiente DECIMAL(9,0);
   DEFINE tmp_deudor_nss CHAR(11);
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente  DECIMAL(9,0) ; -- identificador del derechohabiente
   DEFINE v_ax_id_cre_acreditado   DECIMAL(9,0) ; -- identificador de acr transferencia
   DEFINE v_ax_estado              SMALLINT; -- estado
   DEFINE v_ax_edo_procesar        SMALLINT; -- estado procesar
   DEFINE v_ax_id_lote_acpt        INTEGER      ; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech        INTEGER      ; -- total de registros rechazados
   DEFINE v_ax_operacion           SMALLINT; -- operación del proceso
   DEFINE v_ax_marca_prc           SMALLINT; -- marca procesar
   DEFINE v_ax_tpo_credito         SMALLINT; -- tipo de crédito
   DEFINE v_ax_proceso_cod         SMALLINT; -- código del proceso
   DEFINE v_ax_glo_estado          SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                  SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err               INTEGER;
   DEFINE v_c_msj                  VARCHAR(250);
   DEFINE v_c_nss                  CHAR(11);
   DEFINE v_b_existe_reg          SMALLINT; -- booleana que indica si existe o no la originación de crédito
   DEFINE r_ax_existe_marca_prc    SMALLINT; -- valor de regreso función que verifica si ya existe la marca
   DEFINE r_ax_bandera             SMALLINT; -- valor de regreso de la actualización
   DEFINE r_ax_edo_retorno         SMALLINT; -- estado retorno de alguna funcion
   DEFINE v_estado_reenv           SMALLINT;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/acrIntegDevol.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_ax_estado       = 0;
   LET v_ax_glo_estado   = 2; -- estado Integrado
   LET v_ax_operacion    = 06; -- operación Devolucion de Solicitudes
   LET v_ax_proceso_cod  = 205; -- Devolución de Solicitudes ACR
   LET v_error           = 0;
   LET v_isam_err        = 0;
   LET v_c_msj           = 'El proceso finalizó correctamente';
   LET v_c_nss           = "0"; -- valor del NSS antes de entrar al ciclo

   -- se obtienen los datos de la temporal de devoluciones (detalle)
   FOREACH
      SELECT 
         tpo_registro        ,
         cont_servicio       ,
         tpo_entidad_recep   ,
         cve_entidad_recep   ,
         tpo_entidad_cedente ,
         cve_entidad_cedente ,
         orig_tpo_transf     ,
         f_presentacion      ,
         filler1             ,
         curp                ,
         nss                 ,
         filler2             ,
         rfc                 ,
         ap_paterno          ,
         ap_materno          ,
         nombre              ,
         filler3             ,
         id_lote_solicitud   ,
         filler4             ,
         nss_afore           ,
         rfc_afore           ,
         filler5             ,
         ap_pat_afore_ced    ,
         ap_mat_afore_ced    ,
         nombre_afo_ced      ,
         filler6             ,
         num_aiv98_ult_aport ,
         ult_aport_viv97     ,
         filler7             ,
         result_operacion    ,
         diag_proceso        ,
         nombre_imss         ,
         num_credito         ,
         filler8             ,
         mot_devolucion      ,
         filler9             ,
         periodo_pago        ,
         filler10            
      INTO 
         tmp_acr_devol_tpo_registro        ,
         tmp_acr_devol_cont_servicio       ,
         tmp_acr_devol_tpo_entidad_recep   ,
         tmp_acr_devol_cve_entidad_recep   ,
         tmp_acr_devol_tpo_entidad_cedente ,
         tmp_acr_devol_cve_entidad_cedente ,
         tmp_acr_devol_orig_tpo_transf     ,
         tmp_acr_devol_f_presentacion      ,
         tmp_acr_devol_filler1             ,
         tmp_acr_devol_curp                ,
         tmp_acr_devol_nss                 ,
         tmp_acr_devol_filler2             ,
         tmp_acr_devol_rfc                 ,
         tmp_acr_devol_ap_paterno          ,
         tmp_acr_devol_ap_materno          ,
         tmp_acr_devol_nombre              ,
         tmp_acr_devol_filler3             ,
         tmp_acr_devol_id_lote_solicitud   ,
         tmp_acr_devol_filler4             ,
         tmp_acr_devol_nss_afore           ,
         tmp_acr_devol_rfc_afore           ,
         tmp_acr_devol_filler5             ,
         tmp_acr_devol_ap_pat_afore_ced    ,
         tmp_acr_devol_ap_mat_afore_ced    ,
         tmp_acr_devol_nombre_afo_ced      ,
         tmp_acr_devol_filler6             ,
         tmp_acr_devol_num_aiv98_ult_aport ,
         tmp_acr_devol_ult_aport_viv97     ,
         tmp_acr_devol_filler7             ,
         tmp_acr_devol_result_operacion    ,
         tmp_acr_devol_diag_proceso        ,
         tmp_acr_devol_nombre_imss         ,
         tmp_acr_devol_num_credito         ,
         tmp_acr_devol_filler8             ,
         tmp_acr_devol_mot_devolucion      ,
         tmp_acr_devol_filler9             ,
         tmp_acr_devol_periodo_pago        ,
         tmp_acr_devol_filler10            
      FROM safre_tmp:tmp_acr_devol

      -- se asigna el valor del nss en la variable de retorno
      LET v_c_nss = tmp_acr_devol_nss;

      -- se asume que no existirá la originación de crédito
      LET v_b_existe_reg = 0;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_acr_devol_nss;

      -- se obtiene la información de la tabla de generación de archivo
      FOREACH
       SELECT FIRST 1 t1.id_cre_acreditado
         INTO v_ax_id_cre_acreditado
         FROM safre_tmp:tmp_acr_solic_sdo t1
        WHERE t1.nss = v_c_nss
      END FOREACH;

      -- se obtiene el id acreditado de la tabla maestro
      FOREACH
       SELECT FIRST 1 id_cre_acreditado, estado, edo_procesar, tpo_credito
         INTO v_ax_id_cre_acreditado, v_ax_estado, v_ax_edo_procesar, v_ax_tpo_credito
         FROM cre_acreditado
        WHERE id_cre_acreditado = v_ax_id_cre_acreditado

         -- se indica que existe la originación de crédito
         LET v_b_existe_reg = 1;
      END FOREACH

      -- se verifica si no existió la originación de crédito
      IF v_b_existe_reg = 0 THEN
         -- No existió la originación de crédito. Se rechaza el registro
         LET rch_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET rch_nss                = tmp_acr_devol_nss;
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = tmp_acr_devol_tpo_registro;
         LET rch_num_credito        = tmp_acr_devol_num_credito;
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

      -- se valida el estado procesar
      IF v_ax_edo_procesar = 7 THEN
         -- se rechaza el registro
         LET v_ax_estado = 240;

         -- se asignan los valores al registro a insertar
         LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
         LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
         LET his_tpo_transferencia  = tmp_acr_devol_orig_tpo_transf;
         LET his_edo_procesar       = 100; -- Solicitud devuelta
         LET his_diagnostico        = tmp_acr_devol_mot_devolucion;
         LET his_estado             = v_ax_estado;
         LET his_nss_afore          = tmp_acr_devol_nss_afore;
         LET his_rfc_afore          = tmp_acr_devol_rfc_afore;
         LET his_paterno_afore      = tmp_acr_devol_ap_pat_afore_ced;
         LET his_materno_afore      = tmp_acr_devol_ap_mat_afore_ced;
         LET his_nombre_afore       = tmp_acr_devol_nombre_afo_ced;
         LET his_nom_imss           = tmp_acr_devol_nombre_imss;
         LET his_f_proceso          = TODAY;

         -- se inserta registro
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
         LET rch_nss                = tmp_acr_devol_nss;
         LET rch_tpo_originacion    = 1;
         LET rch_tpo_registro       = tmp_acr_devol_tpo_registro;
         LET rch_num_credito        = tmp_acr_devol_num_credito;
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

         -- se incrementa el numero de registros aceptados y el id transferencia
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
                                      INTO  r_ax_edo_retorno;
         END IF
      END IF

      -- se incrementa el numero de registros aceptados y el id transferencia
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se asignan los valores al registro a insertar
      LET his_id_cre_acreditado  = v_ax_id_cre_acreditado;
      LET his_id_cre_ctr_archivo = p_ax_id_cre_ctr_arch;
      LET his_tpo_transferencia  = tmp_acr_devol_orig_tpo_transf;
      LET his_edo_procesar       = 100; -- DEVUELTA segun cat_maq_cred_det
      LET his_diagnostico        = tmp_acr_devol_mot_devolucion;      LET his_estado             = v_ax_estado;
      LET his_nss_afore          = tmp_acr_devol_nss_afore;
      LET his_rfc_afore          = tmp_acr_devol_rfc_afore;
      LET his_paterno_afore      = tmp_acr_devol_ap_pat_afore_ced;
      LET his_materno_afore      = tmp_acr_devol_ap_mat_afore_ced;
      LET his_nombre_afore       = tmp_acr_devol_nombre_afo_ced;
      LET his_nom_imss           = tmp_acr_devol_nombre_imss;
      LET his_f_proceso          = TODAY;

      -- se inserta registro
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
      IF v_ax_estado = 240 THEN
         CONTINUE FOREACH;
      END IF

      -- se ejecuta el store procedure que actualiza el registro correspondiente de la
      -- tabla maestro a estado procesar 100-Devuelta
      EXECUTE PROCEDURE sp_act_cre_transf(his_id_cre_acreditado, his_edo_procesar);

      -- se inserta registro duplicado con estado_procesar 70 y sin diagnostico
      -- modif solicitada el 24 de Enero de 2012
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
                  70,
                  NULL,
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
      -- por solicitud del Instituto ahora ya no se reenvían las solicitudes devueltas

      IF v_ax_estado = 25 THEN
         LET v_estado_reenv = 70;
      ELSE
         LET v_estado_reenv = 240;
      END IF

      EXECUTE PROCEDURE sp_act_cre_transf(his_id_cre_acreditado, v_estado_reenv);

      -- se asignan los valores en las variables que se usaran para insertar el registro en tmp
      LET tmp_deudor_id_cre_acreditado = his_id_cre_acreditado;
      LET tmp_deudor_id_derechohabiente = v_ax_id_derechohabiente;
      LET tmp_deudor_nss = tmp_acr_devol_nss;
   
      -- se inserta registro
      INSERT INTO safre_tmp:tmp_deudor_devoluciones (
                  id_cre_acreditado,
                  id_derechohabiente,
                  nss)
          VALUES (tmp_deudor_id_cre_acreditado,
                  tmp_deudor_id_derechohabiente,
                  tmp_deudor_nss);

{
         -- se verifica el contenido del campo diagnostico en la tabla temporal
         -- es de 15 caracteres, se divide cada 3, y cada tercia es un diagnostico
         -- por cada diagnostico encontrado, se repiten los datos y se cambia la clave de diagnostico
         -- apartir del segundo bloque, el primero se inserta aunque sea nulo el diagnostico[1,3]
         IF ( tmp_acr_devol_diag_proceso[4,6] IS NOT NULL AND tmp_acr_devol_diag_proceso[4,6]<> "   " AND
              tmp_acr_devol_diag_proceso[4,6]<> "000" ) THEN
            LET his_diagnostico = tmp_acr_devol_diag_proceso[4,6]; -- los 3 caracteres de diagnostico

            -- se inserta registro
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
         END IF -- existe diagnostico [4,6]

         IF ( tmp_acr_devol_diag_proceso[7,9] IS NOT NULL AND tmp_acr_devol_diag_proceso[7,9]<> "   " AND
              tmp_acr_devol_diag_proceso[7,9]<> "000" ) THEN
            LET his_diagnostico = tmp_acr_devol_diag_proceso[7,9]; -- los 3 caracteres de diagnostico
            -- se inserta registro
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
         END IF -- existe diagnostico [7,9]

         IF ( tmp_acr_devol_diag_proceso[10,12] IS NOT NULL AND tmp_acr_devol_diag_proceso[10,12]<> "   " AND
              tmp_acr_devol_diag_proceso[10,12]<> "000" ) THEN
            LET his_diagnostico = tmp_acr_devol_diag_proceso[10,12]; -- los 3 caracteres de diagnostico
            -- se inserta registro
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
         END IF -- existe diagnostico [10,12]

         IF ( tmp_acr_devol_diag_proceso[13,15] IS NOT NULL AND tmp_acr_devol_diag_proceso[13,15]<> "   " AND
              tmp_acr_devol_diag_proceso[13,15]<> "000" ) THEN
            LET his_diagnostico = tmp_acr_devol_diag_proceso[13,15]; -- los 3 caracteres de diagnostico
            -- se inserta registro
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
         END IF -- existe diagnostico [13,15]
}

   END FOREACH;

   -- valor del nss después de finalizar el ciclo
   LET v_c_nss = 1;

   -- actualiza estadisticas a la tabla historica
   UPDATE STATISTICS FOR TABLE cre_his_acreditado;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_ax_glo_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj, v_c_nss;
END FUNCTION;


