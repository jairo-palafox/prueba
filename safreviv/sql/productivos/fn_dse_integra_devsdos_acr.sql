






CREATE PROCEDURE "safreviv".fn_dse_integra_devsdos_acr(p_d_pid DECIMAL(9,0),
                                            p_v_usuario CHAR(20),
                                            p_v_arch_proceso CHAR(100),
                                            p_d_folio DECIMAL(9,0))
   -- REGISTRO tmp dse devolucion
   DEFINE tmp_tpo_registro            CHAR(2);
   DEFINE tmp_cont_servicio           DECIMAL(10,0);
   DEFINE tmp_tpo_entidad_recep       CHAR(2);
   DEFINE tmp_cve_entidad_recep       CHAR(3);
   DEFINE tmp_tpo_entidad_cede        CHAR(2);
   DEFINE tmp_cve_entidad_cede        CHAR(3);
   DEFINE tmp_filler                  CHAR(2);
   DEFINE tmp_f_presentacion          DATE;
   DEFINE tmp_f_movimiento            DATE;
   DEFINE tmp_curp                    CHAR(18);
   DEFINE tmp_nss                     CHAR(11);
   DEFINE tmp_filler1                 CHAR(13);
   DEFINE tmp_id_asignacion           CHAR(1);
   DEFINE tmp_tpo_devolucion          CHAR(1);
   DEFINE tmp_rfc_infonavit           CHAR(13);
   DEFINE tmp_ap_paterno_af           CHAR(40);
   DEFINE tmp_ap_materno_af           CHAR(40);
   DEFINE tmp_nombre_af               CHAR(40);
   DEFINE tmp_filler2                 CHAR(22);
   DEFINE tmp_id_lote                 CHAR(16);
   DEFINE tmp_filler3                 CHAR(219);
   DEFINE tmp_num_aplic_interes97     DECIMAL(15,0); -- 9 enteros 6 decimales
   DEFINE tmp_saldo_vivienda97        DECIMAL(15,0); -- 13 enteros 2 decimales
   DEFINE tmp_nss_separacion          DECIMAL(11,0);
   DEFINE tmp_origen_devolucion       DECIMAL(2,0);
   DEFINE tmp_monto_aportaciones      DECIMAL(15,0); -- 13 enteros 2 decimales
   DEFINE tmp_aplic_interes_aporta    DECIMAL(15,0); -- 9 enteros 6 decimales
   DEFINE tmp_filler4                 CHAR(2);
   DEFINE tmp_num_aplic_interes92     DECIMAL(15,0); -- 9 enteros 6 decimales
   DEFINE tmp_saldo_vivienda92        DECIMAL(15,0); -- 13 enteros 2 decimales
   DEFINE tmp_filler5                 CHAR(3);
   DEFINE tmp_cod_res_operacion       CHAR(2);
   DEFINE tmp_diagnostico             CHAR(15);
   DEFINE tmp_nombre_imss             CHAR(50);
   DEFINE tmp_num_credito_infonavit   DECIMAL(10,0);
   DEFINE tmp_interes_saldo_viv97     DECIMAL(15,0); -- 13 enteros 2 decimales
   DEFINE tmp_interes_saldo_viv92     DECIMAL(15,0); -- 13 enteros 2 decimales
   DEFINE tmp_filler6                 CHAR(41);
   -- REGISTRO dse devolucion
   DEFINE dse_id_dse_devolucion    DECIMAL(9,0);
   DEFINE dse_folio                DECIMAL(9,0);
   DEFINE dse_modulo_cod           CHAR(3);
   DEFINE dse_id_derechohabiente   DECIMAL(9,0);
   DEFINE dse_num_credito          DECIMAL(10,0);
   DEFINE dse_tpo_transferencia    CHAR(2);
   DEFINE dse_origen_devolucion    CHAR(2);
   DEFINE dse_f_pago               DATE;
   DEFINE dse_f_movimiento         DATE;
   DEFINE dse_periodo_pago         CHAR(6);
   DEFINE dse_folio_referencia     DECIMAL(9,0);
   DEFINE dse_subcuenta            DECIMAL(9,0);
   DEFINE dse_monto_aivs           DECIMAL(16,6);
   DEFINE dse_monto_pesos          DECIMAL(12,2);
   DEFINE dse_monto_aportacion     DECIMAL(12,2);
   DEFINE dse_aivs_aportacion      DECIMAL(16,6);
   DEFINE dse_nss_separacion       CHAR(11);
   DEFINE dse_estado               SMALLINT;
   -- REGISTRO dse his devolucion
   DEFINE his_id_dse_devolucion    DECIMAL(9,0);
   DEFINE his_id_derechohabiente   DECIMAL(9,0);
   DEFINE his_operacion            INTEGER;
   DEFINE his_lote                 INTEGER;
   DEFINE his_id_lote              INTEGER;
   DEFINE his_f_presentacion       DATE;
   DEFINE his_tpo_transferencia    CHAR(2);
   DEFINE his_origen_devolucion    CHAR(2);
   DEFINE his_f_movimiento         DATE;
   DEFINE his_aivs97               DECIMAL(16,6);
   DEFINE his_pesos97              DECIMAL(12,2);
   DEFINE his_aivs92               DECIMAL(16,6);
   DEFINE his_pesos92              DECIMAL(12,2);
   DEFINE his_monto_aportacion     DECIMAL(12,2);
   DEFINE his_aivs_aportacion      DECIMAL(16,6);
   DEFINE his_paterno_afore        VARCHAR(20,0);
   DEFINE his_materno_afore        VARCHAR(20,0);
   DEFINE his_nombre_afore         CHAR(40);
   DEFINE his_nom_imss             VARCHAR(20,0);
   DEFINE his_nss_separacion       VARCHAR(20,0);
   DEFINE his_edo_procesar         SMALLINT;
   DEFINE his_diagnostico          INTEGER;
   DEFINE his_estado               SMALLINT;
   DEFINE his_f_proceso            DATE;
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente  DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador     CHAR(1); -- tipo de trabajador
   DEFINE v_ax_subcuenta92         DECIMAL(9,0); -- subcuenta viv92
   DEFINE v_ax_subcuenta97         DECIMAL(9,0); -- subcuenta viv97
   DEFINE v_ax_diag_proceso        CHAR(3); -- dignostico en proceso
   DEFINE v_ax_operacion           SMALLINT; -- operacion del proceso
   DEFINE v_ax_lote                SMALLINT; -- numero de lotes cargados
   DEFINE v_ax_id_lote_acpt        INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech        INTEGER; -- total de registros rechazados
   DEFINE r_ax_bandera             SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_estado               SMALLINT; -- estado a actualizar el registro en glo ctr_archivo

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/dseIntegDevsSdos.log';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_operacion = 15;
   LET v_i_estado = 2; -- estado Integrado
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;

   -- se busca el lote a procesar en la tabla de control de archivos del modulo, con estatus "validado"
   SELECT consecutivo_lote
     INTO v_ax_lote
     FROM safre_tmp:tmp_dse_devolucion_enc;

   -- en caso de no encontrar ningun lote en la tabla se asigna o (VPD: que hacer en este caso)
   IF v_ax_lote IS NULL THEN
     LET v_ax_lote = 0;
   END IF;

   FOREACH
      -- se obtienen los datos de la tabla temporal dse devolucion para el archivo en proceso
      SELECT *
      INTO tmp_tpo_registro,
           tmp_cont_servicio,
           tmp_tpo_entidad_recep,
           tmp_cve_entidad_recep,
           tmp_tpo_entidad_cede,
           tmp_cve_entidad_cede,
           tmp_filler,
           tmp_f_presentacion,
           tmp_f_movimiento,
           tmp_curp,
           tmp_nss,
           tmp_filler1,
           tmp_id_asignacion,
           tmp_tpo_devolucion,
           tmp_rfc_infonavit,
           tmp_ap_paterno_af,
           tmp_ap_materno_af,
           tmp_nombre_af,
           tmp_filler2,
           tmp_id_lote,
           tmp_filler3,
           tmp_num_aplic_interes97,
           tmp_saldo_vivienda97,
           tmp_nss_separacion,
           tmp_origen_devolucion,
           tmp_monto_aportaciones,
           tmp_aplic_interes_aporta,
           tmp_filler4,
           tmp_num_aplic_interes92,
           tmp_saldo_vivienda92,
           tmp_filler5,
           tmp_cod_res_operacion,
           tmp_diagnostico,
           tmp_nombre_imss,
           tmp_num_credito_infonavit,
           tmp_interes_saldo_viv97,
           tmp_interes_saldo_viv92,
           tmp_filler6
      FROM safre_tmp:tmp_dse_devolucion

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente, tipo_trabajador
      INTO v_ax_id_derechohabiente, v_ax_tipo_trabajador
      FROM safre_viv:afi_derechohabiente
      WHERE nss = tmp_nss;

      -- se incrementa el numero de registros aceptados y el id transferencia
      LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;

      -- se valida el tipo de trabajador
      IF v_ax_tipo_trabajador = "I" THEN
         LET v_ax_subcuenta92 = 8;
         LET v_ax_subcuenta97 = 4;
      ELSE
         LET v_ax_subcuenta92 = 42;
         LET v_ax_subcuenta97 = 44;
      END IF

      -- se valida que haya monto en viv97
      IF tmp_saldo_vivienda97 >= 0 THEN
         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET dse_id_dse_devolucion  = safre_viv:seq_dse_devolucion.NEXTVAL;
         LET dse_folio              = 0;
         LET dse_modulo_cod         = "acr";
         LET dse_id_derechohabiente = v_ax_id_derechohabiente;
         LET dse_num_credito        = tmp_num_credito_infonavit;
         LET dse_tpo_transferencia  = v_ax_operacion;
         LET dse_origen_devolucion  = 00;
         LET dse_f_pago             = "01/01/0001";
         LET dse_f_movimiento       = tmp_f_movimiento;
         LET dse_periodo_pago       = "000101"; -- VPD
         LET dse_folio_referencia   = p_d_folio;
         LET dse_subcuenta          = v_ax_subcuenta97;
         LET dse_monto_aivs         = tmp_num_aplic_interes97/1000000;
         LET dse_monto_pesos        = tmp_saldo_vivienda97/100;
         LET dse_monto_aportacion   = tmp_monto_aportaciones/100;
         LET dse_aivs_aportacion    = tmp_aplic_interes_aporta/1000000;
         LET dse_nss_separacion     = tmp_nss_separacion;
         LET dse_estado             = 10;

         -- se inserta el registro en la tabla dse devolucion
         INSERT INTO safre_viv:dse_devolucion (
                     id_dse_devolucion,
                     folio,
                     modulo_cod,
                     id_derechohabiente,
                     num_credito,
                     tpo_transferencia,
                     origen_devolucion,
                     f_pago,
                     f_movimiento,
                     periodo_pago,
                     folio_referencia,
                     subcuenta,
                     monto_aivs,
                     monto_pesos,
                     monto_aportacion,
                     aivs_aportacion,
                     nss_separacion,
                     estado)
             VALUES (dse_id_dse_devolucion,
                     dse_folio,
                     dse_modulo_cod,
                     dse_id_derechohabiente,
                     dse_num_credito,
                     dse_tpo_transferencia,
                     dse_origen_devolucion,
                     dse_f_pago,
                     dse_f_movimiento,
                     dse_periodo_pago,
                     dse_folio_referencia,
                     dse_subcuenta,
                     dse_monto_aivs,
                     dse_monto_pesos,
                     dse_monto_aportacion,
                     dse_aivs_aportacion,
                     dse_nss_separacion,
                     dse_estado);
      END IF

      -- se valida que haya monto en viv92
      IF tmp_saldo_vivienda92 >= 0 THEN
         -- se asignan los valores en las variables que se usaran para insertar el registro
         LET dse_id_dse_devolucion  = safre_viv:seq_dse_devolucion.NEXTVAL;
         LET dse_folio              = 0;
         LET dse_modulo_cod         = "acr";
         LET dse_id_derechohabiente = v_ax_id_derechohabiente;
         LET dse_num_credito        = tmp_num_credito_infonavit;
         LET dse_tpo_transferencia  = v_ax_operacion;
         LET dse_origen_devolucion  = 00;
         LET dse_f_pago             = "01/01/0001";
         LET dse_f_movimiento       = tmp_f_movimiento;
         LET dse_periodo_pago       = "000101"; -- VPD
         LET dse_folio_referencia   = p_d_folio;
         LET dse_subcuenta          = v_ax_subcuenta92;
         LET dse_monto_aivs         = tmp_num_aplic_interes92/1000000;
         LET dse_monto_pesos        = tmp_saldo_vivienda92/100;
         LET dse_monto_aportacion   = tmp_monto_aportaciones/100;
         LET dse_aivs_aportacion    = tmp_aplic_interes_aporta/1000000;
         LET dse_nss_separacion     = tmp_nss_separacion;
         LET dse_estado             = 10;

         -- se inserta el registro en la tabla dse devolucion      
         INSERT INTO safre_viv:dse_devolucion (
                     id_dse_devolucion,
                     folio,
                     modulo_cod,
                     id_derechohabiente,
                     num_credito,
                     tpo_transferencia,
                     origen_devolucion,
                     f_pago,
                     f_movimiento,
                     periodo_pago,
                     folio_referencia,
                     subcuenta,
                     monto_aivs,
                     monto_pesos,
                     monto_aportacion,
                     aivs_aportacion,
                     nss_separacion,
                     estado)
             VALUES (dse_id_dse_devolucion,
                     dse_folio,
                     dse_modulo_cod,
                     dse_id_derechohabiente,
                     dse_num_credito,
                     dse_tpo_transferencia,
                     dse_origen_devolucion,
                     dse_f_pago,
                     dse_f_movimiento,
                     dse_periodo_pago,
                     dse_folio_referencia,
                     dse_subcuenta,
                     dse_monto_aivs,
                     dse_monto_pesos,
                     dse_monto_aportacion,
                     dse_aivs_aportacion,
                     dse_nss_separacion,
                     dse_estado);
      END IF
   END FOREACH;

   -- actualiza estadisticas a la tabla de devolución saldos excedentes
   UPDATE STATISTICS FOR TABLE safre_viv:dse_devolucion;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION safre_viv:fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE safre_viv:fn_act_dse_ctr_archivo(p_d_folio, p_d_pid, v_ax_operacion, v_ax_id_lote_acpt, v_ax_id_lote_rech);

END PROCEDURE

;


