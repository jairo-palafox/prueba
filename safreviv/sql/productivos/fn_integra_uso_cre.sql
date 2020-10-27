






CREATE FUNCTION "safreviv".fn_integra_uso_cre(p_id_derechohabiente   DECIMAL(9,0),
                                   p_importe              DECIMAL(12,2),
                                   p_periodo_pago         CHAR(6),
                                   p_id_ocg_solicitud_ug  DECIMAL(9,0),
                                   p_tpo_uso              SMALLINT,
                                   p_usuario              CHAR(20))
   RETURNING SMALLINT;

   -- REGISTRO uso garantia
   DEFINE uso_id_cre_uso_garantia    DECIMAL(9,0);
   DEFINE uso_id_cre_ctr_archivo     DECIMAL(9,0);
   DEFINE uso_folio_liquida          DECIMAL(9,0);
   DEFINE uso_id_derechohabiente     DECIMAL(9,0);
   DEFINE uso_tpo_transferencia      CHAR(2);
   DEFINE uso_tpo_uso                SMALLINT;
   DEFINE uso_num_credito            DECIMAL(10,0);
   DEFINE uso_f_presentacion         DATE;
   DEFINE uso_f_movimiento           DATE;
   DEFINE uso_periodo_pago           CHAR(6);
   DEFINE uso_importe_v97            DECIMAL(22,2);
   DEFINE uso_nss_afore              CHAR(11);
   DEFINE uso_rfc_afore              CHAR(13);
   DEFINE uso_paterno_afore          CHAR(40);
   DEFINE uso_materno_afore          CHAR(40);
   DEFINE uso_nombre_afore           CHAR(40);
   DEFINE uso_nom_imss               CHAR(50);
   DEFINE uso_edo_procesar           SMALLINT;
   DEFINE uso_diagnostico            CHAR(3);
   DEFINE uso_estado                 SMALLINT;
   DEFINE uso_f_proceso              DATE;

   -- Registro de cre saldo deudor
   DEFINE v_sdo_id_cre_uso_garantia  DECIMAL(9,0);
   DEFINE v_sdo_folio_referencia     DECIMAL(9,0);
   DEFINE v_sdo_f_movimiento         DATE;
   DEFINE v_sdo_movimiento           SMALLINT;
   DEFINE v_sdo_id_referencia        DECIMAL(9,0);
   DEFINE v_sdo_monto_aivs           DECIMAL(22,2);
   DEFINE v_sdo_monto_pesos          DECIMAL(22,2);
   DEFINE v_sdo_f_proceso            DATE;

   -- Campos auxiliares
   DEFINE v_ax_nom_archivo           CHAR(40); -- nombre del archivo
   DEFINE v_ax_tpo_originacion       SMALLINT; -- tipo de originacion
   DEFINE v_ax_sdo_viv_97            DECIMAL(13,2); -- saldo vivienda 97
   DEFINE v_ax_aivs                  DECIMAL(12,6); -- Numero de "Aplicaciones de Intereses de Vivienda" 97 solicitado
   DEFINE v_ax_id_derechohabiente    DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_tipo_trabajador       CHAR(1); -- tipo trabajador
   DEFINE v_ax_cuenta_acpt           INTEGER; -- contador de registros aceptados
   DEFINE v_ax_cuenta_rech           INTEGER; -- contador de registros rechazados
   DEFINE v_i_estado                 SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_ax_sts_registro          SMALLINT; -- estatus del registro, indica si fue o no rechazado
   DEFINE v_ax_edo_procesar          SMALLINT; -- estado procesar
   DEFINE r_ax_bandera               SMALLINT; -- valor de regreso de la actualización
   DEFINE v_error                    SMALLINT; -- código de error en caso de excepción
   DEFINE v_tot_solic_ug             SMALLINT; -- total de solicitudes de uso de garantía sin 09 de Procesar
   DEFINE v_ax_precio_accion         DECIMAL(12,2);
   DEFINE cre_id_cre_ctr_archivo     DECIMAL(9,0);
   DEFINE v_folio_archivo            DECIMAL(10,0);
   DEFINE v_subproc                  SMALLINT;
   DEFINE v_proceso_cod              SMALLINT;
   DEFINE v_result_marca             SMALLINT;
   DEFINE v_periodo_ug               CHAR(2);
   DEFINE v_periodo_ap_sub           CHAR(2);

   ON EXCEPTION SET v_error
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   SET DEBUG FILE TO '/safreviv_int/BD/ocgIntegUsoGarant.trace';
   TRACE ON;

   -- se inicializa variables
   LET v_ax_cuenta_acpt     = 0;
   LET v_ax_cuenta_rech     = 0;
   LET v_error              = 0;
   LET v_i_estado           = 2; -- estado Integrado en la tabla glo ctr archivo
   LET v_ax_tpo_originacion = 2; --  2-Créditos en Garantía 43 bis 
   LET v_ax_sts_registro    = 10;
   LET uso_diagnostico      = "";
   LET v_tot_solic_ug       = 0;
   LET v_subproc            = 3;
   LET v_proceso_cod        = 3906;
   LET v_periodo_ug         = "";
   LET v_periodo_ap_sub     = "";

   CALL fn_verifica_id_archivo_ocg(v_subproc)
   RETURNING cre_id_cre_ctr_archivo, v_folio_archivo;

   IF p_tpo_uso = 3 THEN
      LET v_periodo_ug = p_periodo_pago[5,6];

      SELECT bimestre
        INTO v_periodo_ap_sub
        FROM ocg_ctrl_bimestres
       WHERE periodo = v_periodo_ug;

      LET p_periodo_pago = p_periodo_pago[1,4]||v_periodo_ap_sub;
   END IF

   -----------------------------------------------
   -- SE PROCESA EL REGISTRO DE USO DE GARANTÍA --
   -----------------------------------------------

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_ax_precio_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   -- se comprueba que derechohabiente se encuentre en la tabla maestro
   IF NOT EXISTS (
                  SELECT id_derechohabiente
                    FROM cre_acreditado
                   WHERE id_derechohabiente = p_id_derechohabiente
                     AND tpo_originacion    = v_ax_tpo_originacion
                 ) THEN

      LET v_ax_sts_registro = 240;
      LET uso_diagnostico   = 13;
   END IF

   -- se comprueba que no haya otra solicitud para el mismo periodo de pago
   IF EXISTS (
              SELECT id_derechohabiente
                FROM cre_uso_garantia
               WHERE id_derechohabiente = p_id_derechohabiente
                 AND tpo_transferencia  IN("18","48")
                 AND periodo_pago       = p_periodo_pago
                 AND estado             IN(10,20,140)
              ) THEN
      LET v_ax_sts_registro = 240;
      LET uso_diagnostico   = 17;
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
   LET v_ax_sdo_viv_97 = p_importe;

   -- se asignan los valores en las variables que se usaran para insertar el registro
   LET uso_id_cre_uso_garantia = seq_cre_uso.NEXTVAL;
   LET uso_id_cre_ctr_archivo  = cre_id_cre_ctr_archivo;
   LET uso_folio_liquida       = 0;
   LET uso_id_derechohabiente  = p_id_derechohabiente;
   LET uso_tpo_transferencia   = "18";
   LET uso_tpo_uso             = p_tpo_uso;
   LET uso_num_credito         = 0;
   LET uso_f_presentacion      = TODAY;
   LET uso_f_movimiento        = TODAY - DAY(TODAY) + 1;
   LET uso_f_movimiento        = uso_f_movimiento + 1 UNITS MONTH;
   LET uso_periodo_pago        = p_periodo_pago;
   LET uso_importe_v97         = p_importe;
   LET uso_nss_afore           = "";
   LET uso_rfc_afore           = "";
   LET uso_paterno_afore       = "";
   LET uso_materno_afore       = "";
   LET uso_nombre_afore        = "";
   LET uso_nom_imss            = "";
   LET uso_edo_procesar        = 10;
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

   -- se asignan valores al registro a insertar en saldo deudor
   LET v_sdo_id_cre_uso_garantia = uso_id_cre_uso_garantia;
   LET v_sdo_folio_referencia    = v_folio_archivo;
   LET v_sdo_f_movimiento        = uso_f_proceso;
   LET v_sdo_movimiento          = 401;
   LET v_sdo_id_referencia       = uso_periodo_pago;
   LET v_sdo_monto_aivs          = uso_importe_v97/v_ax_precio_accion;
   LET v_sdo_monto_pesos         = uso_importe_v97;
   LET v_sdo_f_proceso           = uso_f_proceso;

   INSERT INTO cre_saldo_deudor(
               id_cre_acreditado,
               folio_referencia,
               f_movimiento,
               movimiento,
               id_referencia,
               monto_aivs,
               monto_pesos,
               f_proceso)
       VALUES (v_sdo_id_cre_uso_garantia,
               v_sdo_folio_referencia,
               v_sdo_f_movimiento,
               v_sdo_movimiento,
               v_sdo_id_referencia,
               v_sdo_monto_aivs,
               v_sdo_monto_pesos,
               v_sdo_f_proceso);

   INSERT INTO ocg_transaccion_cre
   VALUES( v_subproc,
           p_id_ocg_solicitud_ug,
           uso_id_cre_uso_garantia,
           p_periodo_pago,
           uso_f_proceso );

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_uso_garantia;

   CALL fn_uso_procesa_marca_cuenta(p_usuario,
                                    v_folio_archivo,
                                    cre_id_cre_ctr_archivo,
                                    v_proceso_cod)

   RETURNING v_result_marca;

   RETURN v_error;

END FUNCTION

;


