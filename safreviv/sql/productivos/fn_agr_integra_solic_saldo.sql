






CREATE FUNCTION "safreviv".fn_agr_integra_solic_saldo(p_v_usuario CHAR(20),
                                           p_v_arch_proceso CHAR(100),
                                           p_d_folio DECIMAL(10),
                                           p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT, INTEGER, VARCHAR(250);
   -- REGISTRO tmp solicitud saldo (entrada)
   DEFINE tmpe_tpo_registro       CHAR(1);
   DEFINE tmpe_num_credito        DECIMAL(10,0);
   DEFINE tmpe_nss                CHAR(11);
   -- REGISTRO tmp solicitud saldo (salida)
   DEFINE tmps_tpo_registro       CHAR(1);
   DEFINE tmps_num_credito        DECIMAL(10,0);
   DEFINE tmps_nss                CHAR(11);
   DEFINE tmps_tpo_credito        SMALLINT;
   DEFINE tmps_sdo_viv92          DECIMAL(15,0);
   DEFINE tmps_sdo_viv97          DECIMAL(15,0);
   -- Campos auxiliares
   DEFINE v_ax_id_derechohabiente DECIMAL(9,0); -- identificador del derechohabiente
   DEFINE v_ax_precio_fondo       DECIMAL(19,14); -- precio de acción obtenido
   DEFINE v_ax_aivs_viv92         DECIMAL(20,6); -- aivs subcuenta vivienda 92
   DEFINE v_ax_aivs_viv97         DECIMAL(20,6); -- aivs subcuenta vivienda 97
   DEFINE v_ax_sdo_viv92          DECIMAL(22,2); -- saldo subcuenta vivienda 92
   DEFINE v_ax_sdo_viv97          DECIMAL(22,2); -- saldo subcuenta vivienda 97
   DEFINE v_ax_id_lote_acpt       INTEGER; -- total de registros aceptados
   DEFINE v_ax_id_lote_rech       INTEGER; -- total de registros rechazados
   DEFINE r_ax_bandera            SMALLINT; -- valor de regreso de la actualización
   DEFINE v_i_estado              SMALLINT; -- estado a actualizar el registro en glo ctr_archivo
   DEFINE v_error                 SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err              INTEGER;
   DEFINE v_c_msj                 VARCHAR(250);

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/agrIntegRchSdo.trace';
   --TRACE ON;

   -- se inicializa el contador de registros
   LET v_ax_id_lote_acpt = 0;
   LET v_ax_id_lote_rech = 0;
   LET v_i_estado = 2; -- estado Integrado
   LET v_error = 0;
   LET v_isam_err = 0;
   LET v_c_msj = 'El proceso finalizó correctamente';

   -- se asignan los valores constantes de los registros a insertar
   LET tmps_tpo_registro = "2";

   -- se consulta el precio de acción para el día de hoy
   SELECT precio_fondo
     INTO v_ax_precio_fondo
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   -- se obtienen los datos de la tabla temporal del proceso de solicitud de saldos
   FOREACH
   SELECT *
     INTO tmpe_tpo_registro,
          tmpe_num_credito,
          tmpe_nss        
     FROM safre_tmp:tmp_solic_sdo_det_agr
      -- se inicializan variables
      LET v_ax_id_derechohabiente = NULL;
      LET tmps_num_credito        = tmpe_num_credito;
      LET tmps_nss                = tmpe_nss;

      -- se obtiene el id del derechohabiente para el nss
      SELECT id_derechohabiente
        INTO v_ax_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmpe_nss;

      -- se verifica si fue posible encontrar el id derechohabiente en el catálogo
      IF v_ax_id_derechohabiente IS NULL THEN
         -- se asignan los valores default en el registro a insertar
         LET tmps_tpo_credito = 0;
         LET tmps_sdo_viv92   = 0;
         LET tmps_sdo_viv97   = 0;

         -- se incrementa el numero de registros rechazados
         LET v_ax_id_lote_rech = v_ax_id_lote_rech + 1;
      ELSE
         -- se inicializa el tipo de credito
         LET tmps_tpo_credito = NULL;

         -- se busa el tipo de crédito para el id derechohabiente en proceso
         ---SELECT UNIQUE tpo_credito
         FOREACH
            SELECT FIRST 1 tpo_credito
              INTO tmps_tpo_credito
              FROM cre_acreditado
             WHERE id_derechohabiente = v_ax_id_derechohabiente
             ORDER BY f_otorga DESC, estado
         END FOREACH;

         -- de no encontrarse el tipo de credito se asigna cero
         IF tmps_tpo_credito IS NULL THEN
            -- se asigna cero al tipo de crédito
            LET tmps_tpo_credito = 0;
         END IF

         -- se buscan los montos de cta_movimiento
         SELECT SUM(monto_acciones)
           INTO v_ax_aivs_viv92
           FROM cta_movimiento
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND subcuenta IN (8,42);

         -- se validan las acciones obtenidas (viv 92)
         IF v_ax_aivs_viv92 IS NULL THEN
            LET v_ax_aivs_viv92 = 0;
         END IF

         -- se buscan los montos de cta_movimiento
         SELECT SUM(monto_acciones)
           INTO v_ax_aivs_viv97
           FROM cta_movimiento
          WHERE id_derechohabiente = v_ax_id_derechohabiente
            AND subcuenta IN (4,44);

         -- se validan las acciones obtenidas (viv 97)
         IF v_ax_aivs_viv97 IS NULL THEN
            LET v_ax_aivs_viv97 = 0;
         END IF

         -- se convienten a pesos las aivs obtenidas según el precio de acción
         LET v_ax_sdo_viv92 = v_ax_aivs_viv92 * v_ax_precio_fondo;
         LET v_ax_sdo_viv97 = v_ax_aivs_viv97 * v_ax_precio_fondo;

         -- se guardan los registros con el formato del archivo de salida
         LET tmps_sdo_viv92 = v_ax_sdo_viv92 * 100;
         LET tmps_sdo_viv97 = v_ax_sdo_viv97 * 100;

         -- se incrementa el numero de registros aceptados
         LET v_ax_id_lote_acpt = v_ax_id_lote_acpt + 1;
      END IF

      -- se inserta registro
      INSERT INTO safre_tmp:tmp_solic_saldo_agr(
                  tpo_registro,
                  num_credito,
                  nss,
                  tpo_credito,
                  sdo_viv92,
                  sdo_viv97)
          VALUES (tmps_tpo_registro,
                  tmps_num_credito,
                  tmps_nss,
                  tmps_tpo_credito,
                  tmps_sdo_viv92,
                  tmps_sdo_viv97);
   END FOREACH;

   -- actualiza estadisticas a la tabla temporal
   -- UPDATE STATISTICS FOR TABLE safre_tmp:tmp_solic_saldo_agr;

   -- se ejecuta el sp que actualiza el registro correspondiente de la tabla de control de archivos global
   EXECUTE FUNCTION fn_act_edo_archivo(p_v_arch_proceso, p_d_folio, v_i_estado, p_v_usuario) INTO r_ax_bandera;

   -- se ejecuta el sp que actualiza la tabla de control de archivos, indicando que el archivo ya fue integrado
   EXECUTE PROCEDURE sp_act_cre_ctr_archivo(p_d_folio, v_ax_id_lote_acpt, v_ax_id_lote_rech, 0, p_ax_id_cre_ctr_arch);

   RETURN v_error, v_isam_err, v_c_msj;
END FUNCTION;


