






CREATE FUNCTION "safreviv".fn_uso_integra_uso_garantia_deudor(p_d_folio DECIMAL(9,0),
                                                   p_ax_id_cre_ctr_arch DECIMAL(9,0))
   RETURNING SMALLINT;
   -- Registro de cre saldo deudor
   DEFINE v_sdo_id_cre_acreditado  DECIMAL(9,0);
   DEFINE v_sdo_folio_referencia   DECIMAL(9,0);
   DEFINE v_sdo_f_movimiento       DATE;
   DEFINE v_sdo_movimiento         SMALLINT;
   DEFINE v_sdo_id_referencia      DECIMAL(9,0);
   DEFINE v_sdo_monto_aivs         DECIMAL(22,2);
   DEFINE v_sdo_monto_pesos        DECIMAL(22,2);
   DEFINE v_sdo_f_proceso          DATE;
   -- Registro de Uso de Garantía
   DEFINE uso_id_cre_uso_garantia  DECIMAL(9,0);
   DEFINE uso_periodo_pago         CHAR(6);
   DEFINE uso_importe_v97          DECIMAL(22,2);
   -- Variables auxiliares
   DEFINE v_ax_movimiento          SMALLINT;
   DEFINE v_ax_precio_accion       DECIMAL(22,14);
   DEFINE v_ax_tpo_transferencia   CHAR(2);
   DEFINE v_ax_id_deudor           SMALLINT;
   DEFINE v_error                  SMALLINT; -- codigo de error en caso de excepción

   ON EXCEPTION SET v_error
      -- Devolvera el codigo de error cuando ocurra una excepción
      RETURN v_error;
   END EXCEPTION

   --SET DEBUG FILE TO '/ds/safreviv_int/BD/usoIntegUsoGarantDeudor.trace';
   --TRACE ON;

   -- se inicializa las variables
   LET v_error = 0;

   --se obtiene el valor de la accion
   SELECT precio_fondo
     INTO v_ax_precio_accion
     FROM glo_valor_fondo
    WHERE fondo = 11
      AND f_valuacion = TODAY;

   FOREACH
    SELECT id_cre_uso_garantia, periodo_pago, importe_v97
      INTO uso_id_cre_uso_garantia, uso_periodo_pago, uso_importe_v97
      FROM cre_uso_garantia
     WHERE id_cre_ctr_archivo = p_ax_id_cre_ctr_arch
       AND estado = 20

      -- se asigna el movimiento correspindiente
      LET v_ax_movimiento = 401;

      -- se valida el importe obtenido
      IF uso_importe_v97 IS NULL THEN
         LET uso_importe_v97 = 0;
      END IF

      -- se asignan valores al registro a insertar en saldo deudor
      LET v_sdo_id_cre_acreditado = uso_id_cre_uso_garantia;
      LET v_sdo_folio_referencia  = p_d_folio;
      LET v_sdo_f_movimiento      = TODAY;
      LET v_sdo_movimiento        = v_ax_movimiento;
      LET v_sdo_id_referencia     = uso_periodo_pago;
      LET v_sdo_monto_aivs        = uso_importe_v97/v_ax_precio_accion;
      LET v_sdo_monto_pesos       = uso_importe_v97;
      LET v_sdo_f_proceso         = TODAY;

      -- se inserta en la tabla cre deudor
      INSERT INTO cre_saldo_deudor(
                  id_cre_acreditado,
                  folio_referencia,
                  f_movimiento,
                  movimiento,
                  id_referencia,
                  monto_aivs,
                  monto_pesos,
                  f_proceso)
          VALUES (v_sdo_id_cre_acreditado,
                  v_sdo_folio_referencia,
                  v_sdo_f_movimiento,
                  v_sdo_movimiento,
                  v_sdo_id_referencia,
                  v_sdo_monto_aivs,
                  v_sdo_monto_pesos,
                  v_sdo_f_proceso);
   END FOREACH;

   -- actualiza estadisticas a la tabla de historicos
   UPDATE STATISTICS FOR TABLE cre_saldo_deudor;

   RETURN v_error;
END FUNCTION;


