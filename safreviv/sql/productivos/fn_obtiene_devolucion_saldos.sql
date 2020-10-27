






CREATE FUNCTION "safreviv".fn_obtiene_devolucion_saldos (p_usuario            CHAR(20),
                                              p_ax_id_cre_ctr_arch DECIMAL(9,0),
                                              p_d_folio            DECIMAL(10,0),
                                              p_f_presentacion     DATE)
RETURNING SMALLINT, INTEGER;

   --- variables de trabajo
   DEFINE v_resultado                   SMALLINT;       -- resultado de la integración de devoluciones
   DEFINE v_total_reg                   INTEGER;        -- total de registros
   DEFINE v_id_ocg_devolucion           DECIMAL(9,0);   -- id de devoluciones
   DEFINE v_id_ocg_ctr_transaccion      DECIMAL(9,0);   -- id de transacciones
   DEFINE v_periodo_pago                CHAR(6);        -- periodo pago
   DEFINE v_importe_aportacion          DECIMAL(12,2);  -- importe aportaciones subsecuentes
   DEFINE v_importe_garantia            DECIMAL(12,2);  -- importe usos garantía
   DEFINE v_importe_transaccion         DECIMAL(12,2);  -- importe transacciones
   DEFINE v_edo_registro                SMALLINT;       -- estado registro devolución
   DEFINE v_estado                      SMALLINT;       -- estado registro transacción
   DEFINE v_folio_referencia            DECIMAL(9,0);   -- folio referencia transaccion
   DEFINE v_f_proceso                   DATE;           -- fecha proceso
   DEFINE v_nss                         CHAR(11);       -- nss
   DEFINE v_id_derechohabiente          DECIMAL(9,0);   -- id_derechohabiente
   DEFINE v_tpo                         SMALLINT;

   --- variables de inserción en dse_devolucion
   DEFINE vdev_id_dse_devolucion       DECIMAL(9,0);
   DEFINE vdev_folio                   DECIMAL(9,0);
   DEFINE vdev_modulo_cod              CHAR(3);
   DEFINE vdev_id_derechohabiente      DECIMAL(9,0);
   DEFINE vdev_num_credito             DECIMAL(10,0);
   DEFINE vdev_tpo_transferencia       CHAR(2);
   DEFINE vdev_origen_devolucion       CHAR(2);
   DEFINE vdev_f_pago                  DATE;
   DEFINE vdev_f_movimiento            DATE;
   DEFINE vdev_periodo_pago            CHAR(6);
   DEFINE vdev_folio_referencia        DECIMAL(9,0);
   DEFINE vdev_subcuenta               DECIMAL(9,0);
   DEFINE vdev_monto_aivs              DECIMAL(22,2);
   DEFINE vdev_monto_pesos             DECIMAL(12,2);
   DEFINE vdev_monto_aportacion        DECIMAL(12,2);
   DEFINE vdev_aivs_aportacion         DECIMAL(16,6);
   DEFINE vdev_nss_separacion          CHAR(11);
   DEFINE vdev_estado                  SMALLINT;

   -- se declara que hacer al ocurrir un error
   ON EXCEPTION SET v_resultado
      LET v_resultado = 1;
      LET v_total_reg = 0;

      -- se devuelve el resultado de la operacion indicando que ocurrio un error
      RETURN v_resultado, v_total_reg;
   END EXCEPTION

   ---SET DEBUG FILE TO '/safreviv_int/archivos/devSaldo43bis.trace';
   ---TRACE ON;

   LET v_nss                    = "";
   LET v_resultado              = 0;
   LET v_total_reg              = 0;
   LET v_id_ocg_devolucion      = "";
   LET v_id_ocg_ctr_transaccion = "";
   LET v_periodo_pago           = "";
   LET v_importe_aportacion     = 0;
   LET v_importe_garantia       = 0;
   LET v_importe_transaccion    = 0;
   LET v_edo_registro           = 200;
   LET v_estado                 = 80;
   LET v_folio_referencia       = "";
   LET v_f_proceso              = TODAY;
   LET v_tpo                    = 1;

   LET vdev_folio             = p_d_folio;
   LET vdev_modulo_cod        = "grt";
   LET vdev_num_credito       = 0;
   LET vdev_tpo_transferencia = "19";
   LET vdev_origen_devolucion = 2;
   LET vdev_f_pago            = p_f_presentacion;
   LET vdev_f_movimiento      = MDY(MONTH(p_f_presentacion),1,YEAR(p_f_presentacion));
   LET vdev_f_movimiento      = vdev_f_movimiento + 1 UNITS MONTH;
   LET vdev_folio_referencia  = 0;
   LET vdev_subcuenta         = 4;
   LET vdev_monto_aivs        = 0;
   LET vdev_monto_pesos       = 0;
   LET vdev_monto_aportacion  = 0;
   LET vdev_aivs_aportacion   = 0;
   LET vdev_nss_separacion    = "";
   LET vdev_estado            = 10;

   -- ciclo para devoluciones de conciliación aportaciones
   FOREACH
      SELECT a1.nss,
             d1.id_derechohabiente,
             d1.id_ocg_devolucion,
             d1.importe_subsec_devuelto,
             d1.periodo_pago
        INTO v_nss,
             v_id_derechohabiente,
             v_id_ocg_devolucion,
             v_importe_aportacion,
             v_periodo_pago
        FROM ocg_devolucion d1,
             afi_derechohabiente a1
       WHERE d1.edo_registro IN(170,180)
         AND d1.id_derechohabiente = a1.id_derechohabiente
         AND d1.importe_subsec_devuelto > 0

      LET vdev_id_dse_devolucion  = seq_dse_devolucion.NEXTVAL;
      LET vdev_id_derechohabiente = v_id_derechohabiente;
      LET vdev_periodo_pago       = v_periodo_pago;
      LET vdev_monto_pesos        = v_importe_aportacion;

      INSERT INTO dse_devolucion (id_dse_devolucion,
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
                          VALUES (vdev_id_dse_devolucion,
                                  vdev_folio,
                                  vdev_modulo_cod,
                                  vdev_id_derechohabiente,
                                  vdev_num_credito,
                                  vdev_tpo_transferencia,
                                  vdev_origen_devolucion,
                                  vdev_f_pago,
                                  vdev_f_movimiento,
                                  vdev_periodo_pago,
                                  vdev_folio_referencia,
                                  vdev_subcuenta,
                                  vdev_monto_aivs,
                                  vdev_monto_pesos,
                                  vdev_monto_aportacion,
                                  vdev_aivs_aportacion,
                                  vdev_nss_separacion,
                                  vdev_estado);

      INSERT INTO safre_tmp:tmp_solicitud_devolucion
                                 (nss,
                                  id_derechohabiente,
                                  monto,
                                  tpo)
                          VALUES (v_nss,
                                  v_id_derechohabiente,
                                  v_importe_aportacion,
                                  v_tpo);

      UPDATE ocg_devolucion
         SET edo_registro      = 200
       WHERE id_ocg_devolucion = v_id_ocg_devolucion;

      LET v_periodo_pago = "";
      LET v_total_reg    = v_total_reg + 1;
   END FOREACH;

   LET v_tpo          = 2;
   LET v_periodo_pago = "";

   -- ciclo para devoluciones de conciliación garantías
   FOREACH
      SELECT a2.nss,
             d2.id_derechohabiente,
             d2.id_ocg_devolucion,
             d2.importe_ocg_devuelto,
             d2.periodo_pago
        INTO v_nss,
             v_id_derechohabiente,
             v_id_ocg_devolucion,
             v_importe_garantia,
             v_periodo_pago
        FROM ocg_devolucion d2,
             afi_derechohabiente a2
       WHERE d2.edo_registro IN(170,180)
         AND d2.id_derechohabiente = a2.id_derechohabiente
         AND d2.importe_ocg_devuelto > 0

      LET vdev_id_dse_devolucion  = seq_dse_devolucion.NEXTVAL;
      LET vdev_id_derechohabiente = v_id_derechohabiente;
      LET vdev_periodo_pago       = v_periodo_pago;
      LET vdev_monto_pesos        = v_importe_garantia;

      INSERT INTO dse_devolucion (id_dse_devolucion,
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
                          VALUES (vdev_id_dse_devolucion,
                                  vdev_folio,
                                  vdev_modulo_cod,
                                  vdev_id_derechohabiente,
                                  vdev_num_credito,
                                  vdev_tpo_transferencia,
                                  vdev_origen_devolucion,
                                  vdev_f_pago,
                                  vdev_f_movimiento,
                                  vdev_periodo_pago,
                                  vdev_folio_referencia,
                                  vdev_subcuenta,
                                  vdev_monto_aivs,
                                  vdev_monto_pesos,
                                  vdev_monto_aportacion,
                                  vdev_aivs_aportacion,
                                  vdev_nss_separacion,
                                  vdev_estado);

      INSERT INTO safre_tmp:tmp_solicitud_devolucion
                                 (nss,
                                  id_derechohabiente,
                                  monto,
                                  tpo)
                          VALUES (v_nss,
                                  v_id_derechohabiente,
                                  v_importe_garantia,
                                  v_tpo);

      UPDATE ocg_devolucion
         SET edo_registro      = 200
       WHERE id_ocg_devolucion = v_id_ocg_devolucion;

      LET v_periodo_pago = "";
      LET v_total_reg    = v_total_reg + 1;
   END FOREACH;

   LET v_tpo          = 3;
   LET v_periodo_pago = "";

   -- ciclo para devoluciones de liquidaciones no pagadas
   FOREACH
      SELECT a3.nss,
             d3.id_derechohabiente,
             d3.id_ocg_ctr_transaccion,
             d3.vivienda_97,
             d3.periodo_pago
        INTO v_nss,
             v_id_derechohabiente,
             v_id_ocg_ctr_transaccion,
             v_importe_transaccion,
             v_periodo_pago
        FROM ocg_ctr_transaccion d3,
             cat_concepto_mov    c3,
             afi_derechohabiente a3
       WHERE d3.concepto             = c3.concepto
         AND c3.tpo_concepto         = 4
         AND d3.estado               = 30
         AND d3.vivienda_97          > 0
         AND d3.id_derechohabiente   = a3.id_derechohabiente

      LET vdev_id_dse_devolucion  = seq_dse_devolucion.NEXTVAL;
      LET vdev_id_derechohabiente = v_id_derechohabiente;
      LET vdev_periodo_pago       = v_periodo_pago;
      LET vdev_monto_pesos        = v_importe_transaccion;

      INSERT INTO dse_devolucion (id_dse_devolucion,
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
                          VALUES (vdev_id_dse_devolucion,
                                  vdev_folio,
                                  vdev_modulo_cod,
                                  vdev_id_derechohabiente,
                                  vdev_num_credito,
                                  vdev_tpo_transferencia,
                                  vdev_origen_devolucion,
                                  vdev_f_pago,
                                  vdev_f_movimiento,
                                  vdev_periodo_pago,
                                  vdev_folio_referencia,
                                  vdev_subcuenta,
                                  vdev_monto_aivs,
                                  vdev_monto_pesos,
                                  vdev_monto_aportacion,
                                  vdev_aivs_aportacion,
                                  vdev_nss_separacion,
                                  vdev_estado);

      INSERT INTO safre_tmp:tmp_solicitud_devolucion
                                 (nss,
                                  id_derechohabiente,
                                  monto,
                                  tpo)
                          VALUES (v_nss,
                                  v_id_derechohabiente,
                                  v_importe_transaccion,
                                  v_tpo);

      UPDATE ocg_ctr_transaccion
         SET estado                 = 80,
             concepto               = concepto + 10
       WHERE id_ocg_ctr_transaccion = v_id_ocg_ctr_transaccion;

      LET v_periodo_pago = "";
      LET v_total_reg    = v_total_reg + 1;
   END FOREACH;

   RETURN v_resultado, v_total_reg;

END FUNCTION;


