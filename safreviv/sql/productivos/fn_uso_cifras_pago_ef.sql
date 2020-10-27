






CREATE FUNCTION "safreviv".fn_uso_cifras_pago_ef()

RETURNING SMALLINT, INTEGER, VARCHAR(250), CHAR(300)

   ---variables preliquidación
   DEFINE v_folio_liquida           DECIMAL(10,0);
   DEFINE v_edo_liq                 SMALLINT;
   DEFINE v_tpo_liq                 SMALLINT;
   DEFINE v_monto_pesos             DECIMAL(12,2);
   DEFINE v_total_reg               INTEGER;
   DEFINE v_nss                     CHAR(11);
   DEFINE v_periodo_pago            CHAR(6);

   ---variables registros uso garantía
   DEFINE v_estado                  SMALLINT;
   DEFINE v_tpo_uso                 SMALLINT;
   DEFINE v_impt_ug                 DECIMAL(12,2);
   DEFINE v_tot_ug                  INTEGER;

   ---variables solicitudes uso garantía
   DEFINE v_situacion               SMALLINT;
   DEFINE v_solic_sdo               SMALLINT;
   DEFINE v_impt_utilizado          DECIMAL(12,2);
   DEFINE v_total_solic             INTEGER;

   ---variables dis
   DEFINE v_edo_dis                 SMALLINT;
   DEFINE v_concepto_dis            SMALLINT;
   DEFINE v_impt_ap_pat             DECIMAL(12,2);
   DEFINE v_tot_dis                 INTEGER;

   ---variables trx
   DEFINE v_edo_trx                 SMALLINT;
   DEFINE v_concepto_trx            SMALLINT;
   DEFINE v_impt_trx                DECIMAL(12,2);
   DEFINE v_tot_trx                 INTEGER;

   ---variable tipo registro
   DEFINE v_tpo_registro            SMALLINT;

   ---variables de control
   DEFINE v_error                   SMALLINT; -- en caso de error contiene el código
   DEFINE v_isam_err                INTEGER;
   DEFINE v_c_msj                   VARCHAR(250);

   -- Variables de comparación
   DEFINE v_msj_conciliacion        CHAR(300);
   DEFINE v_sum_importe_ug          DECIMAL(12,2);
   DEFINE v_sum_total_ug            INTEGER;
   DEFINE v_importe_solic_ug        DECIMAL(12,2);
   DEFINE v_total_solic_ug          INTEGER;
   DEFINE v_importe_ug_aplicado     DECIMAL(12,2);
   DEFINE v_total_ug_aplicado       INTEGER;
   DEFINE v_importe_ug_a_pagar      DECIMAL(12,2);
   DEFINE v_total_ug_a_pagar        INTEGER;
   DEFINE v_importe_ug_a_publicar   DECIMAL(12,2);
   DEFINE v_total_ug_a_publicar     INTEGER;

   ON EXCEPTION SET v_error, v_isam_err, v_c_msj
      -- Devolverá el código de error cuando ocurra una excepción
      RETURN v_error, v_isam_err, v_c_msj, v_msj_conciliacion;
   END EXCEPTION

   --SET DEBUG FILE TO '/safreviv_log/grt/UsoCifrasPagoEF.trace';
   ---SET DEBUG FILE TO '/safreviv_int/archivos/UsoCifrasPagoEF.trace';
   ---TRACE ON;

   DROP TABLE IF EXISTS tmp_cifras_ef;

   CREATE TABLE tmp_cifras_ef
   (estado                          SMALLINT,
    tpo_concepto                    SMALLINT,
    importe                         DECIMAL(12,2),
    total                           INTEGER,
    tpo_registro                    SMALLINT);

   DROP TABLE IF EXISTS tmp_reg_dev_ug;

   CREATE TABLE tmp_reg_dev_ug
   (nss                            CHAR(11),
    periodo_pago                   CHAR(6),
    importe                        DECIMAL(12,2));

   ---inicilización de variables
   LET v_folio_liquida  = 0;
   LET v_edo_liq        = 130;
   LET v_tpo_liq        = 1;
   LET v_monto_pesos    = 0;
   LET v_nss            = "";
   LET v_periodo_pago   = "";
   LET v_total_reg      = 0;
   LET v_estado         = 0;
   LET v_tpo_uso        = 0;
   LET v_impt_ug        = 0;
   LET v_tot_ug         = 0;
   LET v_situacion      = 0;
   LET v_solic_sdo      = 0;
   LET v_impt_utilizado = 0;
   LET v_total_solic    = 0;
   LET v_edo_dis        = 0;
   LET v_concepto_dis   = 0;
   LET v_impt_ap_pat    = 0;
   LET v_tot_dis        = 0;
   LET v_edo_trx        = 0;
   LET v_concepto_trx   = 0;
   LET v_impt_trx       = 0;
   LET v_tot_trx        = 0;
   LET v_tpo_registro   = 1; ---preliquidación
   LET v_error          = 0;
   LET v_isam_err       = 0;
   LET v_c_msj          = 'Las cifras se generaron correctamente';
   LET v_sum_importe_ug        = 0;
   LET v_sum_total_ug          = 0;
   LET v_msj_conciliacion      = NULL;
   LET v_importe_solic_ug      = 0;
   LET v_total_solic_ug        = 0;
   LET v_importe_ug_aplicado   = 0;
   LET v_total_ug_aplicado     = 0;
   LET v_importe_ug_a_pagar    = 0;
   LET v_total_ug_a_pagar      = 0;
   LET v_importe_ug_a_publicar = 0;
   LET v_total_ug_a_publicar   = 0;

   ---montos globales
   SELECT folio_liquida, sum(monto_pesos), count(*)
     INTO v_folio_liquida, v_monto_pesos, v_total_reg
     FROM cre_ug_preliquida
   GROUP BY 1;
   
   INSERT INTO tmp_cifras_ef
       (estado,
        tpo_concepto,
        importe,
        total,
        tpo_registro)
   VALUES
       (v_edo_liq,
        v_tpo_liq,
        v_monto_pesos,
        v_total_reg,
        v_tpo_registro);

   ---cifras usos garantía 18
   LET v_tpo_registro = 18;

   FOREACH
      SELECT c.tpo_transferencia, c.estado, c.tpo_uso, SUM(m.monto_pesos), COUNT(*)
        INTO v_tpo_registro, v_estado, v_tpo_uso, v_impt_ug, v_tot_ug 
        FROM cre_ug_preliquida m, cre_uso_garantia c
       WHERE m.folio_liquida = c.folio_liquida
         AND m.id_derechohabiente = c.id_derechohabiente
         AND m.id_referencia = c.periodo_pago
         AND c.tpo_transferencia = "18"
      GROUP BY 1,2,3

      INSERT INTO tmp_cifras_ef
          (estado,
           tpo_concepto,
           importe,
           total,
           tpo_registro)
      VALUES
          (v_estado,
           v_tpo_uso,
           v_impt_ug,
           v_tot_ug,
           v_tpo_registro);

      -- Solicitudes UG
      IF(v_estado = 140) THEN
         LET v_importe_solic_ug = v_importe_solic_ug + v_impt_ug;
         LET v_total_solic_ug   = v_total_solic_ug + v_tot_ug;
      END IF

      LET v_sum_importe_ug = v_sum_importe_ug + v_impt_ug;
      LET v_sum_total_ug   = v_sum_total_ug + v_tot_ug;

   END FOREACH

   ---Registros con devolución
   FOREACH
      SELECT a.nss, c.periodo_pago, SUM(m.monto_pesos)
        INTO v_nss, v_periodo_pago, v_impt_ug
        FROM cre_ug_preliquida m, cre_uso_garantia c, afi_derechohabiente a
       WHERE m.folio_liquida = c.folio_liquida
         AND m.id_derechohabiente = c.id_derechohabiente
         AND m.id_referencia = c.periodo_pago
         AND c.estado IN(320, 325, 340)
         AND c.id_derechohabiente = a.id_derechohabiente
      GROUP BY 1,2

      INSERT INTO tmp_reg_dev_ug
          (nss,
           periodo_pago,
           importe)
      VALUES
          (v_nss,
           v_periodo_pago,
           v_impt_ug);
   END FOREACH

   ---cifras uso garantía 48
   LET v_estado       = 0;
   LET v_tpo_uso      = 0;
   LET v_impt_ug      = 0;
   LET v_tot_ug       = 0;
   LET v_tpo_registro = 48;

   SELECT c.tpo_transferencia, c.estado, c.tpo_uso, SUM(m.monto_pesos), COUNT(*)
     INTO v_tpo_registro, v_estado, v_tpo_uso, v_impt_ug, v_tot_ug 
     FROM cre_ug_preliquida m, cre_uso_garantia c
    WHERE m.folio_liquida = c.folio_liquida
      AND m.id_derechohabiente = c.id_derechohabiente
      AND m.id_referencia = c.periodo_pago
      AND c.tpo_transferencia = "48"
   GROUP BY 1,2,3;

   IF v_tpo_registro IS NULL OR v_tpo_registro = "" THEN
      LET v_estado       = 0;
      LET v_tpo_uso      = 0;
      LET v_impt_ug      = 0;
      LET v_tot_ug       = 0;
      LET v_tpo_registro = 48;
   END IF

   INSERT INTO tmp_cifras_ef
       (estado,
        tpo_concepto,
        importe,
        total,
        tpo_registro)
   VALUES
       (v_estado,
        v_tpo_uso,
        v_impt_ug,
        v_tot_ug,
        v_tpo_registro);

   LET v_sum_importe_ug = v_sum_importe_ug + v_impt_ug;
   LET v_sum_total_ug   = v_sum_total_ug + v_tot_ug;

   -- Evalúa cifras de preliquidación VS cifras de UG
   IF (v_total_reg <> v_sum_total_ug) AND (v_monto_pesos <> v_sum_importe_ug) THEN
      LET v_msj_conciliacion = 'Cifras preliquidadas no concilian con cifras uso de garantía';
   END IF

   LET v_tpo_registro = 0;

   ---cifras solicitudes uso
   FOREACH
      SELECT 3, s.situacion, s.solicitud_saldo, SUM(s.importe_utilizado), COUNT(*)
        INTO v_tpo_registro, v_situacion, v_solic_sdo, v_impt_utilizado, v_total_solic
        FROM cre_ug_preliquida m, cre_uso_garantia c,
             ocg_transaccion_cre t, ocg_solicitud_uso_garantia s
       WHERE m.folio_liquida = c.folio_liquida
         AND m.id_derechohabiente = c.id_derechohabiente
         AND m.id_referencia = c.periodo_pago
         AND c.estado IN(130,140)
         AND c.tpo_transferencia = "18"
         AND c.id_cre_uso_garantia = t.id_referencia_cre
         AND t.id_referencia_ocg = s.id_ocg_solicitud_ug
         AND c.tpo_uso = s.solicitud_saldo
      GROUP BY 1,2,3

      INSERT INTO tmp_cifras_ef
          (estado,
           tpo_concepto,
           importe,
           total,
           tpo_registro)
      VALUES
          (v_situacion,
           v_solic_sdo,
           v_impt_utilizado,
           v_total_solic,
           v_tpo_registro);

      LET v_importe_ug_aplicado = v_importe_ug_aplicado + v_impt_utilizado;
      LET v_total_ug_aplicado   = v_total_ug_aplicado + v_total_solic;

   END FOREACH;

   LET v_importe_ug_aplicado = v_importe_ug_aplicado * -1;

   IF(v_importe_ug_aplicado <> v_importe_solic_ug) AND (v_total_ug_aplicado <> v_total_solic_ug) THEN
      LET v_msj_conciliacion = RTRIM(v_msj_conciliacion)||'||Cifras uso de garantía no concilian con solicitudes';
   END IF

   LET v_tpo_registro = 0;

   ---montos dispersión
   FOREACH
      SELECT 4, estado, concepto, SUM(imp_ap_pat), COUNT(*)
        INTO v_tpo_registro, v_edo_dis, v_concepto_dis, v_impt_ap_pat, v_tot_dis
        FROM dis_ctr_aps_tns
       WHERE folio_liquida = v_folio_liquida
      GROUP BY 1,2,3

      INSERT INTO tmp_cifras_ef
          (estado,
           tpo_concepto,
           importe,
           total,
           tpo_registro)
      VALUES
          (v_edo_dis,
           v_concepto_dis,
           v_impt_ap_pat,
           v_tot_dis,
           v_tpo_registro);

      LET v_importe_ug_a_pagar = v_importe_ug_a_pagar + v_impt_ap_pat ;
      LET v_total_ug_a_pagar   = v_total_ug_a_pagar + v_tot_dis;

   END FOREACH;

   -- Evalúa cifras a pagar VS Solicitudes UG
   LET v_importe_ug_a_pagar = v_importe_ug_a_pagar * -1;

   IF(v_importe_ug_a_pagar <> v_importe_solic_ug) AND (v_total_ug_a_pagar <> v_total_solic_ug) THEN
      LET v_msj_conciliacion = RTRIM(v_msj_conciliacion)||'||Cifras a pagar no cuadran con cifras de solicitudes';
   END IF

   LET v_tpo_registro = 0;

   ---montos dispersión
   FOREACH
      SELECT 5, estado, concepto, SUM(vivienda_97), COUNT(*)
        INTO v_tpo_registro, v_edo_trx, v_concepto_trx, v_impt_trx, v_tot_trx
        FROM ocg_ctr_transaccion
       WHERE folio_referencia = v_folio_liquida
      GROUP BY 1,2,3

      INSERT INTO tmp_cifras_ef
          (estado,
           tpo_concepto,
           importe,
           total,
           tpo_registro)
      VALUES
          (v_edo_trx,
           v_concepto_trx,
           v_impt_trx,
           v_tot_trx,
           v_tpo_registro);

      LET v_importe_ug_a_publicar = v_importe_ug_a_publicar + v_impt_trx;
      LET v_total_ug_a_publicar   = v_total_ug_a_publicar + v_tot_trx;

   END FOREACH;

   LET v_importe_ug_a_publicar = v_importe_ug_a_publicar * -1;

   IF(v_importe_ug_a_publicar <> v_importe_solic_ug) AND (v_total_ug_a_publicar <> v_total_solic_ug) THEN
      LET v_msj_conciliacion = RTRIM(v_msj_conciliacion)||'||Cifras a publicar no cuadran con cifras de solicitudes';
   END IF

   IF(v_msj_conciliacion IS NULL) THEN
      LET v_msj_conciliacion = 'La conciliación de las cifras es correcto';
   END IF

   RETURN v_error, v_isam_err, v_c_msj, v_msj_conciliacion;

END FUNCTION
;


