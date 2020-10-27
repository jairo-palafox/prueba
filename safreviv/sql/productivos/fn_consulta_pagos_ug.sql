






CREATE FUNCTION "safreviv".fn_consulta_pagos_ug(p_id_derechohabiente DECIMAL(9,0))
RETURNING SMALLINT, CHAR(6), DECIMAL(12,2), DATE, CHAR(60), CHAR(300);

   DEFINE v_codigo                  SMALLINT;
   DEFINE v_periodo_pago            CHAR(6);
   DEFINE v_importe                 DECIMAL(12,2);
   DEFINE v_impt_solic              DECIMAL(12,2);
   DEFINE v_f_liquida               DATE;
   DEFINE v_estado                  SMALLINT;
   DEFINE v_diagnostico             CHAR(3);
   DEFINE v_desc_estado             CHAR(60);
   DEFINE v_cod_accion              SMALLINT;
   DEFINE v_accion                  CHAR(300);
   DEFINE v_folio_liquida           DECIMAL(10,0);
   DEFINE v_f_formalizacion         DATE;
   DEFINE v_criterio                SMALLINT;
   DEFINE v_tabla                   CHAR(20);
   DEFINE v_f_liq_cta               DATE;
   DEFINE v_f_proceso               DATE;
   DEFINE v_sqry                    CHAR(500);
   DEFINE v_id_cre_uso_garantia     DECIMAL(9,0);
   DEFINE v_a_formalizacion         SMALLINT;
   DEFINE v_total_ug                INTEGER;
   DEFINE v_acc                     DECIMAL(12,2);
   DEFINE v_valor_fondo             DECIMAL(18,6);
   DEFINE v_sdo                     DECIMAL(12,2);
   DEFINE v_fecha_valua             DATE;

   /*
   SET DEBUG FILE TO '/safreviv_int/archivos/usosCredGtia.trace';
   TRACE ON;
   */

   -- se establece la prioridad
   --SET PDQPRIORITY HIGH;

   DROP TABLE IF EXISTS tmp_uso_garantia;
   DROP TABLE IF EXISTS tmp_movs;

   CREATE TEMP TABLE tmp_uso_garantia
      (id_derechohabiente    CHAR(11),
       periodo_pago          CHAR(6),
       importe               DECIMAL(12,2),
       folio_liquida         DECIMAL(10,0),
       f_liquida             DATE,
       estado                SMALLINT,
       desc_estado           CHAR(60),
       accion                CHAR(300));

   LET v_codigo              = 0;
   LET v_periodo_pago        = "";
   LET v_importe             = "";
   LET v_impt_solic          = 0;
   LET v_f_liquida           = "";
   LET v_estado              = "";
   LET v_desc_estado         = "";
   LET v_accion              = "";
   LET v_folio_liquida       = 0;
   LET v_f_formalizacion     = "";
   LET v_criterio            = 0;
   LET v_f_liq_cta           = "12/31/1899";
   LET v_tabla               = "";
   LET v_diagnostico         = "";
   LET v_id_cre_uso_garantia = "";
   LET v_total_ug            = 0;
   LET v_acc                 = 0;
   LET v_valor_fondo         = 0;
   LET v_sdo                 = 0;
   LET v_fecha_valua         = TODAY;

   IF NOT EXISTS (SELECT a.f_formalizacion
                    FROM ocg_formalizacion f, ocg_acreditado a
                   WHERE f.id_derechohabiente = p_id_derechohabiente
                     AND f.id_ocg_formalizacion = a.id_ocg_formalizacion
                     AND a.f_formalizacion IS NOT NULL) THEN

      LET v_desc_estado = "Sin pago del patrón registrado";

      SELECT desc_sugerencia
        INTO v_accion
        FROM cat_accion_sugerida
       WHERE cod_accion = 2;

      RETURN v_codigo, v_periodo_pago, v_importe, v_f_liquida, v_desc_estado, v_accion;
   ELSE
      FOREACH
         SELECT FIRST 1 a.f_formalizacion
           INTO v_f_formalizacion
           FROM ocg_formalizacion f, ocg_acreditado a
          WHERE f.id_derechohabiente = p_id_derechohabiente
            AND f.id_ocg_formalizacion = a.id_ocg_formalizacion
            AND a.f_formalizacion IS NOT NULL
         ORDER BY 1 DESC
      END FOREACH;
   END IF

   -- Recupera las solicitudes UG
   FOREACH
      SELECT c.periodo_pago,
             c.folio_liquida,
             c.estado,
             c.importe_v97,
             c.diagnostico,
             c.f_proceso
        INTO v_periodo_pago,
             v_folio_liquida,
             v_estado,
             v_impt_solic,
             v_diagnostico,
             v_f_proceso
        FROM cre_uso_garantia c,
             cre_ctr_archivo r
       WHERE c.id_derechohabiente = p_id_derechohabiente
         AND c.tpo_transferencia IN("18","48")
         AND c.estado = 140
         AND c.edo_procesar = 120
         AND c.f_proceso >= v_f_formalizacion
         AND c.id_cre_ctr_archivo = r.id_cre_ctr_archivo
         AND r.operacion = 18

      -- Obtiene montos liquidados
      IF(v_folio_liquida > 0) THEN
         EXECUTE FUNCTION fn_tab_movimiento(v_criterio, v_folio_liquida, v_f_liq_cta) INTO v_tabla;

         IF(v_tabla IS NULL) OR (v_tabla = "") THEN
            CONTINUE FOREACH;
         ELSE
            LET v_sqry = " SELECT cta.id_derechohabiente, cta.folio_liquida, cta.id_referencia, cta.monto_pesos, cta.f_liquida "||
                         "   FROM "||v_tabla ||" cta WHERE cta.folio_liquida = "||v_folio_liquida||
                         "    AND cta.id_derechohabiente = "||p_id_derechohabiente||
                         "    AND cta.id_referencia = "||v_periodo_pago||
                         " INTO TEMP tmp_movs";

            EXECUTE IMMEDIATE v_sqry;

            SELECT monto_pesos,
                   f_liquida
              INTO v_importe,
                   v_f_liquida
              FROM tmp_movs
             WHERE id_derechohabiente = p_id_derechohabiente
               AND folio_liquida = v_folio_liquida
               AND id_referencia = v_periodo_pago;

            IF(v_importe = 0) OR (v_importe = "") OR (v_importe IS NULL) THEN
               DROP TABLE IF EXISTS tmp_movs;
               CONTINUE FOREACH;
            ELSE
               LET v_importe     = v_importe * -1;
               LET v_desc_estado = "Recursos enviados a Cartera (Uso de Garantía)";
               LET v_cod_accion  = 1;
            END IF
            DROP TABLE IF EXISTS tmp_movs;
         END IF
      ELSE
         CONTINUE FOREACH;
      END IF

      LET v_codigo = 1;

      IF(v_importe IS NULL) OR (v_importe = "") OR (v_importe = 0) THEN
         LET v_importe = v_impt_solic;
      END IF

      SELECT desc_sugerencia
        INTO v_accion
        FROM cat_accion_sugerida
       WHERE cod_accion = v_cod_accion;

      INSERT INTO tmp_uso_garantia
           VALUES (p_id_derechohabiente,
                   v_periodo_pago,
                   v_importe,
                   v_folio_liquida,
                   v_f_liquida,
                   v_estado,
                   v_desc_estado,
                   v_accion);

      LET v_periodo_pago    = "";
      LET v_importe         = "";
      LET v_impt_solic      = 0;
      LET v_f_liquida       = "";
      LET v_estado          = "";
      LET v_desc_estado     = "";
      LET v_accion          = "";
      LET v_folio_liquida   = 0;
      LET v_criterio        = 0;
      LET v_f_liq_cta       = "12/31/1899";
      LET v_tabla           = "";
      LET v_diagnostico     = "";
   END FOREACH;

   LET v_periodo_pago    = "";
   LET v_importe         = "";
   LET v_f_liquida       = "";
   LET v_desc_estado     = "";
   LET v_accion          = "";

   -- PAGOS DE DISPERSIÓN

   LET v_desc_estado = "Recursos enviados a Cartera (Aportaciones Subsecuentes)";
   LET v_estado      = 140;
   LET v_cod_accion  = 1;

   SELECT desc_sugerencia
     INTO v_accion
     FROM cat_accion_sugerida
    WHERE cod_accion = v_cod_accion;

   LET v_a_formalizacion = YEAR(v_f_formalizacion);

   FOREACH
      SELECT tabla
        INTO v_tabla
        FROM cat_tab_movimiento
       WHERE anio >= v_a_formalizacion

      LET v_sqry = " INSERT INTO tmp_uso_garantia "||
                   " SELECT m.id_derechohabiente, m.origen[8,13], m.monto_pesos, m.folio_liquida, m.f_liquida, 140, 'Recursos enviados a Cartera (Aportaciones Subsecuentes)', d.desc_sugerencia "||
                   "   FROM "||v_tabla||" m, cat_accion_sugerida d "||
                   "  WHERE id_derechohabiente = "||p_id_derechohabiente||
                   "    AND subcuenta = 4"||
                   "    AND movimiento IN (72)"||
                   "    AND f_liquida >= '"||v_f_formalizacion||"'"||
                   "    AND d.cod_accion = 1";

      EXECUTE IMMEDIATE v_sqry;
   END FOREACH;

   LET v_sqry = " INSERT INTO tmp_uso_garantia "||
                " SELECT m.id_derechohabiente, m.origen[8,13], m.monto_pesos, m.folio_liquida, m.f_liquida, 140, 'Recursos enviados a Cartera (Aportaciones Subsecuentes)', d.desc_sugerencia "||
                "   FROM cta_movimiento m, cat_accion_sugerida d "||
                "  WHERE id_derechohabiente = "||p_id_derechohabiente||
                "    AND subcuenta = 4"||
                "    AND movimiento IN (72)"||
                "    AND f_liquida >= '"||v_f_formalizacion||"'"||
                "    AND d.cod_accion = 1";

   EXECUTE IMMEDIATE v_sqry;

   -- se regresa la prioridad
   --SET PDQPRIORITY DEFAULT;

   -- Verifica que existan pagos 
   SELECT COUNT(*)
     INTO v_total_ug
     FROM tmp_uso_garantia;

   IF(v_total_ug = 0) THEN
      LET v_cod_accion = 2;

      SELECT SUM(monto_acciones)
        INTO v_acc
        FROM cta_movimiento
       WHERE id_derechohabiente = p_id_derechohabiente
         AND subcuenta = 4
         AND fondo_inversion = 11;

      IF v_acc IS NULL OR v_acc = "" THEN
         LET v_acc = 0;
      END IF

      SELECT precio_fondo
        INTO v_valor_fondo
        FROM glo_valor_fondo
       WHERE f_valuacion = v_fecha_valua
         AND fondo = 11;

      LET v_sdo = v_acc * v_valor_fondo;

      IF (v_sdo < 0) OR (v_sdo = 0) THEN
         LET v_desc_estado = "";
         LET v_accion = "";
      ELSE
         SELECT desc_accion, desc_sugerencia
           INTO v_desc_estado, v_accion
           FROM cat_accion_sugerida
          WHERE cod_accion = v_cod_accion;
      END IF

      RETURN v_codigo, v_periodo_pago, v_importe, v_f_liquida, v_desc_estado, v_accion;
   ELSE
      FOREACH
         SELECT periodo_pago,
                importe,
                f_liquida,
                desc_estado,
                accion
           INTO v_periodo_pago,
                v_importe,
                v_f_liquida,
                v_desc_estado,
                v_accion
           FROM tmp_uso_garantia
         ORDER BY f_liquida

         RETURN v_codigo,
                v_periodo_pago,
                v_importe,
                v_f_liquida,
                v_desc_estado,
                v_accion
         WITH RESUME;
      END FOREACH;
   END IF

END FUNCTION;


