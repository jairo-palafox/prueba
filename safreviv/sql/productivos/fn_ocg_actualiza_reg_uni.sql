






CREATE FUNCTION "safreviv".fn_ocg_actualiza_reg_uni(p_nss_unificado        CHAR(11),
                                          p_id_dh_unificado      DECIMAL(9,0),
                                          p_nss_unificador       CHAR(11),
                                          p_id_dh_unificador     DECIMAL(9,0),
                                          p_tpo_credito          CHAR(1),
                                          p_subproceso           SMALLINT)

   RETURNING SMALLINT

   DEFINE v_resultado               SMALLINT;
   DEFINE v_f_proceso               DATE;
   DEFINE v_sqry                    CHAR(1200);
   DEFINE v_id_ocg_formalizacion    DECIMAL(9,0);
   DEFINE v_id_ocg_tramite          DECIMAL(9,0);
   DEFINE v_usuario                 CHAR(20);

   SET DEBUG FILE TO '/safreviv_int/archivos/unifica43bis.trace';
   --SET DEBUG FILE TO '/safreviv_req/SAC43BIS/PruebasUnitarias/cred43bis.trace';
   TRACE ON;

   LET v_resultado            = 0;
   LET v_f_proceso            = TODAY;
   LET v_id_ocg_formalizacion = 0;
   LET v_id_ocg_tramite       = 0;
   LET v_usuario              = "infonavit";

   FOREACH
      SELECT f.id_ocg_formalizacion, f.id_ocg_tramite
        INTO v_id_ocg_formalizacion, v_id_ocg_tramite
        FROM ocg_formalizacion f
       WHERE f.id_derechohabiente = p_id_dh_unificado
         ---AND f.situacion BETWEEN 60 AND 80
         ---AND f.diagnostico        = 1
      --ORDER BY f_formalizacion DESC

      IF v_id_ocg_tramite IS NULL THEN
         LET v_id_ocg_tramite = 0;
      END IF

      IF v_id_ocg_formalizacion IS NOT NULL AND v_id_ocg_formalizacion > 0 THEN
         LET v_sqry = " INSERT INTO ocg_his_tramite "||
                      " SELECT id_ocg_tramite,"||
                          " cve_ent_financiera,"||
                          " id_derechohabiente,"||
                          " '',"||
                          " rfc,"||
                          " curp,"||
                          " ap_paterno,"||
                          " ap_materno,"||
                          " nombre,"||
                          " num_bimestres,"||
                          " viv97,"||
                          " f_saldo,"||
                          " tpo_credito,"||
                          " f_vigencia,"||
                          " diagnostico,"||
                          " estado,"||
                          " situacion,'"||v_f_proceso||"','"||v_usuario||"'"||
                      "   FROM ocg_tramite"||
                      "  WHERE id_ocg_tramite = "||v_id_ocg_tramite||
                      "    AND id_derechohabiente = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_tramite    ---Trámite
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_tramite     = v_id_ocg_tramite
            AND id_derechohabiente = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle del trámite
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_tramite
                 WHERE id_ocg_tramite = v_id_ocg_tramite)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 1;

         UPDATE ocg_fecha_mig   ---Fechas de trámite
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  = v_id_ocg_tramite
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso            = 1;

         LET v_sqry = " INSERT INTO ocg_his_formalizacion "||
                      " SELECT id_ocg_formalizacion,"||
                             " id_derechohabiente,"||
                             " cve_ent_financiera,"||
                             " num_ctr_int_ef,"||
                             " num_escritura,"||
                             " notario,"||
                             " ent_fed_notario,"||
                             " mcpio_notario,"||
                             " num_rpp,"||
                             " folio_real,"||
                             " partida,"||
                             " foja,"||
                             " volumen,"||
                             " libro,"||
                             " tomo,"||
                             " seccion,"||
                             " ent_fed_inmueble,"||
                             " mcpio_inmueble,"||
                             " domicilio_inmueble,"||
                             " valor_avaluo,"||
                             " monto_credito,"||
                             " plazo_credito,"||
                             " tpo_moneda,"||
                             " tasa_base,"||
                             " margen,"||
                             " tpo_credito,"||
                             " f_otorga_ent_fin,"||
                             " f_registro_carta,"||
                             " diagnostico,"||
                             " estado,"||
                             " usuario_reg_carta,"||
                             " situacion,'"||v_f_proceso||"','"||v_usuario||"'"||
                      "   FROM ocg_formalizacion"||
                      "  WHERE id_ocg_formalizacion = "||v_id_ocg_formalizacion||
                      "    AND id_derechohabiente   = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_formalizacion   ----Formalización
            SET id_derechohabiente   = p_id_dh_unificador
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
            AND id_derechohabiente   = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle de formalización
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_formalizacion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 2;

         UPDATE ocg_fecha_mig   ---Fechas de formalización
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  = v_id_ocg_formalizacion
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso         = 2;

         IF p_tpo_credito = "7" OR p_tpo_credito = "8" THEN
             UPDATE ocg_liquidacion_cofi
                SET id_derechohabiente = p_id_dh_unificador,
                    nss                = p_nss_unificador
              WHERE id_derechohabiente = p_id_dh_unificado;
         END IF

         LET v_sqry = " INSERT INTO ocg_his_solic_uso_garantia "||
                      " SELECT id_ocg_solicitud_ug,"||
                             " id_derechohabiente,"||
                             " cve_ent_financiera,"||
                             " num_ctr_int_ef,"||
                             " importe_solicitado,"||
                             " f_vencimiento,"||
                             " importe_utilizado,"||
                             " tpo_credito,"||
                             " solicitud_saldo,"||
                             " diagnostico,"||
                             " estado,"||
                             " situacion,'"||v_f_proceso||"','"||v_usuario||"'"||
                      "   FROM ocg_solicitud_uso_garantia"||
                      "  WHERE id_ocg_formalizacion = "||v_id_ocg_formalizacion||
                      "    AND id_derechohabiente = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_solicitud_uso_garantia   ---Usos de crédito
            SET id_derechohabiente   = p_id_dh_unificador
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
            AND id_derechohabiente   = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle de uso de garantia
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_solicitud_uso_garantia
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 3;

         UPDATE ocg_fecha_mig   ---Fechas de uso de garantía
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  IN(
                SELECT id_ocg_solicitud_ug
                  FROM ocg_solicitud_uso_garantia
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso         = 3;

         LET v_sqry = " INSERT INTO ocg_his_transaccion "||
                      " SELECT id_ocg_ctr_transaccion,"||
                             " id_ocg_formalizacion,"||
                             " id_derechohabiente,"||
                             " id_referencia_cta,"||
                             " folio_referencia,"||
                             " proceso_cod,"||
                             " cve_ent_financiera,"||
                             " num_ctr_int_ef,"||
                             " folio,"||
                             " f_transaccion,"||
                             " nss,"||
                             " curp,"||
                             " vivienda_97,"||
                             " periodo_pago,"||
                             " f_pago,"||
                             " concepto,"||
                             " f_proceso,"||
                             " estado,'"||v_usuario||"'"||
                      "   FROM ocg_ctr_transaccion"||
                      "  WHERE id_ocg_formalizacion = "||v_id_ocg_formalizacion||
                      "    AND id_derechohabiente = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_ctr_transaccion   ---Transacciones
            SET id_derechohabiente   = p_id_dh_unificador,
                nss                  = p_nss_unificador
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
            AND id_derechohabiente   = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle de transacciones
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_ctr_transaccion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 4;

         UPDATE ocg_fecha_mig   ---Fechas de transacciones
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  IN(
                SELECT id_ocg_ctr_transaccion
                  FROM ocg_ctr_transaccion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso = 4;

         LET v_sqry = " INSERT INTO ocg_his_liquidacion "||
                      " SELECT id_ocg_liquidacion,"||
                             " id_derechohabiente,"||
                             " cve_ent_financiera,"||
                             " num_ctr_int_ef,"||
                             " bimestre_ap_subsec,"||
                             " importe_ap_subsec,"||
                             " f_liberacion_gtia,"||
                             " importe_devuelto,"||
                             " id_causa_liquida,"||
                             " f_deposito,"||
                             " tpo_credito,"||
                             " diagnostico,"||
                             " estado,"||
                             " situacion,'"||v_f_proceso||"','"||v_usuario||"'"||
                      "   FROM ocg_liquidacion "||
                      "  WHERE id_ocg_formalizacion = "||v_id_ocg_formalizacion||
                      "    AND id_derechohabiente = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_liquidacion   ---Liquidación
            SET id_derechohabiente   = p_id_dh_unificador
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
            AND id_derechohabiente   = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle de liquidación
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_liquidacion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 5;

         UPDATE ocg_fecha_mig   ---Fechas de liquidación
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  IN(
                SELECT id_ocg_liquidacion
                  FROM ocg_liquidacion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso = 5;

         LET v_sqry = " INSERT INTO ocg_his_devolucion "||
                      " SELECT id_ocg_devolucion,"||
                             " id_ocg_detalle,"||
                             " id_ocg_formalizacion,"||
                             " id_ocg_tramite,"||
                             " id_derechohabiente,"||
                             " cve_ent_financiera,"||
                             " num_ctr_int_ef,"||
                             " periodo_pago,"||
                             " importe_subsec_devuelto,"||
                             " importe_ocg_devuelto,"||
                             " importe_pendiente,"||
                             " f_deposito,"||
                             " tpo_credito,"||
                             " diagnostico,"||
                             " estado,"||
                             " situacion,"||
                             " edo_registro,'"||v_f_proceso||"','"||v_usuario||"'"||
                      "   FROM ocg_devolucion "||
                      "  WHERE id_ocg_formalizacion = "||v_id_ocg_formalizacion||
                      "    AND id_derechohabiente = "||p_id_dh_unificado;

         EXECUTE IMMEDIATE v_sqry;

         UPDATE ocg_devolucion   ---Devolución
            SET id_derechohabiente   = p_id_dh_unificador
          WHERE id_ocg_formalizacion = v_id_ocg_formalizacion
            AND id_derechohabiente   = p_id_dh_unificado;

         UPDATE ocg_detalle   ---Detalle de devolución
            SET id_derechohabiente = p_id_dh_unificador,
                nss                = p_nss_unificador
          WHERE id_ocg_detalle IN(
                SELECT id_ocg_detalle
                  FROM ocg_devolucion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente    = p_id_dh_unificado
            AND subproceso            = 5;

         UPDATE ocg_fecha_mig   ---Fechas de devolución
            SET id_derechohabiente = p_id_dh_unificador
          WHERE id_ocg_referencia  IN(
                SELECT id_ocg_devolucion
                  FROM ocg_devolucion
                 WHERE id_ocg_formalizacion = v_id_ocg_formalizacion)
            AND id_derechohabiente = p_id_dh_unificado
            AND subproceso = 5;

         INSERT INTO safre_tmp:tmp_ocg_uni
         VALUES(p_nss_unificado,
                p_nss_unificador,
                p_subproceso,
                TODAY);
      END IF
   END FOREACH;

   RETURN v_resultado;

END FUNCTION
;


