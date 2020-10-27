






CREATE PROCEDURE "safreviv".sp_ins_dis_aps_tns(p_id_derechohabiente  DECIMAL(9,0),   --ID Derechohabiente
                                    p_id_cre_uso_garantia DECIMAL(9,0),   --ID referencia uso de garantía
                                    p_folio_liquida       DECIMAL(10,0),  --Folio de liquidación
                                    p_f_liquida           DATE,
                                    p_monto_pesos         DECIMAL(22,2),  --Importe de las aportaciones por el precio del día
                                    p_aiv_ap_pat          DECIMAL(18,6),  --Valor de AIVS
                                    p_periodo_pago        CHAR(6),        --Periodo de Pago
                                    p_tpo_credito         SMALLINT,
                                    p_tpo_uso             SMALLINT,
                                    p_pp_orig             CHAR(6))

   RETURNING SMALLINT, INTEGER, CHAR(20);

   DEFINE v_f_pago                  DATE;
   DEFINE v_monto_acciones          DECIMAL(12,2);
   DEFINE v_monto_pesos             DECIMAL(12,2);
   DEFINE v_ind_liquidacion         SMALLINT;      --índice de liquidación con valor cero por omisión
   DEFINE v_nrp                     CHAR(11);       --Numero NRP
   DEFINE v_folio_sua               SMALLINT;

   DEFINE v_bnd_proceso             SMALLINT;      --Estatus del proceso
   DEFINE v_status                  SMALLINT;
   DEFINE sql_err                   INTEGER;
   DEFINE isam_err                  INTEGER;
   DEFINE error_info                CHAR(70);

   DEFINE v_cve_ent_financiera      SMALLINT;
   DEFINE v_num_ctr_int_ef          CHAR(18);
   DEFINE v_num_crd_ifv             DECIMAL(10,0);
   DEFINE v_concepto                SMALLINT;
   DEFINE v_id_ctr_transaccion      DECIMAL(9,0);
   DEFINE v_estado                  SMALLINT;
   DEFINE v_folio_transaccion       DECIMAL(9,0);
   DEFINE v_f_transaccion           DATE;
   DEFINE v_folio_factura           DECIMAL(9,0);
   DEFINE v_f_factura               DATE;
   DEFINE v_char                    CHAR(20);
   DEFINE v_isam                    INTEGER;
   DEFINE v_id_ocg_solicitud_ug     DECIMAL(9,0);

   ON EXCEPTION
      SET sql_err, isam_err, error_info
          LET v_status = sql_err;
      RETURN  v_status, isam_err , error_info;
   END EXCEPTION

   SET DEBUG FILE TO '/safreviv_int/BD/dis_aps_tns.trace';
   TRACE ON;

   LET v_ind_liquidacion    = 0;
   --LET v_concepto           = 0;
   LET v_id_ctr_transaccion = 0;
   LET v_folio_transaccion  = 0;
   LET v_f_transaccion      = "";
   LET v_estado             = 10;  --Registrado
   LET v_folio_factura      = 0;
   LET v_f_factura          = "";
   LET v_bnd_proceso        = 0; --Estado correcto
   LET v_nrp                = "";
   LET v_f_pago             = "";
   LET v_folio_sua          = "";
   LET v_num_crd_ifv        = 0;
   LET v_isam               = 0;
   LET v_char               = "";

   LET v_monto_pesos    = p_monto_pesos * -1;
   LET v_monto_acciones = p_aiv_ap_pat * -1;

   IF p_tpo_uso = 3 THEN
      LET v_concepto = 307;
   ELSE
      LET v_concepto = 407;
   END IF

   SELECT sol.cve_ent_financiera,
          sol.num_ctr_int_ef,
          sol.id_ocg_solicitud_ug
     INTO v_cve_ent_financiera,
          v_num_ctr_int_ef,
          v_id_ocg_solicitud_ug
     FROM cre_uso_garantia uso,
          ocg_transaccion_cre otc,
          ocg_solicitud_uso_garantia sol
    WHERE uso.id_cre_uso_garantia = p_id_cre_uso_garantia
      AND uso.id_cre_uso_garantia = otc.id_referencia_cre
      AND otc.id_referencia_ocg   = sol.id_ocg_solicitud_ug
      AND sol.situacion           = 90;

   INSERT INTO dis_ctr_aps_tns VALUES(seq_dis_interface_ef.NEXTVAL,
                                      p_id_derechohabiente,
                                      v_folio_sua,
                                      p_periodo_pago,
                                      v_f_pago,
                                      v_nrp,
                                      v_ind_liquidacion,
                                      p_folio_liquida,
                                      p_f_liquida,
                                      v_num_crd_ifv,
                                      v_monto_pesos,
                                      v_monto_acciones,
                                      p_tpo_credito,
                                      v_cve_ent_financiera,
                                      v_num_ctr_int_ef,
                                      v_concepto,
                                      v_id_ctr_transaccion,
                                      v_folio_transaccion,
                                      v_f_transaccion,
                                      v_folio_factura,
                                      v_f_factura,
                                      v_estado);

   UPDATE ocg_solicitud_uso_garantia
      SET importe_utilizado   = v_monto_pesos,
          situacion           = 100
    WHERE id_ocg_solicitud_ug = v_id_ocg_solicitud_ug;

   RETURN v_bnd_proceso, v_isam, v_char;

END PROCEDURE;


