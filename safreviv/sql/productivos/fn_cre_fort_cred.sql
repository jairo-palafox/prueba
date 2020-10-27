






CREATE FUNCTION "safreviv".fn_cre_fort_cred(p_folio_liq DECIMAL(9,0),
                                 p_id_cre_acreditado DECIMAL(9,0),
                                 p_id_derechohabiente DECIMAL(9,0),
                                 p_valor_fondo DECIMAL(19,14),
                                 p_tpo_trabajador CHAR(1),
                                 p_tpo_origina SMALLINT)

RETURNING SMALLINT

   ----variables para asignación de valores
   ----a movimientos, fondo,
   ----subcuentas y tipo de trabajador
   DEFINE v_fondo_inv       SMALLINT;
   DEFINE v_origen          CHAR(20);

   DEFINE v_f_liquida       DATE;
   DEFINE v_f_registro      DATE;
   DEFINE v_h_registro      DATETIME HOUR TO SECOND;

   ----variable para status de ejecución
   DEFINE v_status_fc       SMALLINT;

   ----variables para verificación de fortalecimiento del crédito
   DEFINE v_pesos_afc       DECIMAL(12,2);
   DEFINE v_aivs_afc        DECIMAL(12,2);
   DEFINE v_pesos_afc_cargo DECIMAL(12,2);
   DEFINE v_acc_afc_cargo   DECIMAL(12,2);
   DEFINE v_subcta_afc      SMALLINT;
   DEFINE v_subcta_abn      SMALLINT;
   DEFINE v_fondo_afc       SMALLINT;
   DEFINE v_mv_cargo_afc    SMALLINT;
   DEFINE v_mv_abono_afc    SMALLINT;

   ----variables para registro en proceso de devolución de saldos excedentes
   DEFINE v_id_dse_grp_devolucion DECIMAL(9,0);
   DEFINE v_num_credito           DECIMAL(10,0);
   DEFINE v_tpo_transferencia     CHAR(2);
   DEFINE v_origen_devolucion     CHAR(2);
   DEFINE v_f_movimiento          DATE;
   DEFINE v_aivs97                DECIMAL(12,2);
   DEFINE v_pesos97               DECIMAL(12,2);
   DEFINE v_edo_procesar          SMALLINT;
   DEFINE v_estado                SMALLINT;
   DEFINE v_lote                  SMALLINT;
   DEFINE v_f_proceso             DATE;

   DEFINE v_qry_ins               CHAR(1000);
   DEFINE v_tabla                 CHAR(17);

   --SET DEBUG FILE TO 'preliquidaAcrFC.trace';
   --TRACE ON;

   LET v_status_fc       = 0;

   LET v_pesos_afc       = 0;
   LET v_pesos_afc_cargo = 0;
   LET v_acc_afc_cargo   = 0;
   LET v_subcta_afc      = 49;
   LET v_fondo_afc       = 10;
   LET v_fondo_inv       = 11;
   LET v_f_liquida       = TODAY;

   LET v_f_registro      = TODAY;
   LET v_h_registro      = CURRENT;

   IF p_tpo_origina = 1 THEN
      LET v_tabla             = "cre_ta_preliquida";
      LET v_tpo_transferencia = "15";
      LET v_mv_cargo_afc      = 302;
      LET v_mv_abono_afc      = 201;
      LET v_origen            = "FORTALEC CRÉDITO ACR";
   ELIF p_tpo_origina = 4 THEN
      LET v_tabla = "cre_ag_preliquida";
      LET v_tpo_transferencia = "43";
      LET v_mv_cargo_afc      = 1222;
      LET v_mv_abono_afc      = 531;
      LET v_origen            = "FORTALEC CRÉDITO AGR";
   ELSE
      LET v_tabla = "cre_sg_preliquida";
      LET v_tpo_transferencia = "19";
      LET v_mv_cargo_afc      = 1232;
      LET v_mv_abono_afc      = 541;
      LET v_origen            = "FORTALEC CRÉDITO GRT";
   END IF

   ----Verificación de aportaciones para fortalecimiento del crédito
   SELECT SUM(monto_pesos)
     INTO v_pesos_afc
     FROM safre_viv:cta_movimiento
    WHERE id_derechohabiente = p_id_derechohabiente
      AND subcuenta          = v_subcta_afc;

   IF v_pesos_afc IS NOT NULL AND v_pesos_afc > 0 THEN
      LET v_aivs_afc        = ROUND(v_pesos_afc / p_valor_fondo,2);
      LET v_pesos_afc_cargo = v_pesos_afc * (-1);
      LET v_acc_afc_cargo   = v_pesos_afc_cargo;

      IF p_tpo_trabajador = "I" THEN
         LET v_subcta_abn   = 4;
         LET v_edo_procesar = 20;
      ELSE
         LET v_subcta_abn = 44;
         LET v_edo_procesar = 5;
      END IF

      LET v_qry_ins = " INSERT INTO safre_viv:"||v_tabla||
                               "(f_liquida,"||
                               " id_derechohabiente,"||
                               " subcuenta,"||
                               " fondo_inversion,"||
                               " movimiento,"||
                               " folio_liquida,"||
                               " id_referencia,"||
                               " monto_acciones,"||
                               " monto_pesos,"||
                               " f_valor,"||
                               " f_registro,"||
                               " h_registro,"||
                               " origen)"||
                      " VALUES( '"||v_f_liquida||"', "||
                                  p_id_derechohabiente||", "||
                                  v_subcta_afc||", "||
                                  v_fondo_afc||", "||
                                  v_mv_cargo_afc||", "||
                                  p_folio_liq||", "||
                                  p_id_cre_acreditado||", "||
                                  v_acc_afc_cargo||", "||
                                  v_pesos_afc_cargo||", "||
                             "'"||v_f_liquida||"', "||
                             "'"||v_f_registro||"', "||
                             "'"||v_h_registro||"', "||
                             "'"||v_origen||"') ";

      EXECUTE IMMEDIATE v_qry_ins;

      LET v_qry_ins = " INSERT INTO safre_viv:"||v_tabla||
                               "(f_liquida,"||
                               " id_derechohabiente,"||
                               " subcuenta,"||
                               " fondo_inversion,"||
                               " movimiento,"||
                               " folio_liquida,"||
                               " id_referencia,"||
                               " monto_acciones,"||
                               " monto_pesos,"||
                               " f_valor,"||
                               " f_registro,"||
                               " h_registro,"||
                               " origen)"||
                      " VALUES( '"||v_f_liquida||"', "||
                                  p_id_derechohabiente||", "||
                                  v_subcta_abn||", "||
                                  v_fondo_inv||", "||
                                  v_mv_abono_afc||", "||
                                  p_folio_liq||", "||
                                  p_id_cre_acreditado||", "||
                                  v_aivs_afc||", "||
                                  v_pesos_afc||", "||
                             "'"||v_f_liquida||"', "||
                             "'"||v_f_registro||"', "||
                             "'"||v_h_registro||"', "||
                             "'"||v_origen||"') ";

      EXECUTE IMMEDIATE v_qry_ins;

      LET v_id_dse_grp_devolucion = safre_viv:seq_dse_grp_devolucion.NEXTVAL;
      LET v_num_credito           = 0;
      LET v_origen_devolucion     = "02";
      LET v_f_movimiento          = "";
      LET v_aivs97                = v_aivs_afc;
      LET v_pesos97               = v_pesos_afc;
      LET v_estado                = 140;
      LET v_lote                  = 1;
      LET v_f_proceso             = TODAY;

      INSERT INTO safre_viv:dse_agrupa_devolucion
             ( id_dse_grp_devolucion,
               id_derechohabiente,
               num_credito,
               tpo_transferencia,
               origen_devolucion,
               f_movimiento,
               folio_liquida,
               aivs97,
               pesos97,
               aivs92,
               pesos92,
               monto_aportacion,
               aivs_aportacion,
               nss_separacion,
               edo_procesar,
               estado )
      VALUES ( v_id_dse_grp_devolucion,
               p_id_derechohabiente,
               v_num_credito,
               v_tpo_transferencia,
               v_origen_devolucion,
               v_f_movimiento,
               p_folio_liq,
               v_aivs97,
               v_pesos97,
               "",
               "",
               "",
               "",
               "",
               v_edo_procesar,
               v_estado );

      INSERT INTO safre_viv:dse_his_devolucion
             ( id_dse_grp_devolucion,
               folio,
               tpo_transferencia,
               lote,
               id_lote,
               f_presentacion,
               paterno_afore,
               materno_afore,
               nombre_afore,
               nom_imss,
               aivs97,
               pesos97,
               aivs92,
               pesos92,
               monto_aportacion,
               aivs_aportacion,
               nss_separacion,
               edo_procesar,
               diagnostico,
               estado,
               f_proceso )
      VALUES ( v_id_dse_grp_devolucion,
               p_folio_liq,
               v_tpo_transferencia,
               v_lote,
               p_id_cre_acreditado,
               "",
               "",
               "",
               "",
               "",
               v_aivs97,
               v_pesos97,
               "",
               "",
               "",
               "",
               "",
               v_edo_procesar,
               "",
               v_estado,
               v_f_proceso );
   ELSE
      LET v_pesos_afc       = 0;
      LET v_aivs_afc        = 0;
      LET v_pesos_afc_cargo = 0;
      LET v_acc_afc_cargo   = 0;
   END IF

   RETURN v_status_fc;

   --Finaliza la función de verificación de fortalecimiento del crédito
END FUNCTION;


