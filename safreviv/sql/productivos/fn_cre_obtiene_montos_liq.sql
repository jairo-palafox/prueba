






CREATE FUNCTION "safreviv".fn_cre_obtiene_montos_liq(p_id_dh DECIMAL(9,0), p_folio DECIMAL(9,0))

   RETURNING DATE, DECIMAL(18,6), DECIMAL(14,2), DECIMAL(18,6), DECIMAL(14,2)

   DEFINE v_criterio        SMALLINT;
   DEFINE v_f_liquida       DATE;
   DEFINE v_monto_acc92     DECIMAL(18,6);
   DEFINE v_monto_pesos92   DECIMAL(14,2);
   DEFINE v_monto_acc97     DECIMAL(18,6);
   DEFINE v_monto_pesos97   DECIMAL(14,2);
   DEFINE v_tabla           VARCHAR(20);
   DEFINE v_s_qryTxt        CHAR(300);

   ---SET DEBUG FILE TO '/safreviv_int/archivos/creObtieneMontosLiq.trace';
   ---TRACE ON;

   -- se inicializa variables
   LET v_criterio      = 0;
   LET v_f_liquida     = "";
   LET v_monto_acc92   = 0;
   LET v_monto_pesos92 = 0;
   LET v_monto_acc97   = 0;
   LET v_monto_pesos97 = 0;

   EXECUTE FUNCTION fn_tab_movimiento (v_criterio,p_folio,v_f_liquida)
   INTO v_tabla;

   LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)"||
                    "   FROM "||v_tabla||
                    "  WHERE folio_liquida = "||p_folio||
                    "    AND id_derechohabiente = "||p_id_dh||
                    "    AND subcuenta = 4";

   PREPARE prp_obt_montos97 FROM v_s_qryTxt;
   DECLARE cur_obt_montos97 CURSOR FOR prp_obt_montos97;

   OPEN cur_obt_montos97;
   FETCH cur_obt_montos97 INTO v_monto_acc97, v_monto_pesos97;

   CLOSE cur_obt_montos97;
   FREE cur_obt_montos97;

   LET v_s_qryTxt = " SELECT SUM(monto_acciones), SUM(monto_pesos)"||
                    "   FROM "||v_tabla||
                    "  WHERE folio_liquida = "||p_folio||
                    "    AND id_derechohabiente = "||p_id_dh||
                    "    AND subcuenta = 8";

   PREPARE prp_obt_montos92 FROM v_s_qryTxt;
   DECLARE cur_obt_montos92 CURSOR FOR prp_obt_montos92;

   OPEN cur_obt_montos92;
   FETCH cur_obt_montos92 INTO v_monto_acc92, v_monto_pesos92;

   CLOSE cur_obt_montos92;
   FREE cur_obt_montos92;

   LET v_s_qryTxt = " SELECT f_liquida"||
                    "   FROM "||v_tabla||
                    "  WHERE folio_liquida = "||p_folio||
                    "    AND id_derechohabiente = "||p_id_dh||
                    " GROUP BY 1";

   PREPARE prp_obt_f_montos FROM v_s_qryTxt;
   DECLARE cur_obt_f_montos CURSOR FOR prp_obt_f_montos;

   OPEN cur_obt_f_montos;
   FETCH cur_obt_f_montos INTO v_f_liquida;

   CLOSE cur_obt_f_montos;
   FREE cur_obt_f_montos;

   IF v_monto_acc92 IS NULL OR v_monto_acc92 = "" THEN
      LET v_monto_acc92 = 0;
   END IF

   IF v_monto_pesos92 IS NULL OR v_monto_pesos92 = "" THEN
      LET v_monto_pesos92 = 0;
   END IF

   IF v_monto_acc97 IS NULL OR v_monto_acc97 = "" THEN
      LET v_monto_acc97 = 0;
   END IF

   IF v_monto_pesos97 IS NULL OR v_monto_pesos97 = "" THEN
      LET v_monto_pesos97 = 0;
   END IF

   RETURN v_f_liquida, v_monto_acc97, v_monto_pesos97, v_monto_acc92, v_monto_pesos92;

END FUNCTION;


