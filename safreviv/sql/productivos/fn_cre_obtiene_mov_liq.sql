






CREATE FUNCTION "safreviv".fn_cre_obtiene_mov_liq(p_id_dh DECIMAL(9,0), p_folio DECIMAL(9,0))

   RETURNING DECIMAL(18,6), DECIMAL(14,2)

   DEFINE v_criterio        SMALLINT;
   DEFINE v_f_liquida       DATE;
   DEFINE v_monto_acciones  DECIMAL(18,6);
   DEFINE v_monto_pesos     DECIMAL(14,2);
   DEFINE v_tabla           VARCHAR(20);
   DEFINE v_s_qryTxt        CHAR(300);

   --SET DEBUG FILE TO '/safreviv_int/archivos/creObtieneMov.trace';
   --TRACE ON;

   -- se inicializa variables
   LET v_criterio       = 0;
   LET v_f_liquida      = "";
   LET v_monto_acciones = 0;
   LET v_monto_pesos    = 0;

   EXECUTE FUNCTION fn_tab_movimiento (v_criterio,p_folio,v_f_liquida)
   INTO v_tabla;

   LET v_s_qryTxt = " SELECT ABS(SUM(monto_acciones)), ABS(SUM(monto_pesos))"||
                    "   FROM "||v_tabla||
                    "  WHERE id_derechohabiente = "||p_id_dh||
                    "    AND folio_liquida = "||p_folio;

   PREPARE prp_obt_montos FROM v_s_qryTxt;
   DECLARE cur_obt_montos CURSOR FOR prp_obt_montos;
   OPEN cur_obt_montos;
   FETCH cur_obt_montos INTO v_monto_acciones, v_monto_pesos;
   CLOSE cur_obt_montos;
   FREE cur_obt_montos;

   IF v_monto_acciones IS NULL OR v_monto_acciones = "" THEN
      LET v_monto_acciones = 0;
      LET v_monto_pesos    = 0;
   END IF

   RETURN v_monto_acciones, v_monto_pesos;

END FUNCTION
;


