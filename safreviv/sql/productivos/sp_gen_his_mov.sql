






CREATE PROCEDURE "safreviv".sp_gen_his_mov(p_id_derechohabiente DECIMAL(9,0))

   DEFINE v_sel_his           LVARCHAR(1000);
   DEFINE v_sel_act           VARCHAR(255);
   DEFINE v_nombre_tabla      VARCHAR(20)  ;
   DEFINE v_existe_his        SMALLINT;



   DROP TABLE IF EXISTS tmp_movimiento;

   LET v_existe_his = 0 ;
   LET v_sel_his = " ";

   FOREACH
       SELECT tabla
         INTO v_nombre_tabla
         FROM cat_tab_movimiento

       LET v_sel_his = v_sel_his || " SELECT * "||
                      " FROM "|| v_nombre_tabla ||
                      " WHERE id_derechohabiente = "||p_id_derechohabiente||
                      " AND movimiento NOT IN (1099)"||
                      " UNION ALL ";

       LET v_existe_his = 1 ;

   END FOREACH

   LET v_sel_act = " SELECT * "||
                   "   FROM cta_movimiento "||
                   "  WHERE id_derechohabiente = "|| p_id_derechohabiente||
                   "    AND movimiento NOT IN (1099) "||
                   "  INTO TEMP tmp_movimiento " ;

   IF v_existe_his = 1 THEN
      LET v_sel_his = v_sel_his || v_sel_act ;
   ELSE
      LET v_sel_his = v_sel_act ;
   END IF

   EXECUTE IMMEDIATE v_sel_his ;

   CREATE INDEX tmp_movimiento_ix1 ON tmp_movimiento
   (f_liquida
    );

    UPDATE STATISTICS FOR TABLE tmp_movimiento ;

END PROCEDURE

;


