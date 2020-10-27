






CREATE FUNCTION "safreviv".fn_sep_fliquida_mov(v_derechohabiente DECIMAL (9,0), v_subcuenta SMALLINT) RETURNING DATE AS f_liquida;

DEFINE v_error_sql  	INTEGER;
DEFINE v_isam_error 	INTEGER;
DEFINE v_msg_error  	CHAR(254);
DEFINE f_liquida_step   DATE;
DEFINE f_liquida_final  DATE;
DEFINE curr_table   CHAR(50);
DEFINE v_qry		CHAR(254);
   
   ON EXCEPTION SET 	v_error_sql,
                    v_isam_error,
                    v_msg_error
   
      LET f_liquida_final = "";
      RETURN f_liquida_final;
   END EXCEPTION

   LET f_liquida_final = "01/01/0001";
   
   FOREACH SELECT tabname 
   		   INTO curr_table 
           FROM systables 
		   WHERE tabname like 'cta_movimiento%' 
		   
		   --Obtiene la fecha mas reciente para la subcuenta en cuestion sobre la tabla actual
		   LET v_qry = "SELECT FIRST 1 f_liquida FROM "||curr_table||" WHERE subcuenta = ? AND id_derechohabiente = ? ORDER BY f_liquida DESC";
		   PREPARE prp_fecha FROM v_qry;
		   DECLARE cur_fecha CURSOR FOR prp_fecha;
		   OPEN cur_fecha USING v_subcuenta,v_derechohabiente;
		   FETCH cur_fecha INTO f_liquida_step;
		   
		   IF f_liquida_step IS NOT NULL THEN
		      IF f_liquida_step > f_liquida_final THEN
		         LET f_liquida_final = f_liquida_step;
			  END IF;
		   END IF;
		   
           CLOSE cur_fecha;
		   FREE cur_fecha;
		   FREE prp_fecha;
		   
   END FOREACH;
   
   IF f_liquida_final == "01/01/0001" THEN
      LET f_liquida_final = "";
   END IF;
   
   RETURN f_liquida_final;

END FUNCTION;


