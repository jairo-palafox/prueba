






CREATE FUNCTION "safreviv".fn_sep_suma_aivs(v_derechohabiente DECIMAL (9,0), v_subcuenta SMALLINT) RETURNING DECIMAL(12,2) AS saldo_aivs;

DEFINE v_error_sql  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);
DEFINE saldo_final  DECIMAL(12,2);
DEFINE saldo_sum    DECIMAL(12,2);
DEFINE saldo_rest   DECIMAL(12,2);
DEFINE saldo_step   DECIMAL(12,2);
DEFINE curr_table   CHAR(50);
DEFINE v_qry        CHAR(254);
   
   ON EXCEPTION SET 	v_error_sql,
                    v_isam_error,
                    v_msg_error
   
      LET saldo_final = 0;
      RETURN saldo_final;
   END EXCEPTION

   LET saldo_sum = 0;
   LET saldo_rest = 0;
   LET saldo_step = 0;
   LET saldo_final = 0;
   
   FOREACH SELECT tabname 
   		   INTO curr_table 
           FROM systables 
		   WHERE tabname like 'cta_movimiento%' 
		   
		   --Suma abonos movimiento separacion de cuentas 381
		   LET v_qry = "SELECT NVL(SUM(monto_acciones),0) FROM "||curr_table||" WHERE subcuenta = ? AND movimiento =  381 AND id_derechohabiente = ?";
		   PREPARE prp_abonos FROM v_qry;
		   DECLARE cur_abonos CURSOR FOR prp_abonos;
		   OPEN cur_abonos USING v_subcuenta,v_derechohabiente;
		   FETCH cur_abonos INTO saldo_sum;
		   --Suma cargos movimiento separacion de cuentas 382
		   LET v_qry = "SELECT NVL(SUM(monto_acciones),0) FROM "||curr_table||" WHERE subcuenta = ? AND movimiento =  382 AND id_derechohabiente = ?";
		   PREPARE prp_cargos FROM v_qry;
		   DECLARE cur_cargos CURSOR FOR prp_cargos;
		   OPEN cur_cargos USING v_subcuenta,v_derechohabiente;
		   FETCH cur_cargos INTO saldo_rest;
		   --Calcula total de monto_pesos en esta tabla
		   LET saldo_step = saldo_sum - saldo_rest;
		   --Suma el total de esta tabla a las demás tablas
		   LET saldo_final = saldo_final + saldo_step;

           CLOSE cur_abonos;
           CLOSE cur_cargos;
		   FREE cur_abonos;
		   FREE cur_cargos;
		   FREE prp_abonos;
		   FREE prp_cargos;
		   
   END FOREACH;
   
   RETURN saldo_final;

END FUNCTION;


