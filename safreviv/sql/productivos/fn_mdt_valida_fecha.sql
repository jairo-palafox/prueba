






CREATE FUNCTION "safreviv".fn_mdt_valida_fecha(v_cad_fecha CHAR(8))
RETURNING INTEGER,CHAR(254),DATE;

DEFINE v_fecha DATE;

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      LET v_fecha = NULL;
      RETURN v_error_sql,v_msg_sql,v_fecha;
   END EXCEPTION WITH RESUME;
   
   LET v_fecha = NULL;
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   LET v_fecha = v_cad_fecha;
   
   RETURN v_error_sql,v_msg_sql,v_fecha;
END FUNCTION;


