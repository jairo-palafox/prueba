






CREATE FUNCTION "safreviv".fn_valida_numerico(v_cad_numero CHAR(18))
RETURNING INTEGER,CHAR(254),DECIMAL(18);

DEFINE v_numero DECIMAL(18);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      LET v_numero = NULL;
      RETURN v_error_sql,v_msg_sql,v_numero;
   END EXCEPTION WITH RESUME;
   
   LET v_numero = NULL;
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   LET v_numero = v_cad_numero;
   
   RETURN v_error_sql,v_msg_sql,v_numero;
END FUNCTION;


