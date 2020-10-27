






CREATE FUNCTION "safreviv".fn_sep_valida_fecha(v_cad_fecha CHAR(8))
RETURNING INTEGER,CHAR(254),DATE;

DEFINE v_fecha DATE;

DEFINE v_anio  CHAR(4);
DEFINE v_mes   CHAR(2);
DEFINE v_dia   CHAR(2);
--DEFINE v_fecha_val   CHAR(8);
DEFINE v_fecha_val   DATE; 

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      LET v_fecha = NULL;
      RETURN v_error_sql,v_msg_sql,v_fecha;
   END EXCEPTION WITH RESUME;
   
   LET v_anio = v_cad_fecha[1,4];
   LET v_mes  = v_cad_fecha[5,6];
   LET v_dia  = v_cad_fecha[7,8];

   --LET v_fecha_val = v_mes||v_dia||v_anio;
   LET v_fecha_val = mdy(v_mes,v_dia,v_anio);
   
   LET v_fecha = NULL;
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   --LET v_fecha = v_cad_fecha;
   LET v_fecha = v_fecha_val;
   
   RETURN v_error_sql,v_msg_sql,v_fecha;
END FUNCTION;


