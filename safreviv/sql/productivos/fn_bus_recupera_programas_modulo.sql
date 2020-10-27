






CREATE FUNCTION "safreviv".fn_bus_recupera_programas_modulo(p_modulo_cod CHAR(3))
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          CHAR(20),
          CHAR(40);
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:09/05/2014
--===============================================================

DEFINE v_programa_cod  CHAR(20);
DEFINE v_programa_desc CHAR(40);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      
      LET v_programa_cod  = '';
      LET v_programa_desc = '';
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_programa_cod,
             v_programa_desc;
   END EXCEPTION WITH RESUME;
      
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = "";
   LET v_programa_cod  = '';
   LET v_programa_desc = '';
   
   FOREACH SELECT programa_cod,
                  programa_desc
             INTO v_programa_cod,
                  v_programa_desc
             FROM safre_af@iss_tcp:seg_programa
            WHERE modulo_cod = p_modulo_cod
            ORDER BY 1
            
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_programa_cod,
             v_programa_desc WITH RESUME;
           
   END FOREACH 
   
   
END FUNCTION;


