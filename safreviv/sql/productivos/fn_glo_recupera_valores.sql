






CREATE FUNCTION "safreviv".fn_glo_recupera_valores(v_etiqueta            CHAR(40),
                                        p_entidad             CHAR(40),
                                        v_id_columna          CHAR(40),
                                        p_id_registro_entidad DECIMAL(9,0))
RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100),
          VARCHAR(200) ;
          
DEFINE v_consulta   CHAR(1024);
DEFINE v_valor      VARCHAR(200);

DEFINE v_ind        SMALLINT;   -- idicador de error
DEFINE v_diag       CHAR(3);    -- diagnostico de error
DEFINE v_sql_error  INTEGER; 
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(100);
 
   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind   = 0;
      LET v_diag  = '000';
      LET v_valor = "";
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_valor;
             
   END EXCEPTION;
   
   LET v_ind        = 0;
   LET v_diag       = '000';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = " ";
   LET v_valor      = '';

   LET v_valor = '';
   LET v_consulta = 'SELECT '||v_etiqueta||
                    '  FROM '||p_entidad||
                    ' WHERE '||v_id_columna||' = '||p_id_registro_entidad;
      
   PREPARE prp_recupera_valor FROM v_consulta;
   DECLARE cur_recupera_valor CURSOR FOR prp_recupera_valor;
   OPEN cur_recupera_valor ;
   FETCH cur_recupera_valor INTO v_valor;
 
   CLOSE cur_recupera_valor ;
   FREE cur_recupera_valor;
 
   IF(v_valor IS NULL)THEN
      LET v_ind        = 1;
      LET v_diag       = '001';
      LET v_sql_error  = 100;
      LET v_msg_error  = "No se encontró valor de "||v_etiqueta;
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_valor; 
   END IF
     
   RETURN v_ind, 
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error,
          v_valor;   
   
END FUNCTION;


