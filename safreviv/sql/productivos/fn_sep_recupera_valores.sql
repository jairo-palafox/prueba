






CREATE FUNCTION "safreviv".fn_sep_recupera_valores(v_etiqueta CHAR(40),p_entidad CHAR(40),
                                        v_columna_id CHAR(40),p_id_registro_entidad DECIMAL(9,0))
--RETURNING SMALLINT,CHAR(3),CHAR(40), VARCHAR(200);
RETURNING VARCHAR(200);

DEFINE v_sqlQry     CHAR(1024);
DEFINE v_valor     VARCHAR(200);

   LET v_valor = '';
   LET v_sqlQry = 'SELECT '||v_etiqueta||
                  '  FROM safre_viv:'||p_entidad||
                  ' WHERE '||v_columna_id||' = '||p_id_registro_entidad;
      
   PREPARE prp_recupera_valor FROM v_sqlQry;
   DECLARE cur_recupera_valor CURSOR FOR prp_recupera_valor;
   OPEN cur_recupera_valor ;
   FETCH cur_recupera_valor INTO v_valor;
   --EXECUTE v_sqlQry INTO v_valor;
            
   IF(SQLCODE == 0)THEN
      RETURN v_valor;
   END IF
    
   CLOSE cur_recupera_valor ;
   FREE cur_recupera_valor;
   
END FUNCTION;


