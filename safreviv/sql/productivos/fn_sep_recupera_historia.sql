






CREATE FUNCTION "safreviv".fn_sep_recupera_historia(p_entidad CHAR(40), p_id_registro_entidad INTEGER)
RETURNING SMALLINT,CHAR(3),CHAR(40), DATE, CHAR(40), CHAR(40),CHAR(18);

DEFINE v_sqlQry     CHAR(1024);
DEFINE v_ind        SMALLINT;
DEFINE v_diag       CHAR(3);
DEFINE v_columna_id CHAR(40);
DEFINE v_tabid      INTEGER;
DEFINE v_etiqueta   CHAR(40);
DEFINE v_f_modificacion   DATE;
DEFINE v_valor_modificado CHAR(40);
DEFINE v_valor_actual     CHAR(40);
DEFINE v_usuario   CHAR(18);
DEFINE v_sql_error INTEGER;
DEFINE v_existe    SMALLINT;

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      --TRACE 'Ocurrio el error:'||v_sql_error;
      LET v_ind = 1;
      LET v_diag = '001';
      LET v_etiqueta = '';
      LET v_f_modificacion = TODAY;
      LET v_valor_modificado = '';
      LET v_valor_actual = '';
      LET v_usuario = '';
      RETURN v_ind, v_diag, v_etiqueta, v_f_modificacion, v_valor_modificado, v_valor_actual, v_usuario;
   END EXCEPTION WITH RESUME;
   LET v_etiqueta = '';
   LET v_f_modificacion = TODAY;
   LET v_valor_modificado = '';
   LET v_valor_actual = '';
   LET v_usuario = '';

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_recupera_historia.trace';

   SELECT NVL(COUNT(*),0)
     INTO v_existe
     FROM safre_viv:sep_cat_entidad_historico
    WHERE entidad_cod = p_entidad;
   
   -- Se valida que exista la entidad en el catálogo
   IF(v_existe < 1)THEN -- no se encontró el registro
      
      LET v_ind = 1;
      LET v_diag = '001';
      RETURN v_ind, v_diag, v_etiqueta, v_f_modificacion, v_valor_modificado, v_valor_actual, v_usuario;
   END IF
   
   -- Recuper identificador de la tabla para recuperar el nombre del campo id de esa tabla
   SELECT tabid
     INTO v_tabid
     FROM safre_viv:systables
    WHERE tabname =p_entidad;
   
   
   -- Recupera el campo id 
   SELECT colname
     INTO v_columna_id
     FROM safre_viv:syscolumns
    WHERE tabid = v_tabid
      AND colno = 1;
   
   LET v_sqlQry = 'SELECT cd.etiqueta, et.f_modificacion, et.valor_modificado,'||
                  '       et.valor_actual, et.usuario'||
                  '  FROM safre_viv:sep_cat_dato_actualizado cd JOIN safre_viv:'||p_entidad||' et'||
                  '    ON et.id_cat_dato_actualizado = cd.id_cat_dato_actualizado'|| 
                  ' WHERE '||v_columna_id||' = '||p_id_registro_entidad ;
   
   PREPARE prp_recupera FROM v_sqlQry;
   DECLARE cur_recupera CURSOR FOR prp_recupera;
   OPEN cur_recupera ;
   LET v_ind = 0;
   LET v_diag = '000';
   
   WHILE(SQLCODE == 0)
   
      FETCH cur_recupera INTO v_etiqueta, v_f_modificacion, v_valor_modificado,
                              v_valor_actual, v_usuario;
      IF(SQLCODE == 0)THEN
         
         RETURN v_ind, v_diag, v_etiqueta, v_f_modificacion, v_valor_modificado, v_valor_actual, v_usuario;
      END IF
   END WHILE
   CLOSE cur_recupera ;
   FREE cur_recupera;
   
END FUNCTION;


