






CREATE FUNCTION "safreviv".fn_sep_recupera_descripciones(p_id_cat_dato_actualizado DEC(9,0),p_valor_desc VARCHAR(200),
p_valor VARCHAR(200))

RETURNING VARCHAR(200);

DEFINE v_sqlQry      CHAR(1024);
DEFINE v_valor       VARCHAR(200);
DEFINE v_existe      SMALLINT;

DEFINE v_descripcion VARCHAR(200);
DEFINE v_entidad_catalogo    CHAR(40);
DEFINE v_entidad_llave       CHAR(40);
DEFINE v_entidad_descripcion CHAR(40);

  -- SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_recupera_valores.trace';


-- Se busca la entrada para obtener los parametros para obtener la descripcion de los
-- catalogos


   SELECT NVL(COUNT(*),0)
   INTO   v_existe
   FROM   sep_cat_entidad_catalogo a
   WHERE  a.id_cat_dato_actualizado = p_id_cat_dato_actualizado;

   IF (v_existe < 1 OR p_valor_desc IS NULL) THEN   -- no se encuentra registro de descripcion en catalogo

      RETURN p_valor;  -- se regresa el mismo valor

   ELSE 

       SELECT a.entidad_catalogo     ,
              a.entidad_llave        ,
              a.entidad_descripcion
       INTO   v_entidad_catalogo     ,
              v_entidad_llave        ,
              v_entidad_descripcion
       FROM   sep_cat_entidad_catalogo a
       WHERE  a.id_cat_dato_actualizado = p_id_cat_dato_actualizado;
    

       LET v_descripcion = '';
       LET v_sqlQry = 'SELECT '||v_entidad_descripcion||
                      '  FROM safre_viv:'||v_entidad_catalogo||
                      ' WHERE '||v_entidad_llave||' = '||p_valor_desc;
     
--trace "qry recupera: "||v_sqlQry;
 
   PREPARE prp_recupera_desc FROM v_sqlQry;
   DECLARE cur_recupera_desc CURSOR FOR prp_recupera_desc;
   OPEN cur_recupera_desc ;
   FETCH cur_recupera_desc INTO v_descripcion;
   --EXECUTE v_sqlQry INTO v_valor;
            
   IF(SQLCODE == 0)THEN
      RETURN v_descripcion;
   END IF
    
   CLOSE cur_recupera_desc ;
   FREE cur_recupera_desc;
   END IF; 
END FUNCTION;


