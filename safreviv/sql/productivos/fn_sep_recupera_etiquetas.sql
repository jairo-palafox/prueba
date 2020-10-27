






CREATE FUNCTION "safreviv".fn_sep_recupera_etiquetas(p_entidad CHAR(40), p_id_registro_entidad DECIMAL(9,0))
--RETURNING SMALLINT,CHAR(3),CHAR(40), VARCHAR(200);
RETURNING CHAR(40), VARCHAR(200);

DEFINE v_sqlQry     CHAR(1024);
DEFINE v_ind        SMALLINT;
DEFINE v_diag       CHAR(3);
DEFINE v_columna_id CHAR(40);
DEFINE v_tabid      INTEGER;
DEFINE v_etiqueta   CHAR(40);
DEFINE v_cve_natural CHAR(40);
DEFINE v_id_cat_dato_actualizado CHAR(40);
DEFINE v_columna_tipo  smallint;
DEFINE v_valor_dec  DECIMAL(22,2);
DEFINE v_resta      DECIMAL(4,2);

--DEFINE v_f_modificacion   DATE;
--DEFINE v_valor_modificado CHAR(40);
--DEFINE v_valor_actual     CHAR(40);

DEFINE v_valor       VARCHAR(200);
DEFINE v_valor_desc  VARCHAR(200);
DEFINE v_descripcion VARCHAR(200);
DEFINE v_usuario     CHAR(18);
DEFINE v_sql_error   INTEGER;
DEFINE v_existe      SMALLINT;


   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      -- Imprime el codigo de error
      --TRACE 'Ocurrio el error:'||v_sql_error;
      LET v_ind = 1;
      LET v_diag = '001';
      LET v_etiqueta = '';
      --LET v_f_modificacion = TODAY;
      --LET v_valor_modificado = '';
      --LET v_valor_actual = '';
      LET v_valor = '';
      LET v_usuario = '';
      --RETURN v_ind, v_diag, v_etiqueta, v_valor;
      RETURN v_etiqueta, v_valor;
   END EXCEPTION WITH RESUME;
   LET v_etiqueta = '';
   --LET v_f_modificacion = TODAY;
   --LET v_valor_modificado = '';
   --LET v_valor_actual = '';
   LET v_valor = '';
   LET v_usuario = '';

   --Se habilita el LOG del SP
  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_recupera_etiquetas.trace';

   SELECT NVL(COUNT(*),0)
     INTO v_existe
     FROM safre_viv:sep_cat_entidad_historico
    WHERE entidad_cod = p_entidad;
   
   -- Se valida que exista la entidad en el catálogo
   IF(v_existe < 1)THEN -- no se encontró el registro
      
      LET v_ind = 1;
      LET v_diag = '001';
      --RETURN v_ind, v_diag, v_etiqueta, v_valor;
      RETURN v_etiqueta, v_valor;
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
   
   LET v_sqlQry = "SELECT cd.etiqueta,cd.cve_natural,cd.id_cat_dato_actualizado"||
                  "  FROM safre_viv:sep_cat_dato_actualizado cd JOIN safre_viv:sep_cat_entidad_historico et"||
                  "    ON cd.id_cat_entidad_historico = et.id_cat_entidad_historico"|| 
                  " WHERE et.entidad_cod = '"||p_entidad||"'"||" order by cd.id_cat_dato_actualizado ";
--trace "sqlqry: "||v_sqlQry;
   PREPARE prp_recupera_etiqueta FROM v_sqlQry;
   DECLARE cur_recupera_etiqueta CURSOR FOR prp_recupera_etiqueta;
   OPEN cur_recupera_etiqueta ;
   LET v_ind = 0;
   LET v_diag = '000';
      
   WHILE(SQLCODE == 0)
   
      FETCH cur_recupera_etiqueta INTO v_etiqueta,v_cve_natural,v_id_cat_dato_actualizado ;

--trace "v_etiqueta: "||v_etiqueta;
--trace "p_entidad: "||p_entidad;
--trace "p_id_registro: "||p_id_registro_entidad;
--trace "p_id_registro_entidad: "||p_id_registro_entidad;

   
      EXECUTE FUNCTION fn_sep_recupera_valores(v_cve_natural,p_entidad,v_columna_id,
                                               p_id_registro_entidad) 
                      INTO v_valor;
              
      IF(SQLCODE == 0)THEN
         --RETURN v_ind, v_diag, v_etiqueta, v_valor;

          LET v_valor_desc = "'"||v_valor||"'";

          EXECUTE FUNCTION fn_sep_recupera_descripciones(v_id_cat_dato_actualizado,v_valor_desc,v_valor)
                      INTO v_descripcion;

          IF v_valor <> v_descripcion THEN
             LET v_valor = v_valor||" "||v_descripcion;
          ELSE 
             LET v_valor = v_descripcion;
          END IF;


          SELECT coltype
            INTO v_columna_tipo
            FROM safre_viv:syscolumns
           WHERE tabid = v_tabid
             AND colname = v_cve_natural;

             IF v_columna_tipo = 7 THEN 
       
                LET v_valor = v_valor[4,5]||"-"||v_valor[1,2]||"-"||v_valor[7,10];
       
             END IF;


             IF v_columna_tipo = 5    OR 
                v_columna_tipo = 261  THEN 
             
               LET v_valor_dec = v_valor;
               LET v_resta     = 0      ;
               LET v_resta     = v_valor_dec - TRUNC(v_valor_dec);
               IF v_resta <> 0 THEN
                  LET v_valor = TO_CHAR(v_valor_dec,"-###,##&.&&") ;
               END IF
             END IF

         RETURN v_etiqueta, v_valor WITH RESUME;
      END IF

   END WHILE
   CLOSE cur_recupera_etiqueta ;
   FREE cur_recupera_etiqueta;
   
   --CLOSE cur_recupera_valor ;
   --FREE cur_recupera_valor;
   
END FUNCTION
;


