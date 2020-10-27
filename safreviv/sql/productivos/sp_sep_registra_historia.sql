






CREATE PROCEDURE "safreviv".sp_sep_registra_historia(p_entidad_nueva CHAR(40),p_entidad_ant CHAR(40), 
                                          p_usuario CHAR(20), p_entidad_his CHAR(40), 
                                          p_id_endidad_nueva DECIMAL(9,0),p_id_endidad_ant DECIMAL(9,0), 
                                          p_columna_endidad_nueva CHAR(40),p_columna_endidad_ant CHAR(40))

RETURNING SMALLINT, CHAR(3),INTEGER;


DEFINE v_valor_ent    CHAR(40);
DEFINE v_valor_ent_nv CHAR(40);
DEFINE v_sqlQry       CHAR(1024);
DEFINE v_nombre_campo CHAR(40);
DEFINE v_tabid_ant    INTEGER;
DEFINE v_tabid_tmp    INTEGER;
DEFINE v_tabid_his    INTEGER;
DEFINE v_columna_id_his   VARCHAR(128);
DEFINE v_max_id       DECIMAL(9,0);
DEFINE v_id_cat_dato_actualizado SMALLINT;

DEFINE v_id_padre_ant     VARCHAR(128);
DEFINE v_existe       SMALLINT;
DEFINE v_ind          SMALLINT;
DEFINE v_diag         CHAR(3);
DEFINE v_sql_error    INTEGER;
DEFINE v_fecha_actual DATE;
      
   ON EXCEPTION SET v_sql_error
      LET v_ind = 1;
      LET v_diag = '001';
      --TRACE 'Error: '||v_sql_error;
      RETURN v_ind, v_diag,v_sql_error;
   END EXCEPTION WITH RESUME
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_registra_historia.trace';
   --TRACE ON;
   LET v_sql_error = 0;
   -- tabla2 = entidad nueva
   -- tabla1 = endidad
   
   LET v_ind = 0;
   LET v_diag = '000';
   LET v_existe = 0;
   SELECT NVL(COUNT(*),0)
     INTO v_existe
     FROM safre_viv:sep_cat_entidad_historico
    WHERE entidad_cod = p_entidad_ant;
   -- Se valida que exista la entidad en el catálogo
   --TRACE '1';
   IF(v_existe < 1)THEN
      LET v_ind = 1;
      LET v_diag = '001';
      RETURN v_ind, v_diag,v_sql_error;
   END IF
   
   -- se recpera el id de la tabla para insertar en el histórico
   SELECT tabid
     INTO v_tabid_his
     FROM safre_viv:systables
    WHERE tabname = p_entidad_his;
   
   --TRACE '2';
   -- Recupera el campo id para insertar en el histórico
   SELECT colname
     INTO v_columna_id_his
     FROM safre_viv:syscolumns
    WHERE tabid = v_tabid_his
      AND colno = 1;
   --TRACE '3';
   -- recupera el nombre de la columna padre de la tabla histórica para el insert
   SELECT colname
     INTO v_id_padre_ant
     FROM safre_viv:syscolumns
    WHERE tabid = v_tabid_his
      AND colno = 2;
   --TRACE '4';
   -- se recupera
   --SELECT id_cat_entidad_historico
   --  INTO v_id_cat_entidad_historico
   --  FROM sep_cat_entidad_historico
   -- WHERE entidad_cod = entidad_his;
   
   -- recupera identificador de la tabla actual
   SELECT tabid 
     INTO v_tabid_ant	
     FROM safre_viv:systables
    WHERE tabname = p_entidad_ant;
   --TRACE '5';
   -- recupera identificador de la tabla temporal
   SELECT tabid 
     INTO v_tabid_tmp	
     FROM safre_tmp:systables
    WHERE tabname = p_entidad_nueva;
   --TRACE '6';
   -- se indica el proceso esta bien
   LET v_ind = 0;
   LET v_diag = '000';
     
   --FOREACH SELECT colname       INTO v_nombre_campo
   --          FROM safre_viv:syscolumns
   --         WHERE tabid = v_tabid_ant
   -- se recuperan los nombres de los campos que coinsiden entre las dos tablas, de entre tabla que 
   -- se toman datos nuevos( _nueva(safre_tmp) ) y de la tabla de la que se toman datos historicos( _ant(safre_viv) )
   FOREACH SELECT viv.colname INTO v_nombre_campo
             FROM safre_viv:syscolumns viv JOIN safre_tmp:syscolumns tmp
               ON viv.colname = tmp.colname
            WHERE viv.tabid = v_tabid_ant
              AND tmp.tabid = v_tabid_tmp
      --TRACE '7';
      LET v_valor_ent_nv = NULL;
      LET v_sqlQry = " SELECT NVL("||v_nombre_campo||",'')" ||
                     "   FROM safre_tmp:"||p_entidad_nueva||
                     "  WHERE "||p_columna_endidad_nueva||" = "||p_id_endidad_nueva;
                     
      -- Se recupera el valor de la tabla nueva(safre_tmp) para comparar con los 
      -- datos actuales (tabla_ant(safre_viv))
      PREPARE prp_recupera_ent FROM v_sqlQry;
      DECLARE cur_recupera_ent CURSOR FOR prp_recupera_ent;
      OPEN cur_recupera_ent ;
      FETCH cur_recupera_ent INTO v_valor_ent_nv;
      CLOSE cur_recupera_ent;
      FREE prp_recupera_ent;
      FREE cur_recupera_ent;
      --TRACE '8';
                                           
      LET v_valor_ent = NULL;
      LET v_sqlQry = " SELECT NVL("||v_nombre_campo||",'')"||
                     "   FROM safre_viv:"||p_entidad_ant||
                     "  WHERE "||p_columna_endidad_ant||" = "||p_id_endidad_ant;
      
      -- Se recupera el valor de la tabla anterior(safre_viv) para comparar con los 
      -- datos nuevos (tabla_nueva(safre_tmp))
      PREPARE prp_recupera_ent_nv FROM v_sqlQry;
      DECLARE cur_recupera_ent_nv CURSOR FOR prp_recupera_ent_nv;
      OPEN cur_recupera_ent_nv ;
      FETCH cur_recupera_ent_nv INTO v_valor_ent;
      CLOSE cur_recupera_ent_nv;
      FREE prp_recupera_ent_nv;
      FREE cur_recupera_ent_nv;
      --TRACE '9';
      -- Compara si los valores de las columnas cambiaron
      IF v_valor_ent <> v_valor_ent_nv THEN
         --TRACE '9.1';
         -- Si los valores cambiaron, se registra el historico de la columna
         
         -- Recupera el id de la tabla historica para insertar el nuevo registro
         LET v_sqlQry = " SELECT MAX("||v_columna_id_his||") + 1"||
                        "   FROM safre_viv:"||p_entidad_his;
         
         PREPARE prp_recupera_max_id FROM v_sqlQry;
         DECLARE cur_recupera_max_id CURSOR FOR prp_recupera_max_id;
         OPEN cur_recupera_max_id ;
         FETCH cur_recupera_max_id INTO v_max_id;
         CLOSE cur_recupera_max_id;
         FREE prp_recupera_max_id;
         FREE cur_recupera_max_id;
         --TRACE '10';
         IF v_max_id IS NULL THEN 
            LET v_max_id = 1;
         END IF
         
         
         LET v_id_cat_dato_actualizado = NULL;
         -- Se recupera el id de la tabla que lleva el control de las columnas registrados para actualizar
         
         SELECT id_cat_dato_actualizado
           INTO v_id_cat_dato_actualizado
           FROM safre_viv:sep_cat_dato_actualizado da JOIN safre_viv:sep_cat_entidad_historico eh
             --jdym ON eh.entidad_cod = p_entidad_ant
             ON eh.entidad_cod = p_entidad_his
            AND da.id_cat_entidad_historico = eh.id_cat_entidad_historico
          WHERE da.cve_natural = v_nombre_campo ;
         
         -- Si existe la columna recuperada en sep_cat_dato_actualizado, se registra el historico
         -- La relacion es: 
         -- columna(v_nombre_campo) en sep_cat_dato_actualizado(referencia de column de la tabla p_entidad_ant)
         -- para la tabla p_entidad_ant registrada en sep_cat_entidad_historico
         IF(v_id_cat_dato_actualizado IS NOT NULL)THEN
            --TRACE '11';
            -- Actualiza la tabla de safre_viv con el nuevo valor
            LET v_sqlQry = " UPDATE safre_viv:"||TRIM(p_entidad_ant)||
                           "          SET "||v_nombre_campo||" = '"||TRIM(v_valor_ent_nv)||"'"||
                           "        WHERE "||p_columna_endidad_ant||" = "||p_id_endidad_ant;
                           
            --TRACE '11.1: '||v_sqlQry;
            
            EXECUTE IMMEDIATE v_sqlQry;
            LET v_fecha_actual = TODAY;
            LET v_sqlQry = " INSERT INTO safre_viv:"||TRIM(p_entidad_his)||
                           " ( "||v_columna_id_his||","||
                           "   "||v_id_padre_ant||","||
                           "   id_cat_dato_actualizado,"||
                           "   f_modificacion,"||
                           "   valor_modificado,"||
                           "   valor_actual,"||
                           "   usuario)"||
                           " VALUES("||v_max_id||","||
                           " "||p_id_endidad_ant||","||
                           " "||v_id_cat_dato_actualizado||","||
                           " '"||v_fecha_actual||"',"||
                           " '"||TRIM(v_valor_ent)||"',"||
                           " '"||TRIM(v_valor_ent_nv)||"',"||
                           " '"||TRIM(p_usuario)||"')";
            --TRACE '11.1: '||v_sqlQry;
            EXECUTE IMMEDIATE v_sqlQry;
            --TRACE '12';
         END IF
      END IF
   END FOREACH

   RETURN v_ind, v_diag,v_sql_error;
END PROCEDURE
;


