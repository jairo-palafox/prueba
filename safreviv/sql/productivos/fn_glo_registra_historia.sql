






CREATE FUNCTION "safreviv".fn_glo_registra_historia(p_entidad_nueva         CHAR(40),
                                         p_entidad_ant           CHAR(40), 
                                         p_usuario               CHAR(20), 
                                         p_entidad_his           CHAR(40), 
                                         p_id_endidad_nueva      DECIMAL(9,0),
                                         p_id_endidad_ant        DECIMAL(9,0), 
                                         p_columna_endidad_nueva CHAR(40),
                                         p_columna_endidad_ant   CHAR(40))

RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100);


DEFINE v_valor_col_ent_ant         CHAR(40);
DEFINE v_valor_col_ent_nvo         CHAR(40);
DEFINE v_consulta                  CHAR(1024);
DEFINE v_nombre_campo              CHAR(40);
DEFINE v_id_entidad_anterior       INTEGER;
DEFINE v_id_entidad_temporal       INTEGER;
DEFINE v_id_tabla_historica        INTEGER;
DEFINE v_columna_entidad_historica VARCHAR(128);
DEFINE v_max_id_col_his            INTEGER;
DEFINE v_id_entidad_etiqueta       INTEGER;

DEFINE v_id_padre_ant VARCHAR(128);
DEFINE v_existe       SMALLINT;
DEFINE v_fecha_actual DATE;

DEFINE v_ind          SMALLINT;
DEFINE v_diag         CHAR(3);
DEFINE v_sql_error    INTEGER;
DEFINE v_isam_error   INTEGER;
DEFINE v_msg_error    CHAR(100);
      
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind = 1;
      LET v_diag = '001';
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error;
   END EXCEPTION WITH RESUME
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_sep_registra_historia.trace';
   
   
   -- tabla2 = entidad nueva
   -- tabla1 = endidad
   
   LET v_ind        = 0;
   LET v_diag       = '000';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = " ";
   
   LET v_existe    = 0;
   SELECT FIRST 1 1 --NVL(COUNT(*),0)
     INTO v_existe
     FROM glo_entidad_historico_consulta --sep_cat_entidad_historico
    WHERE entidad_cod = p_entidad_ant;
    
   -- Se valida que exista la entidad en el catálogo
   IF(v_existe = 0 OR v_existe IS NULL)THEN
      LET v_ind       = 1;
      LET v_diag      = '001';
      LET v_sql_error = 100;
      LET v_msg_error = 'No se encontró registro de entidad histórica';
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error;
   END IF
   
   -- se recpera el id de la tabla para recuperar el nombre de la columna principal de la tabla histórica
   SELECT tabid
     INTO v_id_tabla_historica
     FROM systables
    WHERE tabname = p_entidad_his;
   
   -- Recupera el campo id para insertar en el histórico
   SELECT colname
     INTO v_columna_entidad_historica
     FROM syscolumns
    WHERE tabid = v_id_tabla_historica
      AND colno = 1;
   
   -- recupera el nombre de la columna padre de la tabla histórica para el insert
   SELECT colname
     INTO v_id_padre_ant
     FROM syscolumns
    WHERE tabid = v_id_tabla_historica
      AND colno = 2;
   
   -- recupera identificador de la tabla actual
   SELECT tabid 
     INTO v_id_entidad_anterior	
     FROM systables
    WHERE tabname = p_entidad_ant;
   
   -- recupera identificador de la tabla temporal
   SELECT tabid 
     INTO v_id_entidad_temporal	
     FROM safre_tmp:systables
    WHERE tabname = p_entidad_nueva;
   
   -- se recuperan los nombres de los campos que coinsiden entre las dos tablas, de entre tabla que 
   -- se toman datos nuevos( _nueva(safre_tmp) ) y de la tabla de la que se toman datos historicos( _ant(safre_viv) )
   FOREACH SELECT viv.colname 
             INTO v_nombre_campo
             FROM syscolumns viv JOIN safre_tmp:syscolumns tmp
               ON viv.colname = tmp.colname
            WHERE viv.tabid = v_id_entidad_anterior
              AND tmp.tabid = v_id_entidad_temporal
      
      LET v_valor_col_ent_nvo = NULL;
      LET v_consulta = " SELECT "||v_nombre_campo|| 
                       "   FROM safre_tmp:"||p_entidad_nueva||
                       "  WHERE "||p_columna_endidad_nueva||" = "||p_id_endidad_nueva;
                     
      -- Se recupera el valor de la tabla nueva(safre_tmp) para comparar con los 
      -- datos actuales (tabla_ant(safre_viv))
      PREPARE prp_recupera_val_col_nvo FROM v_consulta;
      DECLARE cur_recupera_val_col_nvo CURSOR FOR prp_recupera_val_col_nvo;
      OPEN cur_recupera_val_col_nvo ;
      FETCH cur_recupera_val_col_nvo INTO v_valor_col_ent_nvo;
      CLOSE cur_recupera_val_col_nvo;
      FREE prp_recupera_val_col_nvo;
      FREE cur_recupera_val_col_nvo;
                                           
      LET v_valor_col_ent_ant = NULL;
      LET v_consulta = " SELECT "||v_nombre_campo||
                       "   FROM "||p_entidad_ant||
                       "  WHERE "||p_columna_endidad_ant||" = "||p_id_endidad_ant;
      
      -- Se recupera el valor de la tabla anterior(safre_viv) para comparar con los 
      -- datos nuevos (tabla_nueva(safre_tmp))
      PREPARE prp_recupera_val_col_ant FROM v_consulta;
      DECLARE cur_recupera_val_col_ant CURSOR FOR prp_recupera_val_col_ant;
      OPEN cur_recupera_val_col_ant ;
      FETCH cur_recupera_val_col_ant INTO v_valor_col_ent_ant;
      CLOSE cur_recupera_val_col_ant;
      FREE prp_recupera_val_col_ant;
      FREE cur_recupera_val_col_ant;
      
      -- Compara si los valores de las columnas cambiaron
      IF v_valor_col_ent_ant <> v_valor_col_ent_nvo THEN
         
         -- Si los valores cambiaron, se registra el historico de la columna
         
         -- Recupera el id de la tabla historica para insertar el nuevo registro
         LET v_consulta = " SELECT MAX("||v_columna_entidad_historica||") + 1"||
                          "   FROM "||p_entidad_his;
         
         PREPARE prp_recupera_max_id FROM v_consulta;
         DECLARE cur_recupera_max_id CURSOR FOR prp_recupera_max_id;
         OPEN cur_recupera_max_id ;
         FETCH cur_recupera_max_id INTO v_max_id_col_his;
         CLOSE cur_recupera_max_id;
         FREE prp_recupera_max_id;
         FREE cur_recupera_max_id;
         
         IF v_max_id_col_his IS NULL THEN 
            LET v_max_id_col_his = 1;
         END IF
         
         
         LET v_id_entidad_etiqueta = NULL;
         -- Se recupera el id de la tabla que lleva el control de las columnas registrados para actualizar
         
         SELECT ee.id_entidad_etiqueta
           INTO v_id_entidad_etiqueta
           FROM glo_entidad_etiqueta ee JOIN glo_entidad_historico_consulta eh
             ON eh.entidad_cod = p_entidad_ant
             --ON eh.entidad_cod = p_entidad_his
            AND ee.id_entidad_historico = eh.id_entidad_historico
          WHERE ee.cve_natural = v_nombre_campo ;
         
         -- Si existe la columna recuperada en sep_cat_dato_actualizado, se registra el historico
         -- La relacion es: 
         -- columna(v_nombre_campo) en glo_entidad_etiqueta(referencia de column de la tabla p_entidad_ant)
         -- para la tabla p_entidad_ant registrada en glo_entidad_historico_consulta
         IF(v_id_entidad_etiqueta IS NOT NULL)THEN
            
            -- Actualiza la tabla de safre_viv con el nuevo valor
            LET v_consulta = " UPDATE "||TRIM(p_entidad_ant)||
                             "    SET "||v_nombre_campo||" = '"||TRIM(v_valor_col_ent_nvo)||"'"||
                             "  WHERE "||p_columna_endidad_ant||" = "||p_id_endidad_ant;
            
            EXECUTE IMMEDIATE v_consulta;
            LET v_fecha_actual = TODAY;
            -- Inserta el registro histórico de la actualización
            LET v_consulta = " INSERT INTO "||TRIM(p_entidad_his)||
                             " ( "||v_columna_entidad_historica||","||
                             "   "||v_id_padre_ant||","||
                             "   id_cat_dato_actualizado,"||
                             "   f_modificacion,"||
                             "   valor_modificado,"||
                             "   valor_actual,"||
                             "   usuario)"||
                             " VALUES("||v_max_id_col_his||","||
                             " "||p_id_endidad_ant||","||
                             " "||v_id_entidad_etiqueta||","||
                             " '"||v_fecha_actual||"',"||
                             " '"||TRIM(v_valor_col_ent_ant)||"',"||
                             " '"||TRIM(v_valor_col_ent_nvo)||"',"||
                             " '"||TRIM(p_usuario)||"')";
            
            EXECUTE IMMEDIATE v_consulta;
            
         END IF
      END IF 
   END FOREACH
   
   RETURN v_ind, 
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error;
END FUNCTION
;


