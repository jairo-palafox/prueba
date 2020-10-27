






CREATE FUNCTION "safreviv".fn_prt_recupera_etiquetas_bloque(p_entidad             CHAR(40), -- Entidad principal de la cual se obtendran etiquetas y valores
                                                 p_id_cat_bus          DECIMAL(9,0), -- identificador de contrato activo
                                                 p_id_registro_entidad DECIMAL(9,0), -- identificador de la tabla que contiene los valores
                                                 p_nombre_col_campo    VARCHAR(50), -- nombre de la columna que contiene los nombres de los campos
                                                 p_nombre_col_valor    VARCHAR(50), -- nombre de la columna que contiene los valores de los campos
                                                 p_entidad_catalogo    VARCHAR(50),
                                                 p_nombre_col_cve_natural VARCHAR(50))
										  
RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(1024),
          VARCHAR(50),
          VARCHAR(200),
          DECIMAL(9,0),
          DECIMAL(9,0);

DEFINE v_consulta            CHAR(1024);
DEFINE v_existe              SMALLINT;
DEFINE v_nombre_columna      VARCHAR(50);
DEFINE v_nombre_col_id_padre VARCHAR(50);
DEFINE v_nombre_col_orden    VARCHAR(50);
DEFINE v_nombre_columna_id   VARCHAR(50);
DEFINE v_id_tabla            INTEGER;
DEFINE v_id_tabla_catalogo   INTEGER;
DEFINE v_etiqueta            VARCHAR(50);
DEFINE v_id_col_entidad      DECIMAL(9,0);
DEFINE v_id_col_cat_entidad  DECIMAL(9,0);
DEFINE r_valor               VARCHAR(200);

DEFINE v_ind                 SMALLINT;   -- idicador de error
DEFINE v_diag                CHAR(3);    -- diagnostico de error
DEFINE v_sql_error           INTEGER; 
DEFINE v_isam_error          INTEGER;
DEFINE v_msg_error           CHAR(1024);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind         = 0;
      LET v_diag        = '000';
      LET v_etiqueta   = '';
      LET r_valor      = '';
      LET v_id_col_entidad = 0;
      LET v_id_col_cat_entidad = 0;
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_etiqueta,
             r_valor,
             v_id_col_entidad,
             v_id_col_cat_entidad;
             
   END EXCEPTION;
   
   LET v_ind        = 0;
   LET v_diag       = '000';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = " ";
   LET v_etiqueta   = '';
   LET r_valor      = '';
   LET v_id_col_cat_entidad = 0;
   LET v_id_col_entidad = 0;
      
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safre/BD/fn_glo_recupera_etiquetas.trace';

   
   -- Recuper identificador de la tabla para recuperar el nombre del campo id de esa tabla
   SELECT tabid
     INTO v_id_tabla
     FROM systables
    WHERE tabname = p_entidad;
   
   SELECT tabid
     INTO v_id_tabla_catalogo
     FROM systables
    WHERE tabname = p_entidad_catalogo;
	
   -- Recupera el campo id 
   SELECT colname
     INTO v_nombre_columna
     FROM syscolumns
    WHERE tabid = v_id_tabla
      AND colno = 2;
	  
   SELECT colname
     INTO v_nombre_col_id_padre
     FROM syscolumns
    WHERE tabid = v_id_tabla_catalogo
      AND colno = 2;
   
   -- Recupera el campo id 
   SELECT colname
     INTO v_nombre_columna_id
     FROM syscolumns
    WHERE tabid = v_id_tabla
      AND colno = 1;
	  
   SELECT colname
     INTO v_nombre_col_orden
     FROM syscolumns
    WHERE tabid = v_id_tabla_catalogo
      AND colno = 1;
      
   -- recupera las columnas de la entidad
   LET v_consulta = "SELECT cat.etiqueta,"||
                    "       ent."||TRIM(v_nombre_columna_id)||","||
                    "       cat."||TRIM(v_nombre_col_orden)||
                    "  FROM "||TRIM(p_entidad)||" ent LEFT OUTER JOIN "||TRIM(p_entidad_catalogo)||" cat "||
                    "    ON ent."||TRIM(p_nombre_col_campo)||" = cat."||TRIM(p_nombre_col_cve_natural)||
                    " WHERE cat."||TRIM(v_nombre_col_id_padre)||" = "||p_id_cat_bus||
                    "   AND ent."||TRIM(v_nombre_columna)||" = "||p_id_registro_entidad||
                    " ORDER BY ent.id_bus_agrupa_bloque,ent.id_bus_detalle_bloque";
					
   
   PREPARE prp_recupera_etiqueta FROM v_consulta;
   DECLARE cur_recupera_etiqueta CURSOR FOR prp_recupera_etiqueta;
   OPEN cur_recupera_etiqueta ;
   LET v_ind = 0;
   LET v_diag = '000';
                
   WHILE(SQLCODE == 0)
   
      FETCH cur_recupera_etiqueta INTO v_etiqueta,
                                       v_id_col_entidad,
                                       v_id_col_cat_entidad;
                                       
      -- recupera el valor de la columna
      LET v_ind        = 0;
      LET v_diag       = '000';
      LET v_sql_error  = 0;
      LET v_isam_error = 0;
      LET v_msg_error  = " ";
      EXECUTE FUNCTION fn_glo_recupera_valores(p_nombre_col_valor,
                                               p_entidad,
                                               v_nombre_columna_id,
                                               v_id_col_entidad) INTO v_ind, 
                                                                      v_diag,
                                                                      v_sql_error,
                                                                      v_isam_error,
                                                                      v_msg_error,
                                                                      r_valor;

      IF(SQLCODE == 0)THEN
      
         RETURN v_ind, 
                v_diag,
                v_sql_error,
                v_isam_error,
                v_msg_error,
                v_etiqueta,
                r_valor,
                v_id_col_entidad,
                v_id_col_cat_entidad WITH RESUME;
      END IF

   END WHILE
   CLOSE cur_recupera_etiqueta ;
   FREE cur_recupera_etiqueta;
   
END FUNCTION;


