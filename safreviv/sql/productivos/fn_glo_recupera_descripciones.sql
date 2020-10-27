






CREATE FUNCTION "safreviv".fn_glo_recupera_descripciones(p_id_entidad_etiqueta DECIMAL(9,0),
                                              p_valor               VARCHAR(200))

RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100),
          VARCHAR(200) ;

DEFINE v_consulta            CHAR(800);
DEFINE v_existe              SMALLINT;

DEFINE v_descripcion         VARCHAR(200);
DEFINE v_entidad_catalogo    CHAR(40);
DEFINE v_entidad_llave       CHAR(40);
DEFINE v_entidad_descripcion CHAR(40);

DEFINE v_ind                 SMALLINT;   -- idicador de error
DEFINE v_diag                CHAR(3);    -- diagnostico de error
DEFINE v_sql_error           INTEGER; 
DEFINE v_isam_error          INTEGER;
DEFINE v_msg_error           CHAR(100);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind         = 0;
      LET v_diag        = '000';
      LET v_descripcion = "";
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_descripcion;
             
   END EXCEPTION;
   
   ON EXCEPTION IN (-1213) --A character to numeric conversion process failed
      LET v_ind         = 0;
      LET v_diag        = '000';
      LET v_descripcion = "";
      
      LET v_consulta = "SELECT "||v_entidad_descripcion||" CLIPPED"||
                       "  FROM "||v_entidad_catalogo||
                       " WHERE "||v_entidad_llave||" = '"||p_valor||"'";
      
      PREPARE prp_recupera_desc_2 FROM v_consulta;
      DECLARE cur_recupera_desc_2 CURSOR FOR prp_recupera_desc_2;
      OPEN cur_recupera_desc_2 ;
      FETCH cur_recupera_desc_2 INTO v_descripcion;
                  
      IF(v_descripcion IS NULL)THEN
         LET v_ind       = 1;
         LET v_diag      = '002';
         LET v_sql_error = 100;
         LET v_msg_error = 'No se encontró valor de etiqueta';
     
         RETURN v_ind, 
                v_diag,
                v_sql_error,
                v_isam_error,
                v_msg_error,
                v_descripcion;
      END IF
      
      CLOSE cur_recupera_desc_2 ;
      FREE cur_recupera_desc_2;
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_descripcion;
             
   END EXCEPTION;

   -- SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_glo_recupera_descripciones.trace';

   LET v_ind         = 0;
   LET v_diag        = '000';
   LET v_sql_error   = 0;
   LET v_isam_error  = 0;
   LET v_msg_error   = " ";
   LET v_descripcion = "";
   
   -- Se busca la entrada para obtener los parametros para obtener la descripcion de los
   -- catalogos

   LET v_existe = 0;
   SELECT FIRST 1 1 --NVL(COUNT(*),0)
     INTO v_existe
     FROM glo_entidad_catalogo
    WHERE id_entidad_etiqueta = p_id_entidad_etiqueta;

   IF(v_existe = 0 OR v_existe IS NULL)THEN   -- no se encuentra registro de descripcion en catalogo
      LET v_ind       = 1;
      LET v_diag      = '001';
      LET v_sql_error = 100;
      LET v_msg_error = 'No se encontró etiqueta';

      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_descripcion;

   END IF;
   
   -- recupera nombre de columnas y catálogo
   SELECT entidad_catalogo,
          entidad_llave,
          entidad_desc
     INTO v_entidad_catalogo,
          v_entidad_llave,
          v_entidad_descripcion
     FROM glo_entidad_catalogo
    WHERE id_entidad_etiqueta = p_id_entidad_etiqueta;
   

   LET v_descripcion = '';
   -- recupera la descripcion de la etiqueta
   LET v_consulta = 'SELECT '||v_entidad_descripcion||' CLIPPED'||
                    '  FROM '||v_entidad_catalogo||
                    ' WHERE '||v_entidad_llave||' = '||p_valor;
   
   PREPARE prp_recupera_desc FROM v_consulta;
   DECLARE cur_recupera_desc CURSOR FOR prp_recupera_desc;
   OPEN cur_recupera_desc ;
   FETCH cur_recupera_desc INTO v_descripcion;
               
   IF(v_descripcion IS NULL)THEN
      LET v_ind       = 1;
      LET v_diag      = '002';
      LET v_sql_error = 100;
      LET v_msg_error = 'No se encontró valor de etiqueta';

      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_descripcion;
   END IF
   
   CLOSE cur_recupera_desc ;
   FREE cur_recupera_desc;
   
   RETURN v_ind, 
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error,
          v_descripcion;
          
END FUNCTION;


