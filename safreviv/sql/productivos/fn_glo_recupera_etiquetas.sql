






CREATE FUNCTION "safreviv".fn_glo_recupera_etiquetas(p_entidad             CHAR(40), -- Entidad principal
                                          p_id_registro_entidad DECIMAL(9,0))
RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100),
          CHAR(40),
          VARCHAR(200) ;

DEFINE v_consulta            CHAR(1024);
DEFINE v_existe              SMALLINT;
DEFINE v_nombre_columna      CHAR(40);
DEFINE v_id_tabla            INTEGER;
DEFINE v_etiqueta            CHAR(40);
DEFINE v_cve_natural         CHAR(40);
DEFINE v_id_entidad_etiqueta DECIMAL(9);
DEFINE v_tipo_columna        SMALLINT;
DEFINE v_valor_dec           DECIMAL(22,2);
DEFINE v_resta               DECIMAL(4,2);
DEFINE r_valor       VARCHAR(200);
DEFINE v_valor_desc  VARCHAR(200);
DEFINE v_descripcion VARCHAR(200);

DEFINE v_ind                 SMALLINT;   -- idicador de error
DEFINE v_diag                CHAR(3);    -- diagnostico de error
DEFINE v_sql_error           INTEGER; 
DEFINE v_isam_error          INTEGER;
DEFINE v_msg_error           CHAR(100);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind         = 0;
      LET v_diag        = '000';
      LET v_etiqueta   = '';
      LET r_valor      = '';
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_etiqueta,
             r_valor;
             
   END EXCEPTION;
   
   LET v_ind        = 0;
   LET v_diag       = '000';
   LET v_sql_error  = 0;
   LET v_isam_error = 0;
   LET v_msg_error  = " ";
   LET v_etiqueta   = '';
   LET r_valor      = '';

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_glo_recupera_etiquetas.trace';
   --TRACE ON;
 
   LET v_existe = 0;
   SELECT FIRST 1 1 --NVL( 1,0)
     INTO v_existe
     FROM glo_entidad_historico_consulta
    WHERE entidad_cod = p_entidad;

   
   -- Se valida que exista la entidad en el catálogo
   IF(v_existe = 0 OR v_existe IS NULL)THEN -- no se encontró el registro
      LET v_ind       = 1;
      LET v_diag      = '001';
      LET v_sql_error = 100;
      LET v_msg_error = 'No se encontró registro de entidad catálogo';
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_etiqueta,
             r_valor;
   END IF
   
   -- Recuper identificador de la tabla para recuperar el nombre del campo id de esa tabla
   SELECT tabid
     INTO v_id_tabla
     FROM systables
    WHERE tabname =p_entidad;
      
   -- Recupera el campo id 
   SELECT colname
     INTO v_nombre_columna
     FROM syscolumns
    WHERE tabid = v_id_tabla
      AND colno = 1;
   
   -- recupera las columnas de la entidad
   LET v_consulta = "SELECT eti.etiqueta,"||
                    "       eti.cve_natural,"||
                    "       eti.id_entidad_etiqueta"||
                    "  FROM glo_entidad_etiqueta eti JOIN glo_entidad_historico_consulta his"||
                    "    ON eti.id_entidad_historico = his.id_entidad_historico"|| 
                    " WHERE his.entidad_cod = '"||p_entidad||"'"||
                    " ORDER BY eti.id_entidad_etiqueta";

   PREPARE prp_recupera_etiqueta FROM v_consulta;
   DECLARE cur_recupera_etiqueta CURSOR FOR prp_recupera_etiqueta;
   OPEN cur_recupera_etiqueta ;
   LET v_ind = 0;
   LET v_diag = '000';

   WHILE(SQLCODE == 0)
   
      FETCH cur_recupera_etiqueta INTO v_etiqueta,
                                       v_cve_natural,
                                       v_id_entidad_etiqueta ;
      -- recupera el valor de la columna
      LET v_ind        = 0;
      LET v_diag       = '000';
      LET v_sql_error  = 0;
      LET v_isam_error = 0;
      LET v_msg_error  = " ";
      EXECUTE FUNCTION fn_glo_recupera_valores(v_cve_natural,
                                               p_entidad,
                                               v_nombre_columna,
                                               p_id_registro_entidad) INTO v_ind, 
                                                                           v_diag,
                                                                           v_sql_error,
                                                                           v_isam_error,
                                                                           v_msg_error,
                                                                           r_valor;

      IF(SQLCODE == 0)THEN
      
          LET v_valor_desc = "'"||r_valor||"'";
          -- recupera la descripcion del valor
          LET v_ind        = 0;
          LET v_diag       = '000';
          LET v_sql_error  = 0;
          LET v_isam_error = 0;
          LET v_msg_error  = " ";
          IF( v_etiqueta = "ESTADO" )THEN 
             EXECUTE FUNCTION fn_glo_recupera_desc_estado(p_entidad,
                                                          r_valor) INTO v_ind,
                                                                        v_diag,
                                                                        v_sql_error,
                                                                        v_isam_error,
                                                                        v_msg_error,
                                                                        v_descripcion;
          ELSE
             EXECUTE FUNCTION fn_glo_recupera_descripciones(v_id_entidad_etiqueta,
                                                            v_valor_desc) INTO v_ind, 
                                                                               v_diag,
                                                                               v_sql_error,
                                                                               v_isam_error,
                                                                               v_msg_error,
                                                                               v_descripcion;
          END IF
          IF(v_ind = 0 AND v_sql_error = 0)THEN
             LET r_valor = r_valor||" "||v_descripcion;
          ELSE 
             LET r_valor = r_valor;
          END IF;
          -- recupera el tipo de columna, y devolver el valor con el formato correcto
          SELECT coltype
            INTO v_tipo_columna
            FROM syscolumns
           WHERE tabid = v_id_tabla
             AND colname = v_cve_natural;

          IF(v_tipo_columna = 7)THEN -- fecha
       
             LET r_valor = r_valor[4,5]||"-"||r_valor[1,2]||"-"||r_valor[7,10];
       
          END IF;
          -- numérico
          IF(v_tipo_columna = 5    OR 
             v_tipo_columna = 261) THEN 
          
             LET v_valor_dec = r_valor;
             LET v_resta     = 0;
             LET v_resta     = v_valor_dec - TRUNC(v_valor_dec);
             
             IF(v_resta <> 0)THEN
                LET r_valor = TO_CHAR(v_valor_dec,"-###,##&.&&") ;
             END IF
             
          END IF

         RETURN v_ind, 
                v_diag,
                v_sql_error,
                v_isam_error,
                v_msg_error,
                v_etiqueta,
                r_valor WITH RESUME;
      END IF

   END WHILE
   CLOSE cur_recupera_etiqueta ;
   FREE cur_recupera_etiqueta;
   
END FUNCTION
;


