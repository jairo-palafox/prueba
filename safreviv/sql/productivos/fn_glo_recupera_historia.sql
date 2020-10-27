






CREATE FUNCTION "safreviv".fn_glo_recupera_historia(p_entidad             CHAR(40), -- Entidad histórica
                                         p_id_registro_entidad INTEGER)
RETURNING SMALLINT,
          CHAR(3),
          INTEGER,
          INTEGER,
          CHAR(100),
          CHAR(40),
          DATE,
          CHAR(40),
          CHAR(40),
          CHAR(18);

DEFINE v_consulta         CHAR(1024);

DEFINE v_id_columna       CHAR(40);
DEFINE v_id_tabla         INTEGER;
DEFINE v_etiqueta         CHAR(40);
DEFINE v_f_modificacion   DATE;
DEFINE v_valor_modificado CHAR(40);
DEFINE v_valor_actual     CHAR(40);
DEFINE v_usuario          CHAR(18);
DEFINE v_existe           SMALLINT;

DEFINE v_ind              SMALLINT;   -- idicador de error
DEFINE v_diag             CHAR(3);    -- diagnostico de error
DEFINE v_sql_error        INTEGER;
DEFINE v_isam_error       INTEGER;
DEFINE v_msg_error        CHAR(100);

DEFINE r1 SMALLINT ;
DEFINE r2 CHAR(3)  ;
DEFINE r3 INTEGER  ;
DEFINE r4 INTEGER  ;
DEFINE r5 CHAR(100);
DEFINE v_id_entidad_etiqueta DECIMAL(9,0);




   -- Captura el error sql
   ON EXCEPTION SET v_sql_error
      --LET v_ind              = 0;
      --LET v_diag             = '000';
      --LET v_etiqueta         = '';
      --LET v_f_modificacion   = TODAY;
      --LET v_valor_modificado = '';
      --LET v_valor_actual     = '';
      --LET v_usuario          = '';

      RETURN v_ind,
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_etiqueta,
             v_f_modificacion,
             v_valor_modificado,
             v_valor_actual,
             v_usuario ;
   END EXCEPTION WITH RESUME;

   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_glo_recupera_historia.trace';
   --TRACE ON;

   LET v_ind              = 0;
   LET v_diag             = '000';
   LET v_sql_error        = 0;
   LET v_isam_error       = 0;
   LET v_msg_error        = " ";
   LET v_etiqueta         = '';
   LET v_f_modificacion   = TODAY;
   LET v_valor_modificado = '';
   LET v_valor_actual     = '';
   LET v_usuario          = '';
   LET v_id_entidad_etiqueta = '';

   LET v_existe = 0;
   SELECT FIRST 1 1 --NVL(FIRST 1 1,0)
     INTO v_existe
     FROM glo_entidad_historico_consulta
    WHERE entidad_cod = p_entidad;

   -- Se valida que exista la entidad en el catálogo
   IF(v_existe = 0 OR v_existe IS NULL)THEN -- no se encontró el registro

      LET v_ind       = 1;
      LET v_diag      = '001';
      LET v_sql_error = 100;
      LET v_msg_error = "No se encontró registro de entidad catálogo";

      RETURN v_ind,
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_etiqueta,
             v_f_modificacion,
             v_valor_modificado,
             v_valor_actual,
             v_usuario;
   END IF

   -- Recuper identificador de la tabla para recuperar el nombre del campo id de esa tabla
   SELECT tabid
     INTO v_id_tabla
     FROM systables
    WHERE tabname =p_entidad;


   -- Recupera el campo id
   SELECT colname
     INTO v_id_columna
     FROM syscolumns
    WHERE tabid = v_id_tabla
      AND colno = 2;

   LET v_consulta = 'SELECT glo.etiqueta,'||
                    '       ent.f_modificacion,'||
                    '       ent.valor_modificado,'||
                    '       ent.valor_actual,'||
                    '       ent.usuario,'||
                    '       ent.id_cat_dato_actualizado'||
                    '  FROM glo_entidad_etiqueta glo JOIN '||p_entidad||' ent'||
                    --'    ON ent.id_entidad_etiqueta = glo.id_entidad_etiqueta'||
                    '    ON ent.id_cat_dato_actualizado = glo.id_entidad_etiqueta'||
                    ' WHERE ent.'||v_id_columna||' = '||p_id_registro_entidad;
                    --' ORDER BY ent.f_modificacion DESC';
   PREPARE prp_recupera FROM v_consulta;
   DECLARE cur_recupera CURSOR FOR prp_recupera;
   OPEN cur_recupera ;
   LET v_ind = 0;
   LET v_diag = '000';

   WHILE(SQLCODE == 0)
      LET v_etiqueta         = '';
      LET v_f_modificacion   = TODAY;
      LET v_valor_modificado = '';
      LET v_valor_actual     = '';
      LET v_usuario          = '';
      LET v_id_entidad_etiqueta = '';

      FETCH cur_recupera INTO v_etiqueta,
                              v_f_modificacion,
                              v_valor_modificado,
                              v_valor_actual,
                              v_usuario,
                              v_id_entidad_etiqueta;

      IF v_etiqueta = "ESTADO" THEN

         EXECUTE FUNCTION fn_glo_recupera_desc_estado(p_entidad,v_valor_actual) INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_valor_actual;

         EXECUTE FUNCTION fn_glo_recupera_desc_estado(p_entidad,v_valor_modificado) INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_valor_modificado;
      ELSE
         EXECUTE FUNCTION fn_glo_recupera_descripciones(v_id_entidad_etiqueta,v_valor_actual) INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_valor_actual;

         EXECUTE FUNCTION fn_glo_recupera_descripciones(v_id_entidad_etiqueta,v_valor_modificado) INTO v_ind,v_diag,v_sql_error,v_isam_error,v_msg_error,v_valor_modificado;
      END IF;

      IF(SQLCODE == 0)THEN
         RETURN v_ind,
                v_diag,
                v_sql_error,
                v_isam_error,
                v_msg_error,
                v_etiqueta,
                v_f_modificacion,
                v_valor_modificado,
                v_valor_actual,
                v_usuario WITH RESUME;
      END IF

   END WHILE
   CLOSE cur_recupera ;
   FREE cur_recupera;

END FUNCTION
;


