






CREATE FUNCTION "safreviv".fn_glo_maq_individual(p_id_maquinaria CHAR(40),
                                      p_id_registro   DECIMAL(9,0),
                                      p_id_senial     SMALLINT,
                                      p_usuario       CHAR(20))
RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100),
          SMALLINT ;

DEFINE v_estado_destino        SMALLINT;   -- estado destino correspondiente a la señal y estado origen
DEFINE v_id_maq_estado         DECIMAL(9); -- identificador del estado y maquinaria
DEFINE v_id_maq_senal          DECIMAL(9); -- identificador de la señal y maquinaria
DEFINE v_estado_origen         SMALLINT;   -- estado en el que se encuentra el proceso
DEFINE v_tabla_estado_senal    CHAR(40);   -- nombre de la tabla de relaciones de estado y señal
DEFINE v_entidad_maquinaria    CHAR(40);   -- nombre de la tabla de relaciones de maquinarias y entidades (tablas)
DEFINE v_ent_historico         CHAR(40);   -- nombre de la tabla histórico
DEFINE v_secuencia_historico   CHAR(40);   -- nombre de la secuencia utilizada 
DEFINE v_id_columna_maquinaria SMALLINT;   -- identificador unico de la columna de entidad para la maquinaria
DEFINE v_col_maquinaria        CHAR(40);   -- nombre de la columna principal de tabla a afectar
DEFINE v_col_estados           CHAR(20);   -- nombre de la columna estado para la tabla principal de la maquinaria
DEFINE v_ind                   SMALLINT;   -- idicador de error
DEFINE v_diag                  CHAR(3);    -- diagnostico de error
DEFINE v_sql_error             INTEGER; 
DEFINE v_isam_error            INTEGER;
DEFINE v_msg_error             CHAR(100);

DEFINE v_consulta   CHAR(1000);
DEFINE v_consulta1  CHAR(500);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      LET v_ind            = 0;
      LET v_diag           = '000';
      LET v_estado_destino = -1;
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
             
   END EXCEPTION;


   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safre/BD/fn_maquinaria_individual.trace';

   LET v_ind            = 0;
   LET v_diag           = '000';
   LET v_estado_destino = -1;
   LET v_estado_origen  = -1;
   LET v_sql_error      = 0;
   LET v_isam_error     = 0;
   LET v_msg_error      = " ";


   -- Se recuperan tablas de maquinaria
   SELECT ent_maquinaria,
          ent_historico ,
          secuencia_historico,
          col_id_maquinaria,
          col_edo_maquinaria
     INTO v_entidad_maquinaria,
          v_ent_historico     ,
          v_secuencia_historico,
          v_col_maquinaria,
          v_col_estados
     FROM glo_cat_maquinaria
    WHERE id_maquinaria = p_id_maquinaria;

   -- Se recupera el estado origen
   LET v_consulta = " SELECT "||v_col_estados||
                    "   FROM "||v_entidad_maquinaria||
                    "  WHERE "||v_col_maquinaria||" = "||p_id_registro||";";
    
   PREPARE prp_recupera_edo_origen FROM v_consulta;
   DECLARE cur_recupera_edo_origen CURSOR FOR prp_recupera_edo_origen;
   OPEN cur_recupera_edo_origen ;
   FETCH cur_recupera_edo_origen INTO v_estado_origen;
   CLOSE cur_recupera_edo_origen;
   FREE cur_recupera_edo_origen;

   IF(v_estado_origen = -1 OR v_estado_origen IS NULL)THEN
      LET v_ind            = 1;
      LET v_diag           = '001'; -- no se encontró estado origen
      LET v_estado_destino = -1;
      LET v_sql_error      = 100;
      LET v_msg_error      = 'No se encontró estado origen';
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   END IF
   
   -- recupera identificador de estado
   SELECT id_maq_estado
     INTO v_id_maq_estado
     FROM glo_maq_estado
    WHERE id_maquinaria = p_id_maquinaria
      AND estado = v_estado_origen;
      
   -- recupera el identificador de la señal 
   SELECT id_maq_senal
     INTO v_id_maq_senal
     FROM glo_maq_senal
    WHERE id_maquinaria = p_id_maquinaria
      AND senal = p_id_senial;
   -- recupera estado destino
   SELECT estado
     INTO v_estado_destino
     FROM glo_maq_estado_senal JOIN glo_maq_estado
       ON id_maq_estado = id_maq_estado_destino
    WHERE id_maq_estado_origen = v_id_maq_estado
      AND id_maq_senal = v_id_maq_senal;
   -- se valida que exista el registro, de lo contrario se establecen códigos de respuesta
   IF( v_estado_destino = -1 OR v_estado_destino IS NULL)THEN
      LET v_ind            = 1;
      LET v_diag           = '002'; -- no se recuperó estado destino
      LET v_estado_destino = -1;
      LET v_sql_error      = 100;
      LET v_msg_error      = 'No se encontró estado destino';
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   END IF


   LET v_consulta = " UPDATE "||v_entidad_maquinaria||
                    "    SET "||v_col_estados||" = "||v_estado_destino||
                    "  WHERE "||v_col_maquinaria||" = "||p_id_registro||";";
                    
   EXECUTE IMMEDIATE v_consulta;
  
   -- se extrae el identificador de la columna para el histórico, y registrar su historia
   SELECT col.id_entidad_etiqueta
     INTO v_id_columna_maquinaria
     FROM glo_entidad_historico_consulta glo JOIN
          glo_entidad_etiqueta col
       ON glo.id_entidad_historico = col.id_entidad_historico
    WHERE glo.entidad_cod = v_entidad_maquinaria
      AND col.cve_natural = v_col_estados;

   IF(v_id_columna_maquinaria IS NULL)THEN
      LET v_ind            = 1;
      LET v_diag           = '003'; -- No se encontró identificador de columna
      LET v_estado_destino = -1;
      LET v_sql_error      = 100;
      LET v_msg_error      = 'No se pudo registrar histórico';
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error,
             v_estado_destino;
   END IF

   -- registra actualizacion a historico
   LET v_consulta1 = " INSERT INTO "||v_ent_historico||
                     " VALUES ("||TRIM(v_secuencia_historico)||".NEXTVAL,"||
                              p_id_registro||","||
                              v_id_columna_maquinaria||
                              ",TODAY,"||
                              v_estado_origen||","||
                              v_estado_destino||","||
                              "'"||p_usuario||"');";
   EXECUTE IMMEDIATE v_consulta1;

   LET v_consulta = " UPDATE "||v_entidad_maquinaria||
                    "    SET "||v_col_estados||" = "||v_estado_destino||
                    "  WHERE "||v_col_maquinaria||" = "||p_id_registro||";";
   EXECUTE IMMEDIATE v_consulta;
   
   RETURN v_ind, 
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error,
          v_estado_destino;

END FUNCTION;


