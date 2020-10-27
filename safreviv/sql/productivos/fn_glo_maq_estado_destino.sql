






CREATE FUNCTION "safreviv".fn_glo_maq_estado_destino(p_id_maquinaria CHAR(40),
                                          p_senial        SMALLINT,
                                          p_estado_origen SMALLINT)
RETURNING SMALLINT ,
          CHAR(3)  ,
          INTEGER  ,
          INTEGER  ,
          CHAR(100),
          SMALLINT ;

DEFINE v_id_maq_estado       DECIMAL(9); -- identificador del estado y maquinaria
DEFINE v_id_maq_senal        DECIMAL(9); -- identificador de la señal y maquinaria

DEFINE v_estado_destino      SMALLINT;   -- estado correspondiente a estado origen y señal
DEFINE v_tabla_estado_senial CHAR(40);   -- nombre de la tabla de relaciones de estado y señal
DEFINE v_consulta            CHAR(1000);
DEFINE v_ind                 SMALLINT;   -- indicador de resulta de tarea
DEFINE v_diag                CHAR(3);    -- diagnostico de resultado
DEFINE v_sql_error           INTEGER; 
DEFINE v_isam_error          INTEGER;
DEFINE v_msg_error           CHAR(100);

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
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_glo_maq_estado_destino.trace';

   LET v_ind            = 0;
   LET v_diag           = '000';
   LET v_estado_destino = -1;
   LET v_sql_error      = 0;
   LET v_isam_error     = 0;
   LET v_msg_error      = " ";
    
   -- recupera el identificador del estado
   SELECT id_maq_estado
     INTO v_id_maq_estado
     FROM glo_maq_estado
    WHERE id_maquinaria = p_id_maquinaria
      AND estado = p_estado_origen;
      
   -- recupera el identificador de la señal 
   SELECT id_maq_senal
     INTO v_id_maq_senal
     FROM glo_maq_senal
    WHERE id_maquinaria = p_id_maquinaria
      AND senal = p_senial;
    

   -- recupera el estado destino
   SELECT estado
     INTO v_estado_destino
     FROM glo_maq_estado_senal JOIN glo_maq_estado
       ON id_maq_estado = id_maq_estado_destino
    WHERE id_maq_estado_origen = v_id_maq_estado
      AND id_maq_senal = v_id_maq_senal;


   -- se valida que exista el registro, de lo contrario se establecen códigos de respuesta
   IF(v_estado_destino = -1 OR v_estado_destino IS NULL)THEN
      LET v_ind            = 1;
      LET v_diag           = '001';
      LET v_estado_destino = -1;
      LET v_sql_error      = 100;
      LET v_msg_error      = "No se encontró estado destino";
   END IF;

   RETURN v_ind, 
          v_diag, 
          v_sql_error,
          v_isam_error,
          v_msg_error, 
          v_estado_destino;

END FUNCTION;


