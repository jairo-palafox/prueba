






CREATE FUNCTION "safreviv".fn_glo_recupera_desc_estado(p_entidad CHAR(40), -- Entidad histórica
                                            p_estado  INTEGER)
RETURNING SMALLINT,
          CHAR(3),
          INTEGER,
          INTEGER,
          CHAR(100),
          CHAR(40);

DEFINE v_desc_estado      CHAR(40);
DEFINE v_existe           SMALLINT;

DEFINE v_ind              SMALLINT;   -- idicador de error
DEFINE v_diag             CHAR(3);    -- diagnostico de error
DEFINE v_sql_error        INTEGER; 
DEFINE v_isam_error       INTEGER;
DEFINE v_msg_error        CHAR(100);

   -- Captura el error sql
   ON EXCEPTION SET v_sql_error      
      LET v_desc_estado      = '';
            
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error, 
             v_desc_estado;
   END EXCEPTION;
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safre/BD/fn_glo_recupera_desc_estado.trace';
   
   LET v_ind         = 0;
   LET v_diag        = '000';
   LET v_sql_error   = 0;
   LET v_isam_error  = 0;      
   LET v_msg_error   = '';
   LET v_desc_estado = '';

   LET v_existe = 0;
   SELECT FIRST 1 1 --NVL(FIRST 1 1,0)
     INTO v_existe
     FROM glo_cat_maquinaria
    WHERE (ent_historico = p_entidad
       OR ent_maquinaria = p_entidad);
   
   -- Se valida que exista la entidad en el catálogo
   IF(v_existe = 0 OR v_existe IS NULL)THEN -- no se encontró el registro
      
      LET v_ind       = 1;
      LET v_diag      = '001';
      LET v_sql_error = 100;
      LET v_msg_error = "No se encontró registro de entidad histórica";
      
      RETURN v_ind, 
             v_diag,
             v_sql_error,
             v_isam_error,
             v_msg_error, 
             v_desc_estado;
   END IF

   -- Recupera el valor del estado actual
   SELECT est.estado_desc
     INTO v_desc_estado
     FROM glo_cat_maquinaria maq JOIN glo_maq_estado est
       ON est.id_maquinaria = maq.id_maquinaria
    WHERE (maq.ent_historico = p_entidad
       OR maq.ent_maquinaria = p_entidad)
      AND est.estado = p_estado;

   RETURN v_ind, 
          v_diag,
          v_sql_error,
          v_isam_error,
          v_msg_error, 
          v_desc_estado;

END FUNCTION
;


