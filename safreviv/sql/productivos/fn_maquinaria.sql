






CREATE FUNCTION "safreviv".fn_maquinaria(p_maquinaria    char(40),
                              p_senial        SMALLINT,
                              p_estado_origen SMALLINT)
RETURNING SMALLINT,CHAR(3),SMALLINT;

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind SMALLINT;
DEFINE v_diag CHAR(3);
DEFINE v_error INTEGER;
DEFINE v_tabla_estado       CHAR(40);
DEFINE v_tabla_senal        CHAR(40);
DEFINE v_tabla_estado_senal CHAR(40);
DEFINE vtxt   CHAR(1000);
   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_error
      LET v_ind = 1;
      LET v_diag = '001';
      LET v_estado_destino = 0;
      RETURN v_ind, v_diag, v_estado_destino;
   END EXCEPTION WITH RESUME;


   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_maq_sep_expediente.trace';

   LET v_ind  = 0;
   LET v_diag = '000';
   LET v_estado_destino = -1;

   -- Se recuperan tablas de maquinaria

   SELECT a.ent_estado   ,
          a.ent_relacion ,
          a.ent_senal
   INTO   v_tabla_estado ,
          v_tabla_estado_senal ,
          v_tabla_senal
   FROM   cat_maq_entidades a
   WHERE  a.id_maquinaria = p_maquinaria;

   --TRACE "tabla senal: "||v_tabla_estado_senal;

   -- Se recupera el estado destino

   LET vtxt = "SELECT a.estado_destino FROM safre_viv:"||v_tabla_estado_senal||
              " a WHERE a.id_senal = "||p_senial||
              " AND a.estado_origen = "||p_estado_origen||";";

    --TRACE "vtxt: "||vtxt;

         PREPARE prp_recupera_edo FROM vtxt;
         DECLARE cur_recupera_edo CURSOR FOR prp_recupera_edo;
         OPEN cur_recupera_edo ;
         FETCH cur_recupera_edo INTO v_estado_destino;
         CLOSE cur_recupera_edo;
         FREE cur_recupera_edo;

    --TRACE "edo_destino "||v_estado_destino;


   --SELECT estado_destino
     --INTO v_estado_destino
     --FROM safre_viv:sep_estado_senal_expediente
    --WHERE id_senal = p_senial
      --AND estado_origen = p_estado_origen;

   -- se valida que exista el registro, de lo contrario se establecen códigos de respuesta
   IF(v_estado_destino IS NULL)THEN
   --IF(SQLCODE != 0)THEN
      LET v_ind = 1;
      LET v_diag = '001';
      LET v_estado_destino = 0;
   END IF;

   RETURN v_ind, v_diag, v_estado_destino;

END FUNCTION;


