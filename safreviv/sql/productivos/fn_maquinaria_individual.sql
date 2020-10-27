






CREATE FUNCTION "safreviv".fn_maquinaria_individual(p_maquinaria    char(40),
                                         p_id            DEC(9,0),
                                         p_id_nombre     CHAR(40),
                                         p_senial        SMALLINT,
                                         p_usuario       CHAR(20))
RETURNING SMALLINT,CHAR(3),SMALLINT;

DEFINE v_estado_destino SMALLINT;
DEFINE v_ind SMALLINT;
DEFINE v_diag CHAR(3);
DEFINE v_error INTEGER;
DEFINE v_tabla_estado       CHAR(40);
DEFINE v_tabla_senal        CHAR(40);
DEFINE v_tabla_estado_senal CHAR(40);
DEFINE v_entidad_maquinaria CHAR(40);
DEFINE v_estado_origen SMALLINT;
DEFINE v_ent_historico      CHAR(40);
DEFINE v_id_cat_dato_actualizado SMALLINT;
DEFINE vtxt   CHAR(1000);

   -- en caso de error se establecen códigos de error
   ON EXCEPTION SET v_error
      LET v_ind = -1;
      LET v_diag = '001';
      LET v_estado_destino = 0;
      RETURN v_ind, v_diag, v_estado_destino;
   END EXCEPTION WITH RESUME;


   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_maquinaria_individual.trace';

   LET v_ind  = 0;
   LET v_diag = '000';
   LET v_estado_destino = -1;
   LET v_estado_origen = 0;

   -- Se recuperan tablas de maquinaria

   SELECT a.ent_estado   ,
          a.ent_relacion ,
          a.ent_senal    ,
          a.ent_maquinaria  ,
          a.ent_historico
   INTO   v_tabla_estado       ,
          v_tabla_estado_senal ,
          v_tabla_senal        ,
          v_entidad_maquinaria  ,
          v_ent_historico
   FROM   cat_maq_entidades a
   WHERE  a.id_maquinaria = p_maquinaria;


   --TRACE "tabla senal: "||v_tabla_estado_senal;

   -- Se recupera el estado destino


    LET vtxt="SELECT a.estado  FROM safre_viv:"||v_entidad_maquinaria||
              " a WHERE a."||p_id_nombre||" = "||p_id||";";

    --TRACE "qry edo origen: "||vtxt;

         PREPARE prp_edo_origen FROM vtxt;
         DECLARE cur_edo_origen CURSOR FOR prp_edo_origen;
         OPEN cur_edo_origen ;
         FETCH cur_edo_origen INTO v_estado_origen;
         CLOSE cur_edo_origen;
         FREE cur_edo_origen;

   IF(v_estado_origen IS NULL)THEN
      LET v_ind = -1;
      LET v_diag = '001'; -- no se recuperó estado origen
      LET v_estado_destino = 0;
      RETURN v_ind, v_diag, v_estado_destino;
   END IF

    --TRACE "edo origen: "||v_estado_origen;

   LET vtxt = "SELECT a.estado_destino FROM safre_viv:"||v_tabla_estado_senal||
              " a WHERE a.id_senal = "||p_senial||
              " AND a.estado_origen = "||v_estado_origen||";";

     --TRACE "qry edo destino: "||vtxt;

         PREPARE prp_recupera_edo FROM vtxt;
         DECLARE cur_recupera_edo CURSOR FOR prp_recupera_edo;
         OPEN cur_recupera_edo ;
         FETCH cur_recupera_edo INTO v_estado_destino;
         CLOSE cur_recupera_edo;
         FREE cur_recupera_edo;

    --TRACE "edo_destino "||v_estado_destino;

   -- se valida que exista el registro, de lo contrario se establecen códigos de respuesta

   IF(v_estado_destino IS NULL)THEN
   --IF(SQLCODE != 0)THEN
      LET v_ind = -1;
      LET v_diag = '002'; -- no se recuperó estado destino
      LET v_estado_destino = 0;
   ELSE

      LET vtxt="UPDATE "||v_entidad_maquinaria||" SET estado = "||v_estado_destino||
               " WHERE "||p_id_nombre||" = "||p_id||";";

      --TRACE "update: "||vtxt;

      EXECUTE IMMEDIATE vtxt;

      --TRACE "update: "||vtxt;

      -- se extrae el id para el dato estado del catálogo de datos actualizados

      SELECT b.id_cat_dato_actualizado
      INTO   v_id_cat_dato_actualizado
      FROM   sep_cat_entidad_historico a ,
             sep_cat_dato_actualizado  b
      WHERE  a.entidad_cod = v_entidad_maquinaria
      AND    a.id_cat_entidad_historico = b.id_cat_entidad_historico
      AND    b.cve_natural = "estado";

      IF(v_id_cat_dato_actualizado IS NULL)THEN
         LET v_ind = -1;
         LET v_diag = '003';
         LET v_estado_destino = 0;
         RETURN v_ind, v_diag, v_estado_destino;
      END IF
      --TRACE "ent maquinaria: "||v_ent_historico;
      --TRACE "id dato: "||v_id_cat_dato_actualizado;

      LET vtxt="INSERT INTO "||v_ent_historico||" VALUES (seq_sep_his_expediente.NEXTVAL,"||p_id||","||v_id_cat_dato_actualizado||
               ",today,"||v_estado_origen||","||v_estado_destino||",'"||p_usuario||"');";


     --TRACE "act_his: "||vtxt;

     EXECUTE IMMEDIATE vtxt;

   END IF;


   RETURN v_ind, v_diag, v_estado_destino;

END FUNCTION;


