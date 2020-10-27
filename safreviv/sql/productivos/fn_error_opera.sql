






CREATE FUNCTION "safreviv".fn_error_opera(p_pid         DECIMAL(9,0),
                               p_proceso_cod SMALLINT    ,
                               p_opera_cod   SMALLINT    )
RETURNING SMALLINT;

DEFINE r_actualiza       SMALLINT;

DEFINE v_opera_cod_ant   SMALLINT;
DEFINE v_opera_cod_post  SMALLINT;
DEFINE v_estado          SMALLINT;
DEFINE v_estado_ant      SMALLINT;
DEFINE v_tiempo          CHAR(23);

   LET r_actualiza  = 0;
   LET v_estado     = 3;
   LET v_estado_ant = 2;

   IF EXISTS (SELECT bat.opera_cod
                FROM bat_ctr_operacion bat
               WHERE bat.pid         = p_pid
                 AND bat.proceso_cod = p_proceso_cod
                 AND bat.opera_cod   = p_opera_cod
                 AND bat.estado_cod  NOT IN (1,2)) THEN

      LET r_actualiza = 6; --No se esta ejecutando la operacion
   ELSE
      LET v_tiempo = CURRENT;

      UPDATE bat_ctr_operacion
         SET fecha_fin   = CURRENT YEAR TO SECOND ,
             estado_cod  = v_estado
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod;

      UPDATE bat_ctr_proceso
         SET fecha_fin   = CURRENT YEAR TO SECOND ,
             estado_cod  = v_estado
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod;

   END IF
   RETURN r_actualiza;
END FUNCTION;


