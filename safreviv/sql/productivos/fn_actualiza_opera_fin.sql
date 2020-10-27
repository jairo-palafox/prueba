






CREATE FUNCTION "safreviv".fn_actualiza_opera_fin(p_pid         DECIMAL(9,0),
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
   LET v_estado     = 4;
   LET v_estado_ant = 2;
   ---se valida la exixtencia del opera cod 
   IF NOT EXISTS (SELECT bat.opera_cod
                    FROM bat_ctr_operacion bat
                   WHERE bat.pid         = p_pid
                     AND bat.proceso_cod = p_proceso_cod
                     AND bat.opera_cod   = p_opera_cod
                     AND bat.estado_cod  = v_estado_ant) THEN

      LET r_actualiza = 6; --No se esta ejecutando lo operacion
      ---caso contrario se actualizan bat_ctr_proceso y bat_ctr_operacion
   ELSE
      LET v_tiempo = CURRENT;
      
       SELECT a.opera_cod_ant ,
              a.opera_cod_post
         INTO v_opera_cod_ant , 
              v_opera_cod_post
         FROM cat_operacion a
        WHERE a.proceso_cod = p_proceso_cod
          AND a.opera_cod   = p_opera_cod;

      --se actualiza bat_ctr_proceso 
      IF v_opera_cod_post = 0 THEN
         UPDATE bat_ctr_proceso
            SET fecha_fin   = CURRENT YEAR TO SECOND,
                estado_cod  = v_estado
          WHERE pid         = p_pid
            AND proceso_cod = p_proceso_cod;
      END IF
      --se actualiza la fecha final y el estado 
      UPDATE bat_ctr_operacion
         SET fecha_fin   = CURRENT YEAR TO SECOND,
             estado_cod  = v_estado
       WHERE pid         = p_pid
         AND proceso_cod = p_proceso_cod
         AND opera_cod   = p_opera_cod;

      --se actualiza la operacion productora como final para que la 
      -- consumidora pueda ser ejecutada

      UPDATE bat_ctr_predecesor
      SET    bandera_ejecuta  = 1  -- bandera para ejecutar consumidor
      WHERE  pid_prod         = p_pid
      AND    proceso_cod_prod = p_proceso_cod
      AND    opera_cod_prod   = p_opera_cod;

   END IF
   RETURN r_actualiza;
END FUNCTION;


