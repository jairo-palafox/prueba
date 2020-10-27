






CREATE FUNCTION "safreviv".fn_valida_operacion(p_pid          DECIMAL(9,0),
                                    p_proceso_cod  SMALLINT    ,
                                    p_opera_cod    SMALLINT    )
RETURNING SMALLINT

   DEFINE v_opera_ant      SMALLINT;
   DEFINE v_opera_post     SMALLINT;
   DEFINE v_proceso_cod    SMALLINT;
   DEFINE v_estado_cod     SMALLINT;
   DEFINE r_valida         SMALLINT;
   DEFINE v_fecha_ini      DATETIME YEAR TO SECOND ;
   DEFINE v_fecha_fin      DATETIME YEAR TO SECOND ;

   SELECT a.opera_cod_ant,
          a.opera_cod_post
     INTO v_opera_ant,
          v_opera_post
     FROM cat_operacion a
    WHERE a.proceso_cod = p_proceso_cod
      AND a.opera_cod   = p_opera_cod;

   LET r_valida = 0;
   
   IF v_opera_ant = 0 THEN
      SELECT a.proceso_cod
        INTO v_proceso_cod
        FROM bat_ctr_proceso a
       WHERE a.proceso_cod = p_proceso_cod
         AND a.estado_cod IN (1,2)
         AND a.fecha_ini IS NOT NULL
         AND a.fecha_fin IS NULL;
   
      IF v_proceso_cod IS NOT NULL AND 
         v_proceso_cod <> 0 THEN
         LET r_valida = 7;  --#No ha terminado un proceso previo
      END IF
   ELSE
      SELECT a.fecha_ini,
             a.fecha_fin
        INTO v_fecha_ini,
             v_fecha_fin
        FROM bat_ctr_operacion a
       WHERE a.pid         = p_pid
         AND a.proceso_cod = p_proceso_cod
         AND a.opera_cod   = v_opera_ant;

      IF v_fecha_ini IS NULL THEN
         LET r_valida = 9; --#No se ha lanzado la operación anterior
      ELSE
         IF v_fecha_fin IS NULL THEN
            LET r_valida = 3; --#No ha terminado la operación anterior
         ELSE
            SELECT a.fecha_ini,
                   a.fecha_fin
              INTO v_fecha_ini,
                   v_fecha_fin
              FROM bat_ctr_operacion a
             WHERE a.pid         = p_pid
               AND a.proceso_cod = p_proceso_cod
               AND a.opera_cod   = p_opera_cod;
            IF v_fecha_ini IS NOT NULL THEN 
               IF v_fecha_fin IS NOT NULL THEN 
                  LET r_valida = 8; --#Ya se esta ejecutó la operación
               ELSE
                  LET r_valida = 5; --#Se esta ejecutando la operación
               END IF
            ELSE
	       SELECT a.estado_cod
                 INTO v_estado_cod
                 FROM bat_ctr_proceso a
                WHERE a.pid    = p_pid;

               IF v_estado_cod = 3 THEN
                  LET r_valida = 11;  --#Se encuentra en estado de error el proceso
               END IF
            END IF
         END IF
      END IF
   END IF

   RETURN r_valida;
END FUNCTION;


