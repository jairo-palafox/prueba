






CREATE FUNCTION "safreviv".fn_valida_reverso(p_pid         DECIMAL(9,0),
                                  p_proceso_cod SMALLINT    ,
                                  p_opera_cod   SMALLINT    )
RETURNING SMALLINT;

   DEFINE v_opera_cod_ant    SMALLINT;
   DEFINE v_opera_cod_post   SMALLINT;
   DEFINE v_proceso_cod_post SMALLINT;
   DEFINE v_opera_cod        SMALLINT;
   DEFINE r_valida           SMALLINT;

   LET r_valida = 0;

   SELECT a.opera_cod_ant,
          a.opera_cod_post
     INTO v_opera_cod_ant,
          v_opera_cod_post
     FROM cat_operacion a
    WHERE a.proceso_cod = p_proceso_cod
      AND a.opera_cod   = p_opera_cod;
   
   SELECT a.opera_cod
     INTO v_opera_cod
     FROM bat_ctr_operacion a
    WHERE a.pid         = p_pid
      AND a.proceso_cod = p_proceso_cod
      AND a.opera_cod   = v_opera_cod_post
      AND a.estado_cod  <> 1;

   IF v_opera_cod IS NOT NULL AND
      v_opera_cod <> 0 THEN
       LET r_valida = 4; --#Existe un etapa posterior ejecutada
   ELSE
       IF v_opera_cod_post = 0 THEN
           SELECT 1
             INTO v_proceso_cod_post
             FROM bat_ctr_proceso
            WHERE pid > p_pid
              AND proceso_cod = p_proceso_cod
              AND estado_cod  <> 10
            GROUP BY 1;

           IF v_proceso_cod_post IS NOT NULL AND
              v_proceso_cod_post <> 0 THEN
               LET r_valida = 10;
           END IF
       END IF
   END IF
   
   RETURN r_valida;
END FUNCTION
;


