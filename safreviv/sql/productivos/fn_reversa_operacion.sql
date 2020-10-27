






CREATE FUNCTION "safreviv".fn_reversa_operacion(p_pid           DECIMAL(9,0),
                                     p_proceso_cod   SMALLINT    ,
                                     p_opera_cod     SMALLINT    )
RETURNING SMALLINT

   DEFINE v_opera_cod_ant    SMALLINT;
   DEFINE v_opera_cod_post   SMALLINT;
   DEFINE v_proceso_cod_post SMALLINT;
   DEFINE v_opera_cod        SMALLINT;
   DEFINE v_tiempo           CHAR(23);
   DEFINE v_fecha_ini        DATE    ;
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
      AND a.estado_cod <> 1;

   IF v_opera_cod IS NOT NULL AND
      v_opera_cod <> 0 THEN
       LET r_valida = 4; --#Existe un etapa posterior ejecutada
   END IF

   IF r_valida = 0 THEN
      IF v_opera_cod_ant = 0 THEN
         LET v_tiempo = CURRENT;

         UPDATE bat_ctr_proceso
            SET fecha_fin   = CURRENT YEAR TO SECOND ,
                estado_cod  = 10
          WHERE pid         = p_pid
            AND proceso_cod = p_proceso_cod;

         UPDATE bat_ctr_operacion
            SET fecha_ini   = CURRENT YEAR TO SECOND ,
                fecha_fin   = CURRENT YEAR TO SECOND ,
                estado_cod  = 10 ,
                ind_tipo_ejecucion = 0 -- tipo manual 
          WHERE pid         = p_pid
            AND proceso_cod = p_proceso_cod;
      ELSE
         UPDATE bat_ctr_proceso
            SET fecha_fin   = NULL,
                estado_cod  = 2
          WHERE pid         = p_pid
            AND proceso_cod = p_proceso_cod;

         UPDATE bat_ctr_operacion
            SET fecha_ini   = NULL,
                fecha_fin   = NULL,
                estado_cod  = 1   , 
                ind_tipo_ejecucion = 0 -- tipo manual
          WHERE pid         = p_pid
            AND proceso_cod = p_proceso_cod
            AND opera_cod   = p_opera_cod;
      END IF
   END IF

   RETURN r_valida;
END FUNCTION;


