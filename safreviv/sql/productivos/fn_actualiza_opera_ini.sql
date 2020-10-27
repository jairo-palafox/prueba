






CREATE FUNCTION "safreviv".fn_actualiza_opera_ini(p_pid         DECIMAL(9,0),
                                       p_proceso_cod SMALLINT    ,
                                       p_opera_cod   SMALLINT    ,
                                       p_folio       DECIMAL(9,0),
                                       p_programa    CHAR(20)    ,
                                       p_archivo     CHAR(40)    ,
                                       p_usuario     CHAR(20)    )
RETURNING SMALLINT;

DEFINE r_actualiza       SMALLINT;

DEFINE v_opera_cod_ant   SMALLINT;
DEFINE v_opera_cod_post  SMALLINT;
DEFINE v_estado          SMALLINT;
DEFINE v_estado_ant      SMALLINT;

   LET r_actualiza  = 0;
   LET v_estado     = 2;
   LET v_estado_ant = 1;

	 IF EXISTS (SELECT bat.opera_cod
                FROM bat_ctr_operacion bat
               WHERE bat.pid         = p_pid
                 AND bat.proceso_cod = p_proceso_cod
                 AND bat.opera_cod   = p_opera_cod
                 AND bat.estado_cod  = v_estado_ant ) THEN

      SELECT a.opera_cod_ant, a.opera_cod_post
        INTO v_opera_cod_ant, v_opera_cod_post
        FROM cat_operacion a 
       WHERE a.proceso_cod = p_proceso_cod
         AND a.opera_cod   = p_opera_cod;

      IF v_opera_cod_ant > 0 AND
         NOT EXISTS (SELECT bat.opera_cod
                       FROM bat_ctr_operacion bat
                      WHERE bat.pid         = p_pid
                        AND bat.proceso_cod = p_proceso_cod
                        AND bat.opera_cod   = v_opera_cod_ant
                        AND bat.estado_cod  = 4 ) THEN
         LET r_actualiza = 3; --No ha finalizado la operacion anterior
      ELSE
         IF v_opera_cod_post > 0 AND
            EXISTS (SELECT bat.opera_cod
                      FROM bat_ctr_operacion bat
                     WHERE bat.pid         = p_pid
                       AND bat.proceso_cod = p_proceso_cod
                       AND bat.opera_cod   = v_opera_cod_post
                       AND bat.estado_cod  IN (2,4) ) THEN

            LET r_actualiza = 4; --Se ha ejecutado una operacion posterior
         ELSE
            IF EXISTS (SELECT bat.opera_cod
                         FROM bat_ctr_operacion bat
                        WHERE bat.pid         = p_pid
                          AND bat.proceso_cod = p_proceso_cod
                          AND bat.opera_cod   = p_opera_cod
                          AND bat.estado_cod  IN (2,4) ) THEN

               LET r_actualiza = 5; --Ejecutanda o ejecutandose la operacion
            ELSE
               IF v_opera_cod_ant = 0 THEN
                  UPDATE bat_ctr_proceso
                     SET estado_cod  = v_estado
                   WHERE pid         = p_pid
                     AND proceso_cod = p_proceso_cod;
               END IF

               UPDATE bat_ctr_operacion
                  SET folio       = p_folio  ,
                      fecha_ini   = CURRENT YEAR TO SECOND ,
                      nom_archivo = p_archivo,
                      estado_cod  = v_estado ,
                      usuario     = p_usuario
                WHERE pid         = p_pid
                  AND proceso_cod = p_proceso_cod
                  AND opera_cod   = p_opera_cod;
            END IF
         END IF
      END IF
   ELSE
      LET r_actualiza = 2; --No se ha insertado la operacion
   END IF
   
   RETURN r_actualiza;
END FUNCTION;


