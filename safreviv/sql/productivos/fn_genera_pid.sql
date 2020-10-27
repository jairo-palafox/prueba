






CREATE FUNCTION "safreviv".fn_genera_pid(
                p_proceso     SMALLINT,
                p_operacion   SMALLINT,
                p_usuario     CHAR(20))
       RETURNING DECIMAL(9,0);

   DEFINE v_pid     DECIMAL(9,0);

   INSERT INTO glo_pid VALUES ( seq_glo_pid.NEXTVAL, 
                                p_proceso,
                                p_operacion,
                                0,
                                p_usuario,
                                TODAY);
                          
   SELECT seq_glo_pid.CURRVAL
     INTO v_pid
     FROM cat_proceso
    WHERE proceso_cod = p_proceso ;

   RETURN v_pid;

END FUNCTION
;


