






CREATE FUNCTION "safreviv".fn_actualiza_acreditado_tarea264(p_clave INTEGER)
   RETURNING INTEGER, INTEGER
   
   DEFINE v1_id_ocg_formalizacion LIKE ocg_formalizacion.id_ocg_formalizacion ;
   DEFINE v1_estado               LIKE ocg_formalizacion.estado               ;
   DEFINE v1_situacion            LIKE ocg_formalizacion.situacion            ;
   DEFINE v_error                 INTEGER                                     ;
   DEFINE cont_1                  INTEGER                                     ;
 
   ON EXCEPTION SET v_error
      LET cont_1 = 0; 
      RETURN v_error, cont_1;
   END EXCEPTION;
   
   SET DEBUG FILE TO '/safreviv_int/BD/fn_actualiza_acreditado_tare264.trace';
   TRACE ON;
   
   LET v_error = 0 ;
   LET cont_1  = 0 ;
              
   IF p_clave = 18 THEN
       FOREACH
           SELECT a.id_ocg_formalizacion  ,
                  a.estado                ,
                  a.situacion
           INTO   v1_id_ocg_formalizacion ,
                  v1_estado               ,
                  v1_situacion
           FROM   ocg_formalizacion a, ocg_acreditado b
           WHERE  a.id_ocg_formalizacion = b.id_ocg_formalizacion
           
           UPDATE ocg_acreditado
           SET    estado    = v1_estado ,
                  situacion = v1_situacion
           WHERE  id_ocg_formalizacion = v1_id_ocg_formalizacion;

           LET cont_1 = cont_1 + 1;
       END FOREACH
   END IF
   RETURN v_error, cont_1;
END FUNCTION;


