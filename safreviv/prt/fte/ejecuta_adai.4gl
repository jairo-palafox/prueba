# Ejecuta el cliente que informa a adai de la marca (PRTW03)
MAIN
DEFINE v_num_caso     VARCHAR(10),
       v_idestatus    CHAR(5),
       v_id_motivo    CHAR(4),
       ws_error       INTEGER,
       v_resultado_op CHAR(5),
       v_mensaje      VARCHAR(254)
      
   LET v_num_caso  = ARG_VAL(1)
   LET v_idestatus = ARG_VAL(2)
   LET v_id_motivo = ARG_VAL(3)

   CALL recibeMarcaCedente(v_num_caso,
                           v_idestatus,
                           v_id_motivo) RETURNING ws_error,
                                                  v_resultado_op,
                                                  v_mensaje

   DISPLAY "ws_error:       ",ws_error
   DISPLAY "v_resultado_op: ",v_resultado_op
   DISPLAY "v_mensaje:      ",v_mensaje

END MAIN