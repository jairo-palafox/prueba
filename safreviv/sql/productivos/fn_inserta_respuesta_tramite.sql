






CREATE FUNCTION "safreviv".fn_inserta_respuesta_tramite(p_id_bus_tramite              DECIMAL(9,0),
                                             p_id_bus_solicitud_tramite    DECIMAL(9,0),
                                             p_cod_respuesta               CHAR(2),
                                             p_cod_respuesta_opr           CHAR(2),
                                             p_desc_respuesta              VARCHAR(100),
                                             p_cod_oper_cliente            CHAR(50),
                                             p_folio_ack                   CHAR(50)
                                             )
RETURNING DECIMAL(9,0);

   DEFINE v_id_bus_respuesta_tramite         DECIMAL(9,0);
   DEFINE v_f_operacion                      DATE;
   DEFINE v_h_operacion                      DATETIME HOUR TO SECOND;

   LET v_f_operacion = TODAY;
   LET v_h_operacion = CURRENT HOUR TO SECOND;
   
   INSERT INTO bus_respuesta_tramite ( id_bus_respuesta_tramite,
                                       id_bus_tramite,
                                       id_bus_solicitud_tramite,
                                       cod_respuesta,
                                       cod_respuesta_opr,
                                       desc_respuesta,
                                       cod_oper_cliente,
                                       folio_ack,
                                       f_operacion,
                                       h_operacion)
                              VALUES ( seq_bus_respuesta_tramite.NEXTVAL,
                                       p_id_bus_tramite,
                                       p_id_bus_solicitud_tramite,
                                       p_cod_respuesta,
                                       p_cod_respuesta_opr,
                                       p_desc_respuesta,
                                       p_cod_oper_cliente,
                                       p_folio_ack,
                                       v_f_operacion,
                                       v_h_operacion);
                                       
   SELECT seq_bus_respuesta_tramite.CURRVAL
   INTO v_id_bus_respuesta_tramite
   FROM bus_tramite
   WHERE id_bus_tramite = p_id_bus_tramite;

   RETURN v_id_bus_respuesta_tramite;
END FUNCTION;


