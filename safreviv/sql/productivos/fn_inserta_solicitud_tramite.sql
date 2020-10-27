






CREATE FUNCTION "safreviv".fn_inserta_solicitud_tramite(p_id_bus_tramite        DECIMAL(9,0),
                                             p_id_sistema            CHAR(10),
                                             p_id_ebusiness          CHAR(10),
                                             p_id_portafolio         CHAR(10),
                                             p_id_servicio           CHAR(10),
                                             p_id_cliente            CHAR(10),
                                             p_id_canal              CHAR(10),
                                             p_cod_oper_cliente      CHAR(50),
                                             p_folio_transaccion     CHAR(50),
                                             p_cod_respuesta_opr     CHAR(2)
                                             )
RETURNING DECIMAL(9,0);

   DEFINE v_id_bus_solicitud_tramite         DECIMAL(9,0);
   DEFINE v_f_operacion                      DATE;
   DEFINE v_h_operacion                      DATETIME HOUR TO SECOND;

   LET v_f_operacion = TODAY;
   LET v_h_operacion = CURRENT HOUR TO SECOND;
   
   INSERT INTO bus_solicitud_tramite (id_bus_solicitud_tramite,
                                       id_bus_tramite,
                                       id_sistema,
                                       id_ebusiness,
                                       id_portafolio,
                                       id_servicio,
                                       id_cliente,
                                       id_canal,
                                       cod_oper_cliente,
                                       folio_transaccion,
                                       cod_respuesta_opr,
                                       f_operacion,
                                       h_operacion)
                              VALUES ( seq_bus_solicitud_tramite.NEXTVAL,
                                       p_id_bus_tramite,
                                       p_id_sistema,
                                       p_id_ebusiness,
                                       p_id_portafolio,
                                       p_id_servicio,
                                       p_id_cliente,
                                       p_id_canal,
                                       p_cod_oper_cliente,
                                       p_folio_transaccion,
                                       p_cod_respuesta_opr,
                                       v_f_operacion,
                                       v_h_operacion);
                                       
   SELECT seq_bus_solicitud_tramite.CURRVAL
   INTO v_id_bus_solicitud_tramite
   FROM bus_tramite
   WHERE id_bus_tramite = p_id_bus_tramite;

   RETURN v_id_bus_solicitud_tramite;
END FUNCTION;


