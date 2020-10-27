






CREATE PROCEDURE "safreviv".sp_prt_resp_solicita_traspaso_cedente(p_id_bus_solicitud_tramite DECIMAL(9,0),
                                                       p_folio_procesar VARCHAR(50))
                                                    
-- Datos del BUS
DEFINE v_sequencia_bus DECIMAL(9,0);
DEFINE v_campo         CHAR(40);
DEFINE v_cad_error     LVARCHAR(1000);

-- Datos de la solicitud
DEFINE v_folio_cliente    CHAR(50);
DEFINE v_folio_procesar   CHAR(50);

DEFINE p_id_prt_sol_ced DECIMAL(9,0);

DEFINE v_error_sql     INTEGER;
DEFINE v_error_isam    INTEGER;
DEFINE v_msg_sql       CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      
             
   END EXCEPTION WITH RESUME;
   
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_resp_solicita_traspaso_cedente.trace';
   --TRACE ON;
   
   -- El folio puede ser actualizado con el que llega como parámetro, pero es recuperado desde el origen
   SELECT folio_procesar,
          folio_cliente
     INTO v_folio_procesar,
          v_folio_cliente
     FROM prt_bus_traspaso
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
   
   LET p_id_prt_sol_ced = v_folio_cliente; -- Recupera identificador de la solicitud
   
   -- Actualiza el folio procesar en la solicitud traspaso cedente    
   UPDATE prt_traspaso_cedente
      SET folio_procesar = v_folio_procesar
    WHERE id_prt_traspaso_cedente = p_id_prt_sol_ced;
   
   DELETE 
     FROM prt_bus_traspaso
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
                                                    
END PROCEDURE;


