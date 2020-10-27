






CREATE PROCEDURE "safreviv".sp_prt_resp_solicita_marca_cedente(p_id_bus_solicitud_tramite DECIMAL(9,0),
                                                    p_folio_procesar VARCHAR(50))
                                                    
-- Datos del BUS
DEFINE v_sequencia_bus DECIMAL(9,0);
DEFINE v_campo         CHAR(40);
DEFINE v_cad_error     LVARCHAR(1000);

-- Datos de la solicitud
DEFINE v_folio_clente     CHAR(50);
DEFINE v_origen_solicitud CHAR(3);
DEFINE v_nss              CHAR(11);
DEFINE v_curp             CHAR(18);
DEFINE v_nombre           VARCHAR(40);
DEFINE v_paterno          VARCHAR(40);
DEFINE v_materno          VARCHAR(40);
DEFINE v_folio_procesar   CHAR(50);

DEFINE p_id_prt_sol_ced DECIMAL(9,0);

DEFINE v_error_sql     INTEGER;
DEFINE v_error_isam    INTEGER;
DEFINE v_msg_sql       CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      
             
   END EXCEPTION WITH RESUME;
   
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_prt_resp_solicita_marca_cedente.trace';
   --TRACE ON;
   
   -- El folio puede ser actualizado con el que llega como parámetro, pero es recuperado desde el origen
   SELECT folio_procesar,
          folio_cliente
     INTO v_folio_procesar,
          v_folio_clente
     FROM prt_bus_solicitud_marca
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
   
   LET p_id_prt_sol_ced = v_folio_clente; -- Recupera identificador de la solicitud
   
   -- Actualiza el folio procesar en la solicitud cedente
   UPDATE prt_solicitud_cedente
      SET folio_procesar = v_folio_procesar
    WHERE id_prt_solicitud_cedente = p_id_prt_sol_ced;
   
   DELETE 
     FROM prt_bus_solicitud_marca
    WHERE id_bus_solicitud_tramite = p_id_bus_solicitud_tramite;
                                                    
END PROCEDURE;


