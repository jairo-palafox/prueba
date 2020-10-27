






CREATE FUNCTION "safreviv".fn_prt_solicita_marca_cedente(p_id_prt_sol_ced VARCHAR(10),
                                              p_proceso_cod    CHAR(3),
                                              p_operacion_cod  CHAR(4),
                                              p_folio_cliente VARCHAR(10))
RETURNING INTEGER,
          INTEGER,
          CHAR(254);

-- Datos del BUS
DEFINE v_sequencia_bus DECIMAL(9,0);
DEFINE v_campo         CHAR(40);
DEFINE v_cad_error     LVARCHAR(1000);

-- Datos de la solicitud
DEFINE v_folio_cliente    CHAR(50);
DEFINE v_origen_solicitud CHAR(3);
DEFINE v_nss              CHAR(11);
DEFINE v_curp             CHAR(18);
DEFINE v_nombre           VARCHAR(40);
DEFINE v_paterno          VARCHAR(40);
DEFINE v_materno          VARCHAR(40);
DEFINE v_folio_procesar   CHAR(50);
DEFINE v_tipo_portabilidad CHAR(2);

DEFINE v_error_sql     INTEGER;
DEFINE v_error_isam    INTEGER;
DEFINE v_msg_sql       CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      

      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql;
             
   END EXCEPTION;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_prt_solicita_marca_cedente.trace';
   --SET DEBUG FILE TO '/tmp/fn_prt_solicita_marca_cedente.trace';
   --TRACE ON;
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   LET v_sequencia_bus  = seq_bus_solicitud_tramite.NEXTVAL;    -- Secuencia única del BUS
   --LET v_sequencia_bus  = 1; -- valor de prueba
   LET v_campo          = "id_bus_solicitud_tramite"; -- Columna ID de tabla temporal para envio de la solicitud
   LET v_cad_error      = "";
   LET v_origen_solicitud = '001'; -- Origen INFONAVIT
   --LET v_folio_cliente     = p_id_prt_sol_ced;
   LET v_folio_cliente    = p_folio_cliente;
   
   -- Recupera solicitud de portabilidad cedente
   SELECT nss,
          curp,
          nombre,
          paterno,
          materno,
          CONCAT('0',tipo_portabilidad)
     INTO v_nss,
          v_curp,
          v_nombre,
          v_paterno,
          v_materno,
          v_tipo_portabilidad
     FROM prt_solicitud_cedente
    WHERE id_prt_solicitud_cedente = p_id_prt_sol_ced;
    
   -- Registra solicitud en tabla temporal de paso
   INSERT INTO prt_bus_solicitud_marca 
   (id_bus_solicitud_tramite,
    bus_proceso_cod,
    bus_operacion_cod,
    folio_cliente,
    origen_solicitud,
    tipo_solicitud,
    nss,
    ap_paterno,
    ap_materno,
    nombre,
    curp)
   VALUES(
    v_sequencia_bus,
    p_proceso_cod,
    p_operacion_cod,
    v_folio_cliente,
    v_origen_solicitud,
    v_tipo_portabilidad,
    v_nss,
    v_paterno,
    v_materno,
    v_nombre,
    v_curp);
   
 
   -- Llama funcion para invocar WS del BUS para solicitar marca a procesar
   -- A petición de CRM para pruebas, no llama bus
   EXECUTE PROCEDURE sp_bus_convierte_mensaje(v_folio_cliente,
                                              v_sequencia_bus,
                                              p_proceso_cod,
                                              p_operacion_cod,
                                              v_campo,
                                              v_cad_error);
   
       
   RETURN v_error_sql,
          v_error_isam,
          v_msg_sql;

END FUNCTION;


