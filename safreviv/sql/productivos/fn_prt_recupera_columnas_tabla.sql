






CREATE FUNCTION "safreviv".fn_prt_recupera_columnas_tabla(p_entidad_negocio CHAR(40))
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          CHAR(40),
          SMALLINT;

DEFINE v_nombre_columna CHAR(40);
DEFINE v_orden_columna  SMALLINT;

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql      
      LET v_nombre_columna = '';
      LET v_orden_columna = 0;
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_nombre_columna,
             v_orden_columna;
   END EXCEPTION WITH RESUME;
      
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   FOREACH SELECT col.colname,
                  col.colno
             INTO v_nombre_columna,
                  v_orden_columna
             FROM systables tab JOIN syscolumns col
               ON col.tabid = tab.tabid
            WHERE tab.tabname = p_entidad_negocio
              AND col.colname NOT IN ('id_bus_solicitud_tramite', 'folio_procesar','bus_proceso_cod','bus_operacion_cod') -- No se consideran los campos de control para el bus, sólo se deben considerar los del contrato
            
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_nombre_columna,
             v_orden_columna WITH RESUME;
            
   END FOREACH;

END FUNCTION;


