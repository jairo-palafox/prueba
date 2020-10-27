






CREATE FUNCTION "safreviv".fn_prt_recupera_detalle_contrato(p_id_cat_bus_contrato DECIMAL(9,0),
                                                 p_entidad_negocio     CHAR(40))
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          DECIMAL(9,0),
          DECIMAL(9,0),
          CHAR(50),
          CHAR(50),
          CHAR(50),
          CHAR(50),
          CHAR(1),
          SMALLINT,
          SMALLINT,
		      SMALLINT,
		      SMALLINT;
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:09/05/2014
--===============================================================

DEFINE v_id_cat_bus_detalle_contrato DECIMAL(9,0);
DEFINE v_id_cat_bus_contrato         DECIMAL(9,0);
DEFINE v_cve_natural                 CHAR(50);
DEFINE v_etiqueta                    CHAR(50);
DEFINE v_atributo_negocio            CHAR(50);
DEFINE v_atributo_negocio_sys        CHAR(50);
DEFINE v_tipo_dato                   CHAR(1);
DEFINE v_orden                       SMALLINT;
DEFINE v_orden_sys                   SMALLINT;
DEFINE v_sentido                     SMALLINT;
DEFINE v_opcional                    SMALLINT;

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql      
      LET v_id_cat_bus_detalle_contrato = 0;
      LET v_id_cat_bus_contrato         = 0;
      LET v_cve_natural                 = '';
      LET v_etiqueta                    = '';
      LET v_atributo_negocio            = '';
      LET v_atributo_negocio_sys        = '';
      LET v_tipo_dato                   = '';
      LET v_orden                       = 0;
      LET v_orden_sys                   = 0;
      LET v_sentido                     = 0;
      LET v_opcional                    = 0;
      
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_id_cat_bus_detalle_contrato,
             v_id_cat_bus_contrato,
             v_cve_natural,
             v_etiqueta,
             v_atributo_negocio,
             v_atributo_negocio_sys,
             v_tipo_dato,
             v_orden,
             v_orden_sys,
             v_sentido,
             v_opcional;
   END EXCEPTION WITH RESUME;
      
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   FOREACH SELECT det.id_cat_bus_detalle_contrato,
                  det.id_cat_bus_contrato,
                  det.cve_natural,
                  det.etiqueta,
                  det.atributo_negocio,
                  col.colname,
                  det.tipo_dato,
                  det.orden,
                  col.colno,
                  det.ind_sentido,
                  det.ind_opcional
             INTO v_id_cat_bus_detalle_contrato,
                  v_id_cat_bus_contrato,
                  v_cve_natural,
                  v_etiqueta,
                  v_atributo_negocio,
                  v_atributo_negocio_sys,
                  v_tipo_dato,
                  v_orden,
                  v_orden_sys,
                  v_sentido,
                  v_opcional
             FROM systables tab JOIN syscolumns col
               ON col.tabid = tab.tabid
                  LEFT OUTER JOIN cat_bus_detalle_contrato det
                  --JOIN cat_bus_detalle_contrato det
               ON det.id_cat_bus_contrato = p_id_cat_bus_contrato
              AND det.atributo_negocio = col.colname
            WHERE tab.tabname = p_entidad_negocio
              AND col.colname NOT IN ('id_bus_solicitud_tramite', 'folio_procesar','bus_proceso_cod','bus_operacion_cod') -- No se consideran los campos de control para el bus, sólo se deben considerar los del contrato
            ORDER BY col.colno
            
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_id_cat_bus_detalle_contrato,
             v_id_cat_bus_contrato,
             v_cve_natural,
             v_etiqueta,
             v_atributo_negocio,
             v_atributo_negocio_sys,
             v_tipo_dato,
             v_orden,
             v_orden_sys,
             v_sentido,
             v_opcional WITH RESUME;
            
   END FOREACH;
                
END FUNCTION;


