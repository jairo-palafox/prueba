






CREATE FUNCTION "safreviv".fn_prt_recupera_detalle_entidad(p_id_entidad_padre         DECIMAL(9,0),
                                                p_entidad_negocio          CHAR(40),
                                                p_nom_cve_natural_contrato CHAR(30),
                                                p_nom_cve_natural_bloque   CHAR(30))
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
          SMALLINT;    
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:09/05/2014
--===============================================================

DEFINE v_id_cat_bus_detalle_entidad DECIMAL(9,0);
DEFINE v_id_cat_bus_padre            DECIMAL(9,0);
DEFINE v_cve_natural                 CHAR(50);
DEFINE v_etiqueta                    CHAR(50);
DEFINE v_atributo_negocio            CHAR(50);
DEFINE v_atributo_negocio_sys        CHAR(50);
DEFINE v_tipo_dato                   CHAR(1);
DEFINE v_orden                       SMALLINT;
DEFINE v_orden_sys                   SMALLINT;
DEFINE v_opcional                    SMALLINT;

DEFINE v_consulta   CHAR(1000);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, 
                    v_error_isam, 
                    v_msg_sql      
					
      LET v_id_cat_bus_detalle_entidad = 0;
      LET v_id_cat_bus_padre           = 0;
      LET v_cve_natural                = '';
      LET v_etiqueta                   = '';
      LET v_atributo_negocio           = '';
      LET v_atributo_negocio_sys       = '';
      LET v_tipo_dato                  = '';
      LET v_orden                      = 0;
      LET v_orden_sys                  = 0;
      LET v_opcional                   = 0;
      
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_id_cat_bus_detalle_entidad,
             v_id_cat_bus_padre,
             v_cve_natural,
             v_etiqueta,
             v_atributo_negocio,
             v_atributo_negocio_sys,
             v_tipo_dato,
             v_orden,
             v_orden_sys,
             v_opcional;
   END EXCEPTION WITH RESUME;
      
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";
   
   FOREACH SELECT det.id_cat_bus_detalle_bloque,
                  det.id_cat_bus_bloque,
                  det.cve_natural_bloque,
                  det.etiqueta,
                  det.atributo_negocio,
                  b2.fieldname,
                  det.tipo_dato,
                  det.orden,
                  d.colno,
                  det.ind_opcional
             INTO v_id_cat_bus_detalle_entidad,
                  v_id_cat_bus_padre,
                  v_cve_natural,
                  v_etiqueta,
                  v_atributo_negocio,
                  v_atributo_negocio_sys,
                  v_tipo_dato,
                  v_orden,
                  v_orden_sys,
                  v_opcional
             FROM syscolumns d
                  JOIN systables c
               ON d.tabid = c.tabid
                  JOIN sysattrtypes b
               ON d.extended_id = b.extended_id
                  JOIN sysattrtypes b1
               ON b.xtd_type_id = b1.extended_id
                  JOIN sysattrtypes b2
               ON b2.fieldno = b1.fieldno
                  JOIN sysxtdtypes a1
               ON a1.extended_id = b2.extended_id
                  LEFT OUTER JOIN cat_bus_detalle_bloque det
               ON det.atributo_negocio = b2.fieldname
              AND det.id_cat_bus_bloque = p_id_entidad_padre
                  JOIN sysxtdtypes a
               ON b1.extended_id = a.extended_id          
            WHERE d.colname = p_nom_cve_natural_contrato
              AND d.coltype = 20 
              AND c.tabname = p_entidad_negocio
              AND b.levelno = 1 
              AND b1.levelno = 1 
              AND a1.name = p_nom_cve_natural_bloque
              AND b2.levelno = 1 
            ORDER BY d.colno
     
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_id_cat_bus_detalle_entidad,
             v_id_cat_bus_padre,
             v_cve_natural,
             v_etiqueta,
             v_atributo_negocio,
             v_atributo_negocio_sys,
             v_tipo_dato,
             v_orden,
             v_orden_sys, 
             v_opcional WITH RESUME;
			 
   END FOREACH;
                
END FUNCTION;


