






CREATE FUNCTION "safreviv".fn_prt_recupera_columnas_lista(p_entidad_negocio CHAR(40),
                                               p_nom_cve_natural_contrato VARCHAR(50),
                                               p_nom_cve_natural_bloque   VARCHAR(50))
RETURNING INTEGER,
          INTEGER,
          CHAR(254),
          VARCHAR(50);
--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:09/05/2014
--===============================================================
DEFINE v_nombre_columna VARCHAR(50);

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      LET v_nombre_columna = '';
      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_nombre_columna;
   END EXCEPTION WITH RESUME;

   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";

   FOREACH SELECT b2.fieldname
             INTO v_nombre_columna
             FROM sysxtdtypes a, sysxtdtypes a1, sysattrtypes   b ,
                  sysattrtypes b1, sysattrtypes b2, systables   c ,
                  syscolumns   d
            WHERE d.colname = p_nom_cve_natural_contrato
              AND d.coltype = 20
              AND c.tabname = p_entidad_negocio
              AND d.tabid = c.tabid
              AND d.extended_id = b.extended_id
              AND b.levelno = 1
              AND b.xtd_type_id = b1.extended_id
              AND b1.levelno = 1
              AND b1.extended_id = a.extended_id
              AND a1.name = p_nom_cve_natural_bloque
              AND a1.extended_id = b2.extended_id
              AND b2.levelno = 1
              AND b2.fieldno = b1.fieldno

      RETURN v_error_sql,
             v_error_isam,
             v_msg_sql,
             v_nombre_columna WITH RESUME;

   END FOREACH;

END FUNCTION;


