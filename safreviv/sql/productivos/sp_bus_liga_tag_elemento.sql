






CREATE PROCEDURE "safreviv".sp_bus_liga_tag_elemento(p_v_c ROW(c1 CHAR(120),c2 CHAR(120),c3 CHAR(120),c4 CHAR(120),c5 CHAR(120),c6 CHAR(120),c7 CHAR(120),c8 CHAR(120),c9 CHAR(120),c10 CHAR(120),c11 CHAR(120),c12 CHAR(120),c13 CHAR(120),c14 CHAR(120),c15 CHAR(120)),
                                          p_campo_multiset      CHAR(050),
                                          p_tabla_negocio       CHAR(050),
                                          p_indice              SMALLINT,
                                          p_proceso             CHAR(003),
                                          p_operacion           CHAR(004))

RETURNING VARCHAR(50)      ,  -- v_tag
          VARCHAR(120)     ;  -- v_valor_elemento

----------------------------------------------------------------------
-- SafreWEB.SafreBUS. 2014
-- Descripcion. funcion que mapea el valor del campo de un arreglo y
-- devuelve el valor ligado a su correspondiente tag de su estructura
-- XML.
-- Autor. Jesus David Yañez Moreno
-- Fecha. 31/03/2014
-- SAFRE
---------------------------------------------------------------------


DEFINE v_tag              VARCHAR(50);
DEFINE v_valor_elemento   VARCHAR(120);

DEFINE v_id_campo          CHAR(005);
DEFINE v_id_cat_bus_detalle_contrato DECIMAL(9,0);

--SET DEBUG FILE TO '/afore/iss/safre/log/sp_bus_liga_tag_elemento.trace';
--TRACE ON;
  LET v_id_campo = "c"||p_indice;

  IF    p_indice = 1  THEN LET v_valor_elemento = p_v_c.c1;
   ELIF p_indice = 2  THEN LET v_valor_elemento = p_v_c.c2;
   ELIF p_indice = 3  THEN LET v_valor_elemento = p_v_c.c3;
   ELIF p_indice = 4  THEN LET v_valor_elemento = p_v_c.c4;
   ELIF p_indice = 5  THEN LET v_valor_elemento = p_v_c.c5;
   ELIF p_indice = 6  THEN LET v_valor_elemento = p_v_c.c6;
   ELIF p_indice = 7  THEN LET v_valor_elemento = p_v_c.c7;
   ELIF p_indice = 8  THEN LET v_valor_elemento = p_v_c.c8;
   ELIF p_indice = 9  THEN LET v_valor_elemento = p_v_c.c9;
   ELIF p_indice = 10 THEN LET v_valor_elemento = p_v_c.c10;
   ELIF p_indice = 11 THEN LET v_valor_elemento = p_v_c.c11;
   ELIF p_indice = 12 THEN LET v_valor_elemento = p_v_c.c12;
   ELIF p_indice = 13 THEN LET v_valor_elemento = p_v_c.c13;
   ELIF p_indice = 14 THEN LET v_valor_elemento = p_v_c.c14;
   ELIF p_indice = 15 THEN LET v_valor_elemento = p_v_c.c15;
  END IF;

          EXECUTE PROCEDURE sp_bus_busca_id_bloque(p_proceso    ,
                                                   p_operacion  ,
                                                   p_campo_multiset)
          INTO v_id_cat_bus_detalle_contrato;
                                                    
          SELECT f.cve_natural_bloque
          INTO v_tag
          FROM sysxtdtypes a, sysxtdtypes a1, sysattrtypes   b ,
               sysattrtypes b1, sysattrtypes b2, systables   c ,
               syscolumns   d ,
--originalmente de safre_bus
               cat_bus_bloque         e ,        -- efp desa peiss
               cat_bus_detalle_bloque f
          WHERE d.colname = p_campo_multiset
          AND d.coltype = 20 AND c.tabname = p_tabla_negocio AND d.tabid = c.tabid
          AND d.extended_id = b.extended_id AND b.levelno = 1 AND b.xtd_type_id = b1.extended_id
          AND b1.levelno = 1 AND b1.extended_id = a.extended_id 
          AND a1.extended_id = b2.extended_id AND b2.levelno = 1 AND b2.fieldno = b1.fieldno
          AND b1.fieldname = TRIM(v_id_campo)
          AND e.id_cat_bus_detalle_contrato = v_id_cat_bus_detalle_contrato
          AND e.cve_natural                 = a1.name
          AND e.id_cat_bus_bloque           = f.id_cat_bus_bloque
          AND f.atributo_negocio            = b2.fieldname;


        
RETURN v_tag             ,
       v_valor_elemento  ;

END PROCEDURE;


