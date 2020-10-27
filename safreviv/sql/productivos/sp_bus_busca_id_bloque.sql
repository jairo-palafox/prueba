






CREATE PROCEDURE "safreviv".sp_bus_busca_id_bloque(p_proceso    CHAR(003),
                                     p_operacion  CHAR(004),
                                     p_nombre_campo VARCHAR(255))
RETURNING DECIMAL(9,0); 


DEFINE v_id_cat_bus_detalle_contrato   DECIMAL(9,0); 

   --SET DEBUG FILE TO '/afore/iss/safre/sp_bus_busca_id_bloque.trace';
   --TRACE ON;

           SELECT d.id_cat_bus_detalle_contrato
           INTO   v_id_cat_bus_detalle_contrato
-- originalmente tablas de safrebus
           FROM   cat_bus_proceso           a    ,
                  cat_bus_operacion         b    ,
                  cat_bus_contrato          c    ,
                  cat_bus_detalle_contrato  d
           WHERE  a.cod_proceso_bus      = p_proceso
           AND    b.cod_opera_bus        = p_operacion
           AND    a.id_cat_bus_proceso   = b.id_cat_bus_proceso
           AND    b.id_cat_bus_operacion = c.id_cat_bus_operacion
           AND    c.ind_vigencia         = 1
           AND    c.id_cat_bus_contrato  = d.id_cat_bus_contrato
           AND    d.atributo_negocio     = p_nombre_campo;


RETURN v_id_cat_bus_detalle_contrato;

END PROCEDURE;


