






CREATE FUNCTION "safreviv".fn_tab_movimiento (p_ind_criterio SMALLINT,
                                   p_folio        DECIMAL(9,0),
                                   p_fecha        DATE )
RETURNING VARCHAR(20);

DEFINE v_tabla VARCHAR(20);

IF p_ind_criterio = 0 THEN

   SELECT g.tabla
     INTO v_tabla
     FROM glo_his_folio g
    WHERE folio = p_folio;

    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_tabla = "cta_movimiento";
    END IF
ELSE
   SELECT c.tabla
     INTO v_tabla
     FROM cat_tab_movimiento c
    WHERE p_fecha BETWEEN f_inicial AND f_final;

    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_tabla = "cta_movimiento";
    END IF
END IF

RETURN v_tabla;

END FUNCTION
;


