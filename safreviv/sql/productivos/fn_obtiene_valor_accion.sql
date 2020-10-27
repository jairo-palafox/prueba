






CREATE FUNCTION "safreviv".fn_obtiene_valor_accion(p_fondo SMALLINT, p_valuacion DATE)
  RETURNING DECIMAL(19,14)
  --
 DEFINE v_precio_fondo DECIMAL(19,14);

 LET v_precio_fondo = 0;

 -- Obtener el precio de la funcion
 FOREACH
    SELECT precio_fondo
      INTO v_precio_fondo
      FROM safre_viv:glo_valor_fondo
     WHERE fondo = p_fondo
       AND f_valuacion = p_valuacion

    -- Si se encontró registro
    RETURN v_precio_fondo;
 END FOREACH

  RETURN NULL;
END FUNCTION -- gn_obtiene_valor_accion
;


