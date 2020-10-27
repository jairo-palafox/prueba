






CREATE FUNCTION "safreviv".fn_ultimo_dia_mes(p_fecha DATE)
       RETURNING SMALLINT;

DEFINE p_dia SMALLINT;

    SELECT ult_dia
      INTO p_dia
      FROM cat_mes
     WHERE mes = MONTH(p_fecha);

    IF MONTH(p_fecha) = 2 THEN
        IF MOD(YEAR(p_fecha),4) = 0 THEN
            LET p_dia = p_dia + 1;
        END IF
    END IF

    RETURN p_dia;
END FUNCTION;


