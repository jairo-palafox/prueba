






CREATE FUNCTION "safreviv".fn_es_numero(p_valor char(1))
RETURNING smallint ;

DEFINE v_numero smallint;

LET v_numero = 0;

IF EXISTS (  SELECT ASCII(p_valor)
              FROM systables
             WHERE tabid = 1
               AND ascii(p_valor) >= 48
               AND ascii(p_valor) <= 57
           ) THEN
LET v_numero = 1 ;

END IF

RETURN v_numero ;
END FUNCTION;


