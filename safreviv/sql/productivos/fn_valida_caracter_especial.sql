






CREATE FUNCTION "safreviv".fn_valida_caracter_especial(p_valor CHAR(1))
   RETURNING SMALLINT ;

   DEFINE v_bandera SMALLINT;

   LET v_bandera = 0;

   IF EXISTS ( SELECT ASCII(p_valor)
               FROM systables
               WHERE tabid = 1
               AND(ascii(p_valor) NOT BETWEEN 65 and 90 AND
               ascii(p_valor) NOT BETWEEN 97 and 122 AND
               ascii(p_valor) NOT BETWEEN 48 and 57 AND
               ascii(p_valor) <> 32  AND
               ascii(p_valor) <> 33  AND
               ascii(p_valor) <> 35  AND
               ascii(p_valor) <> 36  AND
               ascii(p_valor) <> 37  AND
               ascii(p_valor) <> 38  AND
               ascii(p_valor) <> 39  AND
               ascii(p_valor) <> 63  AND
               ascii(p_valor) <> 64  AND
               ascii(p_valor) <> 131 AND
               ascii(p_valor) <> 145 AND
               ascii(p_valor) <> 194 AND
               ascii(p_valor) <> 195 AND
               ascii(p_valor) <> 209)
               
               ) THEN
      LET v_bandera = 1 ;
   END IF

   RETURN v_bandera ;
END FUNCTION;


