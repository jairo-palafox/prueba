






CREATE FUNCTION "safreviv".fn_valida_letra(p_valor CHAR(1))
      RETURNING SMALLINT ;

  DEFINE v_bandera SMALLINT;

  LET v_bandera = 0;

  IF EXISTS ( SELECT ASCII(p_valor)
                FROM systables
               WHERE tabid = 1
                 AND(ascii(p_valor) between 65 and 90
                  OR ascii(p_valor) between 97 and 122
                  OR ascii(p_valor) = 32
                  OR ascii(p_valor) = 35)
             ) THEN
    LET v_bandera = 1 ;
  END IF

  RETURN v_bandera ;
END FUNCTION;


