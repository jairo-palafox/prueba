






CREATE FUNCTION "safreviv".fn_transforma_fecha_ret_ley73(p_fecha CHAR(10))
RETURNING DATE

   DEFINE v_char_fecha_nueva VARCHAR(10); -- fecha temporal
   DEFINE v_fecha_invalida   DATE; -- fecha invalida 01/01/0001
   DEFINE v_fecha_nueva      DATE; -- fecha nueva en formato fecha

   -- se asume que hay error
   LET v_fecha_invalida = DATE("01/01/0001");

   -- se verifica si la fecha esta vacia
   IF ( p_fecha IS NULL ) THEN
      LET v_fecha_nueva = v_fecha_invalida;
      
   ELSE
      -- si se recibio 00.00.0000
      IF ( p_fecha = "00.00.0000" ) THEN
         LET v_fecha_nueva = v_fecha_invalida;
      ELSE
         -- se recibio una fecha. se verifica que se tengan cifras para dia, mes ano
         IF ( ( p_fecha[1,2] > "00" AND p_fecha[1,2] < "32" ) AND
              ( p_fecha[4,5] > "00" AND p_fecha[4,5] < "13" ) AND
              ( p_fecha[7,10] >= "0000" AND p_fecha[7,10] <= "9999" ) ) THEN
            -- se cambia el formato de fecha (MM/DD/YYYY)
            LET v_char_fecha_nueva = p_fecha[4,5] || "/" || p_fecha[1,2] || "/" || p_fecha[7,10];
            
            LET v_fecha_nueva = DATE(v_char_fecha_nueva);
         ELSE
            -- se da muna fecha invalida
            LET v_fecha_nueva = v_fecha_invalida;
         END IF
      END IF
   END IF          
   
   -- se devuelve la fecha nueva
   RETURN v_fecha_nueva;
END FUNCTION;


