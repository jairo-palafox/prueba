






CREATE FUNCTION "safreviv".fn_valida_nss (p_nss CHAR(11))
RETURNING INTEGER;

   DEFINE r_excepcion    INTEGER;
   DEFINE v_anio         SMALLINT;
   DEFINE v_bandera      SMALLINT;
   DEFINE v_fecha        CHAR(10);
   DEFINE v_digito       SMALLINT;
   DEFINE i              SMALLINT;

   LET r_excepcion = 0;
   LET v_fecha     = TODAY;

   IF LENGTH(p_nss) <> 11 THEN
      LET r_excepcion = 300001;     --Longitud de nss diferente de 11
   END IF

   IF r_excepcion = 0 THEN   --Verifica que solo sean digitos el nss
      FOR i = 1 TO 11
         LET v_bandera = fn_valida_numero(SUBSTR(p_nss,i,1));

         IF v_bandera = 0 THEN
            LET r_excepcion = 300002;       --Solo debe incluir números el nss
            EXIT FOR;
         END IF
      END FOR
   END IF

   IF r_excepcion = 0 THEN   --Verifica el año de registro
      LET v_anio = p_nss[3,4];

      IF v_anio >= 0 AND v_anio <= v_fecha[9,10] THEN --Se obtiene el año de registro
         LET v_anio = 2000 + v_anio;
      ELSE
         LET v_anio = 1900 + v_anio;
      END IF

      IF v_anio < 1943 OR v_anio > YEAR(TODAY) THEN
         LET r_excepcion = 300003;   --El año de registro es erroneo
      END IF
   END IF
   
   RETURN r_excepcion;
END FUNCTION;


