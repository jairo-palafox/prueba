






CREATE FUNCTION "safreviv".fn_valida_fecha_por_formato(p_fecha CHAR(10), p_formato_fecha VARCHAR(8))
   RETURNING SMALLINT, DATE
   
DEFINE v_c_fecha_salida         DATE;
DEFINE v_si_dia                 SMALLINT;
DEFINE v_si_mes                 SMALLINT;
DEFINE v_si_ano                 SMALLINT;
DEFINE v_fecha_es_correcta      SMALLINT;

   -- se asume que la fecha es correcta
   LET v_fecha_es_correcta = 1;

   -- se verifica que formato de fecha se esta enviando
   -- formato yyyymmdd
   IF ( p_formato_fecha = "yyyymmdd" ) THEN

      -- se obtienen las cifras
      LET v_si_dia = p_fecha[7,8];
      LET v_si_mes = p_fecha[5,6];
      LET v_si_ano = p_fecha[1,4];
   END IF
   
   -- formato ddmmyyyy
   IF ( p_formato_fecha = "ddmmyyyy" ) THEN
      -- se obtienen las cifras
      LET v_si_dia = p_fecha[1,2];
      LET v_si_mes = p_fecha[3,4];
      LET v_si_ano = p_fecha[5,8];
   END IF

   -- se verifican las cifras
   IF ( (v_si_dia < 0 OR v_si_dia > 31) OR
        (v_si_mes < 1 OR v_si_mes > 12 ) ) THEN
      -- la fecha esta mal
      LET v_fecha_es_correcta = 0;
      
      LET v_c_fecha_salida = NULL;
   ELSE
      -- se construye la fecha 
      LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);
   END IF

   -- se devuelve el resultado de la consulta
   RETURN v_fecha_es_correcta, v_c_fecha_salida;
END FUNCTION
;


