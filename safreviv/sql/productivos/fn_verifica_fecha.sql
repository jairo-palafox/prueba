






CREATE FUNCTION "safreviv".fn_verifica_fecha(p_fecha CHAR(10), p_formato_fecha VARCHAR(8))
RETURNING SMALLINT, DATE

   DEFINE v_c_fecha_salida          DATE;
   DEFINE v_si_dia                  SMALLINT;
   DEFINE v_si_mes                  SMALLINT;
   DEFINE v_si_ano                  SMALLINT;
   DEFINE v_fecha_es_correcta       SMALLINT;
   DEFINE v_cociente                SMALLINT;
   DEFINE v_residuo                 SMALLINT;

   --SET DEBUG FILE TO '/safreviv_int/archivos/fn_verifica_fecha.trace';
   --TRACE ON;

   -- se asume que la fecha es correcta
   LET v_fecha_es_correcta = 1;

   -- se verifica qué formato de fecha se esta enviando
   -- formato yyyymmdd
   IF ( p_formato_fecha = "yyyymmdd" ) THEN
      -- se obtienen las cifras
      LET v_si_dia = p_fecha[7,8];
      LET v_si_mes = p_fecha[5,6];
      LET v_si_ano = p_fecha[1,4];
   END IF

   -- formato mmddyyyy
   IF ( p_formato_fecha = "mmddyyyy" ) THEN
      -- se obtienen las cifras
      LET v_si_dia = p_fecha[3,4];
      LET v_si_mes = p_fecha[1,2];
      LET v_si_ano = p_fecha[5,8];
   END IF

   -- formato ddmmyyyy
   IF ( p_formato_fecha = "ddmmyyyy" ) THEN
      -- se obtienen las cifras
      LET v_si_dia = p_fecha[1,2];
      LET v_si_mes = p_fecha[3,4];
      LET v_si_ano = p_fecha[5,8];
   END IF

   IF p_fecha IS NULL OR
      p_fecha = "" OR
      p_fecha MATCHES " *" THEN
      LET v_fecha_es_correcta = 0;

      LET v_c_fecha_salida = NULL;
   ELIF
      (p_fecha[1] NOT MATCHES '[0-9]*') OR
      (p_fecha[2] NOT MATCHES '[0-9]*') OR
      (p_fecha[3] NOT MATCHES '[0-9]*') OR
      (p_fecha[4] NOT MATCHES '[0-9]*') OR
      (p_fecha[5] NOT MATCHES '[0-9]*') OR
      (p_fecha[6] NOT MATCHES '[0-9]*') OR
      (p_fecha[7] NOT MATCHES '[0-9]*') OR
      (p_fecha[8] NOT MATCHES '[0-9]*') THEN
      LET v_fecha_es_correcta = 0;

      LET v_c_fecha_salida = NULL;
   ELSE
      -- se verifican las cifras
      IF ( (v_si_dia < 0 OR v_si_dia > 31) OR
           (v_si_mes < 1 OR v_si_mes > 12 ) ) THEN
         -- la fecha esta mal
         LET v_fecha_es_correcta = 0;

         LET v_c_fecha_salida = NULL;
      ELSE
         IF (v_si_mes = 4 OR v_si_mes = 6 OR v_si_mes = 9 OR v_si_mes = 11) AND (v_si_dia > 30) THEN
            LET v_fecha_es_correcta = 0;

            LET v_c_fecha_salida = NULL;
         ELSE
            IF (v_si_mes = 2) AND (v_si_dia > 28) THEN
               LET v_cociente = v_si_ano / 4;

               LET v_residuo  = v_si_ano - ( v_cociente * 4 ) ;

               IF v_residuo  <> 0 THEN
                   LET v_fecha_es_correcta = 0;

                   LET v_c_fecha_salida = "";
               ELSE
                  IF (v_si_dia <> 29) THEN
                     LET v_fecha_es_correcta = 0;

                     LET v_c_fecha_salida = NULL;
                  ELSE
                      -- se construye la fecha
                      LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);
                  END IF
               END IF
            ELSE
               -- se construye la fecha
               LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);
            END IF
         END IF
      END IF
   END IF
   -- se devuelve el resultado de la consulta
   RETURN v_fecha_es_correcta, v_c_fecha_salida;

END FUNCTION
;


