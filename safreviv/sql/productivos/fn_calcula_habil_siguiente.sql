






CREATE FUNCTION "safreviv".fn_calcula_habil_siguiente(p_fecha DATE)
   RETURNING DATE

   DEFINE v_hoy_dia       DATE;
   DEFINE v_dia_sig_habil DATE;
   DEFINE v_dia           SMALLINT;
   DEFINE v_idx           SMALLINT;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/fn_cal_habil_siguiente.trace" ;
   --TRACE ON;

   LET v_hoy_dia = p_fecha + 1 UNITS DAY;
   LET v_dia     = WEEKDAY(v_hoy_dia);
   LET v_idx     = 1;

   WHILE v_idx = 1
      IF (v_dia = 6 OR v_dia = 0)  THEN
         LET v_hoy_dia = v_hoy_dia + 1 UNITS DAY ;
         LET v_dia = WEEKDAY(v_hoy_dia);
         CONTINUE WHILE;
      ELSE
         IF EXISTS ( SELECT "X"
                       FROM cat_feriado_infonavit
                      WHERE feriado_fecha = v_hoy_dia
                     GROUP BY 1 
                    ) THEN
            LET v_hoy_dia = v_hoy_dia + 1 UNITS DAY ;
            CONTINUE WHILE;
         ELSE 
            LET v_idx = v_idx + 1;
            EXIT WHILE;
         END IF
      END IF
   END WHILE
 
   LET v_dia_sig_habil = v_hoy_dia;

   RETURN v_dia_sig_habil;
END FUNCTION
;


