






CREATE FUNCTION "safreviv".fn_chk_fin_mes( p_fecha DATE )
RETURNING SMALLINT ;

DEFINE v_mes  SMALLINT;
DEFINE v_dia  SMALLINT;
DEFINE v_anio SMALLINT;

DEFINE v_fin_mes  SMALLINT;
DEFINE v_cociente SMALLINT;

LET v_mes  = MONTH(p_fecha) ;
LET v_dia  = DAY(p_fecha);
LET v_anio = YEAR(p_fecha);

LET v_fin_mes = 0 ;

IF v_mes = 4 OR
   v_mes = 6 OR
   v_mes = 9 OR
   v_mes = 11 THEN

   IF v_dia = 30 THEN
      LET v_fin_mes = 1 ;
   END IF
ELIF v_mes = 1 OR
     v_mes = 3 OR
     v_mes = 5 OR
     v_mes = 7 OR
     v_mes = 8 OR
     v_mes = 10 OR
     v_mes = 12 THEN

     IF v_dia = 31 THEN
        LET v_fin_mes = 1 ;
     END IF
ELSE
   LET v_cociente = v_anio /4 ;

   IF v_anio - ( v_cociente * 4 ) <> 0 THEN
      IF v_dia = 28 THEN
        LET v_fin_mes = 1 ;
      END IF
   ELSE
      IF v_dia = 29 THEN
        LET v_fin_mes = 1 ;
      END IF
   END IF
END IF
RETURN v_fin_mes;

END FUNCTION
;


