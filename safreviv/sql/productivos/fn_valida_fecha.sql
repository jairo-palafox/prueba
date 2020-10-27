






CREATE FUNCTION "safreviv".fn_valida_fecha( p_fecha_c  CHAR(6))
RETURNING SMALLINT;

DEFINE r_estatus    SMALLINT;

DEFINE v_dia        SMALLINT;
DEFINE v_dia_limite SMALLINT;
DEFINE v_mes        SMALLINT;
DEFINE v_anio       SMALLINT;

DEFINE v_cociente   SMALLINT ;
DEFINE v_residuo    SMALLINT ;
DEFINE v_sum_ascii  SMALLINT ;

SELECT fn_es_numero( p_fecha_c[1]) +
       fn_es_numero( p_fecha_c[2]) +
       fn_es_numero( p_fecha_c[3]) +
       fn_es_numero( p_fecha_c[4]) +
       fn_es_numero( p_fecha_c[5]) +
       fn_es_numero( p_fecha_c[6])
  INTO v_sum_ascii
  FROM systables
 WHERE tabid = 1 ;


IF v_sum_ascii = 6 THEN
   LET v_anio = p_fecha_c[1,2]  ;
   LET v_mes  = p_fecha_c[3,4]  ;
   LET v_dia  = p_fecha_c[5,6] ;

----  Verifica  anio

   IF v_anio  >= 0 AND
      v_anio  <= 99 THEN
      LET v_anio     = 1900 + v_anio ;

      LET v_cociente = v_anio / 4 ;
      LET v_residuo  = v_anio - ( v_cociente * 4 ) ;

      IF v_mes >= 1  AND v_mes <= 12 THEN
         IF v_mes = 2 THEN
            IF v_residuo = 0 THEN
               LET v_dia_limite = 29 ;
            ELSE
               LET v_dia_limite = 28 ;
            END IF
         ELIF v_mes = 4  OR
              v_mes = 6  OR
              v_mes = 9  OR
              v_mes = 11 THEN

              LET v_dia_limite = 30 ;
         ELSE
              LET v_dia_limite = 31 ;
         END IF

         IF v_dia >= 1 AND
            v_dia <= v_dia_limite THEN
            LET r_estatus = 1  ;  -- Fecha  Valida
         ELSE
            LET r_estatus = 0  ;  -- Dia invalido
         END IF

      ELSE
         LET r_estatus = 0  ;  --  mes invalido
      END IF
   ELSE
      LET r_estatus = 0  ;  --  anio invalido
   END IF
ELSE
   LET r_estatus = 0  ;  --  Caracteres invalidos
END IF

RETURN r_estatus ;

END FUNCTION   ;


