






CREATE FUNCTION "safreviv".fn_hps_valida_fecha( p_fecha_c  CHAR(8))
RETURNING SMALLINT,SMALLINT,CHAR(100),DATE;

DEFINE r_estatus    SMALLINT;

DEFINE v_dia        SMALLINT;
DEFINE v_dia_limite SMALLINT;
DEFINE v_mes        SMALLINT;
DEFINE v_anio       SMALLINT;

DEFINE v_cociente   SMALLINT ;
DEFINE v_residuo    SMALLINT ;
DEFINE v_sum_ascii  SMALLINT ;
DEFINE r_fecha_conv DATE;

DEFINE v_error_sql  INTEGER;
DEFINE v_error_isam INTEGER;
DEFINE v_msg_sql    CHAR(254);

   ON EXCEPTION SET v_error_sql, v_error_isam, v_msg_sql
      LET r_fecha_conv = NULL;
      LET r_estatus = -1;
      RETURN r_estatus,v_error_sql,v_msg_sql,r_fecha_conv;
   END EXCEPTION WITH RESUME;

   SET DEBUG FILE TO '/safreviv_int/BD/fn_hps_valida_fecha.trace';
   TRACE ON; 


   LET r_estatus = 0;
   LET r_fecha_conv = NULL;
   LET v_error_sql  = 0;
   LET v_error_isam = 0;
   LET v_msg_sql    = " ";

SELECT fn_es_numero( p_fecha_c[1]) +
       fn_es_numero( p_fecha_c[2]) +
       fn_es_numero( p_fecha_c[3]) +
       fn_es_numero( p_fecha_c[4]) +
       fn_es_numero( p_fecha_c[5]) +
       fn_es_numero( p_fecha_c[6]) +
       fn_es_numero( p_fecha_c[7]) +
       fn_es_numero( p_fecha_c[8])
  INTO v_sum_ascii
  FROM systables
 WHERE tabid = 1 ;


IF v_sum_ascii = 8 THEN

   LET v_anio = p_fecha_c[1,4]  ;
   LET v_mes  = p_fecha_c[5,6]  ;
   LET v_dia  = p_fecha_c[7,8] ;

----  Verifica  anio
     IF v_anio >= 1900  THEN
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

            LET r_fecha_conv = MDY(v_mes,v_dia,v_anio);

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

RETURN r_estatus,v_error_sql,v_msg_sql,r_fecha_conv ;

END FUNCTION   ;


