






CREATE PROCEDURE "safreviv".sp_obtiene_fecha_mes_siguiente_primero()RETURNING DATE

 DEFINE v_dte_fecha_valida   DATE;     -- fecha en formato DATE MM/DD//YYYY
 DEFINE v_c_fecha_dia_actual CHAR(10); --Fecha día actual DATE MM/DD/YYYY
 DEFINE v_si_dia             SMALLINT;
 DEFINE v_si_mes             SMALLINT;
 DEFINE v_si_anio            SMALLINT;

   --Obtener la fecha del día actual
   LET v_c_fecha_dia_actual = TODAY;

   --EL proceso conciste en sumar un mes a la fecha
   --- desglosar la fecha en mm-mes, dd-dia, yyyy-ejercicio
   LET v_si_dia  = v_c_fecha_dia_actual[4,5];
   LET v_si_mes  = v_c_fecha_dia_actual[1,2];
   LET v_si_anio = v_c_fecha_dia_actual[7,10];

   IF(v_si_mes = 12)THEN
      --- Hacer el mes enero
      LET v_si_mes = 1;
      -- Hacer el anio mas uno
      LET v_si_anio = v_si_anio + 1;
   ELSE
      -- Incrementar el mes en 1
      LET v_si_mes = v_si_mes + 1;
   END IF
   -- Hace que el día sea el primero del mes
   LET v_si_dia = 1;

   -- Armar la nueva fecha
   LET v_dte_fecha_valida = MDY(v_si_mes, v_si_dia, v_si_anio);

   RETURN v_dte_fecha_valida;
END PROCEDURE --
;


