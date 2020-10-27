






CREATE PROCEDURE "safreviv".sp_cambia_formato_fecha(p_fecha CHAR(10))
   RETURNING DATE

 DEFINE v_c_fecha_salida         DATE;
 DEFINE v_si_dia                 SMALLINT;
 DEFINE v_si_mes                 SMALLINT;
 DEFINE v_si_ano                 SMALLINT;

   -- Cambia formato de fecha de YYYYMMDD a MMDDYYYY
   LET v_si_dia = p_fecha[7,8];
   LET v_si_mes = p_fecha[5,6];
   LET v_si_ano = p_fecha[1,4];
   LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);

RETURN v_c_fecha_salida;
END PROCEDURE
;

CREATE PROCEDURE "safreviv".sp_cambia_formato_fecha(p_proceso_cod SMALLINT,
                                         p_fecha       CHAR(10))
   RETURNING INTEGER,
             DATE

-- 12112014 PRODINF-531

 DEFINE sql_err                  INTEGER;
 DEFINE v_i_resultado            SMALLINT;
 DEFINE v_c_fecha_salida         DATE;
 DEFINE v_si_dia                 SMALLINT;
 DEFINE v_si_mes                 SMALLINT;
 DEFINE v_si_ano                 SMALLINT;

   ON EXCEPTION SET sql_err


      LET v_i_resultado        = sql_err;
      LET v_c_fecha_salida     = NULL;

      RETURN v_i_resultado,
             v_c_fecha_salida;
   END EXCEPTION

   LET sql_err  = NULL;
   LET v_i_resultado = 0;

   -- Cambia formato de fecha de YYYYMMDD a MMDDYYYY
   LET v_si_dia = p_fecha[7,8];
   LET v_si_mes = p_fecha[5,6];
   LET v_si_ano = p_fecha[1,4];
   LET v_c_fecha_salida = MDY(v_si_mes, v_si_dia, v_si_ano);

   RETURN v_i_resultado,
          v_c_fecha_salida;
END PROCEDURE
;


