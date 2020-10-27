






CREATE FUNCTION "safreviv".fn_calcula_digito_verificador_nss(p_nss CHAR(10)) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), SMALLINT

-- detalle de la tabla temporal

DEFINE v_contador       SMALLINT;
DEFINE v_suma_valores   SMALLINT;
DEFINE v_num_calculo    SMALLINT;
DEFINE v_resultado      SMALLINT;
DEFINE v_digito         SMALLINT;
DEFINE v_nss            CHAR(11);

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);

   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, 100;
   END EXCEPTION

   
   -- se inicializan las variables de trabajo
   LET v_contador      = 0; -- contador del ciclo
   LET v_suma_valores  = 0; -- acumulador calculos
   LET v_digito        = 0; -- digito verificador calculado
   LET v_num_calculo   = 0; -- caracter procesado
   LET v_resultado     = 0; -- resultado parcial
   LET v_si_resultado  = 0; -- resultado del llamado a la funcion
   LET isam_err        = 0; -- error generado
   LET err_txt         = " ";
   LET v_c_msj         = " ";
   
   FOR v_contador = 1 TO 10
       LET v_num_calculo = SUBSTR(p_nss,v_contador,1);
       IF v_contador = 1 OR v_contador = 3 OR v_contador = 5 OR v_contador = 7 OR v_contador = 9 THEN 
           LET v_resultado = v_num_calculo * 1;
       ELSE
           LET v_resultado = v_num_calculo * 2;
           IF v_resultado >= 10 THEN 
               LET v_resultado = (v_resultado - 10) + 1;
           END IF 
       END IF
       LET v_suma_valores = v_suma_valores + v_resultado;      

   END FOR
   WHILE v_suma_valores > 9
       LET v_suma_valores = v_suma_valores - 10;
   END WHILE
   IF v_suma_valores = 0 THEN 
       LET v_digito = 0;
   ELSE 
       LET v_digito = 10 - v_suma_valores;
   END IF
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, v_digito;
END FUNCTION;


