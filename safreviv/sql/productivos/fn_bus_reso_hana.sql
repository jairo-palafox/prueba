






CREATE FUNCTION "safreviv".fn_bus_reso_hana(p_nss CHAR(11))
RETURNING SMALLINT;

DEFINE v_sec_pension           SMALLINT;
DEFINE v_regimen               SMALLINT;
DEFINE v_tpo_pension           CHAR(2);
DEFINE v_tpo_prestacion        CHAR(2);
DEFINE v_porcentaje            DECIMAL(5,2);
DEFINE v_diagnostico           SMALLINT;
DEFINE gi_sin_resolucion_spess SMALLINT;
DEFINE gi_porcentaje_menor_50  SMALLINT;
DEFINE gi_regimen_diferente_73 SMALLINT;

   LET v_diagnostico           =  90 ;
   LET gi_sin_resolucion_spess =  90 ; -- no tiene resolucion en el SPESS
   LET gi_porcentaje_menor_50  = 140 ; -- porcentaje de pension menor al 50 %
   LET gi_regimen_diferente_73 = 145 ; -- Regimen de resolucion diferente a 73


   PREPARE c_stmt FROM "SELECT sec_pension, regimen, tpo_pension, tpo_prestacion, porcentaje_valuacion " ||
                       "FROM ret_datamart WHERE nss = ? ORDER BY sec_pension DESC ; " ;

   DECLARE cur_datamart CURSOR FOR c_stmt ;
   OPEN cur_datamart USING p_nss;

   WHILE ( 1 = 1)
      FETCH cur_datamart INTO v_sec_pension, v_regimen, v_tpo_pension, v_tpo_prestacion, v_porcentaje ;
      IF (SQLCODE != 100) THEN

         IF v_sec_pension IS NOT NULL THEN
            IF v_regimen = 73 THEN
               IF v_tpo_prestacion = '00' THEN
                  IF v_tpo_pension = 'IP' THEN
                     IF v_porcentaje >= 50 THEN
                        LET v_diagnostico = 0 ;
                        EXIT ;
                     ELSE
                        LET v_diagnostico = gi_porcentaje_menor_50 ;
                     END IF
                  ELSE
                     IF v_tpo_pension = 'VI' OR
                        v_tpo_pension = 'OR' OR
                        v_tpo_pension = 'AS' OR
                        v_tpo_pension = 'VO' THEN
                        LET v_diagnostico = gi_sin_resolucion_spess ;
                     ELSE
                        LET v_diagnostico = 0 ;
                        EXIT ;
                     END IF
                  END IF
               ELSE
                  LET v_diagnostico = gi_sin_resolucion_spess ;
               END IF
            ELSE
               LET v_diagnostico = gi_regimen_diferente_73 ;
            END IF
         ELSE
            LET v_diagnostico = gi_sin_resolucion_spess ;
         END IF;

      ELSE
         EXIT;
      END IF
    END WHILE;


   RETURN v_diagnostico ;

END FUNCTION;


