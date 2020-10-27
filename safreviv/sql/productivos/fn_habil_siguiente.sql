






CREATE FUNCTION "safreviv".fn_habil_siguiente(dia_actual     DATE ,
                                   num_dia_habil  SMALLINT
                                  )
    RETURNING DATE;

    DEFINE dia_habil_sig      DATE     ;
    DEFINE cont_1             SMALLINT ;
    DEFINE dia_semana         SMALLINT ;
    DEFINE feriado            SMALLINT ;
    DEFINE fin_de_semana      SMALLINT ;
    DEFINE verdadero          SMALLINT ;

   --SET DEBUG FILE TO 'diaHabil.trace';
   --TRACE ON;

    LET cont_1        = 0          ;
    LET verdadero     = 1          ;
    LET dia_habil_sig = dia_actual ;

    WHILE verdadero = 1
        LET feriado        = 0                      ;
        LET fin_de_semana  = 0                      ;
        LET dia_semana     = WEEKDAY(dia_habil_sig) ;

        IF dia_semana = 0 OR dia_semana = 6 THEN
            LET fin_de_semana = 1 ;
        ELSE
            IF EXISTS (SELECT feriado_fecha
                         FROM cat_feriado
                        WHERE feriado_fecha = dia_habil_sig) THEN
                LET feriado = 1 ;
            END IF
        END IF

        IF feriado = 1 OR fin_de_semana = 1 THEN
            LET dia_habil_sig = dia_habil_sig + 1 UNITS DAY ;
        ELSE
            LET cont_1 = cont_1 + 1 ;

            IF cont_1 > num_dia_habil OR
               cont_1 = 1 AND num_dia_habil = 1 THEN
                EXIT WHILE ;
            ELSE
                LET dia_habil_sig = dia_habil_sig + 1 UNITS DAY ;
            END IF
        END IF
    END WHILE

    RETURN dia_habil_sig ;
END FUNCTION;


