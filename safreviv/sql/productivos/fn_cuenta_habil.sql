






CREATE FUNCTION "safreviv".fn_cuenta_habil(dia_actual DATE ,
                                dia_desde  DATE )
    RETURNING SMALLINT;

    DEFINE dia_habil_sig      DATE     ;
    DEFINE cont_1             SMALLINT ;
    DEFINE dia_semana         SMALLINT ;
    DEFINE feriado            SMALLINT ;
    DEFINE fin_de_semana      SMALLINT ;
    DEFINE verdadero          SMALLINT ;

   ---SET DEBUG FILE TO '/safreviv_int/BD/diaHabilInf.trace';
   ---TRACE ON;

    LET cont_1        = 0;
    LET verdadero     = 1;
    LET dia_habil_sig = dia_desde;

    IF dia_habil_sig <> dia_actual THEN
       WHILE verdadero = 1
           LET feriado        = 0;
           LET fin_de_semana  = 0;
           LET dia_semana     = WEEKDAY(dia_habil_sig);

           IF dia_semana = 0 OR dia_semana = 6 THEN
               LET fin_de_semana = 1 ;
           ELSE
              IF EXISTS (SELECT feriado_fecha
                           FROM cat_feriado_infonavit
                          WHERE feriado_fecha = dia_habil_sig) THEN
                 LET feriado = 1 ;
              END IF
           END IF

           IF feriado = 1 OR fin_de_semana = 1 THEN
               LET dia_habil_sig = dia_habil_sig + 1 UNITS DAY ;
           ELSE
              IF dia_habil_sig = dia_actual THEN
                 EXIT WHILE ;
              END IF

              LET cont_1 = cont_1 + 1;

              LET dia_habil_sig = dia_habil_sig + 1 UNITS DAY ;
           END IF
       END WHILE
    END IF

    RETURN cont_1;

END FUNCTION;


