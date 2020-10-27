################################################################################
#Modulo        => RET                                                          #
#Programa      => RET812                                                       #
#Ojetivo       => Programa de una unica ejecución que actualizara las marcas   #
#                 590, dividiendola en 590, 596 y 597, de acuerdo al           #
#                 estatus_trm de la tabla ret_pago_tm. Quedando las marcas de  #
#                 la siguiente manera:                                         #
#                            estatus_trm  |    marca                           #
#                           -------------------------                          #
#                                016      |    590                             #
#                                026      |    596                             #
#                                036      |    597                             #
#                                                                              #
#Fecha inicio  => Agosto 2015.                                                 #
#Requerimiento => 812                                                          #
################################################################################
DATABASE safre_viv

MAIN

    --Varibles auxiliares en el UPDATE, para relacionar el registro que se actualizara
    DEFINE v_id_derechohabiente     DECIMAL(9,0)
    DEFINE v_marca                  SMALLINT   
    DEFINE v_f_inicio               DATE
    DEFINE v_n_referencia           DECIMAL(9,0)
    DEFINE v_estatus_trm            SMALLINT
    DEFINE v_total_historica        INTEGER
    DEFINE v_total_activa           INTEGER
    --Total de registros con estatus_trm 016 actualizados en sfr_marca_historica
    DEFINE v_total_historica_016    INTEGER
    --Total de registros con estatus_trm 026 actualizados en sfr_marca_historica
    DEFINE v_total_historica_026    INTEGER
    --Total de registros con estatus_trm 036 actualizados en sfr_marca_historica
    DEFINE v_total_historica_036    INTEGER
    --Total de registros con estatus_trm 016 actualizados en sfr_marca_activa
    DEFINE v_total_activa_016       INTEGER
    --Total de registros con estatus_trm 026 actualizados en sfr_marca_activa
    DEFINE v_total_activa_026       INTEGER
    --Total de registros con estatus_trm 036 actualizados en sfr_marca_activa
    DEFINE v_total_activa_036       INTEGER

    --Se inicializan variables
    LET v_total_historica_016 = 0
    LET v_total_historica_026 = 0
    LET v_total_historica_036 = 0
    LET v_total_activa_016 = 0
    LET v_total_activa_026 = 0
    LET v_total_activa_036 = 0

    #### sfr_marca_historica
    DECLARE cur_estatus_trm_historica CURSOR FOR SELECT s.id_derechohabiente,marca,
                                                        f_inicio,n_referencia,estatus_trm
                                                 FROM   ret_pago_trm r, sfr_marca_historica s 
                                                 WHERE  r.id_derechohabiente = s.id_derechohabiente
                                                 AND    estatus_trm IN (016,026,036)
                                                 AND    marca = 590

    #### sfr_marca_activa
    DECLARE cur_estatus_trm_activa CURSOR FOR SELECT a.id_derechohabiente,marca,
                                                     f_inicio,n_referencia,estatus_trm
                                              FROM   ret_pago_trm r, sfr_marca_activa a 
                                              WHERE  r.id_derechohabiente = a.id_derechohabiente
                                              AND    estatus_trm IN (016,026,036)
                                              AND    marca = 590

    SELECT COUNT(*)
    INTO   v_total_historica
    FROM   ret_pago_trm r, sfr_marca_historica s 
    WHERE  r.id_derechohabiente = s.id_derechohabiente
    AND    estatus_trm IN (016,026,036)
    AND    marca = 590

    SELECT COUNT(*)
    INTO   v_total_activa
    FROM   ret_pago_trm r, sfr_marca_activa a 
    WHERE  r.id_derechohabiente = a.id_derechohabiente
    AND    estatus_trm IN (016,026,036)
    AND    marca = 590

    DISPLAY ""
    DISPLAY ""
    DISPLAY ""
    DISPLAY "Total marca historica: ", v_total_historica
    DISPLAY "Total marca activa: ", v_total_activa

    LET v_total_historica = 0
    LET v_total_activa    =  0
    
    #### sfr_marca_historica
    DISPLAY "Actualizando marca historica..."
    FOREACH cur_estatus_trm_historica INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                       v_n_referencia, v_estatus_trm

        LET v_total_historica = v_total_historica + 1

        CASE v_estatus_trm
            WHEN 16

                UPDATE sfr_marca_historica
                SET    marca = 595
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_historica_016 = v_total_historica_016 + 1
                
            WHEN 26
            
                UPDATE sfr_marca_historica
                SET    marca = 596
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_historica_026 = v_total_historica_026 + 1
                
            WHEN 36

                UPDATE sfr_marca_historica
                SET    marca = 597
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_historica_036 = v_total_historica_036 + 1
                
        END CASE
    END FOREACH
    
    #### sfr_marca_activa
   DISPLAY "Actualizando marca activa..."
   FOREACH cur_estatus_trm_activa INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                      v_n_referencia, v_estatus_trm

        LET v_total_activa = v_total_activa + 1

        CASE v_estatus_trm
            WHEN 16

                UPDATE sfr_marca_activa
                SET    marca = 595
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_activa_016 = v_total_activa_016 + 1
                
            WHEN 26

                UPDATE sfr_marca_activa
                SET    marca = 596
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_activa_026 = v_total_activa_026 + 1
                
            WHEN 36

                UPDATE sfr_marca_activa
                SET    marca = 597
                WHERE id_derechohabiente = v_id_derechohabiente
                AND   marca = v_marca
                AND   f_inicio = v_f_inicio
                AND   n_referencia = v_n_referencia
                
                --Se cuentan los registros actualizados
                LET v_total_activa_036 = v_total_activa_036 + 1
                
        END CASE
        
    END FOREACH

    DISPLAY ""
    DISPLAY ""
    DISPLAY "                       Actualización"
    DISPLAY ""
    DISPLAY ""
    DISPLAY "sfr_marca_historica"
    DISPLAY ""
    DISPLAY "estatus_trm 016, marca actual 595:     ",v_total_historica_016," registros"
    DISPLAY "estatus_trm 026, marca actual 596:     ",v_total_historica_026," registros"
    DISPLAY "estatus_trm 036, marca actual 597:     ",v_total_historica_036," registros"
    DISPLAY "Total:                                 ",v_total_historica
    DISPLAY ""
    DISPLAY "sfr_marca_activa"
    DISPLAY ""
    DISPLAY "estatus_trm 016, marca actual 595:     ",v_total_activa_016," registros"
    DISPLAY "estatus_trm 026, marca actual 596:     ",v_total_activa_026," registros"
    DISPLAY "estatus_trm 036, marca actual 597:     ",v_total_activa_036," registros"
    DISPLAY "Total:                                 ",v_total_activa
    DISPLAY ""
    DISPLAY ""
    
END MAIN