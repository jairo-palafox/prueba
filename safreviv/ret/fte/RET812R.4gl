################################################################################
#Modulo        => RET                                                          #
#Programa      => RET812R                                                      #
#Ojetivo       => Reverso del programa RET812, es decir, vuelve a su estado    #
#                 las marcas cambiadas.                                        #
#Fecha inicio  => Agosto 2015.                                                 #
#Requerimiento => 812                                                          #
################################################################################
DATABASE safre_viv

MAIN

    --Varible auxiliar en el UPDATE, para relacionar el registro que se actualizara
    DEFINE v_id_derechohabiente     DECIMAL(9,0)
    DEFINE v_marca                  SMALLINT   
    DEFINE v_f_inicio               DATE
    DEFINE v_n_referencia           DECIMAL(9,0)
    --Total de registros con estatus_trm 016 actualizados en sfr_marca_historica
    DEFINE v_total_historico_016    INTEGER
    --Total de registros con estatus_trm 026 actualizados en sfr_marca_historica
    DEFINE v_total_historico_026    INTEGER
    --Total de registros con estatus_trm 036 actualizados en sfr_marca_historica
    DEFINE v_total_historico_036    INTEGER
    --Total de registros con estatus_trm 016 actualizados en sfr_marca_activa
    DEFINE v_total_activa_016       INTEGER
    --Total de registros con estatus_trm 026 actualizados en sfr_marca_activa
    DEFINE v_total_activa_026       INTEGER
    --Total de registros con estatus_trm 036 actualizados en sfr_marca_activa
    DEFINE v_total_activa_036       INTEGER

    --Se inicializan variables
    LET v_total_historico_016 = 0
    LET v_total_historico_026 = 0
    LET v_total_historico_036 = 0
    LET v_total_activa_016 = 0
    LET v_total_activa_026 = 0
    LET v_total_activa_036 = 0

    --Se obtiene el total de registros 016 en sfr_marca_activa
    SELECT COUNT(*)
      INTO v_total_activa_016
      FROM ret_pago_trm  r, sfr_marca_activa s
      WHERE r.id_derechohabiente = s.id_derechohabiente
        AND estatus_trm = 016
        AND marca = 590

    --Se obtiene el total de registros 016 en sfr_marca_historica
    SELECT COUNT(*)
      INTO v_total_historico_016
      FROM ret_pago_trm  r, sfr_marca_historica s
      WHERE r.id_derechohabiente = s.id_derechohabiente
        AND estatus_trm = 016
        AND marca = 590

    #### sfr_marca_historica
    --estatus_trm 026
    DECLARE cur_estatus_trm_026_h CURSOR FOR SELECT s.id_derechohabiente,
                                                    marca,f_inicio,n_referencia
                                             FROM ret_pago_trm r, sfr_marca_historica s 
                                             WHERE r.id_derechohabiente = s.id_derechohabiente
                                               AND estatus_trm = 026 AND marca = 596
    --estatus_trm 036
    DECLARE cur_estatus_trm_036_h CURSOR FOR SELECT s.id_derechohabiente,
                                                    marca,f_inicio,n_referencia
                                             FROM ret_pago_trm r, sfr_marca_historica s 
                                             WHERE r.id_derechohabiente = s.id_derechohabiente
                                               AND estatus_trm = 036 AND marca = 597

    #### sfr_marca_activa
    --estatus_trm 026
    DECLARE cur_estatus_trm_026_a CURSOR FOR SELECT s.id_derechohabiente,
                                                    marca,f_inicio,n_referencia 
                                             FROM ret_pago_trm r, sfr_marca_activa s 
                                             WHERE r.id_derechohabiente = s.id_derechohabiente
                                               AND estatus_trm = 026 AND marca = 596
    --estatus_trm 036
    DECLARE cur_estatus_trm_036_a CURSOR FOR SELECT s.id_derechohabiente,
                                                    marca,f_inicio,n_referencia
                                             FROM ret_pago_trm r, sfr_marca_activa s 
                                             WHERE r.id_derechohabiente = s.id_derechohabiente
                                               AND estatus_trm = 036 AND marca = 597
    #### sfr_marca_historica
    --Se actualizan las marcas de los estatus_trm 026
    FOREACH cur_estatus_trm_026_h INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                       v_n_referencia  
        UPDATE sfr_marca_historica
          SET marca = 590
          WHERE id_derechohabiente = v_id_derechohabiente
            AND marca = v_marca
            AND f_inicio = v_f_inicio
            AND n_referencia = v_n_referencia
        --Se cuentan los registros actualizados
        LET v_total_historico_026 = v_total_historico_026 + 1
    END FOREACH
    --Se actualizan las marcas de los estatus_trm 036
    FOREACH cur_estatus_trm_036_h INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                       v_n_referencia  
        UPDATE sfr_marca_historica
          SET marca = 590
          WHERE id_derechohabiente = v_id_derechohabiente
            AND marca = v_marca
            AND f_inicio = v_f_inicio
            AND n_referencia = v_n_referencia
        --Se cuentan los registros actualizados
        LET v_total_historico_036 = v_total_historico_036 + 1
    END FOREACH

    #### sfr_marca_activa
    --Se actualizan las marcas de los estatus_trm 026
   FOREACH cur_estatus_trm_026_a INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                       v_n_referencia  
        UPDATE sfr_marca_activa
          SET marca = 590
          WHERE id_derechohabiente = v_id_derechohabiente
            AND marca = v_marca
            AND f_inicio = v_f_inicio
            AND n_referencia = v_n_referencia
        --Se cuentan los registros actualizados
        LET v_total_activa_026 = v_total_activa_026 + 1
    END FOREACH

    
    --Se actualizan las marcas de los estatus_trm 036
    FOREACH cur_estatus_trm_036_a INTO v_id_derechohabiente, v_marca, v_f_inicio,
                                       v_n_referencia  
        UPDATE sfr_marca_activa
          SET marca = 590
          WHERE id_derechohabiente = v_id_derechohabiente
            AND marca = v_marca
            AND f_inicio = v_f_inicio
            AND n_referencia = v_n_referencia
        --Se cuentan los registros actualizados
        LET v_total_activa_036 = v_total_activa_036 + 1
    END FOREACH

    DISPLAY ""
    DISPLAY ""
    DISPLAY "                   Reverso Actualización"
    DISPLAY ""
    DISPLAY ""
    DISPLAY "sfr_marca_activa"
    DISPLAY ""
    DISPLAY "estatus_trm 016, marca actual 590:    ",v_total_activa_016," registros"
    DISPLAY "estatus_trm 026, marca actual 590:    ",v_total_activa_026," registros"
    DISPLAY "estatus_trm 036, marca actual 590:    ",v_total_activa_036," registros"
    DISPLAY ""
    DISPLAY "sfr_marca_historica"
    DISPLAY ""
    DISPLAY "estatus_trm 016, marca actual 590:    ",v_total_historico_016," registros"
    DISPLAY "estatus_trm 026, marca actual 590:    ",v_total_historico_026," registros"
    DISPLAY "estatus_trm 036, marca actual 590:    ",v_total_historico_036," registros"
    DISPLAY ""
    DISPLAY ""
    
END MAIN