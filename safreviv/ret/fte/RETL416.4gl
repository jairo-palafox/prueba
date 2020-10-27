################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL416                                                      #
#Ojetivo       => 
#Fecha inicio  => Marzo 2016                                                   #
#Requerimiento => 
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"
DATABASE safre_viv

MAIN

    -- Parametros recibidos del proceso de liquidacion de Ley73 Contingente
    DEFINE p_usuario_cod         STRING
    -- Folio de las solicitudes liquidadas
    DEFINE p_folio_liquida       LIKE glo_folio.folio

    -- Datos del proceso
    DEFINE p_pid                 LIKE glo_pid.pid
    DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE p_opera_cod           LIKE cat_operacion.opera_cod
    -- Folio generado para llamar el proceso de notificacion mediante SMS o correo
    DEFINE v_folio_notificacion  LIKE glo_folio.folio
    DEFINE g_reg_modulo          RECORD
           ruta_exp              LIKE seg_modulo.ruta_bin,
           ruta_rescate          LIKE seg_modulo.ruta_rescate,
           ruta_listados         LIKE seg_modulo.ruta_listados
    END RECORD
    DEFINE seg_modulo_bat        RECORD
           ruta_listados         LIKE seg_modulo.ruta_listados
    END RECORD

    -- Variables auxiliares
    DEFINE v_i_resultado         INTEGER
    DEFINE r_resultado_opera     SMALLINT
    DEFINE v_mensaje             STRING
    DEFINE v_s_comando           STRING


    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_proceso_cod
    CALL ARG_VAL(3) RETURNING p_opera_cod
    CALL ARG_VAL(4) RETURNING p_folio_liquida

    -- se obtienen las rutas de control del modulo
    SELECT s.ruta_bin, s.ruta_rescate, s.ruta_listados
    INTO   g_reg_modulo.*
    FROM   seg_modulo s
    WHERE  s.modulo_cod = 'ret'

    SELECT b.ruta_listados
    INTO   seg_modulo_bat.ruta_listados
    FROM   seg_modulo b
    WHERE  b.modulo_cod = 'bat'

    -- Log del proceso
    CALL STARTLOG(p_usuario_cod CLIPPED||".RETL416.log")

    -- Se inicia la operacion
    CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_i_resultado

    IF v_i_resultado = 0 THEN

        --  se genera el pid para el proceso
        CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario_cod)
                           RETURNING p_pid

        -- Se genera el folio para las solicitudes
        CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)
                             RETURNING v_folio_notificacion

        CALL fn_inicializa_proceso(p_pid,
                                   p_proceso_cod,
                                   p_opera_cod,
                                   v_folio_notificacion,
                                   "RETL416",
                                   "",
                                   p_usuario_cod)
                                   RETURNING r_resultado_opera

        IF ( r_resultado_opera <> 0 ) THEN
            -- se indica al usuario que el proceso no pudo iniciarse
            CALL fn_muestra_inc_operacion(r_resultado_opera)
        END IF

        -- Se actualiza la operacion
        CALL fn_actualiza_opera_ini(p_pid,
                                    p_proceso_cod,
                                    p_opera_cod,
                                    v_folio_notificacion,
                                    "RETL416",
                                    "",
                                    p_usuario_cod)
                                    RETURNING r_resultado_opera

        IF r_resultado_opera = 0 THEN

            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP416 ",
                             p_usuario_cod CLIPPED, " ",
                             p_pid  , " " ,
                             p_proceso_cod , " " ,
                             p_opera_cod ," ",
                             v_folio_notificacion ," ",
                             p_folio_liquida ," ",
                             " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                             "/nohup:",p_pid USING "&&&&&",":",
                             p_proceso_cod USING "&&&&&",":",
                             p_opera_cod   USING "&&&&&" ,
                             " 2>&1 &"
            --DISPLAY v_s_comando
            DISPLAY ""
            DISPLAY ""
            DISPLAY "Se ha iniciado el proceso, podrá revisar el resultado en el monitor de procesos"
            DISPLAY ""
            RUN v_s_comando

        ELSE                   
            CALL fn_recupera_inconsis_opera(r_resultado_opera) RETURNING v_mensaje
            DISPLAY v_mensaje
        END IF
    ELSE
        CALL fn_recupera_inconsis_opera(v_i_resultado) RETURNING v_mensaje
        DISPLAY v_mensaje
    END IF    

END MAIN