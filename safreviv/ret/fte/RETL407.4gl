#Modulo        => RET                                                          #
#Programa      => RETL407                                                      #
#Objetivo      => Lanzador de notificacion de solicitudes de Retiro Ley 73,    #
#                 Grupo 1.                                                     #
#Fecha inicio  => Diciembre, 2015.                                             #
#Requerimiento =>                                                              #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"
DATABASE safre_viv

MAIN

    DEFINE p_usuario_cod         STRING

    -- Datos del proceso
    DEFINE v_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE v_opera_cod           LIKE cat_operacion.opera_cod
    DEFINE v_pid                 LIKE glo_pid.pid

    DEFINE v_i_resultado         INTEGER
    DEFINE r_resultado_opera     SMALLINT
    DEFINE v_mensaje             STRING
    DEFINE v_s_comando           STRING
    DEFINE g_reg_modulo          RECORD
           ruta_exp              LIKE seg_modulo.ruta_bin,
           ruta_rescate          LIKE seg_modulo.ruta_rescate,
           ruta_listados         LIKE seg_modulo.ruta_listados
    END RECORD
    DEFINE seg_modulo_bat        RECORD
           ruta_listados         LIKE seg_modulo.ruta_listados
    END RECORD


    CALL ARG_VAL(1) RETURNING p_usuario_cod

    LET v_proceso_cod = g_notificar_procesar_ley73_grupo1
    LET v_opera_cod   = g_notificar_procesar_ley73_grupo1_notificacion


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
    CALL STARTLOG(p_usuario_cod CLIPPED||".RETL407.log")    

    -- Se inicia la operacion
    CALL fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING v_i_resultado

    IF v_i_resultado = 0 THEN

        --  se genera el pid para el proceso
        CALL fn_genera_pid(v_proceso_cod,v_opera_cod,p_usuario_cod)
                           RETURNING v_pid
      
        CALL fn_inicializa_proceso(v_pid,
                                   v_proceso_cod,
                                   v_opera_cod,
                                   0,
                                   "RETP407",
                                   "",
                                   p_usuario_cod)
                                   RETURNING r_resultado_opera

        IF ( r_resultado_opera <> 0 ) THEN
            -- se indica al usuario que el proceso no pudo iniciarse
            CALL fn_muestra_inc_operacion(r_resultado_opera)
        END IF

        -- Se actualiza la operacion
        CALL fn_actualiza_opera_ini(v_pid,
                                    v_proceso_cod,
                                    v_opera_cod,
                                    0,
                                    "RETL407",
                                    "",
                                    p_usuario_cod)
                                    RETURNING r_resultado_opera

        IF r_resultado_opera = 0 THEN

            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP407 ",
                             p_usuario_cod CLIPPED," ",
                             v_pid                ," ",
                             v_proceso_cod        ," ",
                             v_opera_cod          ," ",
                             " 1>",seg_modulo_bat.ruta_listados CLIPPED,
                             "/nohup:",v_pid USING "&&&&&",":",
                             v_proceso_cod   USING "&&&&&",":",
                             v_opera_cod     USING "&&&&&" ,
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