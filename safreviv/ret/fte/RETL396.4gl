################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL395                                                      #
#Ojetivo       => Realizar un barrido de la tabla ret_datamart para obtener las#
#                 resoluciones candidatas a enviar SMS o correo para realizar  #
#                 notificaciones.                                              #
#Fecha inicio  => Octubre 2015.                                                #
#Requerimiento => PRODINFXV-91                                                 #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"
DATABASE safre_viv

MAIN

    DEFINE p_usuario_cod         STRING
    -- Folio de las solicitudes procesadas (dia a dia).
    DEFINE p_folio_datamart      DECIMAL(9,0)
    -- Folio generado para llamar el proceso de notificacion mediante SMS o correo
    DEFINE v_folio_notificacion  DECIMAL(9,0)
    -- Datos del proceso
    DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE p_opera_cod           LIKE cat_operacion.opera_cod
    DEFINE v_i_resultado         INTEGER
    DEFINE r_resultado_opera     SMALLINT
    DEFINE v_mensaje             STRING
    DEFINE p_pid                 DECIMAL(9,0)
    DEFINE v_s_comando           STRING
    DEFINE g_reg_modulo          RECORD
           ruta_exp              CHAR(40),
           ruta_rescate          CHAR(40),
           ruta_listados         CHAR(40)
    END RECORD
    DEFINE seg_modulo_bat        RECORD
           ruta_listados         CHAR(40)
    END RECORD

    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_proceso_cod
    CALL ARG_VAL(3) RETURNING p_opera_cod
    CALL ARG_VAL(4) RETURNING p_folio_datamart

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
    CALL STARTLOG(p_usuario_cod CLIPPED||".RETL396.log")    

    -- Se inicia la operacion
    CALL fn_valida_operacion(p_pid,p_proceso_cod,p_opera_cod) RETURNING v_i_resultado

    IF v_i_resultado = 0 THEN
        -- Se genera el folio para las solicitudes

        -- Historico
        IF p_proceso_cod = 1572 THEN 
            LET v_folio_notificacion = 0
        ELSE
            CALL fn_genera_folio(p_proceso_cod, p_opera_cod, p_usuario_cod)
                                RETURNING v_folio_notificacion
        END IF

        --  se genera el pid para el proceso
        CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario_cod)
                           RETURNING p_pid
      
        CALL fn_inicializa_proceso(p_pid,
                                   p_proceso_cod,
                                   p_opera_cod,
                                   v_folio_notificacion,
                                   "RETL396",
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
                                    "RETL396",
                                    "",
                                    p_usuario_cod)
                                    RETURNING r_resultado_opera

        IF r_resultado_opera = 0 THEN

            LET v_s_comando = " nohup time fglrun ",g_reg_modulo.ruta_exp CLIPPED,"/RETP396 ",
                             p_usuario_cod CLIPPED, " ",
                             p_pid  , " " ,
                             p_proceso_cod , " " ,
                             p_opera_cod ," ",
                             v_folio_notificacion ," ",
                             p_folio_datamart ," ",
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