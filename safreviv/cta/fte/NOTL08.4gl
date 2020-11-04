
DATABASE safre_viv

GLOBALS

    TYPE t_resultado RECORD
        estado      SMALLINT,
        mensaje     STRING
    END RECORD

END GLOBALS

MAIN

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo           STRING -- titulo de la ventana

    DEFINE v_proceso_cod    LIKE bat_ctr_proceso.proceso_cod
    DEFINE v_opera_cod      LIKE bat_ctr_operacion.opera_cod

    OPTIONS INPUT WRAP

    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    LET v_proceso_cod = 2923
    LET v_opera_cod = 1

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
    END IF

    CALL fn_not_vigencia_campanias(v_proceso_cod,v_opera_cod,TODAY,p_usuario_cod)

END MAIN


FUNCTION fn_not_vigencia_campanias(p_proceso_cod,p_opera_cod,v_fecha_valida,p_usuario_cod)

    DEFINE p_fecha_ejecuta  DATE
    DEFINE p_proceso_cod    LIKE bat_ctr_proceso.proceso_cod
    DEFINE p_opera_cod      LIKE bat_ctr_operacion.opera_cod
    DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod

    DEFINE v_proceso_desc   LIKE cat_proceso.proceso_desc
    DEFINE v_opera_desc     LIKE cat_operacion.opera_desc
    DEFINE v_fecha_valida   DATE
    DEFINE v_resultado      t_resultado

    DEFINE v_respuesta      SMALLINT

    SELECT proceso_desc
    INTO   v_proceso_desc
    FROM   cat_proceso
    WHERE  proceso_cod = p_proceso_cod

    SELECT opera_desc
    INTO   v_opera_desc
    FROM   cat_operacion
    WHERE  proceso_cod = p_proceso_cod
      AND  opera_cod   = p_opera_cod


    -- Se abre la ventana
    OPEN WINDOW v_not_vigencia_campanias WITH FORM "NOTL081"

        MENU ""

            ON ACTION ACCEPT

                CALL fn_ventana_confirma("Atención","¿Desea ejecutar el proceso?","quest")
                     RETURNING v_respuesta

                IF (v_respuesta == 1) THEN

                    CALL fn_ejecuta_lanzado(p_proceso_cod,p_opera_cod,v_fecha_valida,p_usuario_cod)
                         RETURNING v_resultado.*

                    CALL fn_mensaje("Atención",v_resultado.mensaje,"info")

                    EXIT MENU

                END IF

            -- ON ACTION identificar


            ON ACTION CANCEL
                EXIT MENU


            ON ACTION CLOSE
                EXIT MENU


            BEFORE MENU

                CALL Dialog.setActionHidden("close",TRUE)

                LET v_fecha_valida = TODAY

                DISPLAY v_proceso_desc TO proceso_desc
                DISPLAY v_opera_desc   TO opera_desc
                DISPLAY v_fecha_valida TO fecha_proceso

            -- BEFORE MENU

        END MENU

    CLOSE WINDOW v_not_vigencia_campanias

END FUNCTION


PRIVATE FUNCTION fn_ejecuta_lanzado(p_proceso_cod,p_opera_cod,p_fecha_valida,p_usuario_cod)

    DEFINE p_proceso_cod    LIKE bat_ctr_proceso.proceso_cod
    DEFINE p_opera_cod      LIKE bat_ctr_operacion.opera_cod
    DEFINE p_fecha_valida   DATE
    DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod

    DEFINE v_programa_cod   LIKE cat_operacion.programa_cod
    DEFINE v_nombre_archivo LIKE bat_ctr_operacion.nom_archivo
    DEFINE v_pid            LIKE glo_pid.pid
    DEFINE v_folio          LIKE bat_ctr_operacion.folio
    DEFINE v_comando        STRING

    DEFINE v_ruta_bin       LIKE seg_modulo.ruta_bin
    DEFINE v_ruta_listados  LIKE seg_modulo.ruta_listados

    DEFINE r_resultado      t_resultado

    LET r_resultado.estado  = 0
    LET r_resultado.mensaje = ""

    LET v_pid = 0
    LET v_nombre_archivo = ""
    LET v_folio          = NULL

    SELECT ruta_bin
    INTO   v_ruta_bin
    FROM   seg_modulo
    WHERE  modulo_cod = 'not'

    SELECT ruta_listados
    INTO   v_ruta_listados
    FROM   seg_modulo
    WHERE  modulo_cod = 'bat'

    SELECT programa_cod
    INTO   v_programa_cod
    FROM   cat_operacion
    WHERE  proceso_cod = p_proceso_cod
      AND  opera_cod   = p_opera_cod

    -- Se valida que se pueda lanzar la operacion
    CALL fn_valida_operacion(v_pid,p_proceso_cod,p_opera_cod)
         RETURNING r_resultado.estado

    IF (r_resultado.estado == 0) THEN

        -- Se genera un pid para el proceso
        CALL fn_genera_pid(p_proceso_cod,p_opera_cod,p_usuario_cod)
             RETURNING v_pid

        -- Inicio de proceso
        CALL fn_inicializa_proceso(v_pid,p_proceso_cod,p_opera_cod,v_folio,v_programa_cod,v_nombre_archivo,p_usuario_cod)
             RETURNING r_resultado.estado

        IF (r_resultado.estado == 0) THEN

            -- Inicio operacion.
            CALL fn_actualiza_opera_ini(v_pid,p_proceso_cod,p_opera_cod,v_folio,v_programa_cod,v_nombre_archivo,p_usuario_cod)
                 RETURNING r_resultado.estado

            IF (r_resultado.estado == 0) THEN

                LET v_comando = "nohup time fglrun ",
                                v_ruta_bin CLIPPED,"/NOTP08 ",
                                p_usuario_cod CLIPPED," ",
                                v_pid," " ,
                                p_proceso_cod," ",
                                p_opera_cod," ",
                                v_folio," ",
                                v_nombre_archivo CLIPPED," ",
                                p_fecha_valida CLIPPED," ",
                                " 1>",
                                v_ruta_listados CLIPPED,
                                "/nohup:",
                                v_pid         USING "&&&&&",":",
                                p_proceso_cod USING "&&&&&",":",
                                p_opera_cod   USING "&&&&&",
                                " 2>&1 &"

                DISPLAY v_comando
                RUN v_comando

                LET r_resultado.mensaje = "Se ha enviado la ejecución del proceso.",
                                              "\nPodrá revisar el resultado en el monitor de ",
                                              " ejecución de procesos con el PID: ",v_pid

            ELSE -- No se pudo actualizar la operación
                CALL fn_recupera_inconsis_opera(r_resultado.estado) RETURNING r_resultado.mensaje
                LET r_resultado.mensaje = r_resultado.mensaje," - ",r_resultado.estado
            END IF -- fn_actualiza_opera_ini

        ELSE -- No se puede iniciar la operacion
            CALL fn_recupera_inconsis_opera(r_resultado.estado) RETURNING r_resultado.mensaje
            LET r_resultado.mensaje = r_resultado.mensaje," - ",r_resultado.estado
        END IF -- fn_inicializa_proceso


    ELSE -- No se puede iniciar el proceso
        CALL fn_recupera_inconsis_opera(r_resultado.estado) RETURNING r_resultado.mensaje
        LET r_resultado.mensaje = r_resultado.mensaje," - ",r_resultado.estado
    END IF

    RETURN r_resultado.*

END FUNCTION