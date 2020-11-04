DATABASE safre_viv

GLOBALS "CTAM003.inc"

MAIN

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo           STRING -- titulo de la ventana

    OPTIONS INPUT WRAP

    CLOSE WINDOW SCREEN

    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
    END IF

    CALL fn_registra_aclaraciones(TODAY,p_usuario_cod)

END MAIN


PRIVATE FUNCTION fn_inicializa()

    DEFINE v_query      STRING

    LET v_query = "\n SELECT COUNT(*)           ",
                  "\n FROM   pag_tpo_aclaracion ",
                  "\n WHERE  aclaracion_cod = ? "
    PREPARE prp_existe_numeral FROM v_query

    LET v_query = "\n SELECT COUNT(*)           ",
                  "\n FROM   cat_muestra_acl    ",
                  "\n WHERE  aclaracion_cod = ? "
    PREPARE prp_muestra_acl FROM v_query

END FUNCTION


PRIVATE FUNCTION fn_registra_aclaraciones(p_fecha_captura,p_usuario_cod)

    DEFINE p_fecha_captura      DATE
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_datos_aclara       t_datos_aclara
    DEFINE v_cbx_ind_visible    ui.ComboBox

    DEFINE v_contador           SMALLINT
    DEFINE v_confirma           SMALLINT

    DEFINE v_ventana            ui.Window
    DEFINE v_forma              ui.Form

    CALL fn_inicializa()


    OPEN WINDOW w_actualiza_aclaraciones WITH FORM "CTAM0031"

        LET v_ventana = ui.Window.getCurrent()
        LET v_forma   = v_ventana.getForm()

        LET v_cbx_ind_visible = ui.ComboBox.forName("ind_visible")

        DIALOG ATTRIBUTES (UNBUFFERED)


            INPUT v_datos_aclara.* FROM datos_aclara.*
            END INPUT


            ON ACTION ACCEPT

                IF (v_datos_aclara.aclaracion_cod IS NULL) THEN
                    CALL fn_mensaje("Atención","Debe ingresar el número de causal de aclaración.","info")
                    NEXT FIELD aclaracion_cod
                END IF

                IF (v_datos_aclara.aclaracion_descripcion IS NULL) THEN
                    CALL fn_mensaje("Atención","Debe ingresar la descripción de la aclaración.","info")
                    NEXT FIELD aclaracion_descripcion
                END IF

                IF (v_datos_aclara.ind_visible IS NULL) THEN
                    CALL fn_mensaje("Atención","Debe ingresar si la aclaración será visible.","info")
                    NEXT FIELD ind_visible
                END IF

                EXECUTE prp_existe_numeral USING v_datos_aclara.aclaracion_cod
                                           INTO  v_contador

                IF (v_contador == 0) THEN

                    CALL fn_ventana_confirma("Atención","¿Desea guardar la aclaración?","quest")
                         RETURNING v_confirma

                    IF (v_confirma) THEN

                        CALL fn_guarda_aclaracion(v_datos_aclara.*,p_fecha_captura,p_usuario_cod)
                        CALL fn_mensaje("Actualiza movimiento","Datos guardados correctamente.","about")
                        EXIT DIALOG

                    END IF

                ELSE
                    CALL fn_mensaje("Atención","Ya existe una aclaración con el mismo número de causal.","info")
                END IF

            -- ON ACTION ACCEPT


            ON ACTION CANCEL
                EXIT DIALOG


            ON ACTION CLOSE
                EXIT DIALOG


            BEFORE DIALOG

                CALL Dialog.setActionHidden("close",TRUE)

                CALL v_cbx_ind_visible.clear()
                CALL v_cbx_ind_visible.addItem(0,"NO")
                CALL v_cbx_ind_visible.addItem(1,"SI")

                INITIALIZE v_datos_aclara.* TO NULL

            -- BEFORE DIALOG

        END DIALOG

    CLOSE WINDOW w_actualiza_aclaraciones

END FUNCTION


PRIVATE FUNCTION fn_guarda_aclaracion(p_datos_aclara,p_fecha_captura,p_usuario_cod)

    DEFINE p_datos_aclara       t_datos_aclara
    DEFINE p_fecha_captura      LIKE pag_tpo_aclaracion.f_actualiza
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_contador           SMALLINT

    INSERT INTO pag_tpo_aclaracion(aclaracion_cod,
                                   aclaracion_descripcion,
                                   desc_ciudadana,
                                   f_actualiza,
                                   usuario)
                            VALUES(p_datos_aclara.aclaracion_cod,
                                   p_datos_aclara.aclaracion_descripcion,
                                   NULL, -- desc_ciudadana
                                   p_fecha_captura,
                                   p_usuario_cod)

    -- Se valida la "visibilidad"
    EXECUTE prp_muestra_acl USING p_datos_aclara.aclaracion_cod
                            INTO  v_contador

    IF (p_datos_aclara.ind_visible == 1) THEN

        IF (v_contador == 0) THEN -- Si se va a mostrar

            INSERT INTO cat_muestra_acl(aclaracion_cod,
                                        f_actualiza,
                                        usuario)
                                 VALUES(p_datos_aclara.aclaracion_cod,
                                        TODAY,
                                        p_usuario_cod)

        END IF

    ELSE

        IF (v_contador == 1) THEN -- Si NO se va a mostrar

            DELETE
            FROM   cat_muestra_acl
            WHERE  aclaracion_cod = p_datos_aclara.aclaracion_cod

        END IF

    END IF

END FUNCTION