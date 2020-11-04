-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAM002
-- Funcionalidad => Actualizar catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------


DATABASE safre_viv

GLOBALS "CTAM002.inc"

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

    CALL fn_actualiza_aclaraciones(p_usuario_cod)

END MAIN


PRIVATE FUNCTION fn_inicializa()

    DEFINE v_query      STRING

    LET v_query =  "\n SELECT aclaracion_cod,                                     ",
                   "\n        aclaracion_cod||' - '||TRIM(aclaracion_descripcion) ",
                   "\n FROM   pag_tpo_aclaracion                                  ",
                   "\n ORDER  BY aclaracion_cod                                   "
    PREPARE prp_clave_aclara FROM v_query
    DECLARE cur_clave_aclara CURSOR FOR prp_clave_aclara

    LET v_query = "\n SELECT pa.aclaracion_cod,                               ",
                  "\n        pa.aclaracion_descripcion,                       ",
                  "\n        CASE                                             ",
                  "\n            WHEN (cat.aclaracion_cod IS NOT NULL) THEN 1 ",
                  "\n            ELSE 0                                       ",
                  "\n        END AS ind_visible                               ",
                  "\n FROM   pag_tpo_aclaracion pa                            ",
                  "\n        LEFT JOIN cat_muestra_acl cat                    ",
                  "\n               ON cat.aclaracion_cod = pa.aclaracion_cod ",
                  "\n WHERE  pa.aclaracion_cod = ?                            ",
                  "\n ORDER BY pa.aclaracion_cod ASC                          "
    PREPARE prp_datos_aclara FROM v_query

    LET v_query = "\n SELECT COUNT(*)           ",
                  "\n FROM   cat_muestra_acl    ",
                  "\n WHERE  aclaracion_cod = ? "
    PREPARE prp_muestra_acl FROM v_query

END FUNCTION


PRIVATE FUNCTION fn_actualiza_aclaraciones(p_usuario_cod)

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_datos_aclara       t_datos_aclara
    DEFINE v_resp_aclara        t_datos_aclara

    DEFINE v_tpo_modifica       LIKE cat_his_aclaracion.tpo_modifica

    DEFINE v_cbx_cve_acl_busq   ui.ComboBox
    DEFINE v_cbx_clave_acl      ui.ComboBox
    DEFINE v_aclara_cod         LIKE pag_tpo_aclaracion.aclaracion_cod
    DEFINE v_aclara_desc        LIKE pag_tpo_aclaracion.aclaracion_descripcion
    DEFINE v_cbx_ind_visible    ui.ComboBox

    DEFINE v_confirma           SMALLINT

    DEFINE v_ventana            ui.Window
    DEFINE v_forma              ui.Form

    CALL fn_inicializa()


    OPEN WINDOW w_actualiza_aclaraciones WITH FORM "CTAM0021"

        LET v_ventana = ui.Window.getCurrent()
        LET v_forma   = v_ventana.getForm()

        LET v_cbx_cve_acl_busq = ui.ComboBox.forName("aclara_cod_busq")
        LET v_cbx_clave_acl = ui.ComboBox.forName("aclaracion_cod")
        LET v_cbx_ind_visible = ui.ComboBox.forName("ind_visible")

        DIALOG ATTRIBUTES (UNBUFFERED)

            INPUT v_aclara_cod FROM aclara_cod_busq
            END INPUT

            INPUT v_datos_aclara.* FROM datos_aclara.*

                ON CHANGE aclaracion_descripcion

                    IF (v_datos_aclara.aclaracion_descripcion IS NULL) OR (LENGTH(v_datos_aclara.aclaracion_descripcion CLIPPED) == 0) THEN
                        CALL fn_mensaje("Atención","Debe ingresar la descripción de la aclaración.","info")
                        NEXT FIELD aclaracion_descripcion
                    END IF

            END INPUT


            ON ACTION consultar

                CALL Dialog.setActionHidden("cancel",FALSE)
                CALL Dialog.setActionHidden("actualizar",FALSE)
                CALL Dialog.setActionHidden("consultar",TRUE)
                CALL Dialog.setActionHidden("salir",TRUE)
                CALL v_forma.setElementHidden("grp_datos_aclara",FALSE)
                CALL v_forma.setElementHidden("grp_busqueda",TRUE)

                EXECUTE prp_datos_aclara USING v_aclara_cod
                                         INTO  v_datos_aclara.*

                LET v_resp_aclara.* = v_datos_aclara.*

            -- ON ACTION consultar


            ON ACTION actualizar

                CALL fn_valida_tpo_modifica(v_datos_aclara.*,v_resp_aclara.*)
                     RETURNING v_tpo_modifica

                IF (v_tpo_modifica > 0) THEN

                    IF (v_datos_aclara.aclaracion_descripcion IS NULL) OR (LENGTH(v_datos_aclara.aclaracion_descripcion CLIPPED) == 0) THEN
                        CALL fn_mensaje("Atención","Debe ingresar la descripción de la aclaración.","info")
                        NEXT FIELD aclaracion_descripcion
                    END IF

                    CALL fn_ventana_confirma("Atención","¿Desea aplicar la actualizacion a la aclaración?","quest")
                         RETURNING v_confirma

                    IF (v_confirma) THEN

                        CALL fn_guarda_actualizacion(v_datos_aclara.*,v_resp_aclara.*,v_tpo_modifica,p_usuario_cod)
                        CALL fn_mensaje("Actualiza movimiento","La actualizacion se aplico correctamente en el sistema.","about")

                        CALL Dialog.setActionHidden("cancel",TRUE)
                        CALL Dialog.setActionHidden("actualizar",TRUE)
                        CALL Dialog.setActionHidden("consultar",FALSE)
                        CALL Dialog.setActionHidden("salir",FALSE)
                        CALL v_forma.setElementHidden("grp_datos_aclara",TRUE)
                        CALL v_forma.setElementHidden("grp_busqueda",FALSE)

                    END IF

                ELSE
                    CALL fn_mensaje("Actualiza movimiento","Para continuar es necesario aplicar alguna actualizacion.","about")
                END IF


            -- ON ACTION actualizar


            ON ACTION CANCEL

                INITIALIZE v_datos_aclara.* TO NULL
                CALL Dialog.setActionHidden("cancel",TRUE)
                CALL Dialog.setActionHidden("actualizar",TRUE)
                CALL Dialog.setActionHidden("consultar",FALSE)
                CALL Dialog.setActionHidden("salir",FALSE)
                CALL v_forma.setElementHidden("grp_datos_aclara",TRUE)
                CALL v_forma.setElementHidden("grp_busqueda",FALSE)

            -- ON ACTION CANCEL


            ON ACTION CLOSE
                EXIT DIALOG


            ON ACTION salir
                EXIT DIALOG


            BEFORE DIALOG

                CALL Dialog.setActionHidden("close",TRUE)
                CALL Dialog.setActionHidden("cancel",TRUE)
                CALL Dialog.setActionHidden("actualizar",TRUE)
                CALL v_forma.setElementHidden("grp_datos_aclara",TRUE)

                CALL v_cbx_cve_acl_busq.clear()
                CALL v_cbx_clave_acl.clear()
                CALL v_cbx_ind_visible.clear()

                FOREACH cur_clave_aclara INTO v_aclara_cod,
                                              v_aclara_desc
                    CALL v_cbx_cve_acl_busq.addItem(v_aclara_cod,v_aclara_cod)
                    CALL v_cbx_clave_acl.addItem(v_aclara_cod,v_aclara_cod)
                END FOREACH

                CALL v_cbx_ind_visible.addItem(0,"NO")
                CALL v_cbx_ind_visible.addItem(1,"SI")

                LET v_aclara_cod = NULL

                SELECT MIN(aclaracion_cod)
                INTO   v_aclara_cod
                FROM   pag_tpo_aclaracion

            -- BEFORE DIALOG

        END DIALOG

    CLOSE WINDOW w_actualiza_aclaraciones

END FUNCTION


PRIVATE FUNCTION fn_valida_tpo_modifica(p_datos_aclara,p_resp_aclara)

    DEFINE p_datos_aclara       t_datos_aclara
    DEFINE p_resp_aclara        t_datos_aclara

    DEFINE r_tpo_modifica       LIKE cat_his_aclaracion.tpo_modifica

    LET r_tpo_modifica = 0

    DISPLAY "Datos Anteriores - Descripcion: ",p_resp_aclara.aclaracion_descripcion," - Indicador",p_resp_aclara.ind_visible
    DISPLAY "Datos Nuevos - Descripcion: ",p_datos_aclara.aclaracion_descripcion," - Indicador",p_datos_aclara.ind_visible

    IF (p_datos_aclara.aclaracion_descripcion <> p_resp_aclara.aclaracion_descripcion) THEN
        LET r_tpo_modifica = r_tpo_modifica + 1
    END IF

    IF (p_datos_aclara.ind_visible <> p_resp_aclara.ind_visible) THEN
        LET r_tpo_modifica = r_tpo_modifica + 2
    END IF

    DISPLAY "r_tpo_modifica: ",r_tpo_modifica

    RETURN r_tpo_modifica

END FUNCTION


PRIVATE FUNCTION fn_guarda_actualizacion(p_datos_aclara,p_resp_aclara,p_tpo_modifica,p_usuario_cod)

    DEFINE p_datos_aclara       t_datos_aclara
    DEFINE p_resp_aclara        t_datos_aclara
    DEFINE p_tpo_modifica       LIKE cat_his_aclaracion.tpo_modifica
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_contador           SMALLINT

    -- Se guarda la bitacora
    INSERT INTO cat_his_aclaracion(id_cat_his_aclaracion,
                                   aclaracion_cod,
                                   aclaracion_descripcion,
                                   desc_ciudadana,
                                   f_modifica,
                                   usuario,
                                   tpo_modifica)
                            VALUES(seq_cat_his_acl.NEXTVAL, -- id_cat_his_aclaracion
                                   p_resp_aclara.aclaracion_cod,
                                   p_resp_aclara.aclaracion_descripcion,
                                   NULL, -- desc_ciudadana
                                   TODAY,
                                   p_usuario_cod,
                                   p_tpo_modifica)

    -- Se actualiza el registro
    UPDATE pag_tpo_aclaracion
    SET    aclaracion_descripcion = p_datos_aclara.aclaracion_descripcion
    WHERE  aclaracion_cod = p_resp_aclara.aclaracion_cod

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