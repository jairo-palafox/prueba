-----------------------------------------------------------------------------------------
-- Modulo        => CTA
-- Componente    => CTAC011
-- Funcionalidad => Consulta catálogo de aclaraciones (causales).
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 23 de enero de 2020.
-- Requerimiento =>  
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS "CTAC011.inc"

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

    CALL fn_consulta_aclaraciones()

END MAIN


PRIVATE FUNCTION fn_inicializa()

    DEFINE v_query      STRING

    LET v_query =  "\n SELECT aclaracion_cod,                                     ",
                   "\n        aclaracion_cod||' - '||TRIM(aclaracion_descripcion) ",
                   "\n FROM   pag_tpo_aclaracion                                  ",
                   "\n ORDER  BY aclaracion_cod                                   "
    PREPARE prp_clave_aclara FROM v_query
    DECLARE cur_clave_aclara CURSOR FOR prp_clave_aclara

    LET v_query = "\n SELECT pa.aclaracion_cod,                                                            ",
                  "\n        pa.aclaracion_descripcion,                                                    ",
                  "\n        CASE                                                                          ",
                  "\n            WHEN (cat.aclaracion_cod IS NOT NULL) AND (cat.aclaracion_cod > 0) THEN 1 ",
                  "\n            ELSE 0                                                                    ",
                  "\n        END AS ind_visible,                                                           ",
                  "\n        pa.f_actualiza AS f_modifica,                                                 ",
                  "\n        pa.usuario AS usuario,                                                        ",
                  "\n        'Carga inicial' AS tipo_modifica                                              ",
                  "\n FROM   pag_tpo_aclaracion pa                                                         ",
                  "\n        LEFT JOIN cat_muestra_acl cat                                                 ",
                  "\n               ON cat.aclaracion_cod = pa.aclaracion_cod                              ",
                  "\n WHERE  pa.aclaracion_cod = ?                                                         ",
                  "\n ORDER BY pa.aclaracion_cod ASC                                                       "
    PREPARE prp_lista_aclara FROM v_query
    DECLARE cur_lista_aclara CURSOR FOR prp_lista_aclara

    LET v_query = "\n SELECT pa.aclaracion_cod,                               ",
                  "\n        pa.aclaracion_descripcion,                       ",
                  "\n        CASE                                             ",
                  "\n            WHEN (cat.aclaracion_cod IS NOT NULL) THEN 1 ",
                  "\n            ELSE 0                                       ",
                  "\n        END AS ind_visible,                              ",
                  "\n        pa.f_actualiza AS f_modifica,                    ",
                  "\n        pa.usuario AS usuario,                           ",
                  "\n        'Carga inicial' AS tipo_modifica                 ",
                  "\n FROM   pag_tpo_aclaracion pa                            ",
                  "\n        LEFT JOIN cat_muestra_acl cat                    ",
                  "\n               ON cat.aclaracion_cod = pa.aclaracion_cod ",
                  "\n ORDER BY pa.aclaracion_cod ASC                          "
    PREPARE prp_lista_aclara_todo FROM v_query
    DECLARE cur_lista_aclara_todo CURSOR FOR prp_lista_aclara_todo

    LET v_query = "\n SELECT MAX(id_cat_his_aclaracion) ",
                  "\n FROM   cat_his_aclaracion         ",
                  "\n WHERE  aclaracion_cod = ?         "
    PREPARE prp_max_his_aclara FROM v_query

    LET v_query = "\n SELECT cha.f_modifica,                                                ",
                  "\n        cha.usuario,                                                   ",
                  "\n        CASE                                                           ",
                  "\n            WHEN cha.tpo_modifica = 1 THEN 'Descripción'               ",
                  "\n            WHEN cha.tpo_modifica = 2 THEN 'Visibilidad'               ",
                  "\n            WHEN cha.tpo_modifica = 3 THEN 'Descripción y Visibilidad' ",
                  "\n        END AS tipo_modifica                                           ",
                  "\n FROM   cat_his_aclaracion cha,                                        ",
                  "\n        cat_tpo_modifica   ctm                                         ",
                  "\n WHERE  cha.id_cat_his_aclaracion = ?                                  ",
                  "\n   AND  ctm.tpo_modifica          = cha.tpo_modifica                   "
    PREPARE prp_datos_his_aclara FROM v_query

END FUNCTION


PRIVATE FUNCTION fn_consulta_aclaraciones()

    DEFINE v_lista_aclara       t_lista_aclara

    DEFINE v_cbx_clave_acl      ui.ComboBox
    DEFINE v_aclara_cod         LIKE pag_tpo_aclaracion.aclaracion_cod
    DEFINE v_aclara_desc        LIKE pag_tpo_aclaracion.aclaracion_descripcion
    DEFINE v_cbx_ind_visible    ui.ComboBox

    DEFINE v_ventana            ui.Window
    DEFINE v_forma              ui.Form

    CALL fn_inicializa()


    OPEN WINDOW w_consulta_aclaraciones WITH FORM "CTAC0111"

        LET v_ventana = ui.Window.getCurrent()
        LET v_forma   = v_ventana.getForm()

        LET v_cbx_clave_acl = ui.ComboBox.forName("aclaracion_cod")
        LET v_cbx_ind_visible = ui.ComboBox.forName("ind_visible")

        DIALOG ATTRIBUTES (UNBUFFERED)

            INPUT v_aclara_cod FROM aclaracion_cod

                ON CHANGE aclaracion_cod

                    --CALL v_forma.setElementHidden("grp_lista_aclara",TRUE)
                    CALL v_lista_aclara.clear()

            END INPUT

            DISPLAY ARRAY v_lista_aclara TO lista_aclara.*
            END DISPLAY


            ON ACTION consultar

                CALL fn_recupera_lista_aclara(v_aclara_cod)
                     RETURNING v_lista_aclara

                IF (v_lista_aclara.getLength() > 0) THEN
                    CALL v_forma.setElementHidden("grp_lista_aclara",FALSE)
                ELSE
                    CALL fn_mensaje("Atención","No existe registros con el criterio de búsqueda.","info")
                    --CALL v_forma.setElementHidden("grp_lista_aclara",TRUE)
                END IF

            -- ON ACTION consultar


            ON ACTION CLOSE
                EXIT DIALOG


            ON ACTION salir
                EXIT DIALOG


            BEFORE DIALOG

                CALL Dialog.setActionHidden("close",TRUE)
                CALL v_forma.setElementHidden("grp_lista_aclara",TRUE)

                CALL v_cbx_clave_acl.clear()
                CALL v_cbx_ind_visible.clear()

                CALL v_cbx_clave_acl.addItem(NULL,"TODAS")
                FOREACH cur_clave_aclara INTO v_aclara_cod,
                                              v_aclara_desc
                    CALL v_cbx_clave_acl.addItem(v_aclara_cod,v_aclara_cod)
                END FOREACH

                CALL v_cbx_ind_visible.addItem(0,"NO")
                CALL v_cbx_ind_visible.addItem(1,"SI")

                LET v_aclara_cod = NULL

            -- BEFORE DIALOG

        END DIALOG

    CLOSE WINDOW w_consulta_aclaraciones

END FUNCTION



PRIVATE FUNCTION fn_recupera_lista_aclara(p_aclara_cod)

    DEFINE p_aclara_cod         LIKE pag_tpo_aclaracion.aclaracion_cod

    DEFINE v_indice             SMALLINT
    DEFINE v_id_cat_his_aclara  LIKE cat_his_aclaracion.id_cat_his_aclaracion

    DEFINE r_lista_aclara       t_lista_aclara

    LET v_indice = 1
    IF (p_aclara_cod IS NOT NULL) THEN

        FOREACH cur_lista_aclara USING p_aclara_cod
                                 INTO  r_lista_aclara[v_indice].*

            EXECUTE prp_max_his_aclara USING r_lista_aclara[v_indice].aclaracion_cod
                                       INTO  v_id_cat_his_aclara

            IF (v_id_cat_his_aclara IS NOT NULL) THEN

                EXECUTE prp_datos_his_aclara USING v_id_cat_his_aclara
                                             INTO  r_lista_aclara[v_indice].f_modifica,
                                                   r_lista_aclara[v_indice].usuario,
                                                   r_lista_aclara[v_indice].tipo_modifica

            END IF

            INITIALIZE v_id_cat_his_aclara TO NULL
            LET v_indice = v_indice + 1

        END FOREACH

    ELSE

        FOREACH cur_lista_aclara_todo INTO r_lista_aclara[v_indice].*

            EXECUTE prp_max_his_aclara USING r_lista_aclara[v_indice].aclaracion_cod
                                       INTO  v_id_cat_his_aclara

            IF (v_id_cat_his_aclara IS NOT NULL) THEN

                EXECUTE prp_datos_his_aclara USING v_id_cat_his_aclara
                                             INTO  r_lista_aclara[v_indice].f_modifica,
                                                   r_lista_aclara[v_indice].usuario,
                                                   r_lista_aclara[v_indice].tipo_modifica

            END IF

            INITIALIZE v_id_cat_his_aclara TO NULL
            LET v_indice = v_indice + 1

        END FOREACH
    
    END IF

    IF (r_lista_aclara.getLength() > 0) THEN
        IF (r_lista_aclara[r_lista_aclara.getLength()].aclaracion_cod IS NULL) THEN
            CALL r_lista_aclara.deleteElement(r_lista_aclara.getLength())
        END IF
    END IF

    RETURN r_lista_aclara

END FUNCTION