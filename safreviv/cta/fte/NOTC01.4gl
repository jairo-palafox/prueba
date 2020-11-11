DATABASE safre_viv

GLOBALS

    CONSTANT c_baja_campania        SMALLINT = 0
    CONSTANT c_modif_fecha_ini      SMALLINT = 1
    CONSTANT c_modif_fecha_fin      SMALLINT = 2
    CONSTANT c_modif_descripcion    SMALLINT = 4

    TYPE t_datos_campania RECORD
        id_campana      LIKE not_campana.id_campana,
        seleccion       SMALLINT,
        nombre          LIKE not_campana.nombre,
        fecha_ini       LIKE not_campana.fecha_ini,
        fecha_fin       LIKE not_campana.fecha_fin,
        descripcion     LIKE not_campana.descripcion,
        fecha_alta      LIKE not_campana.fecha_modifica,
        usuario_alta    LIKE not_campana.usuario_modifica,
        tipo_campania   VARCHAR(15),
        estado          LIKE not_campana.estado,
        tpo_modifica    SMALLINT
    END RECORD

    TYPE t_lista_campanias DYNAMIC ARRAY OF t_datos_campania

    TYPE t_campania_actual RECORD
        id_campana          LIKE not_campana.id_campana,
        nombre              LIKE not_campana.nombre,
        fecha_ini           LIKE not_campana.fecha_ini,
        fecha_fin           LIKE not_campana.fecha_fin,
        descripcion         LIKE not_campana.descripcion,
        fecha_modifica      LIKE not_campana.fecha_modifica,
        usuario_modifica    LIKE not_campana.usuario_modifica
    END RECORD

    TYPE t_resultado RECORD
        estado      SMALLINT,
        mensaje     STRING
    END RECORD

END GLOBALS

MAIN

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod -- clave del usuario firmado
    DEFINE p_tipo_ejecucion     SMALLINT -- forma como ejecutara el programa
    DEFINE p_s_titulo           STRING -- titulo de la ventana

    OPTIONS INPUT WRAP

    LET p_usuario_cod    = ARG_VAL(1)
    LET p_tipo_ejecucion = ARG_VAL(2)
    LET p_s_titulo       = ARG_VAL(3)

    -- si se obtuvo el titulo, se pone como titulo de programa
    IF ( p_s_titulo IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_s_titulo)
    END IF

    CALL fn_captura(p_usuario_cod)

END MAIN


PRIVATE FUNCTION fn_inicializa()

    DEFINE v_query      STRING

    LET v_query = "\n SELECT id_campana,                        ",
                  "\n        0 AS seleccion,                    ",
                  "\n        nombre,                            ",
                  "\n        fecha_ini,                         ",
                  "\n        fecha_fin,                         ",
                  "\n        descripcion,                       ",
                  "\n        fecha_modifica,                    ",
                  "\n        usuario_modifica,                  ",
                  "\n        CASE                               ",
                  "\n            WHEN estado = 1 THEN 'Vigente' ",
                  "\n            WHEN estado = 2 THEN 'Futura'  ",
                  "\n        END AS tipo_campania,              ",
                  "\n        estado,                            ",
                  "\n        0 AS ind_modifica                  ",
                  "\n FROM   not_campana                        ",
                  "\n WHERE  ind_baja =  0                      ",
                  "\n ORDER  BY estado,fecha_ini,nombre         "
    PREPARE prp_campanias FROM v_query
    DECLARE cur_campanias CURSOR FOR prp_campanias

    LET v_query = "\n SELECT COUNT(*)                           ",
                  "\n FROM   not_campana                        ",
                  "\n WHERE  id_campana <> ?                    ",
                  "\n   AND  fecha_ini  <= ? AND ? <= fecha_fin ",
                  "\n   AND  ind_baja   =  0                    "
    PREPARE prp_count_periodo FROM v_query

END FUNCTION


PRIVATE FUNCTION fn_captura(p_usuario_cod)

    DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod

    DEFINE v_campanias      t_lista_campanias
    DEFINE v_camp_resp      t_lista_campanias
    DEFINE v_indice         SMALLINT
    DEFINE v_pos_camp       SMALLINT
    DEFINE v_confirma       SMALLINT
    DEFINE v_resultado      t_resultado
    DEFINE v_contador       SMALLINT
    DEFINE v_ind_modifica   SMALLINT

    DEFINE v_ventana        ui.Window
    DEFINE v_forma          ui.Form

    CALL fn_inicializa()

    OPEN WINDOW w_campania WITH FORM "NOTC011.4fd"

        LET v_ventana = ui.Window.getCurrent()
        LET v_forma   = v_ventana.getForm()

        DIALOG ATTRIBUTES(UNBUFFERED)

            INPUT ARRAY v_campanias FROM campanias.*
                  ATTRIBUTES(INSERT ROW=FALSE,APPEND ROW=FALSE,DELETE ROW=FALSE)


                BEFORE ROW
                    LET v_pos_camp = ARR_CURR()

                ON CHANGE fecha_ini

                    IF (v_campanias[v_pos_camp].fecha_ini < TODAY) AND (v_campanias[v_pos_camp].estado == 2) THEN
                        LET v_campanias[v_pos_camp].fecha_ini = v_camp_resp[v_pos_camp].fecha_ini
                        CALL fn_mensaje ("Atención","La fecha de inicio debe ser igual o posterior a la fecha de hoy","info")
                        NEXT FIELD fecha_ini
                    END IF

                    EXECUTE prp_count_periodo USING v_campanias[v_pos_camp].id_campana,
                                                    v_campanias[v_pos_camp].fecha_ini,
                                                    v_campanias[v_pos_camp].fecha_ini
                                              INTO  v_contador

                    IF (v_contador > 0) THEN
                        LET v_campanias[v_pos_camp].fecha_ini = v_camp_resp[v_pos_camp].fecha_ini
                        CALL fn_mensaje ("Atención","La fecha de inicio no puede pertenecer al periodo de otra campaña.","info")
                        NEXT FIELD fecha_ini
                    END IF                    

                -- ON CHANGE fecha_ini
                    

                ON CHANGE fecha_fin

                    IF (v_campanias[v_pos_camp].fecha_fin < v_campanias[v_pos_camp].fecha_ini) THEN
                        LET v_campanias[v_pos_camp].fecha_fin = v_camp_resp[v_pos_camp].fecha_fin
                        CALL fn_mensaje ("Atención","La fecha fin debe ser igual o posterior a la fecha de inicio","info")
                        NEXT FIELD fecha_fin
                    END IF

                    EXECUTE prp_count_periodo USING v_campanias[v_pos_camp].id_campana,
                                                    v_campanias[v_pos_camp].fecha_fin,
                                                    v_campanias[v_pos_camp].fecha_fin
                                              INTO  v_contador

                    IF (v_contador > 0) THEN
                        LET v_campanias[v_pos_camp].fecha_fin = v_camp_resp[v_pos_camp].fecha_fin
                        CALL fn_mensaje ("Atención","La fecha fin no puede pertenecer al periodo de otra campaña.","info")
                        NEXT FIELD fecha_fin
                    END IF

                -- ON CHANGE fecha_fin


                ON CHANGE descripcion

                    IF (v_campanias[v_pos_camp].descripcion IS NULL) OR (LENGTH(v_campanias[v_pos_camp].descripcion CLIPPED) == 0) THEN
                        LET v_campanias[v_pos_camp].descripcion = v_camp_resp[v_pos_camp].descripcion
                        CALL fn_mensaje ("Atención","Debe ingresar una descripción.","info")
                        NEXT FIELD descripcion
                    END IF

                -- ON CHANGE descripcion


            END INPUT


            ON ACTION ACCEPT

                LET v_ind_modifica = FALSE

                -- Se valida que los campos esten correctos
                FOR v_indice = 1 TO v_campanias.getLength()

                    -- Se valida la modificacion
                    CALL fn_valida_tpo_modifica(v_campanias[v_indice].*,v_camp_resp[v_indice].*)
                         RETURNING v_campanias[v_indice].tpo_modifica

                    IF (v_campanias[v_indice].tpo_modifica > 0) THEN

                        -- Fecha inicio
                        IF (v_campanias[v_indice].fecha_ini < TODAY) AND (v_campanias[v_indice].estado == 2) THEN
                            LET v_campanias[v_indice].fecha_ini = v_camp_resp[v_indice].fecha_ini
                            CALL fn_mensaje ("Atención","La fecha de inicio debe ser igual o posterior a la fecha de hoy","info")
                            CALL FGL_SET_ARR_CURR(v_indice)
                            NEXT FIELD fecha_ini
                        END IF

                        EXECUTE prp_count_periodo USING v_campanias[v_indice].id_campana,
                                                        v_campanias[v_indice].fecha_ini,
                                                        v_campanias[v_indice].fecha_ini
                                                  INTO  v_contador

                        IF (v_contador > 0) THEN
                            LET v_campanias[v_indice].fecha_ini = v_camp_resp[v_indice].fecha_ini
                            CALL fn_mensaje ("Atención","La fecha de inicio no puede pertenecer al periodo de otra campaña.","info")
                            CALL FGL_SET_ARR_CURR(v_indice)
                            NEXT FIELD fecha_ini
                        END IF

                        -- Fecha fin
                        IF (v_campanias[v_indice].fecha_fin < v_campanias[v_indice].fecha_ini) THEN
                            LET v_campanias[v_indice].fecha_fin = v_camp_resp[v_indice].fecha_fin
                            CALL fn_mensaje ("Atención","La fecha fin debe ser igual o posterior a la fecha de inicio","info")
                            CALL FGL_SET_ARR_CURR(v_indice)
                            NEXT FIELD fecha_fin
                        END IF

                        EXECUTE prp_count_periodo USING v_campanias[v_indice].id_campana,
                                                        v_campanias[v_indice].fecha_fin,
                                                        v_campanias[v_indice].fecha_fin
                                                  INTO  v_contador

                        IF (v_contador > 0) THEN
                            LET v_campanias[v_indice].fecha_fin = v_camp_resp[v_indice].fecha_fin
                            CALL fn_mensaje ("Atención","La fecha fin no puede pertenecer al periodo de otra campaña.","info")
                            CALL FGL_SET_ARR_CURR(v_indice)
                            NEXT FIELD fecha_fin
                        END IF

                        -- Descripcion
                        IF (v_campanias[v_indice].descripcion IS NULL) OR (LENGTH(v_campanias[v_indice].descripcion CLIPPED) == 0) THEN
                            LET v_campanias[v_indice].descripcion = v_camp_resp[v_indice].descripcion
                            CALL fn_mensaje ("Atención","Debe ingresar una descripción.","info")
                            CALL FGL_SET_ARR_CURR(v_indice)
                            NEXT FIELD descripcion
                        END IF

                        LET v_ind_modifica = TRUE

                    END IF -- (v_campanias[v_indice].ind_modifica == 1)

                END FOR

                IF (v_ind_modifica) THEN

                    CALL fn_ventana_confirma ("Atención","¿Desea modificar la campaña?","stop")
                         RETURNING v_confirma

                    IF (v_confirma == 1) THEN

                        CALL fn_actualiza_campanias(v_campanias,TODAY,p_usuario_cod)
                             RETURNING v_resultado.*

                        IF (v_resultado.estado == 0) THEN
                            CALL fn_mensaje ("Atención","Datos guardados","info")
                            EXIT DIALOG
                        ELSE
                            CALL fn_mensaje ("Atención",v_resultado.mensaje,"info")
                            CONTINUE DIALOG
                        END IF

                    END IF

                ELSE
                    CALL fn_mensaje ("Atención","No se ha modificado ningún dato","info")
                END IF -- (fn_datos_modificados(v_campanias))

            -- ON ACTION ACCEPT


            ON ACTION baja

                CALL fn_baja_campanias(v_campanias,TODAY,p_usuario_cod,v_forma)
                CALL fn_recupera_campanias() RETURNING v_campanias,v_camp_resp

                IF (v_campanias.getLength() == 0) THEN
                    CALL fn_mensaje ("Atención", "No quedan campañas en este momento", "info")
                    EXIT DIALOG
                ELSE
                    CALL FGL_SET_ARR_CURR(1)
                END IF

            -- ON ACTION baja


            ON ACTION cancelar
                EXIT DIALOG

            ON ACTION CLOSE
                EXIT DIALOG

            BEFORE DIALOG

                CALL Dialog.setActionHidden("close",TRUE)
                CALL v_forma.setFieldHidden("seleccion",TRUE)
                CALL v_forma.setElementHidden("lbl_fecha_baja",TRUE)
                CALL v_forma.setFieldHidden("fecha_baja",TRUE)
                CALL v_forma.setElementHidden("lbl_usuario_baja",TRUE)
                CALL v_forma.setFieldHidden("usuario_baja",TRUE)

                CALL fn_recupera_campanias() RETURNING v_campanias,v_camp_resp

                IF (v_campanias.getLength() == 0) THEN
                    CALL fn_mensaje ("Atención", "No existen campañas en este momento", "info")
                    EXIT DIALOG
                END IF

        END DIALOG

    CLOSE WINDOW w_campania

END FUNCTION


PRIVATE FUNCTION fn_recupera_campanias()

    DEFINE v_indice         INTEGER

    DEFINE r_campanias      t_lista_campanias
    DEFINE r_camp_resp      t_lista_campanias

    LET v_indice = 1
    FOREACH cur_campanias INTO r_campanias[v_indice].*
        LET r_camp_resp[v_indice].* = r_campanias[v_indice].*
        LET v_indice = v_indice + 1
    END FOREACH

    IF (r_campanias.getLength() > 0) THEN
        IF (r_campanias[r_campanias.getLength()].nombre IS NULL) THEN
            CALL r_campanias.deleteElement(r_campanias.getLength())
        END IF
    END IF

    RETURN r_campanias,r_camp_resp

END FUNCTION


PRIVATE FUNCTION fn_valida_tpo_modifica(p_campania,p_camp_resp)

    DEFINE p_campania       t_datos_campania
    DEFINE p_camp_resp      t_datos_campania

    DEFINE v_cadena         STRING

    DEFINE r_tpo_modifica   SMALLINT

    LET r_tpo_modifica = 0 -- Sin modificacion

    IF ( p_campania.fecha_ini <> p_camp_resp.fecha_ini ) THEN
        LET r_tpo_modifica = r_tpo_modifica + c_modif_fecha_ini
    END IF

    IF ( p_campania.fecha_fin <> p_camp_resp.fecha_fin ) THEN
        LET r_tpo_modifica = r_tpo_modifica + c_modif_fecha_fin
    END IF

    LET v_cadena = p_campania.descripcion CLIPPED
    IF ( NOT(v_cadena.equals(p_camp_resp.descripcion CLIPPED)) ) THEN
        LET r_tpo_modifica = r_tpo_modifica + c_modif_descripcion
    END IF

    RETURN r_tpo_modifica

END FUNCTION


PRIVATE FUNCTION fn_baja_campanias(p_campanias,p_fecha_baja,p_usuario_cod,p_forma)

    DEFINE p_campanias      t_lista_campanias
    DEFINE p_fecha_baja     DATE
    DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod
    DEFINE p_forma          ui.Form

    DEFINE v_indice         SMALLINT
    DEFINE v_confirma       SMALLINT

    CALL p_forma.setFieldHidden("seleccion",FALSE)
    CALL p_forma.setElementHidden("lbl_fecha_baja",FALSE)
    CALL p_forma.setFieldHidden("fecha_baja",FALSE)
    CALL p_forma.setElementHidden("lbl_usuario_baja",FALSE)
    CALL p_forma.setFieldHidden("usuario_baja",FALSE)

    DIALOG ATTRIBUTES(UNBUFFERED)

        INPUT ARRAY p_campanias FROM campanias.*
              ATTRIBUTES(INSERT ROW=FALSE,APPEND ROW=FALSE,DELETE ROW=FALSE)

            ON CHANGE seleccion

                CALL Dialog.setActionHidden("accept",TRUE)
                FOR v_indice = 1 TO p_campanias.getLength()
                    IF (p_campanias[v_indice].seleccion == 1) THEN
                        CALL Dialog.setActionHidden("accept",FALSE)
                    END IF
                END FOR

        END INPUT


        ON ACTION ACCEPT

            CALL fn_ventana_confirma ("Atención","¿Desea dar de baja las campañas seleccionadas?","stop")
                 RETURNING v_confirma

            IF (v_confirma == 1) THEN
                FOR v_indice = 1 TO p_campanias.getLength()
                    IF (p_campanias[v_indice].seleccion == 1) THEN
                        CALL fn_elimina_campania(p_campanias[v_indice].*,p_fecha_baja,p_usuario_cod)
                    END IF
                END FOR
            END IF

            EXIT DIALOG


        ON ACTION cancelar
            EXIT DIALOG


        ON ACTION CLOSE
            CONTINUE DIALOG


        BEFORE DIALOG
            CALL Dialog.setActionHidden("close",TRUE)
            CALL Dialog.setActionHidden("accept",TRUE)
            CALL Dialog.setFieldActive("fecha_ini",FALSE)
            CALL Dialog.setFieldActive("fecha_fin",FALSE)
            CALL Dialog.setFieldActive("descripcion",FALSE)

            DISPLAY p_fecha_baja TO fecha_baja
            DISPLAY p_usuario_cod TO usuario_baja


        AFTER DIALOG
            CALL Dialog.setFieldActive("fecha_ini",TRUE)
            CALL Dialog.setFieldActive("fecha_fin",TRUE)
            CALL Dialog.setFieldActive("descripcion",TRUE)

    END DIALOG

    CALL p_forma.setFieldHidden("seleccion",TRUE)
    CALL p_forma.setElementHidden("lbl_fecha_baja",TRUE)
    CALL p_forma.setFieldHidden("fecha_baja",TRUE)
    CALL p_forma.setElementHidden("lbl_usuario_baja",TRUE)
    CALL p_forma.setFieldHidden("usuario_baja",TRUE)

END FUNCTION


PRIVATE FUNCTION fn_elimina_campania(p_datos_campania,p_fecha_baja,p_usuario_cod)

    DEFINE p_datos_campania     t_datos_campania
    DEFINE p_fecha_baja         DATE
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_contador           SMALLINT

    SELECT COUNT(*)
    INTO   v_contador
    FROM   not_campana
    WHERE  id_campana = p_datos_campania.id_campana
      AND  ind_baja   = 0

    IF (v_contador == 1) THEN

        -- Se respalda en la tabla bitacora
        INSERT INTO not_campana_bitacora(id_bitacora,
                                         id_campana,
                                         nombre,
                                         fecha_ini,
                                         fecha_fin,
                                         descripcion,
                                         fecha_alta,
                                         usuario_alta,
                                         fecha_baja,
                                         hora_baja,
                                         usuario_baja,
                                         tpo_modifica)
                                  VALUES(seq_not_campana_bitacora.NEXTVAL,
                                         p_datos_campania.id_campana,
                                         p_datos_campania.nombre,
                                         p_datos_campania.fecha_ini,
                                         p_datos_campania.fecha_fin,
                                         p_datos_campania.descripcion,
                                         p_datos_campania.fecha_alta,
                                         p_datos_campania.usuario_alta,
                                         p_fecha_baja,
                                         CURRENT HOUR TO SECOND,
                                         p_usuario_cod,
                                         c_baja_campania)

        -- Se da de baja la campaña
        UPDATE not_campana
        SET    ind_baja = 1
        WHERE  id_campana = p_datos_campania.id_campana

    END IF

END FUNCTION


PRIVATE FUNCTION fn_actualiza_campanias(p_campanias,p_fecha_baja,p_usuario_cod)

    DEFINE p_campanias          t_lista_campanias
    DEFINE p_fecha_baja         DATE
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_indice             SMALLINT
    DEFINE v_campania_act       t_campania_actual
    DEFINE v_ind_modifica       SMALLINT
    DEFINE v_estado             LIKE not_campana.estado
    DEFINE v_ind_baja           LIKE not_campana.ind_baja

    DEFINE r_resultado      t_resultado

    LET r_resultado.estado = 0
    LET r_resultado.mensaje = ""

    LET v_ind_modifica = FALSE
    FOR v_indice = 1 TO p_campanias.getLength()

        IF (p_campanias[v_indice].tpo_modifica > 0) THEN

            DISPLAY "Se valida campania: ",p_campanias[v_indice].nombre

            SELECT id_campana,
                   nombre,
                   fecha_ini,
                   fecha_fin,
                   descripcion,
                   fecha_modifica,
                   usuario_modifica
            INTO   v_campania_act.*
            FROM   not_campana
            WHERE  id_campana = p_campanias[v_indice].id_campana
              AND  ind_baja   = 0 -- No se ha dado de baja

            -- Si se encontro la campaña, y no se ha dado de baja
            IF (v_campania_act.id_campana) THEN

                DISPLAY "Se modifica: ",p_campanias[v_indice].nombre
                LET v_ind_modifica = TRUE

                INSERT INTO not_campana_bitacora(id_bitacora,
                                                 id_campana,
                                                 nombre,
                                                 fecha_ini,
                                                 fecha_fin,
                                                 descripcion,
                                                 fecha_alta,
                                                 usuario_alta,
                                                 fecha_baja,
                                                 hora_baja,
                                                 usuario_baja,
                                                 tpo_modifica)
                                          VALUES(seq_not_campana_bitacora.NEXTVAL,
                                                 v_campania_act.id_campana,
                                                 v_campania_act.nombre,
                                                 v_campania_act.fecha_ini,
                                                 v_campania_act.fecha_fin,
                                                 v_campania_act.descripcion,
                                                 v_campania_act.fecha_modifica, -- fecha_alta
                                                 v_campania_act.usuario_modifica, -- usuario_alta
                                                 p_fecha_baja, -- fecha_baja
                                                 CURRENT HOUR TO SECOND, -- hora_baja
                                                 p_usuario_cod, -- usuario_baja
                                                 p_campanias[v_indice].tpo_modifica)

                IF (p_campanias[v_indice].fecha_fin < p_fecha_baja) THEN
                    LET v_estado = 0 -- Vencida
                    LET v_ind_baja = 1
                ELSE

                    IF (p_fecha_baja < p_campanias[v_indice].fecha_ini) THEN
                        LET v_estado = 2 -- Futura
                        LET v_ind_baja = 0
                    ELSE
                        LET v_estado = 1 -- Vigente
                        LET v_ind_baja = 0
                    END IF

                END IF

                UPDATE not_campana
                SET    fecha_ini   = p_campanias[v_indice].fecha_ini,
                       fecha_fin   = p_campanias[v_indice].fecha_fin,
                       descripcion = p_campanias[v_indice].descripcion,
                       estado      = v_estado,
                       ind_baja    = v_ind_baja
                WHERE  id_campana = p_campanias[v_indice].id_campana

            END IF -- (v_campania_act.id_campana)

        END IF -- (p_campanias[v_indice].ind_modifica == 1)

    END FOR

    IF NOT(v_ind_modifica) THEN
        DISPLAY "No se ha modificado ningún dato"
        LET r_resultado.estado = 1
        LET r_resultado.mensaje = "No se ha modificado ningún dato"
    END IF

    RETURN r_resultado.*

END FUNCTION