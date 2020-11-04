
DATABASE safre_viv

GLOBALS

    TYPE t_datos_campania RECORD
        nombre          LIKE not_campana_bitacora.nombre,
        fecha_ini       LIKE not_campana_bitacora.fecha_ini,
        fecha_fin       LIKE not_campana_bitacora.fecha_fin,
        descripcion     LIKE not_campana_bitacora.descripcion,
        fecha_alta      LIKE not_campana_bitacora.fecha_alta,
        usuario_alta    LIKE not_campana_bitacora.usuario_alta,
        fecha_baja      LIKE not_campana_bitacora.fecha_baja,
        hora_baja       LIKE not_campana_bitacora.hora_baja,
        usuario_baja    LIKE not_campana_bitacora.usuario_baja,
        tpo_modifica    VARCHAR(40)
    END RECORD

    TYPE t_lista_campanias DYNAMIC ARRAY OF t_datos_campania

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

    CALL fn_consulta_bitacora(p_usuario_cod)

END MAIN


#OBJETIVO: Consultar las campañas anteriores, registradas en la bitácora.
PRIVATE FUNCTION fn_consulta_bitacora(p_usuario_cod)

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_lista_campanias    t_lista_campanias
    DEFINE v_indice             INTEGER

    DECLARE cur_campanas CURSOR FOR
    SELECT nombre,
           fecha_ini,
           fecha_fin,
           descripcion, 
           fecha_alta,
           usuario_alta,
           fecha_baja,
           hora_baja,
           usuario_baja,
           CASE
               WHEN tpo_modifica = 0 THEN "Baja"
               WHEN tpo_modifica = 1 THEN "Fecha Inicio"
               WHEN tpo_modifica = 2 THEN "Fecha Fin"
               WHEN tpo_modifica = 3 THEN "Fecha Inicio y Fecha Fin"
               WHEN tpo_modifica = 4 THEN "Descripción"
               WHEN tpo_modifica = 5 THEN "Fecha Inicio y Descripción"
               WHEN tpo_modifica = 6 THEN "Fecha Fin y Descripción"
               WHEN tpo_modifica = 7 THEN "Fecha Inicio, Fecha Fin y Descripción"
           END AS tpo_modifica
    FROM   not_campana_bitacora
    ORDER  BY fecha_baja DESC,hora_baja DESC,id_bitacora DESC

    LET v_indice = 1
    FOREACH cur_campanas INTO v_lista_campanias[v_indice].*
        LET v_indice = v_indice +1
    END FOREACH

    IF (v_lista_campanias.getLength() > 0) THEN
        IF (v_lista_campanias[v_lista_campanias.getLength()].nombre IS NULL) THEN
            CALL v_lista_campanias.deleteElement(v_lista_campanias.getLength())
        END IF
    END IF

    OPEN WINDOW w_camp_bitacora WITH FORM "NOTC021.4fd"

        DIALOG ATTRIBUTES(UNBUFFERED)

            DISPLAY ARRAY v_lista_campanias TO scr_anteriores.*
            END DISPLAY

            ON ACTION salir
                EXIT DIALOG

            ON ACTION CLOSE
                EXIT DIALOG

            BEFORE DIALOG

                CALL Dialog.setActionHidden("close",TRUE)
                IF (v_lista_campanias.getLength() == 0) THEN
                    CALL fn_mensaje("Atención","No hay campañas en este momento.","info")
                    EXIT DIALOG
                END IF

        END DIALOG

    CLOSE WINDOW w_camp_bitacora

END FUNCTION 