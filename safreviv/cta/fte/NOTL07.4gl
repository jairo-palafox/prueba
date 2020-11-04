{===============================================================================
Proyecto          => SISTEMA DE AFORE( SAFRE )
Propietario       => E.F.P.
Modulo            => NOT
Programa          => NOTL07
Descripcion       => Programa Lanzado Captura de Campañas
Fecha creacion    => Enero 23, 2020.
================================================================================}

DATABASE safre_viv


GLOBALS

    CONSTANT c_baja_campania        SMALLINT = 0

    TYPE t_datos_campania RECORD
        nombre              LIKE not_campana.nombre,
        fecha_ini           LIKE not_campana.fecha_ini,
        fecha_fin           LIKE not_campana.fecha_fin,
        descripcion         LIKE not_campana.descripcion
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

    CALL fn_captura(p_usuario_cod,TODAY)

END MAIN


PRIVATE FUNCTION fn_captura(p_usuario_cod,p_fecha_captura)

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod
    DEFINE p_fecha_captura      LIKE not_campana.fecha_modifica

    DEFINE v_compania           t_datos_campania
    DEFINE v_resultado          t_resultado


    OPEN WINDOW w_campania_capt WITH FORM "NOTL071"

        DIALOG ATTRIBUTES (UNBUFFERED)

            INPUT v_compania.* FROM compania.*
            END INPUT

            ON ACTION ACCEPT

                IF ((v_compania.nombre IS NULL) OR (LENGTH(v_compania.nombre CLIPPED) IS NULL)) THEN
                    CALL fn_mensaje ("Atención","Favor de ingresar el nombre de la campaña","info")
                    NEXT FIELD nombre
                END IF

                IF (v_compania.fecha_ini IS NULL) THEN
                    CALL fn_mensaje ("Atención","Favor de ingresar la fecha de inicio","info")
                    NEXT FIELD fecha_ini
                END IF

                IF (v_compania.fecha_fin IS NULL) THEN
                    CALL fn_mensaje ("Atención","Favor de ingresar la fecha fin","info")
                    NEXT FIELD fecha_fin
                END IF

                IF ((v_compania.descripcion IS NULL) OR (LENGTH(v_compania.descripcion CLIPPED) IS NULL)) THEN
                    CALL fn_mensaje ("Atención","Favor de ingresar la descripción", "info")
                    NEXT FIELD descripcion
                END IF

                IF (v_compania.fecha_fin < v_compania.fecha_ini) THEN
                    CALL fn_mensaje ("Atención","La fecha fin debe ser igual o posterior a la fecha de inicio","info")
                    NEXT FIELD fecha_fin
                END IF

                IF (v_compania.fecha_ini < p_fecha_captura) THEN
                     CALL fn_mensaje("Atención","La fecha de inicio debe ser igual o posterior a la fecha de hoy","stop")
                     NEXT FIELD fecha_ini
                END IF

                CALL fn_valida_registro_campania(v_compania.*)
                     RETURNING v_resultado.*

                IF (v_resultado.estado == 0) THEN
                    CALL fn_registra_campania(v_compania.*,p_fecha_captura,p_usuario_cod)
                    CALL fn_mensaje ("Atención","La campaña se ha capturado correctamente","info")
                    EXIT DIALOG
                ELSE
                    CALL fn_mensaje("Atención",v_resultado.mensaje,"stop")
                END IF

            -- ON ACTION ACCEPT

            ON ACTION cancelar
                EXIT DIALOG


            ON ACTION CLOSE
                CONTINUE DIALOG


            BEFORE DIALOG
        
                CALL Dialog.setActionHidden("close",TRUE)
                LET v_compania.fecha_ini = TODAY
                LET v_compania.fecha_fin = TODAY

                DISPLAY p_fecha_captura TO fecha_captura

        END DIALOG

    CLOSE WINDOW w_campania_capt

END FUNCTION


PRIVATE FUNCTION fn_valida_registro_campania(p_compania)

    DEFINE p_compania           t_datos_campania

    DEFINE v_contador       SMALLINT

    DEFINE r_resultado      t_resultado

    LET r_resultado.estado = 0
    LET r_resultado.mensaje = ""

    SELECT COUNT(*)
    INTO   v_contador
    FROM   not_campana
    WHERE  nombre   = p_compania.nombre
      AND  ind_baja = 0

    IF (v_contador == 0) THEN

        -- Se valida si las fechas elegidas esta dentro de algun otro periodo, para una campaña vigente
        SELECT COUNT(*)
        INTO   v_contador
        FROM   not_campana
        WHERE  ind_baja = 0
          AND  ( (fecha_ini <= p_compania.fecha_ini AND p_compania.fecha_ini <= fecha_fin) OR
                 (fecha_ini <= p_compania.fecha_fin AND p_compania.fecha_fin <= fecha_fin) )

        IF (v_contador > 0) THEN
            LET r_resultado.estado = 1
            LET r_resultado.mensaje = "Ya existe una campaña en el periodo seleccionado. Para modificarla entra a la opción Actualización."
        END IF

    ELSE
        LET r_resultado.estado = 1
        LET r_resultado.mensaje = "Ya existe una campaña con el mismo nombre."
    END IF

    RETURN r_resultado.*

END FUNCTION


PRIVATE FUNCTION fn_registra_campania(p_compania,p_fecha_captura,p_usuario_cod)

    DEFINE p_compania           t_datos_campania
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod
    DEFINE p_fecha_captura      LIKE not_campana.fecha_modifica

    DEFINE v_fecha_vigente      LIKE not_campana.fecha_ini
    DEFINE v_estado             LIKE not_campana.estado

    LET v_fecha_vigente = NULL

    -- Se valida si la campaña vigente es anterior a la campaña a registrar.
    SELECT MIN(fecha_ini)
    INTO   v_fecha_vigente
    FROM   not_campana
    WHERE  estado   = 1
      AND  ind_baja = 0

    IF (v_fecha_vigente IS NULL) THEN -- Si no existe ninguna campaña activa
        LET v_estado = 1 -- Activa
    ELSE

        IF (p_compania.fecha_ini < v_fecha_vigente) THEN -- Si la campaña activa es posterior a la nueva

            LET v_estado = 1 -- Activa

            -- Se cambia el estado a pendiente de la campaña activa
            UPDATE not_campana
            SET    estado = 2
            WHERE  estado   = 1
              AND  ind_baja = 0

        ELSE
            LET v_estado = 2 -- Pendiente
        END IF

    END IF

    -- Se registra
    INSERT INTO not_campana(id_campana,
                            nombre,
                            fecha_ini,
                            fecha_fin,
                            descripcion,
                            estado,
                            fecha_modifica,
                            usuario_modifica,
                            ind_baja)
                     VALUES(seq_not_campana.NEXTVAL, -- id_campana
                            p_compania.nombre,
                            p_compania.fecha_ini,
                            p_compania.fecha_fin,
                            p_compania.descripcion,
                            v_estado,
                            p_fecha_captura,
                            p_usuario_cod,
                            0) -- ind_baja

END FUNCTION