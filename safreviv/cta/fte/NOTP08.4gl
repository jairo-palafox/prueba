DATABASE safre_viv

GLOBALS

    CONSTANT c_etapa_desc           STRING   = "Vigencia Campañas"
    CONSTANT c_etapa_inicio         SMALLINT = 0
    CONSTANT c_etapa_fin            SMALLINT = 1

    CONSTANT c_baja_campania        SMALLINT = 0

    TYPE t_datos_campania RECORD
        id_campana          LIKE not_campana_bitacora.id_campana,
        nombre              LIKE not_campana_bitacora.nombre,
        fecha_ini           LIKE not_campana_bitacora.fecha_ini,
        fecha_fin           LIKE not_campana_bitacora.fecha_fin,
        descripcion         LIKE not_campana_bitacora.descripcion,
        fecha_alta          LIKE not_campana_bitacora.fecha_alta,
        usuario_alta        LIKE not_campana_bitacora.usuario_alta
    END RECORD

    TYPE t_resultado RECORD
        estado      SMALLINT,
        mensaje     STRING
    END RECORD

END GLOBALS

MAIN

    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod
    DEFINE p_pid                LIKE glo_pid.pid
    DEFINE p_proceso_cod        LIKE bat_ctr_proceso.proceso_cod
    DEFINE p_opera_cod          LIKE bat_ctr_operacion.opera_cod
    DEFINE p_folio              LIKE glo_ctr_archivo.folio
    DEFINE p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo
    DEFINE p_fecha_valida       DATE

    DEFINE v_resultado          t_resultado

    LET p_usuario_cod    = ARG_VAL(1)
    LET p_pid            = ARG_VAL(2)
    LET p_proceso_cod    = ARG_VAL(3)
    LET p_opera_cod      = ARG_VAL(4)
    LET p_folio          = ARG_VAL(5)
    LET p_nombre_archivo = ARG_VAL(6)
    LET p_fecha_valida   = ARG_VAL(7)

    -- Se escribe en el log del monitor el inicio de la operacion
    CALL fn_display_proceso(c_etapa_inicio,c_etapa_desc)
    DISPLAY "\n\n"

    IF (p_fecha_valida IS NULL) THEN
        LET p_fecha_valida = TODAY
    END IF

    DISPLAY "\nFecha:   ",p_fecha_valida USING "dd/mm/yyyy",
            "\nUsuario: ",p_usuario_cod

    -- Se llama la funcion realiza el barrido de las solicitudes
    CALL fn_valida_vigencias_campanias(p_pid,p_proceso_cod,p_opera_cod,p_fecha_valida,p_usuario_cod)
         RETURNING v_resultado.*

    -- Se actualizan las tablas de bitacora
    IF (v_resultado.estado == 0) THEN

        -- Se actualiza la operacion a finalizada
        CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING v_resultado.estado

        IF (v_resultado.estado <> 0) THEN
            DISPLAY "Error al actualizar la bitacora a Finalizado"
            CALL fn_muestra_inc_operacion(v_resultado.estado)
        END IF

    ELSE -- Error al ejecutar

        DISPLAY "ERROR AL VALIDAR LAS VIGENCIAS:\n\n"
        DISPLAY v_resultado.mensaje

        -- Se actualiza la operacion a "error"
        CALL fn_error_opera(p_pid,p_proceso_cod,p_opera_cod)
             RETURNING v_resultado.estado

        IF (v_resultado.estado <> 0) THEN
            DISPLAY "Error al actualizar la bitacora a Error"
            CALL fn_muestra_inc_operacion(v_resultado.estado)
        END IF

    END IF -- (v_resultado.estado == 0)

    -- Se escribe en el log del monitor el fin de la operacion
    DISPLAY "\n\n\n"
    CALL fn_display_proceso(c_etapa_fin,c_etapa_desc)

END MAIN


PRIVATE FUNCTION fn_valida_vigencias_campanias(p_pid,p_proceso_cod,p_opera_cod,p_fecha_ejecucion,p_usuario_cod)

    DEFINE p_pid                LIKE bat_ctr_proceso.pid
    DEFINE p_proceso_cod        LIKE bat_ctr_proceso.proceso_cod
    DEFINE p_opera_cod          LIKE bat_ctr_operacion.opera_cod
    DEFINE p_fecha_ejecucion    DATE
    DEFINE p_usuario_cod        LIKE seg_usuario.usuario_cod

    DEFINE v_datos_campania     t_datos_campania

    DEFINE r_resultado          t_resultado

    LET r_resultado.estado  = 0
    LET r_resultado.mensaje = ""

    -- Campañas vigentes que vencieron
    DECLARE cur_not_vigencia_vencida CURSOR FOR
    SELECT id_campana,
           nombre,
           fecha_ini,
           fecha_fin,
           descripcion,
           fecha_modifica,
           usuario_modifica
    FROM   not_campana
    WHERE  fecha_fin < p_fecha_ejecucion
      AND  ind_baja  = 0

    FOREACH cur_not_vigencia_vencida INTO v_datos_campania.*

        DISPLAY "\nCampaña Vencida: ",
                "\n  ID: ",v_datos_campania.id_campana,
                "\n  Nombre: ",v_datos_campania.nombre,
                "\n  Fecha Inicio: ",v_datos_campania.fecha_ini USING "dd/mm/yyyy",
                "\n  Fecha Fin: ",v_datos_campania.fecha_fin USING "dd/mm/yyyy"

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
                                         v_datos_campania.id_campana,
                                         v_datos_campania.nombre,
                                         v_datos_campania.fecha_ini,
                                         v_datos_campania.fecha_fin,
                                         v_datos_campania.descripcion,
                                         v_datos_campania.fecha_alta,
                                         v_datos_campania.usuario_alta,
                                         p_fecha_ejecucion,
                                         CURRENT HOUR TO SECOND,
                                         "OPISACI",
                                         c_baja_campania)

        UPDATE not_campana
        SET    estado   = 0, -- Vencida
               ind_baja = 1 -- Dada de baja
        WHERE  id_campana = v_datos_campania.id_campana

    END FOREACH
    CLOSE cur_not_vigencia_vencida
    FREE cur_not_vigencia_vencida


    -- Campañas vigentes actuales
    DECLARE cur_not_campania_vigente CURSOR FOR
    SELECT id_campana
    FROM   not_campana
    WHERE  fecha_ini <= p_fecha_ejecucion
      AND  fecha_fin >= p_fecha_ejecucion
      AND  ind_baja  = 0

    FOREACH cur_not_campania_vigente INTO v_datos_campania.id_campana

        UPDATE not_campana
        SET    estado     = 1 -- Vigente
        WHERE  id_campana = v_datos_campania.id_campana

    END FOREACH
    CLOSE cur_not_campania_vigente
    FREE cur_not_campania_vigente


    -- Campañas futuras
    DECLARE cur_not_campania_futura CURSOR FOR
    SELECT id_campana
    FROM   not_campana
    WHERE  fecha_ini > p_fecha_ejecucion
      AND  ind_baja  = 0

    FOREACH cur_not_campania_futura INTO v_datos_campania.id_campana

        UPDATE not_campana
        SET    estado     = 2 -- Futura
        WHERE  id_campana = v_datos_campania.id_campana

    END FOREACH
    CLOSE cur_not_campania_futura
    FREE cur_not_campania_futura


    -- Campañas vencidas
    DECLARE cur_not_campania_vencida CURSOR FOR
    SELECT id_campana
    FROM   not_campana
    WHERE  fecha_fin < p_fecha_ejecucion
      AND  estado    = 0
      AND  ind_baja  = 0

    FOREACH cur_not_campania_vencida INTO v_datos_campania.id_campana

        UPDATE not_campana
        SET    ind_baja   = 1 -- Dada de baja
        WHERE  id_campana = v_datos_campania.id_campana

    END FOREACH
    CLOSE cur_not_campania_vencida
    FREE cur_not_campania_vencida


    RETURN r_resultado.*

END FUNCTION