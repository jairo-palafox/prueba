################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP416                                                      #
#Ojetivo       => 
#Fecha inicio  => Marzo 2016                                                   #
#Requerimiento => 
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
GLOBALS "RETG01.4gl"
DATABASE safre_viv

MAIN

    -- Parametros recibidos
    DEFINE p_usuario_cod         STRING
    DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE p_opera_cod           LIKE cat_operacion.opera_cod
    DEFINE p_pid                 LIKE glo_pid.pid
    DEFINE p_folio_notificacion      DECIMAL(9,0)
    DEFINE p_folio_liquida       LIKE glo_folio.folio

    -- Folio generado para llamar el proceso de notificacion mediante SMS o correo
    -- Es el folio generado para el proceso
    
    -- Datos del proceso
    DEFINE v_estado                 SMALLINT
    DEFINE v_num_notificaciones     INTEGER

    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_pid
    CALL ARG_VAL(3) RETURNING p_proceso_cod
    CALL ARG_VAL(4) RETURNING p_opera_cod
    CALL ARG_VAL(5) RETURNING p_folio_notificacion
    CALL ARG_VAL(6) RETURNING p_folio_liquida

    -- Log del proceso
    CALL STARTLOG(p_usuario_cod CLIPPED||".RETP416.log")

    -- se envia la cadena que indica el inicio de etapa
    CALL fn_display_proceso(0, "VALIDACION")

    CALL fn_llenar_ret_notificacion(p_folio_liquida,p_folio_notificacion)
                                    RETURNING v_estado,v_num_notificaciones

    IF v_estado = 0 THEN
        DISPLAY ""
        DISPLAY "PROCESO DE VALIDACION COMPLETADO EXITOSAMENTE."
        DISPLAY "SOLICITUDES QUE SE INTENTARAN NOTIFICAR:      ",v_num_notificaciones
        DISPLAY "LLAMANDO AL PROCESO DE NOTIFICACION..."
        DISPLAY ""
        DISPLAY ""

        CALL fn_notifica_proceso(p_folio_notificacion,p_proceso_cod,p_usuario_cod)
        
    ELSE
        DISPLAY "PROCESO DE VALIDACION COMPLETADO EXITOSAMENTE."
        DISPLAY "PERO NO HAY CANDIDATOS A NOTIFICACION."
        DISPLAY ""
    END IF

    CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                                    RETURNING v_estado

    CALL fn_display_proceso(1,"VALIDACION")    

END MAIN

#Funcion que realiza el llenado de la tabla ret_notificacion
FUNCTION fn_llenar_ret_notificacion(p_folio_liquida,p_folio_notificacion)

    -- Parametros recibidos
    DEFINE p_folio_liquida           LIKE glo_folio.folio
    DEFINE p_folio_notificacion      LIKE glo_folio.folio -- Folio tomado por el proceso de notificacion

    -- Datos a devolver
    DEFINE r_estado                 SMALLINT
    DEFINE r_num_notificaciones     INTEGER

    -- Variables a insertar en ret_notifica_ley73_contingente
    DEFINE v_id_derechohabiente     LIKE ret_ley73.id_derechohabiente
    DEFINE v_id_solicitud           LIKE ret_ley73.id_solicitud

    -- Variables auxiliares
    DEFINE v_sql                    STRING

    LET r_estado = 0

    LET v_sql = "SELECT id_solicitud,id_derechohabiente "||
                "FROM   ret_ley73                       "||
                "WHERE  folio       = "||p_folio_liquida||
                "  AND  cod_rechazo = 0 "

    PREPARE prp_solicitudes FROM v_sql
    DECLARE cur_solicitudes CURSOR FOR prp_solicitudes

    FOREACH cur_solicitudes INTO v_id_solicitud,v_id_derechohabiente

        INSERT INTO ret_notifica_ley73_contingente
        (id_solicitud,id_derechohabiente,folio_liquida,folio_notificacion)
        VALUES
        (v_id_solicitud,v_id_derechohabiente,p_folio_liquida,p_folio_notificacion)

        LET r_num_notificaciones = r_num_notificaciones + 1

    END FOREACH

    RETURN r_estado,r_num_notificaciones

END FUNCTION