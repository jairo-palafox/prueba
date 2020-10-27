################################################################################
#Modulo        => RET                                                          #
#Programa      => RETP395                                                      #
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
    DEFINE v_folio_datamart      DECIMAL(9,0)
    -- Folio generado para llamar el proceso de notificacion mediante SMS o correo
    -- Es el folio generado para el proceso
    DEFINE v_folio_notificacion      DECIMAL(9,0)
    -- Datos del proceso
    DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod
    DEFINE p_opera_cod           LIKE cat_operacion.opera_cod
    DEFINE v_estado              SMALLINT
    DEFINE p_pid                 DECIMAL(9,0)

    CALL ARG_VAL(1) RETURNING p_usuario_cod
    CALL ARG_VAL(2) RETURNING p_pid
    CALL ARG_VAL(3) RETURNING p_proceso_cod
    CALL ARG_VAL(4) RETURNING p_opera_cod
    CALL ARG_VAL(5) RETURNING v_folio_notificacion
    CALL ARG_VAL(6) RETURNING v_folio_datamart

    -- Log del proceso
    CALL STARTLOG(p_usuario_cod CLIPPED||".RETP396.log")

    -- se envia la cadena que indica el inicio de etapa
    CALL fn_display_proceso(0, "VALIDACION")

    CALL fn_llenar_ret_notificacion(v_folio_datamart,v_folio_notificacion,p_proceso_cod)
                                    RETURNING v_estado

    IF v_estado = 0 THEN
        DISPLAY "Proceso de llenado de datos terminado exitosamente."
        DISPLAY ""
        DISPLAY "Llamando al proceso de notificacion..."
        DISPLAY ""
        DISPLAY ""

        CALL fn_notifica_proceso(v_folio_notificacion,p_proceso_cod,p_usuario_cod)
        
    ELSE
        DISPLAY "El proceso de validación termino exitosamente,"
        DISPLAY "pero no hay candidatos a notificación."
        DISPLAY ""
    END IF

    CALL fn_actualiza_opera_fin(p_pid,p_proceso_cod,p_opera_cod)
                                    RETURNING v_estado

    CALL fn_display_proceso(1,"VALIDACION")    

END MAIN

#Funcion que realiza el llenado de la tabla ret_notificacion
FUNCTION fn_llenar_ret_notificacion(p_folio_datamart,p_folio_notificacion,p_proceso_cod)

    -- Folio de las solicitudes procesadas (dia a dia).
    DEFINE p_folio_datamart      DECIMAL(9,0)
    DEFINE p_proceso_cod         LIKE cat_proceso.proceso_cod
    
    DEFINE v_sql                 STRING

    -- Variables a insertar en ret_notificacion
    DEFINE p_folio_notificacion      DECIMAL(9,0)

    -- Variables de control
    DEFINE v_num_notificaciones_marca  INTEGER
    DEFINE v_num_notificaciones        INTEGER
	DEFINE v_isam_err                  INTEGER
	DEFINE v_err_txt                   CHAR(250)

    LET v_num_notificaciones_marca = 0
    LET v_num_notificaciones       = 0

    DISPLAY ""
    DISPLAY ""

    -- Si no se recibio folio
    IF p_folio_datamart = 0 OR p_folio_datamart IS NULL THEN
        -- Si es el proceso diario, se regresa error
        IF p_proceso_cod = g_proceso_notificacion_datamart_diario THEN
            DISPLAY ""
            DISPLAY "No se ha recibido un folio válido."
            DISPLAY "."||p_folio_datamart||"."
            DISPLAY ""
            RETURN 1
        END IF
    END IF

    -- Se validan las solicitudes que no convivieron en un principio por marca
    -- Se emplea la funcion almacenada fn_ret_notificacion_marca
    IF p_proceso_cod = g_proceso_notificacion_datamart_diario THEN
        DISPLAY "REVISANDO SOLICITUDES RECHAZADAS..."
        LET v_sql = "EXECUTE FUNCTION fn_ret_notificacion_marca("||p_folio_notificacion||")"
        PREPARE prp_marcas FROM v_sql
        EXECUTE prp_marcas INTO v_num_notificaciones_marca,v_isam_err,v_err_txt
        IF v_num_notificaciones >= 0 THEN
            DISPLAY "Solicitudes que se intentaran notificar:    "||v_num_notificaciones_marca
        ELSE
            DISPLAY "Error:     "||v_num_notificaciones_marca
            DISPLAY "ISAM:      "||v_isam_err
            DISPLAY "Mensaje:   "||v_err_txt
        END IF
        DISPLAY ""
        DISPLAY ""
        -- Se realiza el proceso diario
        DISPLAY "INICIANDO VALIDACIONES..."
        LET v_sql = "EXECUTE FUNCTION fn_ret_datamart_notificacion_diario("||p_folio_datamart||","||p_folio_notificacion||")"
        --DISPLAY v_sql
        PREPARE prp_diario FROM v_sql
        EXECUTE prp_diario INTO v_num_notificaciones,v_isam_err,v_err_txt
        IF v_num_notificaciones >= 0 THEN
            DISPLAY "Solicitudes que se intentaran notificar:    "||v_num_notificaciones
        ELSE
            DISPLAY "Error:     "||v_num_notificaciones
            DISPLAY "ISAM:      "||v_isam_err
            DISPLAY "Mensaje:   "||v_err_txt
        END IF
    ELSE
        -- Se realiza la validacion toda la tabla ret_datamart
        IF p_proceso_cod = g_proceso_notificacion_datamart_historico THEN
            DISPLAY "INICIANDO VALIDACIONES..."
            LET v_sql = "EXECUTE FUNCTION fn_ret_datamart_notificacion_historico("||p_folio_notificacion||")"
            PREPARE prp_historico FROM v_sql
            EXECUTE prp_historico INTO v_num_notificaciones,v_isam_err,v_err_txt
            IF v_num_notificaciones >= 0 THEN
                DISPLAY "Solicitudes que se notificaran:    "||v_num_notificaciones
            ELSE
                DISPLAY "Error:     "||v_num_notificaciones
                DISPLAY "ISAM:      "||v_isam_err
                DISPLAY "Mensaje:   "||v_err_txt
            END IF
        END IF
    END IF

    IF (v_num_notificaciones + v_num_notificaciones_marca) > 0 THEN
        RETURN 0
    ELSE
        DISPLAY ""
        DISPLAY "No se generara ninguna notificación."
        DISPLAY ""
        RETURN 1
    END IF
END FUNCTION