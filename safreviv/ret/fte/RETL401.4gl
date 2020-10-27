################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL401                                                      #
#Ojetivo       => Realizar la carga de un archivo con NSS, para consultar      #
#                 solicitudes con marca.                                       #
#Fecha inicio  => Noviembre, 2015.                                             #
#Requerimiento => PRODINF-845                                                  #
#------------------------------------------------------------------------------#
#Fecha de modificacion =>   Modificación                                       #
#                      =>                                                      #
#                      =>                                                      #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

MAIN

    --Datos del proceso y la operacion
    DEFINE v_pid            LIKE bat_ctr_proceso.pid --Id del proceso
    DEFINE v_proceso_cod    LIKE cat_proceso.proceso_cod --Codigo del proceso
    DEFINE v_opera_cod      LIKE cat_operacion.opera_cod --Codigo de la operacion

    --Datos recibidos
    DEFINE p_usuario        CHAR(30)
    DEFINE p_tpo_ejecucion  SMALLINT
    DEFINE p_nom_ventana    CHAR(30)
    --Valor regresado
    DEFINE r_band_carga     SMALLINT

    --Se guardan los valores pasados al programa al llamarlo
    LET p_usuario       = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_nom_ventana   = ARG_VAL(3)

    --Se definen los valores para el proceso y la operacion
    LET v_pid         = 0
    LET v_proceso_cod = g_archivo_solicitud_marca_procesar
    LET v_opera_cod   = g_archivo_solicitud_marca_procesar_carga

    --Se asigna el nombre de la ventana
    IF (p_nom_ventana IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_nom_ventana)
    END IF

    --Se verifica que se pueda realizar la operacion
    IF (fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) = 0) THEN
        CALL fn_carga_archivo(v_pid,
                              v_proceso_cod,
                              v_opera_cod,
                              2, --Ejecucion por nohup
                              "RETL401", --Nombre del programa
                              "", --Programa que ejecutara validaciones
                              p_usuario,
                              TRUE) --La carga inicializara el proceso
                              RETURNING r_band_carga
        IF (r_band_carga = FALSE) THEN
            CALL fn_mensaje("Atención","Carga Cancelada","about")
        END IF
    ELSE
      --No se puede enviar la carga
      CALL fn_muestra_inc_operacion(fn_valida_operacion(v_pid,
                                                        v_proceso_cod,
                                                        v_opera_cod))
    END IF

    RETURN r_band_carga

END MAIN