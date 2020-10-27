################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL376                                                      #
#Ojetivo       => Programa lanzador para cambiar el estado solicitud de 8 o 10 #
#                 a 100 en las tablas ret_solicitud_generico y                 #
#                 ret_amort_excedente.                                         #
#Fecha inicio  => Agosto, 2016.                                                #
#Requerimiento =>                                                              #
################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"

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

MAIN

    DEFINE v_estado INTEGER

    --Se guardan los valores pasados al programa al llamarlo
    LET p_usuario = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_nom_ventana = ARG_VAL(3)

    --Se definen los valores para el proceso y la operacion
    LET v_pid         = 0
    LET v_proceso_cod = 1565
    LET v_opera_cod   = 1

    --Se asigna el nombre de la ventana
    IF (p_nom_ventana IS NOT NULL ) THEN
        CALL ui.Interface.setText(p_nom_ventana)
    END IF

    --Se verifica que se pueda realizar la operacion
    CALL fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) RETURNING v_estado
    IF v_estado = 0 THEN
        CALL fn_carga_archivo(v_pid,
                              v_proceso_cod,
                              v_opera_cod,
                              2, --Ejecucion por nohup
                              "RETL376", --Nombre del programa
                              "", --Programa que ejecutara validaciones
                              p_usuario,
                              TRUE) --La carga inicializara el proceso
                              RETURNING r_band_carga
        IF (r_band_carga = FALSE) THEN
            CALL fn_mensaje("Atención","Carga Cancelada","about")
        END IF
    ELSE
      --No se puede enviar la carga
      CALL fn_muestra_inc_operacion(v_estado)
    END IF

    RETURN r_band_carga

END MAIN