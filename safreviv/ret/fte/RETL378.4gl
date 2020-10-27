################################################################################
#Modulo        => RET                                                          #
#Programa      => RETL378                                                      #
#Ojetivo       => Consulta de historico anexo 1 mediante carga de archivo con  #
#                 NSS, para obtener un archivo de salida con importe viv 92, 97#
#                 y el improte tesofe                                          #
#Fecha inicio  => 10 de Agosto, 2015.                                          #
#Requerimiento => 844                                                          #
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

    --Se guardan los valores pasados al programa al llamarlo
    LET p_usuario = ARG_VAL(1)
    LET p_tpo_ejecucion = ARG_VAL(2)
    LET p_nom_ventana = ARG_VAL(3)

    --Se definen los valores para el proceso y la operacion
    LET v_pid = 0
    LET v_proceso_cod = 1567
    LET v_opera_cod = 1

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
                              "RETL378", --Nombre del programa
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