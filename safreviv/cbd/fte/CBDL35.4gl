#################################################################
#MODULO         -->CBD                                          #
#PROGRAMA       -->CBDL35                                       #
#OBJETIVO       -->PROGRAMA PARA CARGAR A TABLA TEMPORAL        #
#                  Archivo de Ajuste de saldo                   #   
#FECHA             11/09/2015                                   #
#################################################################
DATABASE safre_viv

PRIVATE DEFINE v_pid             DECIMAL (9,0)
PRIVATE DEFINE v_proceso_cod     SMALLINT 
PRIVATE DEFINE v_opera_cod       SMALLINT 
PRIVATE DEFINE v_bandera         SMALLINT 

PRIVATE DEFINE v_usuario         CHAR(20)
PRIVATE DEFINE v_tpo_ejecucion   SMALLINT 
PRIVATE DEFINE v_nom_ventana     STRING


MAIN 
    CLOSE WINDOW SCREEN 
    LET v_usuario       = ARG_VAL(1)
    LET v_tpo_ejecucion = ARG_VAL(2)
    LET v_nom_ventana   = ARG_VAL(3)

    IF(v_nom_ventana IS NOT NULL) THEN 
        CALL ui.interface.setText(v_nom_ventana)
    END IF

CALL fn_valida_archivo_ajuste()

END MAIN  

FUNCTION fn_valida_archivo_ajuste()
   LET v_proceso_cod = 2116
   LET v_opera_cod = 1
    
-- Valida operacion para verificar si se puede continuar.
   IF (fn_valida_operacion(v_pid, v_proceso_cod, v_opera_cod) = 0 ) THEN
      CALL fn_carga_archivo(  v_pid,
                              v_proceso_cod,
                              v_opera_cod,
                              2,--1 o 2, 1 la carga va a ser en linea, siempre va a ser codigo 2
                              "CBDL35",
                              "",
                              v_usuario,
                              1)--va a generar el pid, en sentido contrario va 0, regularmente va con 1
      RETURNING v_bandera
      
      IF v_bandera = FALSE THEN
         CALL fn_mensaje("Atención","Carga Cancelada","about")
      END IF
      
   ELSE 
      CALL fn_muestra_inc_operacion(fn_valida_operacion(v_pid, v_proceso_cod, v_opera_cod))
   END IF
END FUNCTION 