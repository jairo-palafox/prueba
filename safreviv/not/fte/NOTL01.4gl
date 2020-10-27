#####################################################################################################
#Modulo       => CARGA ARCHIVO 'not'                                                                #
#Programa     => NOTL01                                                                             #
#Objetivo     => Programa que carga archivos de pagos omisos usando el layout de TRM                #        
#Fecha_inicio =>                                                                                    # 
#####################################################################################################
DATABASE safre_viv 

GLOBALS
   DEFINE glo_pid             LIKE bat_ctr_proceso.pid  -- ID del proceso
   DEFINE glo_proceso_cod     INTEGER 
   DEFINE glo_operacion_cod   INTEGER
   
   DEFINE p_usuario           CHAR(20)   -- clave de usuario
   DEFINE p_titulo_pantalla   STRING     -- titulo de la ventana

   --DEFINE v_folio             DECIMAL(9,0)
END GLOBALS

MAIN 
   DEFINE p_tipo_ejecucion    SMALLINT   -- forma en que se ejecuta el programa
   DEFINE v_result_carga      SMALLINT   -- Bandera de carga de archivo

   -- se obtiene los parametros que se obtienen del fglrun
   LET p_usuario         = ARG_VAL(1)
   LET p_tipo_ejecucion  = ARG_VAL(2)
   LET p_titulo_pantalla = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_titulo_pantalla IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_pantalla)
   END IF

   LET glo_proceso_cod = 2907
   LET glo_operacion_cod = 1

   --CALL fn_genera_pid(glo_proceso_cod, glo_operacion_cod, p_usuario) RETURNING glo_pid    -- se llama a la funcion que genera el pid
   --   DISPLAY "Pid: ", glo_pid

   -- se llama a la funcion para poder validar si puede continuar el proceso
   IF (fn_valida_operacion(glo_pid,glo_proceso_cod,glo_operacion_cod) = 0 ) THEN
      CALL fn_carga_archivo(  glo_pid,
                              glo_proceso_cod,
                              glo_operacion_cod,
                              2,
                              "NOTL01",
                              "",
                              p_usuario,
                              TRUE)
                              RETURNING v_result_carga
         IF (v_result_carga = FALSE) THEN
            CALL fn_mensaje("Atención","Cancelar carga","stop")
         END IF
   END IF
END MAIN