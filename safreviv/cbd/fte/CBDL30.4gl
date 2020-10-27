##################################################################################
# Version: 1.0.0                                                                 #
# Fecha ultima modificacion: 16/10/2014                                          #
##################################################################################
##################################################################################
#Proyecto          => SAFRE VIVIENDA                                             #
#Propietario       => E.F.P.                                                     #
----------------------------------------------------------------------------------
#Modulo            => CBD                                                        #
#Programa          => CBDL30                                                     #
#Objetivo          => Programa lanzador para validar archivo de Ajuste operativo #
#Fecha inicio      => 16/01/2014                                                 #
##################################################################################
DATABASE safre_viv

PRIVATE DEFINE p_usuario            VARCHAR(30) -- Almacena al usuario
PRIVATE DEFINE p_tipo_ejecucion     SMALLINT -- Forma como ejecutara el programa 
PRIVATE DEFINE p_titulo             VARCHAR(30) -- Almacena opción del menú

PRIVATE DEFINE v_pid                LIKE bat_ctr_proceso.pid 
PRIVATE DEFINE v_proceso_cod        LIKE cat_proceso.proceso_cod -- codigo del proceso
PRIVATE DEFINE v_opera_cod          LIKE cat_operacion.opera_cod -- codigo de operacion

MAIN

   DEFINE v_result_carga            SMALLINT -- Bandera de carga de archivo
   
   -- se asignan los parametros que vienen del fglrun
   LET p_usuario = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo = ARG_VAL(3)

   -- se asigna el titulo del programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   LET v_proceso_cod = 2111
   LET v_opera_cod = 1

   -- Valida operacion para verificar si se puede continuar.
   IF (fn_valida_operacion(v_pid,v_proceso_cod,v_opera_cod) = 0 ) THEN
      CALL fn_carga_archivo(  v_pid,v_proceso_cod,
                              v_opera_cod,
                              2,
                              "CBDL30",
                              "",
                              p_usuario,
                              TRUE)
                              RETURNING v_result_carga

      IF v_result_carga = FALSE THEN
         CALL fn_mensaje("Atención","Carga Cancelada","about")
      END IF
      --END IF
   ELSE
     CALL fn_muestra_inc_operacion(fn_valida_operacion(v_pid,v_proceso_cod,
                                                       v_opera_cod))
   END IF

END MAIN