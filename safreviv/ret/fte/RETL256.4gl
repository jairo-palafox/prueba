################################################################################
#Versi�n                    => 1.0.0                                           #
#Fecha ultima modificacion  => 14/10/2013                                      #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETL256                                                  #
#Objetivo          => Programa lanzador para la carga de la respuesta de 
#                    FICO                                                      #
#Fecha inicio      => 14/10/2013                                               #
################################################################################


DATABASE safre_viv
GLOBALS "RETG01.4gl"

GLOBALS
DEFINE g_pid LIKE bat_ctr_proceso.pid, -- ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod LIKE cat_operacion.opera_cod -- codigo de operacion

       
END GLOBALS

MAIN
DEFINE v_usuario         VARCHAR(30), -- Almacena al usuario
       v_tipo_proceso    SMALLINT, -- Forma como ejecutara el programa 
       v_nom_prog        VARCHAR(30), -- Almacena opci�n del men� 
       r_bnd_carga       SMALLINT -- Bandera de carga de archivo


   -- se asignan los parametros que vienen del fglrun
   LET v_usuario = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog = ARG_VAL(3)
   LET g_proceso_cod = g_proceso_archivos_respuesta_fico
   LET g_opera_cod = g_opera_cod_valida_respuesta_fico

   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF


   IF (fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
         CALL fn_carga_archivo(g_pid,
                              g_proceso_cod,
                              g_opera_cod,
                              2, --Tipo de carga: 1 es en l�nea y 2 es nohup
                              "RETL256", --Programa
                              "", --Programa lanzado
                              v_usuario, 
                              TRUE) --Con TRUE la carga inicializa el proceso
         RETURNING r_bnd_carga
         
         IF r_bnd_carga = FALSE THEN
            CALL fn_mensaje("Atenci�n","Carga Cancelada","about")
         END IF
   ELSE
     CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod,
                                                       g_opera_cod))
   END IF
END MAIN

