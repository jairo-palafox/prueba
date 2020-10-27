################################################################################
#Versión                    => 1.0.0                                           #
#Fecha ultima modificacion  =>                                                 #
################################################################################
################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => RETL274                                                  #
#Objetivo          => Programa lanzador para la carga de la respuesta de       #
#                     FICO pago por DAP                                        #
#Fecha inicio      => 28 Noviembre, 2013                                       #
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
       v_nom_prog        VARCHAR(30), -- Almacena opción del menú 
       r_bnd_carga       SMALLINT -- Bandera de carga de archivo

   -- se asignan los parametros que vienen del fglrun
   LET v_usuario      = ARG_VAL(1)
   LET v_tipo_proceso = ARG_VAL(2)
   LET v_nom_prog     = ARG_VAL(3)
   
   -- se asigna el proceso
   LET g_proceso_cod  = g_proceso_cod_ret_resp_archiv_fico_DAP -- respuesta de archivo pago por DAP
   LET g_opera_cod    = g_opera_cod_valida_respuesta_fico_dap -- carga

   -- se asigna el titulo del programa
   IF ( v_nom_prog IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_nom_prog)
   END IF

   -- se verifica que se pueda iniciar la carga
   IF ( fn_valida_operacion(g_pid,g_proceso_cod,g_opera_cod) = 0 ) THEN
      -- se invoca la carga del archivo
      CALL fn_carga_archivo(g_pid,
                            g_proceso_cod,
                            g_opera_cod,
                            2, --Tipo de carga: 1 es en línea y 2 es nohup
                            "RETL274", --Programa
                            "", --Programa lanzado
                            v_usuario, 
                            TRUE) --Con TRUE la carga inicializa el proceso
      RETURNING r_bnd_carga
      
      IF r_bnd_carga = FALSE THEN
         CALL fn_mensaje("Atención","Carga Cancelada","about")
      END IF
   ELSE
      -- no se puede enviar la carga
      CALL fn_muestra_inc_operacion(fn_valida_operacion(g_pid,g_proceso_cod,
                                                        g_opera_cod))
   END IF
END MAIN

