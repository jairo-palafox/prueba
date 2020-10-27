--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 07/10/2015
--==============================================================================
################################################################################
#Modulo       => UNI                                                           #
#Programa     => UNIC13                                                        #
#Objetivo     => Consulta de preliquidacion de unificacion de cuentas          #
#Fecha inicio => 07/10/2015                                                    #
################################################################################
DATABASE safre_viv
GLOBALS "UNIG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Se asignan las variables de control 
   LET g_proceso_cod = 2318
   LET g_opera_cod   = 3

   -- se invoca la funcion general de consulta de preliquidacion
   -- es necesario cargar en cat_preliquida el proceso y el nombre de la tabla
   -- que contiene los datos de la preliquidacion
   CALL fn_consulta_preliq(p_usuario_cod, g_proceso_cod, g_opera_cod)
END MAIN