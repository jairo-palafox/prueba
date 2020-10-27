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
       p_s_titulo       STRING, -- titulo de la ventana
       p_operacion      SMALLINT -- operacion 1: ejecuta, 2: consulta

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET g_proceso_cod = 2318 -- uni
   LET g_opera_cod   = 4 -- liquidacion
   LET p_operacion   = 1 -- consultar la liquidacion

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
END MAIN