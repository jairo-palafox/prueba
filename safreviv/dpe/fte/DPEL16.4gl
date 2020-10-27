--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 01/05/2012
--===============================================================

#########################################################################################
#Modulo       => DPE                                                                    #
#Programa     => DPEL16                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para la devolucion de pagos indevidos o en exceso.                     #
#Fecha inicio => 01/05/2012                                                             #
#########################################################################################
GLOBALS "DPEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING, -- titulo de la ventana
       p_operacion      SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DPEL16.log")

   LET g_proceso_cod = g_proceso_cod_dpe_credito-- devolucion de operaciones
   LET g_opera_cod   = g_opera_cod_dpe_liquidacion -- liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
 
END MAIN