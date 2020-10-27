--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/04/2013
--==============================================================================

################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => DAE                                                      #
#Programa          => DAEL04                                                   #
#Objetivo          => Programa que ejecuta la función general de liquidación   #
#                     para la devolucion de amortizaciones excedentes          #
#Fecha inicio      => 19/04/2013                                               #
################################################################################

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".DAEL04.log")

   LET g_proceso_cod = g_proceso_cod_dae --2400
   LET g_opera_cod   = g_opera_cod_dae_liquidacion -- 4 
   LET p_operacion   = 2 -- ejecutar liquidación

   -- se invoca la funcion para enviar la liquidación
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
 
END MAIN