--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 28/Nov/2013
--==============================================================================

################################################################################
#Proyecto     => SAFRE VIVIENDA                                                #
#Propietario  => E.F.P.                                                        #
#Modulo       => DAE                                                           #
#Programa     => DAEL19                                                        #
#Objetivo     => Programa lanzador de liquidación de Ajuste Amortizaciones     #
#                Excedentes                                                    #
#Fecha inicio => 28/Nov/2013                                                   #
################################################################################

DATABASE safre_viv
GLOBALS "DAEG01.4gl"
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid, --  ID del proceso
       g_proceso_cod  LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod    LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario        LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_titulo         STRING, -- titulo de la ventana
       p_operacion      SMALLINT

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo   IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo  )
   END IF
   CALL STARTLOG (p_usuario        CLIPPED|| ".DAEL04.log")

   LET g_proceso_cod = 2403 --Ajuste Amortización Excedente
   LET g_opera_cod   = 4    --Liquidación
   LET p_operacion   = 2    --Ejecutar liquidación
DISPLAY p_usuario, g_proceso_cod, g_opera_cod, p_operacion
   -- se invoca la funcion para enviar la liquidación
   CALL fn_liquida(p_usuario, g_proceso_cod, g_opera_cod, p_operacion)
 
END MAIN