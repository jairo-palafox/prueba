--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLL13                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para salida manual de aclaraciones                                     #
#Fecha inicio => Febrero 24, 2012                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "ACLG02.4gl"
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLL13.log")

   LET g_proceso_cod = g_proceso_cod_acl_salida_manual -- Salida maual de aclaraciones
   LET g_opera_cod   = 3 -- liquidacion
   LET p_operacion   = 2 -- ejecutar liquidacion

   -- se invoca la funcion para enviar la liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, p_operacion)
 
END MAIN