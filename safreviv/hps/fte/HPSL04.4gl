--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => HPS                                                     #
#Programa          => HPSL04                                                  #
#Objetivo          => LIQUIDACION TRASPASO FONDO SCUBCUENTA SERVICIOS         #
#Autor             => JESUS YA�EZ MORENO, EFP                                 #
#Fecha Inicio      => 16/Marzo/2015                                           #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
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

   -- se crear el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".HPSL04.log")

   LET g_proceso_cod = 3101 -- liquidacion de operacion 28
   LET g_opera_cod   = 2 -- liquida saldo operacion 28

   -- se invoca la funcion general de liquidacion operacion 28
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod,2) 
   
END MAIN