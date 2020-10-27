--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => SEP                                                     #
#Programa          => SEPC30                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE SEP                       #
#Autor             => Alexandro Hollmann, EFP                                 #
#Fecha Inicio      => 16/Mayo/2012                                            #
###############################################################################
DATABASE safre_viv
GLOBALS
DEFINE g_proceso_cod      LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod        LIKE cat_operacion.opera_cod, -- codigo de operacion
       g_usuario          LIKE seg_usuario.usuario_cod  -- clave del usuario firmado
END GLOBALS

MAIN
DEFINE p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)
   -- se crear el archivo log
   CALL STARTLOG(g_usuario CLIPPED|| ".SEPC30.log")

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- Se asignan las variables de control 
   LET g_proceso_cod = 2202 -- liquidación transferencia operacion 28
   LET g_opera_cod   = 3 -- preliquida saldo operacion 28

   -- se invoca la funcion general de consulta de preliquidacion
   -- es necesario cargar en cat_preliquida el proceso y el nombre de la tabla
   -- que contiene los datos de la preliquidacion
   CALL fn_consulta_preliq(g_usuario, g_proceso_cod, g_opera_cod)
END MAIN

