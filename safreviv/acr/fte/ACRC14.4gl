--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => Devolución de saldos excedentes ACR                     #
#Programa          => ACRC14                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE ACR                       #
#Fecha Inicio      => 25/Enero/2012                                           #
###############################################################################
DATABASE safre_viv
GLOBALS "ACRG10.4gl"

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  # Código de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
       p_tipo_ejecucion SMALLINT, # Forma como ejecutara el programa
       p_s_titulo       STRING    # Titulo de la ventana
   
   # Se recupera la clave de usuario desde parametro
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   -- se crea el archivo log
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".ACRC14.log")

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF(p_s_titulo IS NOT NULL)THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   # Se asignan las variables de control 
   LET g_proceso_cod = g_proc_cod_acr_liquida_dse -- Proceso Devolución de saldos excedentes ACR
   LET g_opera_cod   = 1 # Operacion Consulta de pre liquidación

   # Se invoca la funcion general de consulta de preliquidacion
   # Es necesario cargar en cat_preliquida el proceso y operacion y el nombre de 
   # la tabla correspondiente a la informacion de preliquidación   
   CALL fn_consulta_preliq(p_usuario_cod, g_proceso_cod, g_opera_cod) # funcion de GLOG02.4gl
END MAIN