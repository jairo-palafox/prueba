--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => ACLC                                                    #
#Programa          => ACLC02                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE ACLARACIONES SI CAMBIO NSS#
#Fecha Inicio      => 17/02/2012                                              #
#Modificacion      => se agrega archivo globales de aclaratorio y se sustitu- #
#                     yen las variables correspondientes; hilda rivas         #
###############################################################################

DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

--GLOBALS
--DEFINE g_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
--       g_proceso_cod LIKE cat_proceso.proceso_cod, # Código del proceso
--       g_opera_cod   LIKE cat_operacion.opera_cod  # Código de operacion
--END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_titulo       STRING                          # Titulo de la ventana

   # Se recupera la clave de usuario desde parámetro 
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   # Se asignan los valores a las variables de control
  -- LET g_proceso_cod = 102 # Aclaracioners sin cambio NSS
  -- LET g_opera_cod   = 3 # PRELIQUIDACION REGISTRO PAGOS 
   
   CALL fn_consulta_preliq(p_usuario_cod, g_proceso_cod_acl_reg_pag_sin_cambio, g_opera_cod_preliquidacion)
END MAIN