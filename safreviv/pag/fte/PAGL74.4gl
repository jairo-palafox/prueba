--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGL74                                                        #
#Objetivo     => Programa lanzador del proceso de liqudiación de registro de   #
#                pagos de garantia de estados y municipios                     #
#Fecha inicio => 04 Junio de 2013                                              #
################################################################################

DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod

DEFINE g_operacion    SMALLINT                      # 1 = CONSULTA; 2 = LIQUIDA

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_titulo       STRING                          # Titulo de la ventana

   # Se recupera la clave de usuario desde parámetro 
   # Argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF
   
   # se indica que se realizara la liquidacion
   LET g_operacion   = 2 # Liquidar

   # se invoca la funcion general de liquidacion
   CALL fn_liquida(p_usuario_cod, 
                   g_proceso_cod_pag_registro_pagos_gem, 
                   g_opera_cod_pag_liquidacion, 
                   g_operacion)
 
END MAIN