--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL54                                                                 #
#Objetivo     => Programa lanzador del proceso de liqudiacion para registro de pagos    #
#                de fondo anterior                                                      #
#Fecha inicio => Febrero 01, 2013                                                       #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_operacion    SMALLINT                      # 1 = CONSULTA; 2 = LIQUIDA
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_s_titulo       STRING                        # Titulo de la ventana

   # Se recupera la clave de usuario desde parámetro 
   # Argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGL54.log")

   -- se indica que se realizara la preliquidacion
   LET g_operacion   = 2 # Liquidar

   -- se invoca la funcion general de liquidacion
   CALL fn_liquida_fondo72(p_usuario_cod, g_proceso_cod_pag_registro_pagos_fa, g_opera_cod_pag_liquidacion, g_operacion)
 
END MAIN