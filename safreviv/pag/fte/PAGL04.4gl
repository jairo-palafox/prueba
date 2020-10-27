--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 18/04/2012
--===============================================================

#########################################################################################
#Modulo       => PAG                                                                    #
#Programa     => PAGL04                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para FORTALECIMIENTO DE CRÉDITO                                        #
#Fecha inicio => Junio 19, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  ---archivo de variables globales proceso_cod, opera_cod
GLOBALS
DEFINE g_pid          LIKE bat_ctr_proceso.pid,     # ID del proceso
       --g_proceso_cod  LIKE cat_proceso.proceso_cod, # Código del proceso
       --g_opera_cod    LIKE cat_operacion.opera_cod, # Código de operacion
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
   
   # Se asigna proceso y operación
   --LET g_proceso_cod = 16 # Devolucion por errores de operacion
   --LET g_opera_cod   = 4  # Preliquidacion
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".PAGL04.log")

   --LET g_proceso_cod = 1405 # Registro de pagos FORTALECIMIENTO DE CRÉDITO
   --LET g_opera_cod   = 4 # LIQUIDACION FORTALECIMIENTO DE CRÉDITO
   LET g_operacion   = 2 # Liquidar

   
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_pag_registro_pagos_fc, g_opera_cod_pag_liquidacion, g_operacion)
 
END MAIN