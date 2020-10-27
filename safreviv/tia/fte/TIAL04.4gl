--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => TIA                                                                    #
#Programa     => TIAL02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para Traspasos I-A                                                     #
#Fecha inicio => Marzo 26, 2012                                                         #
#########################################################################################
DATABASE safre_viv
GLOBALS "TIAG01.4gl" --archivo de variables globales proceso_cod
GLOBALS 
DEFINE g_operacion    SMALLINT                      # 1 = CONSULTA; 2 = LIQUIDA
       --g_pid          LIKE bat_ctr_proceso.pid,     # ID del proceso
       --g_proceso_cod  LIKE cat_proceso.proceso_cod, # Código del proceso
       --g_opera_cod    LIKE cat_operacion.opera_cod, # Código de operacion
       
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
   --LET g_proceso_cod =  # Devolucion por errores de operacion
   --LET g_opera_cod   =   # Preliquidacion
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".TIAL04.log")

   --LET g_proceso_cod = 1701 # Traspasos
   --LET g_opera_cod   = 4 # LIQUIDACION 
   LET g_operacion   = 2 # Liquidar

   # Funcion global (GLOG) de liquidación
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_tia, g_opera_cod_tia_liquidacion, g_operacion)
 
END MAIN