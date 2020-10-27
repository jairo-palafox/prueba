--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLL02                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para Aclaraciones sin cambio NSS                                       #
#Fecha inicio => Febrero 15, 2012                                                         #
#Modificacion => se agrega archivo globales de aclaratorio y se sustituyen              #
#                las variables correspondientes; hilda rivas                            #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE --g_pid          LIKE bat_ctr_proceso.pid,     # ID del proceso
--       g_proceso_cod  LIKE cat_proceso.proceso_cod, # C�digo del proceso
--       g_opera_cod    LIKE cat_operacion.opera_cod, # C�digo de operacion
       g_operacion    SMALLINT                      # 1 = CONSULTA; 2 = LIQUIDA
END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_s_titulo       STRING                        # Titulo de la ventana

   # Se recupera la clave de usuario desde par�metro 
   # Argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF
   
   # Se asigna proceso y operaci�n
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLL04.log")

   --LET g_proceso_cod = 103 # Aclaracion con cambio
   --LET g_opera_cod   = 4 # LIQUIDACION 
   LET g_operacion   = 2 # Liquidar

   # Funcion global (GLOG) de liquidaci�n
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_acl_reg_pag_cambio, g_opera_cod_liquidacion, g_operacion)
 
END MAIN