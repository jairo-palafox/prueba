--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

#########################################################################################
#Modulo       => ACL                                                                    #
#Programa     => ACLL23                                                                 #
#Objetivo     => Programa que ejecuta el stored procedure que realiza la liquidacion    #
#                para Aclaraciones con cambio Nombre                                    #
#Fecha inicio => Agosto 16, 2012                                                        #
#########################################################################################
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

GLOBALS
DEFINE g_operacion    SMALLINT                      # 1 = CONSULTA; 2 = LIQUIDA
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
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLL23.log")

   --LET g_proceso_cod = 103 # Aclaracion con cambio
   --LET g_opera_cod   = 4 # LIQUIDACION 
   LET g_operacion   = 2 # Liquidar

--   DISPLAY "@@... VARIABLES... ", p_usuario_cod, g_proceso_cod_acl_reg_pag_cambio_nombre, g_opera_cod_liquidacion, g_operacion   

   # Funcion global (GLOG) de liquidación
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_acl_reg_pag_cambio_nombre, g_opera_cod_liquidacion, g_operacion)
 
END MAIN