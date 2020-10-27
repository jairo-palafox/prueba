--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:18/04/2012
--===============================================================

###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          => CONSULTA DE    LIQUIDACION DE SOLO INFONAVIT            #
#Fecha Inicio      =>                                                         #
#Modificacion      => se agrega archivo globales de aclaratorio y se sustitu- #
#                     yen las variables correspondientes; hilda rivas         #
###############################################################################
DATABASE safre_viv
GLOBALS "ACLG02.4gl"  -- se agrega archivo globales y se sustituyen las variables necesarias

--GLOBALS
--DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
--       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
--       g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
--END GLOBALS

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro 
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)


   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   -- LET g_proceso_cod = 102
   -- LET g_opera_cod   = 4  --3

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod_acl_reg_pag_sin_cambio, g_opera_cod_liquidacion,1)
END MAIN