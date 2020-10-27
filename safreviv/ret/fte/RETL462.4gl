 --===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================
#########################################################################################
#Proyecto          => SAFRE VIVIENDA                                                    #
#Propietario       => E.F.P.                                                            #
-----------------------------------------------------------------------------------------
#Modulo            => RETL462                                                           #
#Programa          =>                                                                   #
#Objetivo          => CONSULTA DE LIQUIDACION DE RETIRO TRASPASOS FONDO DE AHORRO       #
#Fecha Inicio      =>                                                                   #
#########################################################################################
DATABASE safre_viv
GLOBALS "RETG01.4gl"
GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     --  ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  -- codigo de operacion
END GLOBALS

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

   LET g_proceso_cod = g_proceso_cod_ret_fondo_ahorro_trasp     --100
   LET g_opera_cod   = g_opera_fondo_ahorro_trasp_liquida --2
   
   --DISPLAY "begin fn_liquida_fondo72"
   -- se invoca la funcion de consulta de liquidacion fondo 72   
   CALL fn_liquida_fondo72(p_usuario_cod, g_proceso_cod, g_opera_cod,2)
  
END MAIN