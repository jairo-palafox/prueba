--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--===============================================================

####################################################################
#Modulo            =>GRT                                           #
#Programa          =>GRTC07                                        #
#Objetivo          =>Programa que permite la consulta de la        #
#                    liquidaciÛn de DeoluviÛn de Saldos Excedentes #
#                    del mÛdulo de CrÈditos en GarantÌa 43 bis     #
#Autor             =>Daniel Buendia, EFP                           #
#Fecha inicio      =>21 Junio 2012                                 #
####################################################################
DATABASE safre_viv
GLOBALS "GRTG01.4gl"

GLOBALS
DEFINE g_pid         LIKE bat_ctr_proceso.pid,     # ID del proceso
       g_proceso_cod LIKE cat_proceso.proceso_cod, # CÛdigo del proceso
       g_opera_cod   LIKE cat_operacion.opera_cod  # CÛdigo de operacion
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
   CALL STARTLOG(p_usuario_cod CLIPPED|| ".GRTC07.log")

   # Si se obtuvo el titulo, se pone como titulo de programa
   IF(p_s_titulo IS NOT NULL)THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   # Se asignan las variables de control 
   LET g_proceso_cod = g_proc_cod_grt_liquida_dse -- liquidaciÛn devoluci”n saldos exc grt
   LET g_opera_cod   = 2 -- liquida devoluci”n de saldos exc grt

   -- se invoca la funcion general del consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, g_proceso_cod, g_opera_cod, 1)
END MAIN
