--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22-10-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC62                                                   #
#Objetivo          => consulta de liquidación de expedientes solo infonavit    #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 22 Octubre 2012                                          #
################################################################################
DATABASE safre_viv
GLOBALS "SEPG02.4gl"
DEFINE g_proceso_cod      LIKE cat_proceso.proceso_cod, # proceso
       g_opera_cod        LIKE cat_operacion.opera_cod, # operación
       g_usuario          LIKE seg_usuario.usuario_cod  # usuario

MAIN
DEFINE p_tipo_ejecucion SMALLINT, # forma como ejecutara el programa
       p_titulo         STRING    # titulo de la ventana

   LET g_usuario        = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)
   
   LET g_proceso_cod = v_proc_expedientes_solo_infonavit      # expedientes sólo infonavit
   LET g_opera_cod   = v_opera_liq_expedientes_solo_infonavit # liquida expedientes sólo infonavit

   # se invoca la funcion general de consulta de liquidación
   CALL fn_liquida(g_usuario, g_proceso_cod, g_opera_cod,1) 
   
END MAIN

