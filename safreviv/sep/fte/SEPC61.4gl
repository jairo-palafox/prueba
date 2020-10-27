--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 22-10-2012
--==============================================================================

################################################################################
#Modulo            => SEP                                                      #
#Programa          => SEPC61                                                   #
#Objetivo          => consulta de preliquidación de expedientes solo infonavit #
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
   
   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   LET g_proceso_cod = v_proc_expedientes_solo_infonavit         # expedientes sólo infonavit
   LET g_opera_cod   = v_opera_preliq_expedientes_solo_infonavit # preliquida expedientes sólo infonavit

   # se invoca la funcion general de consulta de preliquidacion
   # es necesario cargar en cat_preliquida el proceso y el nombre de la tabla
   # que contiene los datos de la preliquidacion
   CALL fn_consulta_preliq(g_usuario, g_proceso_cod, g_opera_cod)
END MAIN

