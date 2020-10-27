--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/10/2012
--==============================================================================

################################################################################
#Módulo          => SEP                                                        #
#Programa        => SEPL62                                                     #
#Objetivo        => Programa de liquidación de expedientes solo infonavit      #
#Fecha Inicio    => 17 Octubre 2012                                            #
################################################################################
DATABASE safre_viv
GLOBALS "SEPG02.4gl"
DEFINE v_proceso_cod LIKE cat_proceso.proceso_cod, # código del proceso
       v_opera_cod   LIKE cat_operacion.opera_cod  # código de operación


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # Clave del usuario
       p_tipo_ejecucion SMALLINT,                     # Forma como ejecutara el programa
       p_s_titulo       STRING                        # Titulo de la ventana

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)

   LET v_proceso_cod = v_proc_expedientes_solo_infonavit # liquidación de expedientes solo infonavit
   LET v_opera_cod   = v_opera_liq_expedientes_solo_infonavit # liquida saldo expedientes solo infonavit

   # se invoca la funcion general de liquidacion expedientes solo infonavit
   CALL fn_liquida(p_usuario_cod, 
                   v_proceso_cod, 
                   v_opera_cod, 
                   2) # 1 - consulta, 2 - liquidación
   
END MAIN