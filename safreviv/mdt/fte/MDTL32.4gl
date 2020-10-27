--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 12-07-2013
--==============================================================================

################################################################################
#Modulo       => MDT                                                           #
#Programa     => MDTL32                                                        #
#Objetivo     => Liquidacion de pagos mandatos                                 #
#Autor        => Hugo Ram�rez                                                  #
#Fecha inicio => 12 Junio 2013                                                 #
################################################################################
DATABASE safre_viv
GLOBALS "MDTG02.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT,                     # forma como ejecutara el programa
       p_titulo_vtna    STRING,                       # titulo de la ventana
       v_operacion_liquidacion SMALLINT

   # Recupera par�metros
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   LET v_operacion_liquidacion   = g_opera_cod_integracion # liquidacion pago mandatos

   # se invoca la funcion general de liquidacion
   CALL fn_liquida(p_usuario_cod, 
                   g_proceso_cod_pago_mandatos, 
                   v_operacion_liquidacion,
                   2) # Liquidar 
   
END MAIN

