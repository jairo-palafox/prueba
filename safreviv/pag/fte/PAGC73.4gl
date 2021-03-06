--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 05/06/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGC62                                                        #
#Objetivo     => consulta de liquidación de garantia de estados y municipios   #
#Fecha inicio => 05 Junio de 2013                                              #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna    STRING    # titulo de la ventana

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF

   # se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, 
                   g_proceso_cod_pag_registro_pagos_gem, 
                   g_opera_cod_pag_liquidacion,
                   1)
   
END MAIN