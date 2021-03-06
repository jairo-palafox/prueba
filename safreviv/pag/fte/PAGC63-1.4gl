--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 17/04/2013
--==============================================================================

################################################################################
#Modulo       => PAG                                                           #
#Programa     => PAGC62                                                        #
#Objetivo     => consulta de liquidación de aportaciones voluntarias           #
#Fecha inicio => 17 Abril de 2013                                              #
################################################################################
DATABASE safre_viv
GLOBALS "PAGG01.4gl"  # archivo de variables globales proceso_cod, opera_cod


MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, # clave del usuario firmado
       p_tipo_ejecucion SMALLINT, # forma como ejecutara el programa
       p_titulo_vtna    STRING,    # titulo de la ventana
       g_proceso_cod_pag_registro_pagos_av1 SMALLINT
       
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo_vtna IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo_vtna)
   END IF

    LET g_proceso_cod_pag_registro_pagos_av1 = 1410;

   # se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, 
                   g_proceso_cod_pag_registro_pagos_av1, 
                   g_opera_cod_pag_liquidacion,
                   1)
   
END MAIN