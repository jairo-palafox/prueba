--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 06/04/2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTL09                                                   #
#Descripcion       => Liquidación de traspasos subsecuentes receptora          #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 06 Abril 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_titulo_vtna    STRING,
       v_tpo_op_liquida SMALLINT

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # 1 --> Consultar
   # 2 --> Liquidar
   LET v_tpo_op_liquida = 2 # Liquidar
   
   # Función general de liquidación
   CALL fn_liquida(p_usuario_cod, 
                   C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA, 
                   C_OPERA_COD_LIQUIDACION,
                   v_tpo_op_liquida) 
   
END MAIN

