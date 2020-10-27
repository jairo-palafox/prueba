--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 08/04/2015
--==============================================================================
################################################################################
#Modulo            => PRT                                                      #
#Programa          => PRTL11                                                   #
#Descripcion       => Liquidación de traspasos de saldos cedente               #
#Autor             => Hugo César Ramírez García                                #
#Fecha inicio      => 08 Abril 2015                                            #
################################################################################
DATABASE safre_viv

GLOBALS "PRTG01.4gl"

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
       p_tipo_ejecucion SMALLINT,
       p_titulo_vtna    STRING,
       v_tpo_op_liquida SMALLINT,
       v_comando        STRING,
       v_ruta_axway     STRING

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo_vtna    = ARG_VAL(3)

   # 1 --> Consultar
   # 2 --> Liquidar
   LET v_tpo_op_liquida = 2 # Liquidar

   DISPLAY "EJECUTANDO SCRIPT AXWAY: /opt/Interpel/Scripts/variables/FOVISSSTE/EnvioFOVISSSTE.sh"
   
   LET v_ruta_axway = "/opt/Interpel/Scripts/variables/FOVISSSTE/EnvioFOVISSSTE.sh"
   LET v_comando = "sh "||v_ruta_axway CLIPPED   
   RUN v_comando
   
   # Función general de liquidación
   CALL fn_liquida(p_usuario_cod, 
                   C_PROCESO_COD_TRANS_SDO_CEDENTE, 
                   C_OPERA_COD_LIQ_TANS_CED,
                   v_tpo_op_liquida) 
   
END MAIN

