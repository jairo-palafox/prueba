###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CBD                                                     #
#Programa          => CBDC32                                                  #
#Objetivo          => CONSULTA DE PRELIQUIDACION DE AJUSTE OPERATIVO          #
#Fecha Inicio      => 22/10/2014                                              #
###############################################################################
DATABASE safre_viv

PRIVATE DEFINE v_proceso_cod				SMALLINT
PRIVATE DEFINE v_opera_cod					SMALLINT
PRIVATE DEFINE v_usuario_cod           CHAR(20)
PRIVATE DEFINE v_tipo_ejecucion        SMALLINT
PRIVATE DEFINE v_titulo                STRING

MAIN

   LET v_usuario_cod    = ARG_VAL(1)
   LET v_tipo_ejecucion = ARG_VAL(2)
   LET v_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( v_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(v_titulo)
   END IF

   -- se asignan los valores a las variables de control
   LET v_proceso_cod = 2111 -- Liquidacion de pagos
   LET v_opera_cod   = 3 -- PRELIQUIDACION AJUSTE OPERATIVO

   CALL fn_consulta_preliq(v_usuario_cod, v_proceso_cod, v_opera_cod)
END MAIN