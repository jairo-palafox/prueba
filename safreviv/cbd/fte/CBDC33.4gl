###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CBD                                                     #
#Programa          => CBDC33                                                  #
#Objetivo          => CONSULTA DE LIQUIDACION                                 #
#Fecha Inicio      => 22/10/2014                                              #
###############################################################################
DATABASE safre_viv

PRIVATE DEFINE v_proceso_cod				SMALLINT
PRIVATE DEFINE v_opera_cod					SMALLINT
PRIVATE DEFINE v_funcion					SMALLINT
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

   LET v_proceso_cod = 2111		#Ajuste Operativo
   LET v_opera_cod   = 4			#Liquidacion
	LET v_funcion = 1					#Consulta de liquidacion

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(v_usuario_cod, v_proceso_cod, v_opera_cod, v_funcion)
END MAIN