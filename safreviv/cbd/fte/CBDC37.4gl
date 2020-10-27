###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CBD                                                     #
#Programa          => CBDC37                                                  #
#Objetivo          => CONSULTA DE LIQUIDACION AJUSTE SALDO                    #
#Fecha Inicio      => 24/09/2015                                              #
###############################################################################
DATABASE safre_viv

PRIVATE DEFINE v_proceso_cod				SMALLINT
PRIVATE DEFINE v_opera_cod					SMALLINT
PRIVATE DEFINE v_funcion					SMALLINT

MAIN
DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod, -- clave del usuario firmado
       p_tipo_ejecucion SMALLINT, -- forma como ejecutara el programa
       p_s_titulo       STRING -- titulo de la ventana

   -- se recupera la clave de usuario desde parametro
   -- argumento con indice 1
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_s_titulo       = ARG_VAL(3)


   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_s_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_s_titulo)
   END IF

   LET v_proceso_cod = 2116		#Ajuste Operativo
   LET v_opera_cod   = 4			#Liquidacion
	LET v_funcion = 1					#Consulta de liquidacion

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, v_proceso_cod, v_opera_cod, v_funcion)
END MAIN