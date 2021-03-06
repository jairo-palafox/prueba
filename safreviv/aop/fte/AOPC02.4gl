###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            =>                                                         #
#Programa          =>                                                         #
#Objetivo          => CONSULTA DE LIQUIDACION                                 #
#Fecha Inicio      =>                                                         #
###############################################################################
DATABASE safre_viv

GLOBALS "AOPC02.inc"

PRIVATE DEFINE p_usuario_cod     VARCHAR(20)
PRIVATE DEFINE p_tipo_ejecucion  SMALLINT
PRIVATE DEFINE p_titulo          STRING

MAIN

   LET p_usuario_cod    = ARG_VAL(1)
   LET p_tipo_ejecucion = ARG_VAL(2)
   LET p_titulo         = ARG_VAL(3)

   -- si se obtuvo el titulo, se pone como titulo de programa
   IF ( p_titulo IS NOT NULL ) THEN
      CALL ui.Interface.setText(p_titulo)
   END IF

   -- se invoca la funcion general de consulta de liquidacion
   CALL fn_liquida(p_usuario_cod, PROCESO, OPERACION, IND_CONSULTA)
END MAIN