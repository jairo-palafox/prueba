################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => NOT                                                      #
#Programa          => NOTP04                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 03/03/2015                                               #
################################################################################

GLOBALS
   #Constantes del proceso de integracion
   CONSTANT ESTADO_ARCHIVO_INTEGRADO    SMALLINT = 2
   CONSTANT ESTADO_ARCHIVO_ERROR        SMALLINT = 3

   CONSTANT ERROR_FECHAS                SMALLINT = 1
   CONSTANT ERROR_ARCHIVO_SUMARIO       SMALLINT = 2
   CONSTANT ERROR_INTEGRA_DETALLES      SMALLINT = 3

END GLOBALS