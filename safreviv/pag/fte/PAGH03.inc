################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => PAG                                                      #
#Programa          => PAGH03                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 21/03/2018                                               #
################################################################################

GLOBALS
   #Constantes del proceso de integracion
   CONSTANT ARCHIVO_PENDIENTE             SMALLINT = 1
   CONSTANT ESTADO_ARCHIVO_INTEGRADO      SMALLINT = 2
   CONSTANT ESTADO_ARCHIVO_ERROR          SMALLINT = 3

END GLOBALS