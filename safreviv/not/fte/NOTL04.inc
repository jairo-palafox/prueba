################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => NOT                                                      #
#Programa          => NOTL04.inc                                               #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 09/03/2015                                               #
################################################################################

GLOBALS
   #Constantes del proceso de integracion
   CONSTANT PROC_OMISOS_TRM           SMALLINT = 2910
   
   CONSTANT OP_VALIDA_ARCHIVO         SMALLINT = 1
   CONSTANT OP_INTEGRA_ARCHIVO        SMALLINT = 2

   CONSTANT EDO_ARCHIVO_VALIDADO      SMALLINT = 1
   CONSTANT EDO_ARCHIVO_INTEGRADO     SMALLINT = 2
   CONSTANT EDO_ARCHIVO_ERROR         SMALLINT = 3
   
END GLOBALS