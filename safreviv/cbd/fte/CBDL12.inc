################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => RET                                                      #
#Programa          => CBDL12.inc                                               #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 06/06/2014                                               #
################################################################################

GLOBALS
   #Constantes del proceso de integracion
   CONSTANT PROC_BDNSVIV              SMALLINT = 2106
   
   CONSTANT OP_VALIDA_ARCHIVO         SMALLINT = 1
   CONSTANT OP_INTEGRA_ARCHIVO        SMALLINT = 2

   CONSTANT EDO_ARCHIVO_VALIDADO      SMALLINT = 1
   CONSTANT EDO_ARCHIVO_INTEGRADO     SMALLINT = 2
   CONSTANT EDO_ARCHIVO_ERROR         SMALLINT = 3
   
END GLOBALS