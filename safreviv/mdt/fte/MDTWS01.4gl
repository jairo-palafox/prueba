######################################################################
#Modulo            =>MDT                                             #
#Programa          =>MDTWS01.4gl                                     #
#Objetivo          =>Programa que lanza la ejecucion del             #
#                    webServices que expone el servicio de validacion#
#                    de instruccion de mandato                       #
#Fecha inicio      =>05 MARZO 2012                                   #
######################################################################

GLOBALS "MDTWS02.inc"

MAIN
    CALL executaServidor()
END MAIN