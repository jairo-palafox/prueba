####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS01.4gl                                   #
#Objetivo          =>Programa que lanza la ejecucion del           #
#                    webServices que expone el servicio de consulta#
#                    de saldo total 9297                           #
#Fecha inicio      =>10 FEBRERO 2012                               #
####################################################################

GLOBALS "CTAWS02.inc"

MAIN
   CALL executaServidor()
END MAIN