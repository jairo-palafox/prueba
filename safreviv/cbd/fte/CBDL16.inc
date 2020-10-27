####################################################################
#Modulo            =>CBD                                           #
#Programa          =>CBDL16.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>16/07/2014                                    #
####################################################################

GLOBALS

   TYPE saldos RECORD
      modulo									CHAR(3),
      subcuenta                        STRING,
      pesos                            DECIMAL(18,2),
      acciones                         DECIMAL(18,6)
   END RECORD

END GLOBALS