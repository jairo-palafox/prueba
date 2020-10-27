####################################################################
#Modulo            =>CBD                                           #
#Programa          =>CBDL17.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>18/08/2014                                    #
####################################################################

GLOBALS

   CONSTANT SALDO_SAFRE                SMALLINT = 1
   CONSTANT SALDO_BDNSVIV              SMALLINT = 2

   TYPE saldos RECORD
      subcuenta                        STRING,
      acciones                         DECIMAL(22,6)
   END RECORD

END GLOBALS