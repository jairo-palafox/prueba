####################################################################
#Modulo            =>CBD                                           #
#Programa          =>CBDS04.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>15/01/2013                                    #
####################################################################

GLOBALS

   CONSTANT VIVIENDA_97                      SMALLINT = 4
   CONSTANT VIVIENDA_92                      SMALLINT = 8

   TYPE detalle RECORD
      nss                                    CHAR(11),
      f_liquidacion                          DATE,
      modulo                                 CHAR(3),
      subcuenta                              CHAR(1),
      aivs                                   DECIMAL(22,6)
   END RECORD

END GLOBALS