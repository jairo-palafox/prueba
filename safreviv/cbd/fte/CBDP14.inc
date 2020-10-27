#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
#------------------------------------------------------------------------------#
#Modulo            => CBD                                                      #
#Programa          => CBDP14                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha Inicio      => 07-JULIO-2014                                            #
################################################################################
DATABASE safre_viv

GLOBALS

   TYPE cat_tab_movimiento DYNAMIC ARRAY OF RECORD
      tabla       VARCHAR(20),
      anio        SMALLINT
   END RECORD
   
END GLOBALS