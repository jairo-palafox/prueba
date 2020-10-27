#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
#-----------------------------------------------------------------------------#
#Modulo            => CNT                                                     #
#Programa          => CNTM03                                                  #
#Objetivo          => PROGRAMA DE MANTENIMIENTO PARA EL CATALOGO DE CUENTAS   #
#                     CONTABLES                                               #
#Fecha Inicio      => 12-SEPTIEMBRE-2012                                      #
###############################################################################

GLOBALS

   CONSTANT INSERTA     SMALLINT = 1
   CONSTANT ACTUALIZA   SMALLINT = 2
   

   TYPE cat_cuenta RECORD
      cta_contable            CHAR(10),
      desc_cta_contable       CHAR(55),
      f_actualiza             DATE,
      usuario                 CHAR(20)
   END RECORD
   
END GLOBALS