###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAC001                                                 #
#Objetivo          => Actualizar el catalogo de movimientos                   #
#Fecha Inicio      => 01-AGOSTO-2018                                          #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 200
   
   TYPE movimiento RECORD
      movimiento        SMALLINT,
      movimiento_desc   VARCHAR(40),
      tipo_mov          VARCHAR(20),
      modulo            VARCHAR(60),
      ind_visible       VARCHAR(2),
      desc_ciudadana    VARCHAR(60),
      f_modifica        DATE,
      usuario           VARCHAR(40),
      tipo_modifica     VARCHAR(60)
   END RECORD

END GLOBALS