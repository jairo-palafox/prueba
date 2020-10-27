###############################################################################
#Proyecto          => SAFRE VIVIENDA                                          #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => CUENTAS                                                 #
#Programa          => CTAM001                                                 #
#Objetivo          => Actualizar el catalogo de movimientos                   #
#Fecha Inicio      => 01-AGOSTO-2018                                          #
###############################################################################

GLOBALS
   CONSTANT MAX_REGISTROS     SMALLINT = 200
   
   TYPE movimiento RECORD
      movimiento SMALLINT,
      movimiento_desc CHAR(40),
      desc_ciudadana VARCHAR(60),
      modulo_desc   VARCHAR(80)
   END RECORD

   TYPE bitacora_mov RECORD 
      movimiento            SMALLINT,
      movimiento_desc       VARCHAR(40),
      tipo                  SMALLINT,
      categoria             SMALLINT,
      modulo_cod            VARCHAR(3),
      desc_ciudadana        VARCHAR(60),
      f_modifica            DATE,
      usuario               VARCHAR(20),
      tpo_modifica          SMALLINT
   END RECORD

END GLOBALS