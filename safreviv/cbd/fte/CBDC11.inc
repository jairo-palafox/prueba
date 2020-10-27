################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => CBD                                                      #
#Programa          => CBDC11                                                   #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 26/08/2014                                               #
################################################################################

GLOBALS

   TYPE resumen_saldo RECORD
      subcuenta                              VARCHAR(80),
      acciones                               DECIMAL(22,6)
   END RECORD

   TYPE detalle_saldo RECORD
      modulo                                CHAR(3),
      subcuenta                             SMALLINT,
      modulo_desc                           VARCHAR(50),
      subcuenta_desc                        VARCHAR(50),
      cargo                                 DECIMAL(22,6),
      abono                                 DECIMAL(22,6),
      neto                                  DECIMAL(22,6),
      no_confirmados                        DECIMAL(22,6),
      no_confirmados_his                    DECIMAL(22,6)
   END RECORD

   TYPE sumario RECORD
      subcuenta_sum                          VARCHAR(50),
      cargo_sum                              DECIMAL(22,6),
      abono_sum                              DECIMAL(22,6),
      neto_sum                               DECIMAL(22,6),
      no_confirmado_sum                      DECIMAL(22,6),
      no_confirmado_his_sum                 DECIMAL(22,6)
   END RECORD

END GLOBALS