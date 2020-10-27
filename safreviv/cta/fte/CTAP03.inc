####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAP03.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>28/02/2013                                    #
####################################################################

GLOBALS

   CONSTANT VIVIENDA_97                      SMALLINT = 4
   CONSTANT VIVIENDA_92                      SMALLINT = 8

   TYPE detalle RECORD
      nss               CHAR(11),
      f_proceso         DATE,
      rfc               CHAR(13),
      curp              CHAR(18),
      ap_paterno        CHAR(50),
      ap_materno        CHAR(50),
      nombre            CHAR(50),
      pesos_92          DECIMAL(13,2),
      pesos_97          DECIMAL(13,2),
      f_valor           DATE,
      afore             CHAR(3)
   END RECORD

END GLOBALS