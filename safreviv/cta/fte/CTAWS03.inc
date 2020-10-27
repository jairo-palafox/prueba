####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS03.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 FEBRERO 2012                               #
#                                                                  #
####################################################################
#Fecha cambio      =>13 OCTUBRE 2014                               #
#Cambio              Se agrega como parte del saldo la subcuenta   #
#                    de RISS VOLUNTARIO                            #
#                                                                  #
####################################################################
DATABASE safre_sdo

GLOBALS

   CONSTANT VIVIENDA_97       SMALLINT = 4
   CONSTANT VIVIENDA_92       SMALLINT = 8
   CONSTANT SOLO_INFONAVIT_97 SMALLINT = 44
   CONSTANT SOLO_INFONAVIT_92 SMALLINT = 42
   CONSTANT RISS_VOLUNTARIO   SMALLINT = 55
   CONSTANT FONDO_INV         SMALLINT = 11

   #Codigos de retorno para el servicio
   CONSTANT WS_CONSULTA9297_OK               CHAR(3) = '000'
   CONSTANT WS_CONSULTA9297_PENSION_OK       CHAR(3) = '001'
   CONSTANT WS_CONSULTA9297_NSS_NOT_FOUND    CHAR(3) = '002'
   #CONSTANT WS_CONSULTA9297_NNS_INVALID      CHAR(3) = '003'
   CONSTANT WS_CONSULTA9297_ESTADO           CHAR(3) = '004'
   #CONSTANT WS_CONSULTA9297_NSS_VERIFICA     CHAR(3) = '005'

   TYPE consultaSaldo RECORD
      id_derechohabiente      DECIMAL(9,0),
      nombre                  VARCHAR(40),
      paterno                 VARCHAR(40),
      materno                 VARCHAR(40),
      saldo_92                DECIMAL(20,2),
      saldo_97                DECIMAL(20,2),
      rfc                     CHAR(13),
      nss                     CHAR(11),
      f_corte                 DATE
   END RECORD
   
END GLOBALS
