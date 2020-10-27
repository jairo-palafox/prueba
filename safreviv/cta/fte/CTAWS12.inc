####################################################################
#Modulo            =>CTA                                           #
#Programa          =>CTAWS12.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>29 MARZO 2012                                 #
####################################################################
DATABASE safre_viv

GLOBALS

   #Codigos de respuesta
   CONSTANT SALDO_ANT_0001_REQ_RFC_NSS                CHAR(4) = '0001' 
   CONSTANT SALDO_ANT_0010_NSS_NOT_FOUND              CHAR(4) = '0010' 
   CONSTANT SALDO_ANT_0000_OK                         CHAR(4) = '0000'
   CONSTANT SALDO_ANT_0030_NSS_MULT                   CHAR(4) = '0030'
   CONSTANT SALDO_ANT_0040_RFC_NOT_FOUND              CHAR(4) = '0040'
   CONSTANT SALDO_ANT_0050_RFC_10_DIFF                CHAR(4) = '0050'
   CONSTANT SALDO_ANT_0060_RFC_MULT                   CHAR(4) = '0060'
   CONSTANT SALDO_ANT_0070_NSS_DIFF_RFC               CHAR(4) = '0070'
   CONSTANT SALDO_ANT_0080_RFC_13_DIFF                CHAR(4) = '0080'
   CONSTANT SALDO_ANT_0090_NSS_RFC_10_13_NOT_FOUND    CHAR(4) = '0090'

   #Tipos de consulta
   CONSTANT CONSULTA_NSS                              INTEGER = 1
   CONSTANT CONSULTA_RFC                              INTEGER = 2
   CONSTANT CONSULTA_AMBOS                            INTEGER = 3

   TYPE saldo_fondo RECORD
      nombre         LIKE afi_fondo72.nombre,
      saldo          LIKE cta_fondo72.importe,
      rfc            LIKE afi_fondo72.rfc,
      nss            LIKE afi_fondo72.nss
   END RECORD

   TYPE saldo_multiple RECORD
      codigo         STRING,
      lista_saldo DYNAMIC ARRAY OF saldo_fondo
   END RECORD
END GLOBALS