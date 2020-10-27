####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETWS02.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>10 JULIO 2013                                 #
####################################################################

DATABASE safre_viv

GLOBALS

   CONSTANT NO_ESTA_EN_PROGRAMA       SMALLINT = 120

   TYPE consultaPago RECORD
      nss                     CHAR(11),
      rfc                     CHAR(13),
      nombre                  VARCHAR(60),
      estatusPago             SMALLINT,
      monto_pago              DECIMAL(14,2),
      codigoRechazo           SMALLINT
   END RECORD
   
END GLOBALS
