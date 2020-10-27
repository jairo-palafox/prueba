####################################################################
#Modulo            =>RET                                           #
#Programa          =>RETWS06.inc                                   #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>12 AGOSTO 2013                                #
####################################################################

DATABASE safre_viv

GLOBALS

   CONSTANT NO_ESTA_EN_PROGRAMA       SMALLINT = 120

   TYPE consultaPago RECORD
      id_solicitud         DECIMAL(9,0),
      id_afi_fondo72       DECIMAL(9,0),
      clabe                CHAR(18),
      saldo                DECIMAL(14,2),
      f_aplicacion         DATE,
      estado_solicitud     INTEGER,
      cod_rechazo          INTEGER
   END RECORD
   
END GLOBALS
