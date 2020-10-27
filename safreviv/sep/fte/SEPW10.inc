####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW10.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>18 MAYO 2012                                  #
####################################################################
DATABASE safre_viv

GLOBALS

   CONSTANT WS_RECLAMO          CHAR(5) = 'sep_3'

   #
   # TYPE : datosExpediente
   #
   TYPE avisoReclamo RECORD
      impAmortizacionIndebida    DECIMAL(32,2),
      nssAsociado                STRING,
      numeroCaso                 STRING
   END RECORD

   #
   # TYPE : respuestaDatosExpediente
   #
   TYPE respuestaAvisoReclamo RECORD
      diagRechazo                STRING,
      impAmortizacionIndebida    DECIMAL(32,2),
      nssAsociado                STRING,
      numeroCaso                 STRING,
      resultOperacion            STRING
   END RECORD

   #Variable para almacenar la fecha del dia
   DEFINE g_f_dia DATE

END GLOBALS