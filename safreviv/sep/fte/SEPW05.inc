####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW05.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>18 MAYO 2012                                  #
####################################################################
DATABASE safre_viv

GLOBALS

   CONSTANT WS_EXPEDIENTE          CHAR(5) = 'sep_2'

   #
   # TYPE : datosExpediente
   #
   TYPE datosExpediente RECORD
      fCaptura          DATE,
      fRecepcion        DATE,
      folioProcesar     STRING,
      nombreReclamante  STRING,
      nssAsociado       STRING,
      nssInvadido       STRING,
      numeroCaso        STRING,
      tipoFlujo         INTEGER
   END RECORD

   #
   # TYPE : respuestaDatosExpediente
   #
   TYPE respuestaDatosExpediente RECORD
      diagRechazo       STRING,
      numeroCaso        STRING,
      resultOperacion   STRING
   END RECORD

   #Variable para almacenar la fecha del dia
   DEFINE g_f_dia DATE

END GLOBALS