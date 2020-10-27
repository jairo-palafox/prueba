####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW01.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>17 MAYO 2012                                  #
####################################################################
DATABASE safre_viv

GLOBALS
   CONSTANT WS_NUM_CASO          CHAR(5) = 'sep_1'

   #
   # TYPE : asignaNumeroCaso
   #
   TYPE asignaNumeroCaso RECORD
      nssAsociado       CHAR(11),
      nssInvadido       CHAR(11)
   END RECORD

   #
   # TYPE : respuestaNumeroCaso
   #
   TYPE respuestaNumeroCaso RECORD
      diagRechazo       STRING,
      indExistencia     INTEGER,
      nssAsociado       STRING,
      nssInvadido       STRING,
      numeroCaso        STRING,
      resultOperacion   STRING
   END RECORD

   #Variable para almacenar la fecha del dia
   DEFINE g_f_dia DATE

END GLOBALS