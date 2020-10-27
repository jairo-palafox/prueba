################################################################################
#Proyecto          => SAFRE VIVIENDA                                           #
#Propietario       => E.F.P.                                                   #
--------------------------------------------------------------------------------
#Modulo            => NOT                                                      #
#Programa          => NOTL05.inc                                               #
#Objetivo          => Archivo de definicion de datos globales                  #
#Fecha inicio      => 30/07/2015                                               #
################################################################################

GLOBALS

   TYPE info_folio RECORD
		folio												DECIMAL(9,0),
		modulo											CHAR(50),
      proceso_cod										SMALLINT,
		proceso_desc									CHAR(40),
		usuario_desc									CHAR(40),
		f_proceso										DATE
   END RECORD

   TYPE notificaciones RECORD
      codigo                                 SMALLINT,
		descripcion                            VARCHAR(60)
   END RECORD
   
END GLOBALS