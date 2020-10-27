####################################################################
#Modulo            =>AOP                                           #
#Programa          =>AOPC03.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>09/05/2013                                    #
####################################################################

GLOBALS

   CONSTANT PROCESO                    		SMALLINT = 2501
   CONSTANT OPERACION                  		SMALLINT = 1
	CONSTANT MODULO                    			CHAR(3) = 'aop'

   TYPE info_folio RECORD
		folio												DECIMAL(9,0),
		modulo											VARCHAR(50),
		proceso_desc									CHAR(50),
		usuario_desc									CHAR(40)
   END RECORD

	TYPE info_reverso RECORD
		folio												DECIMAL(9,0),
		f_proceso										DATE,
      folio_ajustado									DECIMAL(9,0),
		estado											VARCHAR(50),
		usuario_desc									CHAR(40)
   END RECORD

	TYPE lista RECORD
      f_proceso             						DATE,
      folio                  						DECIMAL(9,0),
      folio_ajustado              				DECIMAL(9,0),
      proceso                 					VARCHAR(40),
		estado											VARCHAR(50),
		usuario											VARCHAR(30)
   END RECORD

	TYPE cifras RECORD
      subcuenta             						VARCHAR(50),
      fondo                  						VARCHAR(50),
      acciones              						DECIMAL(22,6),
      pesos                 						DECIMAL(18,2)
   END RECORD

END GLOBALS