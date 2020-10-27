####################################################################
#Modulo            =>AOP                                           #
#Programa          =>AOPR01.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>09/05/2013                                    #
####################################################################

GLOBALS

   CONSTANT PROCESO                    		SMALLINT = 2501
   CONSTANT OPERACION                  		SMALLINT = 1
	CONSTANT MODULO                    			CHAR(3) = 'aop'

   TYPE info_folio RECORD
		folio												DECIMAL(9,0),
		modulo											CHAR(50),
      proceso_cod										SMALLINT,
		proceso_desc									CHAR(40),
		usuario_desc									CHAR(40),
		f_liquida										DATE
   END RECORD

	TYPE info_reverso RECORD
		folio												DECIMAL(9,0),
		f_proceso										DATE,
      folio_ajustado									DECIMAL(9,0),
		cve_estado										SMALLINT,
		estado											VARCHAR(50),
		usuario_desc									CHAR(40)
   END RECORD

	TYPE cifras RECORD
      subcuenta             						VARCHAR(50),
      fondo                  						VARCHAR(50),
      acciones              						DECIMAL(22,6),
      pesos                 						DECIMAL(18,2)
   END RECORD

END GLOBALS