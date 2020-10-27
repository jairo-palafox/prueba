###############################################################################
#Proyecto          => SACI VIVIENDA                                           #
#Propietario       => E.F.P.                                                  #
-------------------------------------------------------------------------------
#Modulo            => AFILIACION                                              #
#Programa          => AFIWS17.inc                                             #
#Objetivo          => ARCHIVO DE DEFINICION DE DATOS                          #
#Fecha Inicio      => 25-OCTUBRE-2018                                         #
###############################################################################

GLOBALS

   CONSTANT ACTUALIZACION_EXITOSA    CHAR(3) = '001'
   CONSTANT ACTUALIZACION_RECHAZADA  CHAR(3) = '002'
   CONSTANT ID_SISTEMA_CRM           INTEGER = 3

#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# Global HTTP server incoming variable type definition
#
TYPE tGlobalServerHttpInputVariableType RECORD
	verb	STRING,	# HTTP request VERB
	url	STRING,	# HTTP request URL
	headers	DYNAMIC ARRAY OF RECORD	# HTTP Headers
		name	STRING,
		value	STRING
	END RECORD
END RECORD

#
# Global HTTP server outgoing variable type definition
#
TYPE tGlobalServerHttpOutputVariableType RECORD
	code	INTEGER,	# HTTP status code
	desc	STRING,	# HTTP description
	headers	DYNAMIC ARRAY OF RECORD	# HTTP Headers
		name	STRING,
		value	STRING
	END RECORD
END RECORD


#
# TYPE : tns2DatosSACIRequest
#
TYPE tns2DatosSACIRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIRequest",XSTypeNamespace="http://vo.service.saci.efp.com")
   nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
   curp              CHAR(18)       ATTRIBUTE(XMLName="curp"),
   apellidoPaterno   VARCHAR(40)    ATTRIBUTE(XMLName="primerApellido"),
   apellidoMaterno   VARCHAR(40)    ATTRIBUTE(XMLName="segundoApellido",XMLOptional),
   nombre            VARCHAR(40)    ATTRIBUTE(XMLName="nombre"),
   fNacimiento       DATE           ATTRIBUTE(XMLName="fechaDeNacimiento"),
   genero            CHAR(1)        ATTRIBUTE(XMLName="genero"),
   caso              INTEGER        ATTRIBUTE(XMLName="casoCRM")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2DatosSACIResponse
#
TYPE tns2DatosSACIResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosSACIResponse",XSTypeNamespace="http://vo.service.saci.efp.com")
   codRespuesta      CHAR(3)        ATTRIBUTE(XMLName="codRespuesta"),
   descripcion       VARCHAR(200)   ATTRIBUTE(XMLName="descripcion")
END RECORD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# HTTP INPUT VARIABLE : ActualizaCurpHttpIn
DEFINE ActualizaCurpHttpIn tGlobalServerHttpInputVariableType

# HTTP OUTPUT VARIABLE : ActualizaCurpHttpOut
DEFINE ActualizaCurpHttpOut tGlobalServerHttpOutputVariableType

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : actualizaRequest
DEFINE actualizaRequest RECORD ATTRIBUTE(XMLSequence,XMLName="actualizaRequest",XMLNamespace="http://service.saci.efp.com")
         request tns2DatosSACIRequest ATTRIBUTE(XMLName="request")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : actualizaResponse
DEFINE actualizaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="actualizaResponse",XMLNamespace="http://service.saci.efp.com")
         actualizaReturn tns2DatosSACIResponse ATTRIBUTE(XMLName="consultarReturn")
       END RECORD


END GLOBALS
