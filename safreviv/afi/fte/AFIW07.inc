####################################################################
#Proyecto          => SACI VIVIENDA                                #
#Propietario       => E.F.P.                                       #
#------------------------------------------------------------------#
#Modulo            => AFI                                          #
#Programa          => AFIW07                                       #
#Objetivo          => Archivo de definicion de datos               #
#Fecha Inicio      => 07 - Septiembre - 2017                       #
####################################################################

GLOBALS

#
# Global WebService error structure
# Check the contents of this structure for a detailed error description
# when a WebService function returns with a non-zero status.
#

DEFINE wsError RECORD
	code		STRING,
	codeNS		STRING,
	description	STRING,
	action		STRING
END RECORD




#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# Global Endpoint user-defined type definition
#
TYPE tGlobalEndpointType RECORD	 # End point
	Address	RECORD	# Address
		Uri	STRING	# URI 
	END RECORD,
	Binding	RECORD	# Binding
		Version	STRING,	# HTTP Version (1.0 or 1.1)
		Cookie	STRING,	# Cookie to be set
		Request RECORD	 # HTTP request
			Headers DYNAMIC ARRAY OF RECORD	# HTTP Headers
				Name	STRING,
				Value	STRING
			END RECORD
		END RECORD,
		Response RECORD	 # HTTP response
			Headers DYNAMIC ARRAY OF RECORD	# HTTP Headers
				Name	STRING,
				Value	STRING
			END RECORD
		END RECORD,
		ConnectionTimeout	INTEGER,# Connection timeout
		ReadWriteTimeout	INTEGER,# Read write timeout
		CompressRequest		STRING	# HTTP request compression mode (gzip or deflate)
	END RECORD
END RECORD



#
# TYPE : tns2DatosConsultaCurp
#
TYPE tns2DatosConsultaCurp RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosConsultaCurp",XSTypeNamespace="http://services.wserv.ecurp.dgti.segob.gob.mx/xsd")
  cveCurp STRING ATTRIBUTE(XMLName="cveCurp",XMLOptional),
  cveEntidadEmisora STRING ATTRIBUTE(XMLName="cveEntidadEmisora",XMLOptional),
  direccionIp STRING ATTRIBUTE(XMLName="direccionIp",XMLOptional),
  password STRING ATTRIBUTE(XMLName="password",XMLOptional),
  tipoTransaccion INTEGER ATTRIBUTE(XMLName="tipoTransaccion",XMLOptional),
  usuario STRING ATTRIBUTE(XMLName="usuario",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ConsultaPorCurpService
# Port:    ConsultaPorCurpServiceHttpSoap11Endpoint
# Server:  https://127.0.0.1:143/WebServicesConsulta/services/ConsultaPorCurp_QA11
#-------------------------------------------------------------------------------
#
# Location of the SOAP endpoint.
# You can reassign this value at run-time.
#
DEFINE ConsultaPorCurpService_ConsultaPorCurpServiceHttpSoap11EndpointEndpoint tGlobalEndpointType

#-------------------------------------------------------------------------------
#
# Operation: consultarPorCurp
#
# FUNCTION: consultarPorCurp_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1consultarPorCurp
#   OUTPUT: GLOBAL ns1consultarPorCurpResponse
#
# FUNCTION: consultarPorCurpRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1consultarPorCurp
#
# FUNCTION: consultarPorCurpResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ns1consultarPorCurpResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1consultarPorCurp
DEFINE ns1consultarPorCurp RECORD ATTRIBUTE(XMLSequence,XMLName="consultarPorCurp",XMLNamespace="http://services.wserv.ecurp.dgti.segob.gob.mx")
         datos tns2DatosConsultaCurp ATTRIBUTE(XMLName="datos",XMLOptional)
       END RECORD


#-------------------------------------------------------------------------------

# VARIABLE : ns1consultarPorCurpResponse
DEFINE ns1consultarPorCurpResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultarPorCurpResponse",XMLNamespace="http://services.wserv.ecurp.dgti.segob.gob.mx")
         return STRING ATTRIBUTE(XMLName="return",XMLOptional)
       END RECORD

#-------------------------------------------------------------------------------
#
# Operation: getConfirm
#
# FUNCTION: getConfirm_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1getConfirm
#
# FUNCTION: getConfirm(p_sessionID, p_Mssg)
#
# FUNCTION: getConfirmRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1getConfirm
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1getConfirm
DEFINE ns1getConfirm RECORD ATTRIBUTE(XMLSequence,XMLName="getConfirm",XMLNamespace="http://services.wserv.ecurp.dgti.segob.gob.mx")
         sessionID STRING ATTRIBUTE(XMLName="sessionID",XMLOptional),
         Mssg STRING ATTRIBUTE(XMLName="Mssg",XMLOptional)
       END RECORD



END GLOBALS
