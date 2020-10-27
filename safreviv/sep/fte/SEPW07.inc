####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW07.4gl                                    #
#Objetivo          =>Programa de definicion de datos               #
#Fecha inicio      =>18 MAYO 2012                                  #
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
# TYPE : tns2DatosExpedienteRequest
#
TYPE tns2DatosExpedienteRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosExpedienteRequest",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  fCaptura DATE ATTRIBUTE(XMLName="fCaptura"),
  fRecepcion DATE ATTRIBUTE(XMLName="fRecepcion"),
  folioProcesar STRING ATTRIBUTE(XMLName="folioProcesar"),
  nombreReclamante STRING ATTRIBUTE(XMLName="nombreReclamante"),
  nssAsociado STRING ATTRIBUTE(XMLName="nssAsociado"),
  nssInvadido STRING ATTRIBUTE(XMLName="nssInvadido"),
  numeroCaso STRING ATTRIBUTE(XMLName="numeroCaso"),
  tipoFlujo INTEGER ATTRIBUTE(XMLName="tipoFlujo")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1recibeDatosExpediente_recibeDatosExpediente
#
TYPE tns1recibeDatosExpediente_recibeDatosExpediente RECORD ATTRIBUTE(XMLSequence,XSTypeName="recibeDatosExpediente_recibeDatosExpediente",XSTypeNamespace="http://separacion.safre.efp.com")
  datosExpedienteRequest tns2DatosExpedienteRequest ATTRIBUTE(XMLName="datosExpedienteRequest")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2DatosExpedienteResponse
#
TYPE tns2DatosExpedienteResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="DatosExpedienteResponse",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  diagRechazo STRING ATTRIBUTE(XMLName="diagRechazo"),
  numeroCaso STRING ATTRIBUTE(XMLName="numeroCaso"),
  resultOperacion STRING ATTRIBUTE(XMLName="resultOperacion")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1recibeDatosExpedienteResponse
#
TYPE tns1recibeDatosExpedienteResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="recibeDatosExpedienteResponse_recibeDatosExpedienteResponse",XSTypeNamespace="http://separacion.safre.efp.com")
  recibeDatosExpedienteReturn tns2DatosExpedienteResponse ATTRIBUTE(XMLName="recibeDatosExpedienteReturn")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: RecibeDatosExpedienteService
# Port:    RecibeDatosExpedienteServicePortType
# Server:  http://172.16.16.201/Separacion/ws/r/RecibeDatosExpedienteService
#-------------------------------------------------------------------------------
#
# Location of the SOAP server.
# You can reassign this value at run-time.
#
DEFINE RecibeDatosExpedienteService_RecibeDatosExpedienteServicePortTypeLocation STRING

#-------------------------------------------------------------------------------
#
# Operation: recibeDatosExpediente
#
# FUNCTION: recibeDatosExpediente_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1recibeDatosExpediente
#   OUTPUT: GLOBAL ns1recibeDatosExpedienteResponse
#
# FUNCTION: recibeDatosExpedienteRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1recibeDatosExpediente
#
# FUNCTION: recibeDatosExpedienteResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ns1recibeDatosExpedienteResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1recibeDatosExpediente
DEFINE ns1recibeDatosExpediente tns1recibeDatosExpediente_recibeDatosExpediente ATTRIBUTE(XMLName="recibeDatosExpediente",XMLNamespace="http://separacion.safre.efp.com")


#-------------------------------------------------------------------------------

# VARIABLE : ns1recibeDatosExpedienteResponse
DEFINE ns1recibeDatosExpedienteResponse tns1recibeDatosExpedienteResponse ATTRIBUTE(XMLName="recibeDatosExpedienteResponse",XMLNamespace="http://separacion.safre.efp.com")



END GLOBALS
