####################################################################
#Modulo            =>SEP                                           #
#Programa          =>SEPW03.4gl                                    #
#Objetivo          =>Programa de definicion de datos               #
#Fecha inicio      =>17 MAYO 2012                                  #
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
# TYPE : tns2AsignaCasoRequest
#
TYPE tns2AsignaCasoRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="AsignaCasoRequest",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  nssAsociado STRING ATTRIBUTE(XMLName="nssAsociado"),
  nssInvadido STRING ATTRIBUTE(XMLName="nssInvadido")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1asignaCaso_asignaCaso
#
TYPE tns1asignaCaso_asignaCaso RECORD ATTRIBUTE(XMLSequence,XSTypeName="asignaCaso_asignaCaso",XSTypeNamespace="http://separacion.safre.efp.com")
  asignaCasoRequest tns2AsignaCasoRequest ATTRIBUTE(XMLName="asignaCasoRequest")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2AsignaCasoResponse
#
TYPE tns2AsignaCasoResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="AsignaCasoResponse",XSTypeNamespace="http://vo.separacion.safre.efp.com")
  diagRechazo STRING ATTRIBUTE(XMLName="diagRechazo"),
  indExistencia INTEGER ATTRIBUTE(XMLName="indExistencia"),
  nssAsociado STRING ATTRIBUTE(XMLName="nssAsociado"),
  nssInvadido STRING ATTRIBUTE(XMLName="nssInvadido"),
  numeroCaso STRING ATTRIBUTE(XMLName="numeroCaso"),
  resultOperacion STRING ATTRIBUTE(XMLName="resultOperacion")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1asignaCasoResponse_asignaCasoResponse
#
TYPE tns1asignaCasoResponse_asignaCasoResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="asignaCasoResponse_asignaCasoResponse",XSTypeNamespace="http://separacion.safre.efp.com")
  asignaCasoReturn tns2AsignaCasoResponse ATTRIBUTE(XMLName="asignaCasoReturn")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: AsignaNumeroCasoService
# Port:    AsignaNumeroCasoServicePortType
# Server:  http://172.16.16.201/Separacion/ws/r/AsignaNumeroCasoService
#-------------------------------------------------------------------------------
#
# Location of the SOAP server.
# You can reassign this value at run-time.
#
DEFINE AsignaNumeroCasoService_AsignaNumeroCasoServicePortTypeLocation STRING

#-------------------------------------------------------------------------------
#
# Operation: fn_asigna_numero_caso
#
# FUNCTION: fn_asigna_numero_caso_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1asignaCaso
#   OUTPUT: GLOBAL ns1asignaCasoResponse
#
# FUNCTION: fn_asigna_numero_casoRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1asignaCaso
#
# FUNCTION: fn_asigna_numero_casoResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ns1asignaCasoResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1asignaCaso
DEFINE ns1asignaCaso tns1asignaCaso_asignaCaso ATTRIBUTE(XMLName="asignaCaso",XMLNamespace="http://separacion.safre.efp.com")


#-------------------------------------------------------------------------------

# VARIABLE : ns1asignaCasoResponse
DEFINE ns1asignaCasoResponse tns1asignaCasoResponse_asignaCasoResponse ATTRIBUTE(XMLName="asignaCasoResponse",XMLNamespace="http://separacion.safre.efp.com")



END GLOBALS
