####################################################################
#Modulo            =>MDT                                           #
#Programa          =>MDTW01.inc                                    #
#Objetivo          =>Archivo de definicion de datos                #
#Fecha inicio      =>09 MARZO 2012                                 #
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
# TYPE : tns2NotificaInstruccionRequest
#
TYPE tns2NotificaInstruccionRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="NotificaInstruccionRequest",XSTypeNamespace="http://vo.mandatos.safre.efp.com")
  id_origen INTEGER ATTRIBUTE(XMLName="id_origen"),
  nss STRING ATTRIBUTE(XMLName="nss"),
  id_credito DECIMAL(10) ATTRIBUTE(XMLName="id_credito"),
  --id_mandato STRING ATTRIBUTE(XMLName="id_mandato"),
  cve_mandato STRING ATTRIBUTE(XMLName="cve_mandato"),
  tpo_descuento INTEGER ATTRIBUTE(XMLName="tpo_descuento"),
  valor_descuento DECIMAL(24) ATTRIBUTE(XMLName="valor_descuento"),
  f_canales DATE ATTRIBUTE(XMLName="f_canales"),
  f_inicio_mandato DATE ATTRIBUTE(XMLName="f_inicio_mandato"),
  f_culmina_mandato DATE ATTRIBUTE(XMLName="f_culmina_mandato"),
  referencia STRING ATTRIBUTE(XMLName="referencia"),
  id_canales DECIMAL(9) ATTRIBUTE(XMLName="id_canales"),
  tipo_operacion STRING ATTRIBUTE(XMLName="tipo_operacion"),
  resultado_operacion STRING ATTRIBUTE(XMLName="resultado_operacion"),
  diagnostico STRING ATTRIBUTE(XMLName="diagnostico")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2NotificaInstruccionResponse
#
TYPE tns2NotificaInstruccionResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="NotificaInstruccionResponse",XSTypeNamespace="http://vo.mandatos.safre.efp.com")
  id_origen INTEGER ATTRIBUTE(XMLName="id_origen"),
  nss STRING ATTRIBUTE(XMLName="nss"),
  id_credito DECIMAL(10) ATTRIBUTE(XMLName="id_credito"),
  --id_mandato STRING ATTRIBUTE(XMLName="id_mandato"),
  cve_mandato STRING ATTRIBUTE(XMLName="cve_mandato"),
  tpo_descuento INTEGER ATTRIBUTE(XMLName="tpo_descuento"),
  valor_descuento DECIMAL(24) ATTRIBUTE(XMLName="valor_descuento"),
  f_canales DATE ATTRIBUTE(XMLName="f_canales"),
  f_inicio_mandato DATE ATTRIBUTE(XMLName="f_inicio_mandato"),
  f_culmina_mandato DATE ATTRIBUTE(XMLName="f_culmina_mandato"),
  referencia STRING ATTRIBUTE(XMLName="referencia"),
  id_canales DECIMAL(9) ATTRIBUTE(XMLName="id_canales"),
  tipo_operacion STRING ATTRIBUTE(XMLName="tipo_operacion"),
  resultado_operacion STRING ATTRIBUTE(XMLName="resultado_operacion"),
  diagnostico STRING ATTRIBUTE(XMLName="diagnostico"),
  diag_notifica STRING ATTRIBUTE(XMLName="diag_notifica")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: NotificaInstruccionMandatosServiceService
# Port:    NotificaInstruccionMandatosService
# Server:  http://localhost:8080/WSMandatos/services/NotificaInstruccionMandatosService
#-------------------------------------------------------------------------------
#
# Location of the SOAP server.
# You can reassign this value at run-time.
#
DEFINE NotificaInstruccionMandatosServiceService_NotificaInstruccionMandatosServiceLocation STRING

#-------------------------------------------------------------------------------
#
# Operation: notifica_instruccion_mdt_can
#
# FUNCTION: notifica_instruccion_mdt_can_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1notifica_instruccion_mdt_can
#   OUTPUT: GLOBAL ns1notifica_instruccion_mdt_canResponse
#
# FUNCTION: notifica_instruccion_mdt_canRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1notifica_instruccion_mdt_can
#
# FUNCTION: notifica_instruccion_mdt_canResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ns1notifica_instruccion_mdt_canResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1notifica_instruccion_mdt_can
DEFINE ns1notifica_instruccion_mdt_can RECORD ATTRIBUTE(XMLSequence,XMLName="notifica_instruccion_mdt_can",XMLNamespace="http://mandatos.safre.efp.com")
         notificaInstruccionRequest tns2NotificaInstruccionRequest ATTRIBUTE(XMLName="notificaInstruccionRequest")
       END RECORD


#-------------------------------------------------------------------------------

# VARIABLE : ns1notifica_instruccion_mdt_canResponse
DEFINE ns1notifica_instruccion_mdt_canResponse RECORD ATTRIBUTE(XMLSequence,XMLName="notifica_instruccion_mdt_canResponse",XMLNamespace="http://mandatos.safre.efp.com")
         notifica_instruccion_mdt_canReturn tns2NotificaInstruccionResponse ATTRIBUTE(XMLName="notifica_instruccion_mdt_canReturn")
       END RECORD



END GLOBALS
