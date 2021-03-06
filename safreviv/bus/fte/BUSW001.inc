####################################################################
#Modulo            =>BUS                                           #
#Programa          =>BUSW001.inc                                   #
#Objetivo          =>Archivo con la definicion de datos que se     #
#                    utilizaran en el servicio web                 #
#Fecha inicio      =>20 NOVIEMBRE 2013                             #
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
# TYPE : tns2IDSSN
#
TYPE tns2IDSSN RECORD ATTRIBUTE(XMLSequence,XSTypeName="IDSSN",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/")
  idSistema       DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idSistema",XMLNamespace=""),
  idEbusiness     DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idEbusiness",XMLNamespace=""),
  idPortafolio    DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idPortafolio",XMLNamespace=""),
  idServicio      DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idServicio",XMLNamespace=""),
  idCliente       DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idCliente",XMLNamespace=""),
  idCanal         DECIMAL(32,0) ATTRIBUTE(XSDinteger,XSDPattern="(0|([1-9][0-9]*))",XSDMinInclusive="0",XSDMaxInclusive="2147483647",XMLName="idCanal",XMLNamespace=""),
  codoperCliente  STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="50",XSDPattern="[a-zA-Z0-9]*",XMLName="codoperCliente",XMLNamespace=""),
  fecha           DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fecha",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns3alfanumerico_50
#
TYPE tns3alfanumerico_50 STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="50",XSTypeName="alfanumerico_50",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3numerico_3
#
TYPE tns3numerico_3 DECIMAL(3,0) ATTRIBUTE(XSDinteger,XSDTotalDigits="3",XSTypeName="numerico_3",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3numerico_4
#
TYPE tns3numerico_4 DECIMAL(4,0) ATTRIBUTE(XSDinteger,XSDTotalDigits="4",XSTypeName="numerico_4",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3alfanumerico_2
#
TYPE tns3alfanumerico_2 STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="2",XSTypeName="alfanumerico_2",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3alfanumerico_10
#
TYPE tns3alfanumerico_10 STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="10",XSTypeName="alfanumerico_10",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3alfanumerico_100
#
TYPE tns3alfanumerico_100 STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="100",XSTypeName="alfanumerico_100",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns1motivoRechazo
#
TYPE tns1motivoRechazo RECORD ATTRIBUTE(XMLSequence,XSTypeName="motivoRechazo",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  idMotivoRechazo    tns3alfanumerico_10 ATTRIBUTE(XMLName="idMotivoRechazo",XMLNamespace=""),
  descripcionRechazo tns3alfanumerico_100 ATTRIBUTE(XMLName="descripcionRechazo",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1ArrayOfMotivo
#
TYPE tns1ArrayOfMotivo RECORD ATTRIBUTE(XMLSequence,XSTypeName="ArrayOfMotivo",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  motivoRechazo DYNAMIC ARRAY ATTRIBUTE(XMLList) OF tns1motivoRechazo ATTRIBUTE(XMLName="motivoRechazo")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarTramiteEntrada
#
TYPE tns1notificarTramiteEntrada RECORD ATTRIBUTE(XMLSequence,XSTypeName="notificarTramiteEntrada",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  folioDeTramiteProcesar   tns3alfanumerico_50 ATTRIBUTE(XMLName="folioDeTramiteProcesar",XMLNamespace=""),
  folioDeTransaccion       tns3alfanumerico_50 ATTRIBUTE(XMLName="folioDeTransaccion",XMLNamespace=""),
  idProceso                tns3numerico_3 ATTRIBUTE(XMLName="idProceso",XMLNamespace=""),
  idOperacion              tns3numerico_4 ATTRIBUTE(XMLName="idOperacion",XMLNamespace=""),
  codRespuestaOpr          tns3alfanumerico_2 ATTRIBUTE(XMLName="codRespuestaOpr",XMLNamespace=""),
  motivosRechazo           tns1ArrayOfMotivo ATTRIBUTE(XMLName="motivosRechazo",XMLNamespace="",XMLOptional),
  notificacionXML          STRING ATTRIBUTE(XMLName="notificacionXML",XMLNamespace="",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarTramiteContrato
#
TYPE tns1notificarTramiteContrato RECORD ATTRIBUTE(XMLSequence,XSTypeName="notificarTramiteContrato",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  idssn  tns2IDSSN ATTRIBUTE(XMLName="idssn",XMLNamespace=""),
  cuerpo tns1notificarTramiteEntrada ATTRIBUTE(XMLName="cuerpo",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2Motivo
#
TYPE tns2Motivo RECORD ATTRIBUTE(XMLSimpleContent,XSTypeName="Motivo",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/")
  base STRING ATTRIBUTE(XMLBase),
  idMotivo STRING ATTRIBUTE(XMLName="idMotivo",XMLOptional,XMLAttribute),
  descripcion STRING ATTRIBUTE(XMLName="descripcion",XMLOptional,XMLAttribute)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2ArrayOfMotivos
#
TYPE tns2ArrayOfMotivos RECORD ATTRIBUTE(XMLSequence,XSTypeName="ArrayOfMotivos",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/")
  motivo DYNAMIC ARRAY ATTRIBUTE(XMLList) OF tns2Motivo ATTRIBUTE(XMLName="motivo",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2SSNROP
#
TYPE tns2SSNROP RECORD ATTRIBUTE(XMLSequence,XSTypeName="SSNROP",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/")
  codoper            STRING ATTRIBUTE(XMLName="codoper",XMLNamespace=""),
  codRespuesta       STRING ATTRIBUTE(XMLName="codRespuesta",XMLNamespace=""),
  codRespuestaOpr    STRING ATTRIBUTE(XMLName="codRespuestaOpr",XMLNamespace=""),
  descRespuesta      STRING ATTRIBUTE(XMLName="descRespuesta",XMLNamespace=""),
  motivos            tns2ArrayOfMotivos ATTRIBUTE(XMLName="motivos",XMLNamespace="",XMLOptional),
  codoperCliente     STRING ATTRIBUTE(XMLName="codoperCliente",XMLNamespace=""),
  tiempoRespuesta    STRING ATTRIBUTE(XMLName="tiempoRespuesta",XMLNamespace=""),
  fecha              DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fecha",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarTramiteSalida
#
TYPE tns1notificarTramiteSalida RECORD ATTRIBUTE(XMLSequence,XSTypeName="notificarTramiteSalida",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  folioAck tns3alfanumerico_50 ATTRIBUTE(XMLName="folioAck",XMLNamespace="",XMLOptional)
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1notificarTramiteRespuesta
#
TYPE tns1notificarTramiteRespuesta RECORD ATTRIBUTE(XMLSequence,XSTypeName="notificarTramiteRespuesta",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")
  ssnrop          tns2SSNROP ATTRIBUTE(XMLName="ssnrop",XMLNamespace=""),
  objetoRespuesta tns1notificarTramiteSalida ATTRIBUTE(XMLName="objetoRespuesta",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns3fecha
#
TYPE tns3fecha DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XSTypeName="fecha",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#
# TYPE : tns3alfanumerico_3
#
TYPE tns3alfanumerico_3 STRING ATTRIBUTE(XSDMinLength="1",XSDMaxLength="3",XSTypeName="alfanumerico_3",XSTypeNamespace="http://www.procesar.com.mx/BusTramites/comunes/comunes/")
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Service: BusTramites
# Port:    BusTramitesPortType
# Server:  alias://serverProcesar
#-------------------------------------------------------------------------------
#
# Location of the SOAP server.
# You can reassign this value at run-time.
#
DEFINE BusTramites_BusTramitesPortTypeLocation STRING

#-------------------------------------------------------------------------------
#
# Operation: notificarTramite
#
# FUNCTION: notificarTramite_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1notificarTramiteRequest
#   OUTPUT: GLOBAL ns1notificarTramiteResponse
#
# FUNCTION: notificarTramiteRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL ns1notificarTramiteRequest
#
# FUNCTION: notificarTramiteResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL ns1notificarTramiteResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : ns1notificarTramiteRequest
DEFINE ns1notificarTramiteRequest tns1notificarTramiteContrato ATTRIBUTE(XMLName="notificarTramiteRequest",XMLNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")


#-------------------------------------------------------------------------------

# VARIABLE : ns1notificarTramiteResponse
DEFINE ns1notificarTramiteResponse tns1notificarTramiteRespuesta ATTRIBUTE(XMLName="notificarTramiteResponse",XMLNamespace="http://www.procesar.com.mx/BusTramites/NotificarTramite/")

END GLOBALS
