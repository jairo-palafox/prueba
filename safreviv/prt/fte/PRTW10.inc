#-------------------------------------------------------------------------------
# File: PRTW10.inc
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------


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
# TYPE : trecibeSolTraspInfonavit_idssn_idsistema
#
TYPE trecibeSolTraspInfonavit_idssn_idsistema DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idsistema",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_idebusiness
#
TYPE trecibeSolTraspInfonavit_idssn_idebusiness DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idebusiness",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_idportafolio
#
TYPE trecibeSolTraspInfonavit_idssn_idportafolio DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idportafolio",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_idservicio
#
TYPE trecibeSolTraspInfonavit_idssn_idservicio DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idservicio",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_idcliente
#
TYPE trecibeSolTraspInfonavit_idssn_idcliente DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idcliente",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_idcanal
#
TYPE trecibeSolTraspInfonavit_idssn_idcanal DECIMAL(32,0) ATTRIBUTE(XSDTotalDigits="32",XSDFractionDigits="0",XSTypeName="recibeSolTraspInfonavit_idssn_idcanal",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn_codopercliente
#
TYPE trecibeSolTraspInfonavit_idssn_codopercliente STRING ATTRIBUTE(XSDMaxLength="50",XSTypeName="recibeSolTraspInfonavit_idssn_codopercliente",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_idssn
#
TYPE trecibeSolTraspInfonavit_idssn RECORD ATTRIBUTE(XMLSequence,XSTypeName="recibeSolTraspInfonavit_idssn",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
  idsistema trecibeSolTraspInfonavit_idssn_idsistema ATTRIBUTE(XMLName="idsistema",XMLNamespace=""),
  idebusiness trecibeSolTraspInfonavit_idssn_idebusiness ATTRIBUTE(XMLName="idebusiness",XMLNamespace=""),
  idportafolio trecibeSolTraspInfonavit_idssn_idportafolio ATTRIBUTE(XMLName="idportafolio",XMLNamespace=""),
  idservicio trecibeSolTraspInfonavit_idssn_idservicio ATTRIBUTE(XMLName="idservicio",XMLNamespace=""),
  idcliente trecibeSolTraspInfonavit_idssn_idcliente ATTRIBUTE(XMLName="idcliente",XMLNamespace=""),
  idcanal trecibeSolTraspInfonavit_idssn_idcanal ATTRIBUTE(XMLName="idcanal",XMLNamespace=""),
  codopercliente trecibeSolTraspInfonavit_idssn_codopercliente ATTRIBUTE(XMLName="codopercliente",XMLNamespace=""),
  fecha DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fecha",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_foliocliente
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_foliocliente STRING ATTRIBUTE(XSDMaxLength="50",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_foliocliente",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_origensolicitud
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_origensolicitud STRING ATTRIBUTE(XSDMaxLength="3",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_origensolicitud",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_tipomovimiento
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_tipomovimiento STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_tipomovimiento",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_nss
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_nss STRING ATTRIBUTE(XSDMaxLength="11",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_nss",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_apellidopaterno
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_apellidopaterno STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_apellidopaterno",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_apellidomaterno
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_apellidomaterno STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_apellidomaterno",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_nombre
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_nombre STRING ATTRIBUTE(XSDMaxLength="40",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_nombre",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_curp
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_curp STRING ATTRIBUTE(XSDMaxLength="18",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_curp",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_tipooperacion
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_tipooperacion STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_tipooperacion",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_idcreditoinfonavit
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_idcreditoinfonavit STRING ATTRIBUTE(XSDMaxLength="10",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_idcreditoinfonavit",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_idcreditofovissste
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_idcreditofovissste STRING ATTRIBUTE(XSDMaxLength="10",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_idcreditofovissste",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditoinfonavit
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditoinfonavit DECIMAL(22,2) ATTRIBUTE(XSDTotalDigits="22",XSDFractionDigits="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_mtocreditoinfonavit",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditofovissste
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditofovissste DECIMAL(22,2) ATTRIBUTE(XSDTotalDigits="22",XSDFractionDigits="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_mtocreditofovissste",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_fechacredito
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_fechacredito STRING ATTRIBUTE(XSDMaxLength="8",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_fechacredito",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_valoraivinf97
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_valoraivinf97 DECIMAL(9,6) ATTRIBUTE(XSDTotalDigits="9",XSDFractionDigits="6",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_valoraivinf97",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivinf97
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivinf97 STRING ATTRIBUTE(XSDMaxLength="8",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivinf97",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_aivsinf97
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_aivsinf97 DECIMAL(9,6) ATTRIBUTE(XSDTotalDigits="9",XSDFractionDigits="6",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_aivsinf97",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_pesosinf97
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_pesosinf97 DECIMAL(10,2) ATTRIBUTE(XSDTotalDigits="10",XSDFractionDigits="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_pesosinf97",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_valoraivfov08
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_valoraivfov08 DECIMAL(9,6) ATTRIBUTE(XSDTotalDigits="9",XSDFractionDigits="6",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_valoraivfov08",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivfov08
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivfov08 STRING ATTRIBUTE(XSDMaxLength="8",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivfov08",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_aivsfov08
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_aivsfov08 DECIMAL(9,6) ATTRIBUTE(XSDTotalDigits="9",XSDFractionDigits="6",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_aivsfov08",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_pesosfov08
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_pesosfov08 DECIMAL(10,2) ATTRIBUTE(XSDTotalDigits="10",XSDFractionDigits="2",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_pesosfov08",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso_fechatransferencia
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso_fechatransferencia STRING ATTRIBUTE(XSDMaxLength="8",XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso_fechatransferencia",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavit_solicitudtraspaso
#
TYPE trecibeSolTraspInfonavit_solicitudtraspaso RECORD ATTRIBUTE(XMLSequence,XSTypeName="recibeSolTraspInfonavit_solicitudtraspaso",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
  foliocliente trecibeSolTraspInfonavit_solicitudtraspaso_foliocliente ATTRIBUTE(XMLName="foliocliente",XMLNamespace=""),
  origensolicitud trecibeSolTraspInfonavit_solicitudtraspaso_origensolicitud ATTRIBUTE(XMLName="origensolicitud",XMLNamespace=""),
  tipomovimiento trecibeSolTraspInfonavit_solicitudtraspaso_tipomovimiento ATTRIBUTE(XMLName="tipomovimiento",XMLNamespace=""),
  nss trecibeSolTraspInfonavit_solicitudtraspaso_nss ATTRIBUTE(XMLName="nss",XMLNamespace=""),
  apellidopaterno trecibeSolTraspInfonavit_solicitudtraspaso_apellidopaterno ATTRIBUTE(XMLName="apellidopaterno",XMLNamespace=""),
  apellidomaterno trecibeSolTraspInfonavit_solicitudtraspaso_apellidomaterno ATTRIBUTE(XMLName="apellidomaterno",XMLNamespace=""),
  nombre trecibeSolTraspInfonavit_solicitudtraspaso_nombre ATTRIBUTE(XMLName="nombre",XMLNamespace=""),
  curp trecibeSolTraspInfonavit_solicitudtraspaso_curp ATTRIBUTE(XMLName="curp",XMLNamespace=""),
  tipooperacion trecibeSolTraspInfonavit_solicitudtraspaso_tipooperacion ATTRIBUTE(XMLName="tipooperacion",XMLNamespace=""),
  idcreditoinfonavit trecibeSolTraspInfonavit_solicitudtraspaso_idcreditoinfonavit ATTRIBUTE(XMLName="idcreditoinfonavit",XMLNamespace=""),
  idcreditofovissste trecibeSolTraspInfonavit_solicitudtraspaso_idcreditofovissste ATTRIBUTE(XMLName="idcreditofovissste",XMLNamespace=""),
  mtocreditoinfonavit trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditoinfonavit ATTRIBUTE(XMLName="mtocreditoinfonavit",XMLNamespace=""),
  mtocreditofovissste trecibeSolTraspInfonavit_solicitudtraspaso_mtocreditofovissste ATTRIBUTE(XMLName="mtocreditofovissste",XMLNamespace=""),
  fechacredito trecibeSolTraspInfonavit_solicitudtraspaso_fechacredito ATTRIBUTE(XMLName="fechacredito",XMLNamespace=""),
  valoraivinf97 trecibeSolTraspInfonavit_solicitudtraspaso_valoraivinf97 ATTRIBUTE(XMLName="valoraivinf97",XMLNamespace=""),
  fechavaloraivinf97 trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivinf97 ATTRIBUTE(XMLName="fechavaloraivinf97",XMLNamespace=""),
  aivsinf97 trecibeSolTraspInfonavit_solicitudtraspaso_aivsinf97 ATTRIBUTE(XMLName="aivsinf97",XMLNamespace=""),
  pesosinf97 trecibeSolTraspInfonavit_solicitudtraspaso_pesosinf97 ATTRIBUTE(XMLName="pesosinf97",XMLNamespace=""),
  valoraivfov08 trecibeSolTraspInfonavit_solicitudtraspaso_valoraivfov08 ATTRIBUTE(XMLName="valoraivfov08",XMLNamespace=""),
  fechavaloraivfov08 trecibeSolTraspInfonavit_solicitudtraspaso_fechavaloraivfov08 ATTRIBUTE(XMLName="fechavaloraivfov08",XMLNamespace=""),
  aivsfov08 trecibeSolTraspInfonavit_solicitudtraspaso_aivsfov08 ATTRIBUTE(XMLName="aivsfov08",XMLNamespace=""),
  pesosfov08 trecibeSolTraspInfonavit_solicitudtraspaso_pesosfov08 ATTRIBUTE(XMLName="pesosfov08",XMLNamespace=""),
  fechatransferencia trecibeSolTraspInfonavit_solicitudtraspaso_fechatransferencia ATTRIBUTE(XMLName="fechatransferencia",XMLNamespace="")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_folioprocesar
#
TYPE trecibeSolTraspInfonavitResponse_folioprocesar STRING ATTRIBUTE(XSDMaxLength="50",XSTypeName="recibeSolTraspInfonavitResponse_folioprocesar",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_codoper
#
TYPE trecibeSolTraspInfonavitResponse_codoper STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="recibeSolTraspInfonavitResponse_codoper",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_codresp
#
TYPE trecibeSolTraspInfonavitResponse_codresp STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="recibeSolTraspInfonavitResponse_codresp",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_codrespopr
#
TYPE trecibeSolTraspInfonavitResponse_codrespopr STRING ATTRIBUTE(XSDMaxLength="2",XSTypeName="recibeSolTraspInfonavitResponse_codrespopr",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_descresp
#
TYPE trecibeSolTraspInfonavitResponse_descresp STRING ATTRIBUTE(XSDMaxLength="254",XSTypeName="recibeSolTraspInfonavitResponse_descresp",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_rechazo_element_cod_rechazo
#
TYPE trecibeSolTraspInfonavitResponse_rechazo_element_cod_rechazo STRING ATTRIBUTE(XSDMaxLength="6",XSTypeName="recibeSolTraspInfonavitResponse_rechazo_element_cod_rechazo",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_rechazo_element_desc_rechazo
#
TYPE trecibeSolTraspInfonavitResponse_rechazo_element_desc_rechazo STRING ATTRIBUTE(XSDMaxLength="100",XSTypeName="recibeSolTraspInfonavitResponse_rechazo_element_desc_rechazo",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_rechazo
#
TYPE trecibeSolTraspInfonavitResponse_rechazo RECORD ATTRIBUTE(XMLSequence,XSTypeName="recibeSolTraspInfonavitResponse_rechazo",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
  element DYNAMIC ARRAY ATTRIBUTE(XMLList) OF RECORD ATTRIBUTE(XMLSequence,XMLName="element",XMLNamespace="")
      cod_rechazo trecibeSolTraspInfonavitResponse_rechazo_element_cod_rechazo ATTRIBUTE(XMLName="cod_rechazo"),
      desc_rechazo trecibeSolTraspInfonavitResponse_rechazo_element_desc_rechazo ATTRIBUTE(XMLName="desc_rechazo")
    END RECORD
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : trecibeSolTraspInfonavitResponse_codopercliente
#
TYPE trecibeSolTraspInfonavitResponse_codopercliente STRING ATTRIBUTE(XSDMaxLength="50",XSTypeName="recibeSolTraspInfonavitResponse_codopercliente",XSTypeNamespace="http:www.procesar.com/recibe/traspaso")
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: recibeSolicitudTraspaso
# Port:    recibeSolicitudTraspasoPortType
# Server:  http://172.16.16.204:8078/ws/r/recibeSolicitudTraspaso
#-------------------------------------------------------------------------------
#
# Location of the SOAP server.
# You can reassign this value at run-time.
#
DEFINE recibeSolicitudTraspaso_recibeSolicitudTraspasoPortTypeLocation STRING

#-------------------------------------------------------------------------------
#
# Operation: recibeSolTraspInfonavit
#
# FUNCTION: recibeSolTraspInfonavit_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL recibeSolTraspInfonavit
#   OUTPUT: GLOBAL recibeSolTraspInfonavitResponse
#
# FUNCTION: recibeSolTraspInfonavitRequest_g()
#   RETURNING: soapStatus
#   INPUT: GLOBAL recibeSolTraspInfonavit
#
# FUNCTION: recibeSolTraspInfonavitResponse_g()
#   RETURNING: soapStatus
#   OUTPUT: GLOBAL recibeSolTraspInfonavitResponse
#

#-------------------------------------------------------------------------------

# VARIABLE : recibeSolTraspInfonavit
DEFINE recibeSolTraspInfonavit RECORD ATTRIBUTE(XMLSequence,XMLName="recibeSolTraspInfonavit",XMLNamespace="http:www.procesar.com/recibe/traspaso")
         idssn trecibeSolTraspInfonavit_idssn ATTRIBUTE(XMLName="idssn",XMLNamespace=""),
         solicitudtraspaso trecibeSolTraspInfonavit_solicitudtraspaso ATTRIBUTE(XMLName="solicitudtraspaso",XMLNamespace="")
       END RECORD


#-------------------------------------------------------------------------------

# VARIABLE : recibeSolTraspInfonavitResponse
DEFINE recibeSolTraspInfonavitResponse RECORD ATTRIBUTE(XMLSequence,XMLName="recibeSolTraspInfonavitResponse",XMLNamespace="http:www.procesar.com/recibe/traspaso")
         folioprocesar trecibeSolTraspInfonavitResponse_folioprocesar ATTRIBUTE(XMLName="folioprocesar",XMLNamespace=""),
         codoper trecibeSolTraspInfonavitResponse_codoper ATTRIBUTE(XMLName="codoper",XMLNamespace=""),
         codresp trecibeSolTraspInfonavitResponse_codresp ATTRIBUTE(XMLName="codresp",XMLNamespace=""),
         codrespopr trecibeSolTraspInfonavitResponse_codrespopr ATTRIBUTE(XMLName="codrespopr",XMLNamespace=""),
         descresp trecibeSolTraspInfonavitResponse_descresp ATTRIBUTE(XMLName="descresp",XMLNamespace=""),
         rechazo trecibeSolTraspInfonavitResponse_rechazo ATTRIBUTE(XMLName="rechazo",XMLNamespace=""),
         codopercliente trecibeSolTraspInfonavitResponse_codopercliente ATTRIBUTE(XMLName="codopercliente",XMLNamespace=""),
         tiemporesp STRING ATTRIBUTE(XMLName="tiemporesp",XMLNamespace=""),
         fecha DATETIME YEAR TO FRACTION(5) ATTRIBUTE(XMLName="fecha",XMLNamespace="")
       END RECORD



END GLOBALS
