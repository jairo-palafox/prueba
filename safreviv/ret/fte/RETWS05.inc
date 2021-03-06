#-------------------------------------------------------------------------------
# File: RETWS05Service.inc
# GENERATED BY fglwsdl 101601
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------



GLOBALS


#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# TYPE : tns1ComprobanteRequest
#
TYPE tns1ComprobanteRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ComprobanteRequest",XSTypeNamespace="http://vo.safre.efp.com")
  nss                CHAR (11) ATTRIBUTE(XMLName="nss"),
  tipoPersona        CHAR (1) ATTRIBUTE(XMLName="tipoPersona"),
  nombre             CHAR (100) ATTRIBUTE(XMLName="nombre"),
  paterno            CHAR (100) ATTRIBUTE(XMLName="paterno"),
  materno            CHAR (100) ATTRIBUTE(XMLName="materno"),
  calle              CHAR (50) ATTRIBUTE(XMLName="calle"),
  numExterior        CHAR (50) ATTRIBUTE(XMLName="numExterior"),
  numInterior        CHAR (50) ATTRIBUTE(XMLName="numInterior"),
  colonia            CHAR (50) ATTRIBUTE(XMLName="colonia"),
  estado             CHAR (10) ATTRIBUTE(XMLName="estado"),
  municipio          CHAR (10) ATTRIBUTE(XMLName="municipio"),
  codigoPostal       CHAR (5) ATTRIBUTE(XMLName="codigoPostal"),
  lada               CHAR (3) ATTRIBUTE(XMLName="lada"),
  telefono           CHAR (8) ATTRIBUTE(XMLName="telefono"),
  celular            CHAR (10) ATTRIBUTE(XMLName="celular"),
  correoElectronico  CHAR (70) ATTRIBUTE(XMLName="correoElectronico")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1ComprobanteResponse
#
TYPE tns1ComprobanteResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ComprobanteResponse",XSTypeNamespace="http://vo.safre.efp.com")
  nss                CHAR (11)      ATTRIBUTE(XMLName="nss"),
  ctaCLABE           CHAR (18)      ATTRIBUTE(XMLName="ctaCLABE"),
  fPago              DATE           ATTRIBUTE(XMLName="fPago"),
  montoPago          DECIMAL(14,2)  ATTRIBUTE(XMLName="montoPago"),
  estatusPago        INTEGER        ATTRIBUTE(XMLName="estatusPago"),
  codRechazo         INTEGER        ATTRIBUTE(XMLName="codRechazo")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: ComprobantePagoService
# Port:    ComprobantePago
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns2request
DEFINE ns2request tns1ComprobanteRequest ATTRIBUTE(XMLName="request",XMLNamespace="http://services.safre.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2comprobantePagoReturn
DEFINE ns2comprobantePagoReturn tns1ComprobanteResponse ATTRIBUTE(XMLName="comprobantePagoReturn",XMLNamespace="http://services.safre.efp.com")


END GLOBALS
