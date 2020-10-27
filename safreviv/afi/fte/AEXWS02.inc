

GLOBALS

TYPE tConsultaSegmentosRequest_ConsultaSegmentosRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaSegmentosRequest_ConsultaSegmentosRequest",XSTypeNamespace="http://www.infonavit.org.mx/")
  fecha STRING ATTRIBUTE(XMLName="fecha")
END RECORD

TYPE tConsultaSegmentosResponse_ConsultaSegmentosResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaSegmentosResponse_ConsultaSegmentosResponse",XSTypeNamespace="http://www.infonavit.org.mx/")
  element DYNAMIC ARRAY ATTRIBUTE(XMLList) OF RECORD ATTRIBUTE(XMLSequence,XMLName="element")
      cod_segmento SMALLINT ATTRIBUTE(XSDshort,XMLName="cod_segmento"),
      des_corta STRING ATTRIBUTE(XSDMaxLength="30",XMLName="des_corta")
    END RECORD
END RECORD
# VARIABLE : ConsultaSegmentosRequest
DEFINE ConsultaSegmentosRequest tConsultaSegmentosRequest_ConsultaSegmentosRequest ATTRIBUTE(XMLName="ConsultaSegmentosRequest",XMLNamespace="http://www.infonavit.org.mx/")


#-------------------------------------------------------------------------------

# VARIABLE : ConsultaSegmentosResponse
DEFINE ConsultaSegmentosResponse tConsultaSegmentosResponse_ConsultaSegmentosResponse ATTRIBUTE(XMLName="ConsultaSegmentosResponse",XMLNamespace="http://www.infonavit.org.mx/")



END GLOBALS
