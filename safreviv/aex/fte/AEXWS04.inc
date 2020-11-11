

GLOBALS



#
# TYPE : tInsertaIndicadoresRequest_InsertaIndicadoresRequest
#
TYPE tInsertaIndicadoresRequest_InsertaIndicadoresRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="InsertaIndicadoresRequest_InsertaIndicadoresRequest",XSTypeNamespace="http://www.infonavit.org.mx/")
  nss STRING ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tInsertaIndicadoresResponse_InsertaIndicadoresResponse
#
TYPE tInsertaIndicadoresResponse_InsertaIndicadoresResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="InsertaIndicadoresResponse_InsertaIndicadoresResponse",XSTypeNamespace="http://www.infonavit.org.mx/")
  nss STRING ATTRIBUTE(XMLName="nss"),
  resul_op STRING ATTRIBUTE(XSDMaxLength="2",XMLName="resul_op")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------

# VARIABLE : InsertaIndicadoresRequest
DEFINE InsertaIndicadoresRequest tInsertaIndicadoresRequest_InsertaIndicadoresRequest ATTRIBUTE(XMLName="InsertaIndicadoresRequest",XMLNamespace="http://www.infonavit.org.mx/")

#-------------------------------------------------------------------------------

# VARIABLE : InsertaIndicadoresResponse
DEFINE InsertaIndicadoresResponse tInsertaIndicadoresResponse_InsertaIndicadoresResponse ATTRIBUTE(XMLName="InsertaIndicadoresResponse",XMLNamespace="http://www.infonavit.org.mx/")


END GLOBALS
