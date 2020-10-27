####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS01.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de Homonimia                  #
#Fecha inicio      =>05 DICIEMBRE 2014                             #
#                                                                  #
####################################################################

GLOBALS

#
# TYPE : tns2ConsultaHomonimiaRequest
#
TYPE tns2ConsultaHomonimiaRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaHomonimiaRequest",XSTypeNamespace="http://vo.safre.efp.com")
  nss             CHAR(11) ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2ConsultaHomonimiaResponse
#
TYPE tns2ConsultaHomonimiaResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaHomonimiaResponse",XSTypeNamespace="http://vo.safre.efp.com")
  nss             CHAR(11) ATTRIBUTE(XMLName="nss"),
  codigoRespuesta CHAR(1)  ATTRIBUTE(XMLName="codigoRespuesta"),
  fechaTramite    CHAR(8)     ATTRIBUTE(XMLName="fechaTramite")
  
END RECORD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : ns1consultarHomonimia
DEFINE ns1consultarHomonimia RECORD ATTRIBUTE(XMLSequence,XMLName="consultarHomonimia",XMLNamespace="http://services.safre.efp.com")
         consultaHomonimiaRequest tns2ConsultaHomonimiaRequest ATTRIBUTE(XMLName="consultaHomonimiaRequest")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : ns1consultarHomonimiaResponse
DEFINE ns1consultarHomonimiaResponse RECORD ATTRIBUTE(XMLSequence,XMLName="consultarHomonimiaResponse",XMLNamespace="http://services.safre.efp.com")
         consultarHomonimiaReturn tns2ConsultaHomonimiaResponse ATTRIBUTE(XMLName="consultarHomonimiaReturn")
       END RECORD


END GLOBALS
