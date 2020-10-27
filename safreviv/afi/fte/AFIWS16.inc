####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS16.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de consulta de fallecido      #
#Fecha inicio      =>16 de mayo de 2018                            #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns1Request: Parametros de entrada
   TYPE tns1Request RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaRequest",XSTypeNamespace="http://vo.safre.efp.com")
     nss CHAR(11) ATTRIBUTE(XMLName="nss")
   END RECORD

   # TYPE : tns1Response: Parametros de salida
   TYPE tns1Response RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaResponse",XSTypeNamespace="http://vo.safre.efp.com")
     nss             CHAR(11)       ATTRIBUTE(XMLName="nss"),
     codigoRespuesta CHAR(2)        ATTRIBUTE(XMLName="codigoRespuesta"),
     descRespuesta   CHAR(25)       ATTRIBUTE(XMLName="descRespuesta")
   END RECORD

   # VARIABLE : ns2request
   DEFINE ns2request tns1Request ATTRIBUTE(XMLName="consultaFallecidoRequest",XMLNamespace="http://services.safre.efp.com")

   # VARIABLE : ns2consultaFallecidoReturn
   DEFINE ns2consultaFallecidoReturn tns1Response ATTRIBUTE(XMLName="consultaFallecidoReturn",XMLNamespace="http://services.safre.efp.com")

END GLOBALS
