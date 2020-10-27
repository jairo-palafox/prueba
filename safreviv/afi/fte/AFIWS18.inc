####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AFIWS18.inc                                   #
#Objetivo          =>Programa que contiene la definición del WSDL  #
#                    del webServices de causa raiz                 #
#Fecha inicio      =>19 de julio de 2018                           #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns1Request: Parametros de entrada
   TYPE tns1Request RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaRequest",XSTypeNamespace="http://vo.safre.efp.com")
     nss CHAR(11) ATTRIBUTE(XMLName="nss")
   END RECORD

   # TYPE : tns1Response: Parametros de salida
   TYPE tns1Response RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaResponse",XSTypeNamespace="http://vo.safre.efp.com")
     nss          CHAR(11)       ATTRIBUTE(XMLName="nss"),
     rfc          CHAR(13)       ATTRIBUTE(XMLName="rfc"),
     curp         CHAR(18)       ATTRIBUTE(XMLName="curp"),
     ap_paterno   CHAR(40)       ATTRIBUTE(XMLName="ap_paterno"),
     ap_materno   CHAR(40)       ATTRIBUTE(XMLName="ap_materno"),
     nombre       CHAR(40)       ATTRIBUTE(XMLName="nombre")
   END RECORD

   # VARIABLE : ns2request
   DEFINE ns2request tns1Request ATTRIBUTE(XMLName="consultaCausaRaizRequest",XMLNamespace="http://services.safre.efp.com")

   # VARIABLE : ns2consultaCausaRaizReturn
   DEFINE ns2consultaCausaRaizReturn tns1Response ATTRIBUTE(XMLName="consultaCausaRaizReturn",XMLNamespace="http://services.safre.efp.com")

END GLOBALS
