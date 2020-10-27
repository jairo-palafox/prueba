####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS03.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de solicitud de marca         #
#Fecha inicio      =>04 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns1MarcaRequest: Parametros de entrada
   TYPE tns1MarcaRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="MarcaRequest",XSTypeNamespace="http://vo.safre.efp.com")
     nss             CHAR(11) ATTRIBUTE(XMLName="nss"),
     numeroCredito   CHAR(10) ATTRIBUTE(XMLName="numeroCredito"),
     fechaVigencia   STRING     ATTRIBUTE(XMLName="fechaVigencia")
   END RECORD

   # TYPE : tns1MarcaResponse: Parametros de salida
   TYPE tns1MarcaResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="MarcaResponse",XSTypeNamespace="http://vo.safre.efp.com")
     nss                CHAR(11)       ATTRIBUTE(XMLName="nss"),
     diasReintento      INTEGER        ATTRIBUTE(XMLName="diasReintento"),
     codigoRespuesta    CHAR(4)        ATTRIBUTE(XMLName="codigoRespuesta"),
     descripcion        VARCHAR(140)   ATTRIBUTE(XMLName="descripcion")
   END RECORD

   # VARIABLE : ns2request
   DEFINE ns2request tns1MarcaRequest ATTRIBUTE(XMLName="solicitaMarcaRequest",XMLNamespace="http://services.safre.efp.com")

   # VARIABLE : ns2solicitaMarcaReturn
   DEFINE ns2solicitaMarcaReturn tns1MarcaResponse ATTRIBUTE(XMLName="solicitaMarcaReturn",XMLNamespace="http://services.safre.efp.com")
   
END GLOBALS