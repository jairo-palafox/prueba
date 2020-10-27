####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS01.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de consulta de marca          #
#Fecha inicio      =>04 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns1Request: Parametros de entrada
   TYPE tns1Request RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaRequest",XSTypeNamespace="http://vo.safre.efp.com")
     nss CHAR(11) ATTRIBUTE(XMLName="nss")
   END RECORD

   # TYPE : tns1Response: Parametros de salida
   TYPE tns1Response RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaResponse",XSTypeNamespace="http://vo.safre.efp.com")
     codigoRespuesta CHAR(4)        ATTRIBUTE(XMLName="codigoRespuesta"),
     descripcion     VARCHAR(140)   ATTRIBUTE(XMLName="descripcion"),
     nss             CHAR(11)       ATTRIBUTE(XMLName="nss"),
     saldoVivienda92 DECIMAL(12,2)  ATTRIBUTE(XMLName="saldoVivienda92"),
     saldoVivienda97 DECIMAL(12,2)  ATTRIBUTE(XMLName="saldoVivienda97")
   END RECORD

   # VARIABLE : ns2request
   DEFINE ns2request tns1Request ATTRIBUTE(XMLName="consultaMarcaRequest",XMLNamespace="http://services.safre.efp.com")

   # VARIABLE : ns2consultaMarcaReturn
   DEFINE ns2consultaMarcaReturn tns1Response ATTRIBUTE(XMLName="consultaMarcaReturn",XMLNamespace="http://services.safre.efp.com")

END GLOBALS