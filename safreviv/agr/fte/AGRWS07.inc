####################################################################
#Modulo            =>AFI                                           #
#Programa          =>AGRWS07.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de solicitud saldo cambiavit  #
#Fecha inicio      =>18 OCTUBRE 2018                               #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns1Request: Parametros de entrada
   TYPE tns1Request RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaRequest",XSTypeNamespace="http://vo.safre.efp.com")
      nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
      numeroCredito     CHAR(10)       ATTRIBUTE(XMLName="numeroCredito"),
      tipoSolicitud     SMALLINT       ATTRIBUTE(XMLName="tipoSolicitud"),
      numeroCaso        CHAR(10)       ATTRIBUTE(XMLName="numeroCaso")
   END RECORD

   # TYPE : tns1Response: Parametros de salida
   TYPE tns1Response RECORD ATTRIBUTE(XMLSequence,XSTypeName="consultaResponse",XSTypeNamespace="http://vo.safre.efp.com")
      resultOperacion   CHAR(2)        ATTRIBUTE(XMLName="resultOperacion"),
      sdoGarantia       DECIMAL(10,2)  ATTRIBUTE(XMLName="sdoGarantia"),
      motivo            CHAR(2)        ATTRIBUTE(XMLName="motivo")
   END RECORD

   # VARIABLE : ns2request
   DEFINE ns2request tns1Request ATTRIBUTE(XMLName="consultaSaldoCambiavitRequest",XMLNamespace="http://services.safre.efp.com")

   # VARIABLE : ns2SaldoCambiavitReturn
   DEFINE ns2Return tns1Response ATTRIBUTE(XMLName="consultaSaldoCambiavitReturn",XMLNamespace="http://services.safre.efp.com")

END GLOBALS
