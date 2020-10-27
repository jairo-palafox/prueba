####################################################################
#Modulo            =>AGR                                           #
#Programa          =>AGRWS05.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de credito ejercido           #
#Fecha inicio      =>18 NOVIEMBRE 2014                             #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns2CreditoEjercidoRequest
   TYPE tns2CreditoEjercidoRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="CreditoEjercidoRequest",XSTypeNamespace="http://vo.safre.efp.com")
      nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
      tipoCredito       CHAR(3)        ATTRIBUTE(XMLName="tipoCredito"),
      numeroCredito     CHAR(10)       ATTRIBUTE(XMLName="numeroCredito"),
      fechaOtorgamiento STRING           ATTRIBUTE(XMLName="fechaOtorgamiento"),
      tipoDescuento     INTEGER        ATTRIBUTE(XMLName="tipoDescuento"),
      valorDescuento    DECIMAL(32)    ATTRIBUTE(XMLName="valorDescuento"),
      montoLiquida      DECIMAL(15,2)  ATTRIBUTE(XMLName="montoLiquida"),
      fechaProceso      STRING           ATTRIBUTE(XMLName="fechaProceso"),
      nrp               CHAR(11)       ATTRIBUTE(XMLName="nrp"),
      tipoOperacion     CHAR(3)        ATTRIBUTE(XMLName="tipoOperacion")
   END RECORD

   # TYPE : tns2CreditoEjercidoResponse
   TYPE tns2CreditoEjercidoResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="CreditoEjercidoResponse",XSTypeNamespace="http://vo.safre.efp.com")
      codigoRespuesta   CHAR(4)        ATTRIBUTE(XMLName="codigoRespuesta"),
      descripcion       VARCHAR(140)   ATTRIBUTE(XMLName="descripcion"),
      nss               CHAR(11)       ATTRIBUTE(XMLName="nss"),
      tipoCredito       CHAR(3)        ATTRIBUTE(XMLName="tipoCredito"),
      numeroCredito     CHAR(10)       ATTRIBUTE(XMLName="numeroCredito")
   END RECORD

   # VARIABLE : ns1informarCreditoEjercido
   DEFINE ns1informarCreditoEjercido RECORD ATTRIBUTE(XMLSequence,XMLName="informarCreditoEjercidoIn",XMLNamespace="http://services.safre.efp.com")
      request tns2CreditoEjercidoRequest ATTRIBUTE(XMLName="informarCreditoEjercidoRequest")
   END RECORD

   # VARIABLE : ns1informarCreditoEjercidoResponse
   DEFINE ns1informarCreditoEjercidoResponse RECORD ATTRIBUTE(XMLSequence,XMLName="informarCreditoEjercidoResponse",XMLNamespace="http://services.safre.efp.com")
      informarCreditoEjercidoReturn tns2CreditoEjercidoResponse ATTRIBUTE(XMLName="informarCreditoEjercidoReturn")
   END RECORD


END GLOBALS
