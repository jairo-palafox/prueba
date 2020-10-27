####################################################################
#Modulo            =>HPS                                           #
#Programa          =>HPSWS01.inc                                   #
#Objetivo          =>Programa que contiene la definicion del WSDL  #
#                    del webServices de credito ejercido incluyendo#
#                    los mandatos de servicio                      #
#Fecha inicio      =>23 FEB 2015                                   #
#                                                                  #
####################################################################

GLOBALS

   # TYPE : tns2CreditoEjercidoRequest
   TYPE tns2CreditoEjercidoRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="CreditoEjercidoRequest",XSTypeNamespace="http://vo.safre.efp.com")
      nss                             CHAR(11)       ATTRIBUTE(XMLName="nss"),
      tipoCredito                     CHAR(3)        ATTRIBUTE(XMLName="tipoCredito"),
      numeroCredito                   CHAR(10)       ATTRIBUTE(XMLName="numeroCredito"),
      fechaOtorgamiento               DATE           ATTRIBUTE(XMLName="fechaOtorgamiento"),
      tipoDescuento                   INTEGER        ATTRIBUTE(XMLName="tipoDescuento"),
      valorDescuento                  DECIMAL(32)    ATTRIBUTE(XMLName="valorDescuento"),
      montoLiquida                    DECIMAL(15,2)  ATTRIBUTE(XMLName="montoLiquida"),
      fechaProceso                    DATE        ATTRIBUTE(XMLName="fechaProceso"),
      nrp                             CHAR(11)       ATTRIBUTE(XMLName="nrp"),
      tipoOperacion                   CHAR(3)        ATTRIBUTE(XMLName="tipoOperacion"),
      identificadorAltaMandato        INTEGER        ATTRIBUTE(XMLName="identificadorAltaMandato"),
      identificadorAltaPredial        CHAR(18)       ATTRIBUTE(XMLName="identificadorAltaPredial"),
      fechaInicioPredial              DATE        ATTRIBUTE(XMLName="fechaInicioPredial"),
      fechaFinPredial                 DATE        ATTRIBUTE(XMLName="fechaFinPredial"),
      montoFondoPredial               DECIMAL(15,2)  ATTRIBUTE(XMLName="montoFondoPredial"),
      fechaPrimerPagoPredial          DATE           ATTRIBUTE(XMLName="fechaPrimePagoPredial"),
      montoPrimerPagoPredial          DECIMAL(15,2)  ATTRIBUTE(XMLName="montoPrimerPagoPredial"),
      referenciaPagoPredial           CHAR(40)       ATTRIBUTE(XMLName="referenciaPagoPredial"),
      identificadorAltaConservacion   CHAR(18)       ATTRIBUTE(XMLName="identificadorAltaConservacion"),
      fechaInicioConservacion         DATE           ATTRIBUTE(XMLName="fechaInicioConservacion"),
      fechaFinConservacion            DATE           ATTRIBUTE(XMLName="fechaFinConservacion"),
      montoFondoConservacion          DECIMAL(15,2)  ATTRIBUTE(XMLName="montoFondoConservacion"),
      fechaPrimerPagoConservacion     DATE           ATTRIBUTE(XMLName="fechaPrimePagoConservacion"),
      montoPrimerPagoConservacion     DECIMAL(15,2)  ATTRIBUTE(XMLName="montoPrimerPagoConservacion"),
      referenciaPagoConservacion      CHAR(40)       ATTRIBUTE(XMLName="referenciaPagoConservacion")
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
