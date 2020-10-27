--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 20/11/2015
--==============================================================================
################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS12.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información                                                   #
#Fecha inicio => 20 Noviembre 2015                                             #
################################################################################

GLOBALS

TYPE tns2MensajeEntradaCancelacion RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaCancelacion",XSTypeNamespace="http://infonavit.portabilidad.cancelacion.com")
        noCaso    CHAR(10)    ATTRIBUTE(XSDString,XMLName="noCaso",XSDMaxLength="10")
     END RECORD

TYPE tns2MensajeSalidaCancelacion RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaCancelacion",XSTypeNamespace="http://infonavit.portabilidad.cancelacion.com")
        nss       CHAR(11)    ATTRIBUTE(XSDString,XMLName="nss",XSDMaxLength="11"),
        noCaso    CHAR(10)    ATTRIBUTE(XSDString,XMLName="noCaso",XSDMaxLength="10"),
        apPaterno CHAR(40)    ATTRIBUTE(XSDString,XMLName="apPaterno",XSDMaxLength="40",XSDWhiteSpace="replace"),
        apMaterno CHAR(40)    ATTRIBUTE(XSDString,XMLName="apMaterno",XSDMaxLength="40",XSDWhiteSpace="replace"),
        nombre    CHAR(40)    ATTRIBUTE(XSDString,XMLName="nombre",XSDMaxLength="40",XSDWhiteSpace="replace"),
        estatusAclaracion SMALLINT ATTRIBUTE(XSDShort,XMLName="estatusAclaracion"),
        diagnostico SMALLINT   ATTRIBUTE(XSDShort,XMLName="diagnostico")
     END RECORD

DEFINE mensajeEntradaCancelacion RECORD ATTRIBUTE(XMLSequence,XMLName="cancelacionSolicitudPortabilidad",XMLNamespace="http://infonavit.portabilidad.cancelacion.com")
         mensajeEntradaCancelacion tns2MensajeEntradaCancelacion ATTRIBUTE(XMLName="mensajeEntradaCancelacion")
       END RECORD

DEFINE mensajeSalidaCancelacion RECORD ATTRIBUTE(XMLSequence,XMLName="solicitudCancelacionPortabilidadResponse",XMLNamespace="http://infonavit.portabilidad.cancelacion.com")
         mensajeSalidaCancelacion tns2MensajeSalidaCancelacion ATTRIBUTE(XMLName="mensajeSalidaCancelacion")
       END RECORD

END GLOBALS