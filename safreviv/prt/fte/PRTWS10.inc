--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/11/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS10.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información                                                   #
#Fecha inicio => 10 Noviembre 2015                                             #
################################################################################

GLOBALS

TYPE tns2MensajeEntradaAclaracion RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaAclaracion",XSTypeNamespace="http://efp.infonavit.portabilidad.aclaracion.diagnostico.com")
        nss    CHAR(11)    ATTRIBUTE(XSDString,XMLName="nss",XSDMaxLength="11")
     END RECORD

TYPE tns2MensajeSalidaAclaracion RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaSolicitud",XSTypeNamespace="http://efp.infonavit.portabilidad.aclaracion.diagnostico.com")
        nss       CHAR(11)    ATTRIBUTE(XSDString,XMLName="nss",XSDMaxLength="11"),
        apPaterno CHAR(40)    ATTRIBUTE(XSDString,XMLName="apPaterno",XSDMaxLength="40",XSDWhiteSpace="replace"),
        apMaterno CHAR(40)    ATTRIBUTE(XSDString,XMLName="apMaterno",XSDMaxLength="40",XSDWhiteSpace="replace"),
        nombre    CHAR(40)    ATTRIBUTE(XSDString,XMLName="nombre",XSDMaxLength="40",XSDWhiteSpace="replace"),
        estatusAclaracion SMALLINT ATTRIBUTE(XSDShort,XMLName="estatusAclaracion"),
        tipo        SMALLINT  ATTRIBUTE(XSDShort,XMLName="tipo"),
        diagnostico CHAR(4)   ATTRIBUTE(XSDString,XMLName="diagnostico",XSDMaxLength="4")
     END RECORD

DEFINE mensajeEntradaAclaracion RECORD ATTRIBUTE(XMLSequence,XMLName="diagnosticaAclaracionPortabilidad",XMLNamespace="http://infonavit.portabilidad.aclaracion.diagnostico.com")
         mensajeEntradaAclaracion tns2MensajeEntradaAclaracion ATTRIBUTE(XMLName="mensajeEntradaAclaracion")
       END RECORD

DEFINE mensajeSalidaAclaracion RECORD ATTRIBUTE(XMLSequence,XMLName="diagnosticaAclaracionPortabilidadResponse",XMLNamespace="http://infonavit.portabilidad.aclaracion.diagnostico.com")
         mensajeSalidaAclaracion tns2MensajeSalidaAclaracion ATTRIBUTE(XMLName="mensajeSalidaAclaracion")
       END RECORD

END GLOBALS