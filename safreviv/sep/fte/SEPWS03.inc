--==============================================================================
################################################################################
#Modulo       => SEP                                                           #
#Programa     => SEPWS03.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información                                                   #
#Fecha inicio => 28 de Junio de 2016                                           #
################################################################################

GLOBALS

TYPE tns2MensajeEntradaConsulta RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaConsulta",XSTypeNamespace="http://infonavit.separacion.consultamarca.com")
        nss    CHAR(11)    ATTRIBUTE(XSDString,XMLName="nss",XSDMaxLength="11")
     END RECORD

TYPE tns2MensajeSalidaConsulta RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaConsulta",XSTypeNamespace="http://infonavit.separacion.consultamarca.com")
        nss                CHAR(11)    ATTRIBUTE(XSDString,XMLName="nss",XSDMaxLength="11"),
        diagMarcaActiva    CHAR(1)     ATTRIBUTE(XSDString,XMLName="diagMarcaActiva",XSDMaxLength="1"),
        codAforeActiva     CHAR(3)     ATTRIBUTE(XSDString,XMLName="codAforeActiva",XSDMaxLength="3"),
        descAforeActiva    CHAR(40)    ATTRIBUTE(XSDString,XMLName="descAforeActiva",XSDMaxLength="40",XSDWhiteSpace="replace"),
        diagMarcaHistorica CHAR(1)     ATTRIBUTE(XSDString,XMLName="diagMarcaHistorica",XSDMaxLength="1"),
        codAforeHistorica  CHAR(3)     ATTRIBUTE(XSDString,XMLName="codAforeHistorica",XSDMaxLength="3"),
        descAforeHistorica CHAR(40)    ATTRIBUTE(XSDString,XMLName="descAforeHistorica",XSDMaxLength="40",XSDWhiteSpace="replace"),
        resultadoOperacion SMALLINT    ATTRIBUTE(XSDShort,XMLName="resultadoOperacion")
     END RECORD

DEFINE mensajeEntradaConsulta RECORD ATTRIBUTE(XMLSequence,XMLName="consultaMarcaSeparacion",XMLNamespace="http://infonavit.separacion.consultamarca.com")
         entradaConsulta tns2MensajeEntradaConsulta ATTRIBUTE(XMLName="entradaConsulta")
       END RECORD

DEFINE mensajeSalidaConsulta RECORD ATTRIBUTE(XMLSequence,XMLName="consultaMarcaSeparacionResponse",XMLNamespace="http://infonavit.separacion.consultamarca.com")
         salidaConsulta tns2MensajeSalidaConsulta ATTRIBUTE(XMLName="salidaConsulta")
       END RECORD

END GLOBALS