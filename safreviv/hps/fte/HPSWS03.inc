--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 10/08/2015
--==============================================================================

################################################################################
#Modulo       => HPS                                                           #
#Programa     => HPSWS01.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información                                                   #
#Fecha inicio => 10 Agosto 2015                                                #
################################################################################

GLOBALS
DEFINE g_usuario_cod CHAR(20)

#
# TYPE : tns2MensajeEntradaSolicitud
#
TYPE tns2MensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaSolicitud",XSTypeNamespace="http://ws.infonavit.hipoteca.cancela.com")
   nss               CHAR(11) ATTRIBUTE(XMLName="nss"),
   numeroCaso        DECIMAL(10,0) ATTRIBUTE(XMLName="numeroCaso"),
   idEstatus         CHAR(5) ATTRIBUTE(XMLName="idEstatus"),
   correoElectronico CHAR(120) ATTRIBUTE(XMLName="correoElectronico"),
   telCelular        CHAR(10) ATTRIBUTE(XMLName="telCelular")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2MensajeSalidaSolicitud
#
TYPE tns2MensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaSolicitud",XSTypeNamespace="http://ws.infonavit.hipoteca.cancela.com")
   numeroCaso  DECIMAL(10,0) ATTRIBUTE(XMLName="numeroCaso"), 
   nss         CHAR(11) ATTRIBUTE(XMLName="nss"),
   apPaterno   CHAR(40) ATTRIBUTE(XMLName="apPaterno"),
   apMaterno   CHAR(40) ATTRIBUTE(XMLName="apMaterno"),
   nombre      CHAR(40) ATTRIBUTE(XMLName="nombre"),
   idEstatus   CHAR(5) ATTRIBUTE(XMLName="idEstatus"),
   diagnostico CHAR(4) ATTRIBUTE(XMLName="diagnostico")
END RECORD
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns1solicitaCancelacionPagos
DEFINE mensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="solicitaCancelacionPagos",XMLNamespace="http://infonavit.hipoteca.cancela.com")
         mensajeEntradaSolicitud tns2MensajeEntradaSolicitud ATTRIBUTE(XMLName="mensajeEntradaSolicitud")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : solicitaCancelacionPagosResponse
DEFINE mensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="solicitaCancelacionPagosResponse",XMLNamespace="http://infonavit.hipoteca.cancela.com")
         mensajeSalidaSolicitudReturn tns2MensajeSalidaSolicitud ATTRIBUTE(XMLName="mensajeSalidaSolicitudReturn")
       END RECORD

END GLOBALS
