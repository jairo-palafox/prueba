--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS01.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información                                                   #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################

GLOBALS
DEFINE g_usuario_cod CHAR(20)

#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

#
# TYPE : tns2MensajeEntradaSolicitud
#
TYPE tns2MensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaSolicitud",XSTypeNamespace="http://vo.fovissste.credito.solicita.com.portabilidad")
   nss           CHAR(11) ATTRIBUTE(XMLName="nss"),
   curp          CHAR(18) ATTRIBUTE(XMLName="curp"),
   numeroCredito VARCHAR(10) ATTRIBUTE(XMLName="numeroCredito"),
   numeroCaso    DECIMAL(10,0) ATTRIBUTE(XMLName="numeroCaso"),
   idEstatus     CHAR(5) ATTRIBUTE(XMLName="idEstatus"),
   correoElectronico CHAR(120) ATTRIBUTE(XMLName="correoElectronico"),
   telCelular        CHAR(10) ATTRIBUTE(XMLName="telCelular")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2MensajeSalidaSolicitud
#
TYPE tns2MensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaSolicitud",XSTypeNamespace="http://vo.fovissste.credito.solicita.com.portabilidad")
   numeroCaso DECIMAL(10,0) ATTRIBUTE(XMLName="numeroCaso"), 
   nss        CHAR(11) ATTRIBUTE(XMLName="nss"),
   apPaterno  CHAR(40) ATTRIBUTE(XMLName="apPaterno"),
   apMaterno  CHAR(40) ATTRIBUTE(XMLName="apMaterno"),
   nombre     CHAR(40) ATTRIBUTE(XMLName="nombre"),
   tipoPortabilidad SMALLINT ATTRIBUTE(XMLName="tipoPortabilidad"),
   --resultadoOperacion CHAR(5) ATTRIBUTE(XMLName="resultadoOperacion"),
   idEstatus        CHAR(5) ATTRIBUTE(XMLName="idEstatus"),
   --idMotivo           CHAR(4) ATTRIBUTE(XMLName="idMotivo")
   diagnostico      CHAR(4) ATTRIBUTE(XMLName="diagnostico")
END RECORD
#-------------------------------------------------------------------------------



#-------------------------------------------------------------------------------
# Service: SolicitudCreditoFovisssteService
# Port:    SolicitudCreditoFovissste
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

# VARIABLE : ns1solicitaCreditoFovissste
DEFINE mensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="solicitaCreditoFovissste",XMLNamespace="http://fovissste.credito.solicita.com.portabilidad")
         mensajeEntradaSolicitud tns2MensajeEntradaSolicitud ATTRIBUTE(XMLName="mensajeEntradaSolicitud")
       END RECORD

#-------------------------------------------------------------------------------

# VARIABLE : ns1solicitaCreditoFovisssteResponse
DEFINE mensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="solicitaCreditoFovisssteResponse",XMLNamespace="http://fovissste.credito.solicita.com.portabilidad")
         solicitaCreditoFovisssteReturn tns2MensajeSalidaSolicitud ATTRIBUTE(XMLName="solicitaCreditoFovisssteReturn")
       END RECORD


       # Respuesta de WS de cartera
DEFINE r_respuesta_cartera RECORD
          v_nss               CHAR(11),
          v_num_credito       VARCHAR(10),--DECIMAL(10,0),
          v_diag_cartera      CHAR(2),
          v_tpo_credito       CHAR(2),
          v_saldo_credito     DECIMAL(10,2),
          v_monto_originacion DECIMAL(10,2),
          v_f_originacion     CHAR(8),
          v_causa_diag        CHAR(2)
       END RECORD,
       # Consulta a fovissste
       v_mensajeEntradaParaFovissste RECORD
          nss                     CHAR(11),
          curp                    CHAR(18),
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          nombre                  VARCHAR(40),
          apPaterno               VARCHAR(40),
          apMaterno               VARCHAR(40)          
       END RECORD,
       r_respuesta_fovissste RECORD
          nss                     CHAR(11),
          curp                    CHAR(18),
          fechaOtorgamientoCad    CHAR(8),
          fechaOtorgamiento       DATE,
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          folioRespuesta          VARCHAR(10),
          saldoInsolutoCredito    DECIMAL(13,2),
          tpoCredito              SMALLINT,
          diagnostico             SMALLINT,
          idMotivo                CHAR(5)
       END RECORD
END GLOBALS
