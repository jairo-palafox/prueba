--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 09/02/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS06.inc                                                   #
#Objetivo     => Archivo de variables que utiliza el WS para enviar y recibir  #
#                información en envío de crédito infonavit receptora           #
#Fecha inicio => 09 Febrero 2015                                               #
################################################################################

GLOBALS
DEFINE g_usuario_cod CHAR(20)

# Mensaje entrada solicitud, solicita información de crédito
TYPE tns2MensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeEntradaSolicitud",XSTypeNamespace="http://vo.infonavit.portabilidad.credito.envia")
   nss             CHAR(11)    ATTRIBUTE(XMLName="nss",XMLOptional),
   curp            CHAR(18)    ATTRIBUTE(XMLName="curp",XMLOptional),
   numeroDeCredito VARCHAR(10) ATTRIBUTE(XMLName="numeroDeCredito",XMLOptional),
   folioConsulta   VARCHAR(10) ATTRIBUTE(XMLName="folioConsulta",XMLOptional)
END RECORD

# Mensaje de salida, envía respuesta crédito
TYPE tns2MensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XSTypeName="MensajeSalidaSolicitud",XSTypeNamespace="http://vo.infonavit.portabilidad.credito.envia")
   nss                   CHAR(11)      ATTRIBUTE(XMLName="nss",XMLOptional),   
   curp                  CHAR(18)      ATTRIBUTE(XMLName="curp",XMLOptional),
   fechaOtorgamiento     CHAR(8)       ATTRIBUTE(XMLName="fechaOtorgamiento",XMLOptional),
   numeroDeCredito       VARCHAR(10)   ATTRIBUTE(XMLName="numeroDeCredito",XMLOptional),   
   folioConsulta         VARCHAR(10)   ATTRIBUTE(XMLName="folioConsulta",XMLOptional),
   folioRespuesta        VARCHAR(10)   ATTRIBUTE(XMLName="folioRespuesta",XMLOptional),
   saldoInsoluto         DECIMAL(13,2) ATTRIBUTE(XMLName="saldoInsoluto",XMLOptional),   
   indicadorTipoCredito  SMALLINT      ATTRIBUTE(XMLName="indicadorTipoCredito",XMLOptional),   
   diagnosticoInstitutos CHAR(1)       ATTRIBUTE(XMLName="diagnosticoInstitutos",XMLOptional),
   motivoRechazo         CHAR(5)       ATTRIBUTE(XMLName="motivoRechazo",XMLOptional)   
END RECORD

--numeroCaso            DECIMAL(10,0) ATTRIBUTE(XMLName="numeroCaso"), 
--idCredito             VARCHAR(10)   ATTRIBUTE(XMLName="idCredito"),
--tpoCredito            CHAR(1)       ATTRIBUTE(XMLName="tpoCredito"),
--diagnostico           CHAR(1)       ATTRIBUTE(XMLName="diagnostico"),
--idMotivo              CHAR(2)       ATTRIBUTE(XMLName="idMotivo")
--descripcion          VARCHAR(100)  ATTRIBUTE(XMLName="descripcion")
DEFINE mensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="enviaCreditoInfonavit",XMLNamespace="http://infonavit.portabilidad.credito.envia")
         mensajeEntradaSolicitud tns2MensajeEntradaSolicitud ATTRIBUTE(XMLName="mensajeEntradaSolicitud")
       END RECORD

DEFINE mensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="enviaCreditoInfonavitResponse",XMLNamespace="http://infonavit.portabilidad.credito.envia")
         enviaCreditoInfonavitReturn tns2MensajeSalidaSolicitud ATTRIBUTE(XMLName="enviaCreditoInfonavitReturn")
       END RECORD

DEFINE r_respuesta_cartera RECORD
          v_nss               CHAR(11),
          v_num_credito       DECIMAL(10,0),
          v_diag_cartera      CHAR(2),
          v_tpo_credito       SMALLINT,
          v_saldo_credito     DECIMAL(13,2),
          v_monto_originacion DECIMAL(13,2),
          v_f_originacion     CHAR(8),
          v_causa_diag        CHAR(2)
       END RECORD
DEFINE v_derechohabiente RECORD
          v_id_derechohabiente DECIMAL(9,0),
          v_nombre             VARCHAR(40),
          v_apPaterno          VARCHAR(40),
          v_apMaterno          VARCHAR(40)
       END RECORD

END GLOBALS