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
       # Datos de entrada y salida de comunicación con portal 

TYPE tenss                CHAR(11)     ATTRIBUTE(XSDMaxLength="11",XSTypeName="mensajeEntradaSolicitud_nss",              XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tecurp               CHAR(18)     ATTRIBUTE(XSDMaxLength="18",XSTypeName="mensajeEntradaSolicitud_curp",             XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tenumeroCredito      VARCHAR(10)  ATTRIBUTE(XSDMaxLength="10",XSTypeName="mensajeEntradaSolicitud_numeroCredito",    XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tenumeroCaso  DECIMAL(10,0)       ATTRIBUTE(XSDTotalDigits="10",XSDFractionDigits="0",XSTypeName="mensajeEntradaSolicitud_numeroCaso",XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE teidEstatus          CHAR(5)      ATTRIBUTE(XSDMaxLength="5",XSTypeName="mensajeEntradaSolicitud_idEstatus",        XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tencorreoElectronico VARCHAR(120) ATTRIBUTE(XSDMaxLength="120",XSTypeName="mensajeEntradaSolicitud_correoElectronico",XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tetelCelular         CHAR(10)     ATTRIBUTE(XSDMaxLength="10",XSTypeName="mensajeEntradaSolicitud_telCelular",       XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")

TYPE tsnumeroCaso         DECIMAL(10,0) ATTRIBUTE(XSDTotalDigits="10",XSDFractionDigits="0",XSTypeName="mensajeSalidaSolicitud_numeroCaso",XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsnss                CHAR(11)    ATTRIBUTE(XSDMaxLength="11",XSTypeName="mensajeSalidaSolicitud_nss",               XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsapPaterno          VARCHAR(40) ATTRIBUTE(XSDMaxLength="40",XSTypeName="mensajeSalidaSolicitud_apPaterno",         XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsapMaterno          VARCHAR(40) ATTRIBUTE(XSDMaxLength="40",XSTypeName="mensajeSalidaSolicitud_apMaterno",         XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsnombre             VARCHAR(40) ATTRIBUTE(XSDMaxLength="40",XSTypeName="mensajeSalidaSolicitud_nombre",            XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsresultadoOperacion CHAR(5)     ATTRIBUTE(XSDMaxLength="5", XSTypeName="mensajeSalidaSolicitud_resultadoOperacion",XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsidMotivo           CHAR(4)     ATTRIBUTE(XSDMaxLength="4", XSTypeName="mensajeSalidaSolicitud_idMotivo",          XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
TYPE tsdescripcion        VARCHAR(50) ATTRIBUTE(XSDMaxLength="50",XSTypeName="mensajeSalidaSolicitud_descripcion",       XSTypeNamespace="http://www.portabilidad.com.solicita.credito.fovissste")

DEFINE {mensajeEntradaSolicitud RECORD
          nss               CHAR(11),--      ATTRIBUTE(XSDString,XSDMaxLength="11"),
          curp              CHAR(18),--      ATTRIBUTE(XSDString,XSDMaxLength="18"),
          numeroCredito     VARCHAR(10),--   ATTRIBUTE(XSDString,XSDMaxLength="10"),
          numeroCaso        DECIMAL(10,0),--   ATTRIBUTE(XSDString,XSDMaxLength="10"),--DECIMAL(10,0) ATTRIBUTE(XSDDecimal,XSDTotalDigits="10",XSDFractionDigits="0"),
          idEstatus         CHAR(5),--       ATTRIBUTE(XSDString,XSDMaxLength="5"),
          correoElectronico VARCHAR(120),--  ATTRIBUTE(XSDString,XSDMaxLength="120"),
          telCelular        CHAR(10)--      ATTRIBUTE(XSDString,XSDMaxLength="10")
       END RECORD,
       mensajeSalidaSolicitud RECORD
          numeroCaso         DECIMAL(10,0),-- ATTRIBUTE(XSDDecimal,XSDTotalDigits="10",XSDFractionDigits="0"),
          nss                CHAR(11),--      ATTRIBUTE(XSDString,XSDMaxLength="11"),
          apPaterno          VARCHAR(40),--   ATTRIBUTE(XSDString,XSDMaxLength="40"),
          apMaterno          VARCHAR(40),--   ATTRIBUTE(XSDString,XSDMaxLength="40"),
          nombre             VARCHAR(40),--   ATTRIBUTE(XSDString,XSDMaxLength="40"),
          resultadoOperacion CHAR(5),--       ATTRIBUTE(XSDString,XSDMaxLength="5"),
          idMotivo           CHAR(4),--       ATTRIBUTE(XSDString,XSDMaxLength="4"),
          descripcion        VARCHAR(50)--   ATTRIBUTE(XSDString,XSDMaxLength="50")
       END RECORD}
       mensajeEntradaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="mensajeEntradaSolicitud",XMLNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
          nss               tenss                ATTRIBUTE(XMLName="nss",              XMLNamespace=""),
          curp              tecurp               ATTRIBUTE(XMLName="curp",             XMLNamespace=""),
          numeroCredito     tenumeroCredito      ATTRIBUTE(XMLName="numeroCredito",    XMLNamespace=""),
          numeroCaso        tenumeroCaso         ATTRIBUTE(XMLName="numeroCaso",       XMLNamespace=""),
          idEstatus         teidEstatus          ATTRIBUTE(XMLName="idEstatus",        XMLNamespace=""),
          correoElectronico tencorreoElectronico ATTRIBUTE(XMLName="correoElectronico",XMLNamespace=""),
          telCelular        tetelCelular         ATTRIBUTE(XMLName="telCelular",       XMLNamespace="")
       END RECORD,
       mensajeSalidaSolicitud RECORD ATTRIBUTE(XMLSequence,XMLName="mensajeSalidaSolicitud",XMLNamespace="http://www.portabilidad.com.solicita.credito.fovissste")
          numeroCaso         tsnumeroCaso         ATTRIBUTE(XMLName="numeroCaso",        XMLNamespace=""),
          nss                tsnss                ATTRIBUTE(XMLName="nss",               XMLNamespace=""),
          apPaterno          tsapPaterno          ATTRIBUTE(XMLName="apPaterno",         XMLNamespace=""),
          apMaterno          tsapMaterno          ATTRIBUTE(XMLName="apMaterno",         XMLNamespace=""),
          nombre             tsnombre             ATTRIBUTE(XMLName="nombre",            XMLNamespace=""),
          resultadoOperacion tsresultadoOperacion ATTRIBUTE(XMLName="resultadoOperacion",XMLNamespace=""),
          idMotivo           tsidMotivo           ATTRIBUTE(XMLName="idMotivo",          XMLNamespace=""),
          descripcion        tsdescripcion        ATTRIBUTE(XMLName="descripcion",       XMLNamespace="")
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
          idCredito               VARCHAR(10),
          folioConsulta           VARCHAR(10),
          saldoInsolutoCredito    DECIMAL(13,2),
          tpoCredito              SMALLINT,
          diagnostico             SMALLINT,
          idMotivo                CHAR(2),
          descripcion             VARCHAR(100)
       END RECORD

END GLOBALS