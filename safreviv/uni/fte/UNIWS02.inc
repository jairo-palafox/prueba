#-------------------------------------------------------------------------------
# File: UNIWS02Service.inc
# GENERATED BY fglwsdl 
#-------------------------------------------------------------------------------
# THIS FILE WAS GENERATED. DO NOT MODIFY.
#-------------------------------------------------------------------------------

GLOBALS

#
#-------------------------------------------------------------------------------
# XML Type definitions 
#-------------------------------------------------------------------------------
#

# TYPE : tns1ConsultaUnificadosCreditoRequest
#
TYPE tns1ConsultaUnificacionCreditosRequest RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaUnificacionCreditosRequest",XSTypeNamespace="http://vo.consultaUnificacionCreditos.efp.com")
   nss           CHAR(11) ATTRIBUTE(XMLName="nss")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1UnificadosCredito
#
TYPE tns1Unificados RECORD ATTRIBUTE(XMLSequence,XSTypeName="Movimiento",XSTypeNamespace="http://vo.consultaUnificacionCreditos.efp.com")
   nss_unificado       CHAR(11)     ATTRIBUTE(XMLName="nss_unificado"),
   marca_activa_ado    SMALLINT     ATTRIBUTE(XMLName="marca_activa"),
   marca_historica_ado SMALLINT     ATTRIBUTE(XMLName="marca_historica"),
   fecha_marca_ado     CHAR(10)     ATTRIBUTE(XMLName="fecha_marca"),
   nombre_ado          VARCHAR(60)  ATTRIBUTE(XMLName="nombre_ado"),
   paterno_ado         VARCHAR(60)  ATTRIBUTE(XMLName="paterno_ado"),
   materno_ado         VARCHAR(60)  ATTRIBUTE(XMLName="materno_ado"),
   curp_ado            VARCHAR(18)  ATTRIBUTE(XMLName="curp_ado"),
   rfc_ado             VARCHAR(13)  ATTRIBUTE(XMLName="rfc_ado"),
   num_credito_ado     DECIMAL(10,0) ATTRIBUTE(XMLName="num_credito_ado"),
   estado_credito_ado  STRING       ATTRIBUTE(XMLName="estado_credito_ado")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns2ArrayOf_tns1_Unificados
#
TYPE tns2ArrayOf_tns1_Unificados RECORD ATTRIBUTE(XMLSequence,XSTypeName="ArrayOf_tns1_Unificados",XSTypeNamespace="http://consultaUnificacionCreditos.efp.com")
   item DYNAMIC ARRAY ATTRIBUTE(XMLList) OF tns1Unificados ATTRIBUTE(XMLName="unificado")
END RECORD
#-------------------------------------------------------------------------------

#
# TYPE : tns1ConsultaUnificadosCreditoResponse
#
TYPE tns1ConsultaUnificacionCreditosResponse RECORD ATTRIBUTE(XMLSequence,XSTypeName="ConsultaUnificacionCreditosResponse",XSTypeNamespace="http://vo.consultaUnificacionCreditos.efp.com")
   nss_unificador  CHAR(11)     ATTRIBUTE(XMLName="nss_unificador"),
   marca_activa    SMALLINT     ATTRIBUTE(XMLName="marca_activa"),
   marca_historica SMALLINT     ATTRIBUTE(XMLName="marca_historica"),
   fecha_marca     CHAR(10)     ATTRIBUTE(XMLName="fecha_marca"),
   nombre_dor      VARCHAR(60)  ATTRIBUTE(XMLName="nombre_dor"),
   paterno_dor     VARCHAR(60)  ATTRIBUTE(XMLName="paterno_dor"),
   materno_dor     VARCHAR(60)  ATTRIBUTE(XMLName="materno_dor"),
   curp_dor        VARCHAR(18)  ATTRIBUTE(XMLName="curp_dor"),
   rfc_dor         VARCHAR(13)  ATTRIBUTE(XMLName="rfc_dor"),
   num_credito     DECIMAL(10,0) ATTRIBUTE(XMLName="num_credito"),
   estado_credito  STRING       ATTRIBUTE(XMLName="estado_credito"),
   unificados tns2ArrayOf_tns1_Unificados ATTRIBUTE(XMLName="Lista_unificados"),
   codigoRespuesta STRING      ATTRIBUTE(XMLName="codigo_respuesta")
END RECORD
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Service: ConsultaUnificadosCreditoServicesService
# Port:    ConsultaUnificacionCreditoServices
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# VARIABLE : ns2unificadocionCreditoRequest --- arreglo de entrada
DEFINE ns2unificacionCreditosRequest tns1ConsultaUnificacionCreditosRequest ATTRIBUTE(XMLName="unificacionCreditosRequest",XMLNamespace="http://consultaUnificacionCreditos.efp.com")

#-------------------------------------------------------------------------------

# VARIABLE : ns2consultarUnificacionCreditoReturn --- arreglo de salida
DEFINE ns2consultarUnificacionCreditosReturn tns1ConsultaUnificacionCreditosResponse ATTRIBUTE(XMLName="consultarUnificacionCreditosReturn",XMLNamespace="http://consultaUnificacionCreditos.efp.com")


END GLOBALS
