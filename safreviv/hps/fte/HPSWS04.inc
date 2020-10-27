--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 04/01/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTWS02.inc                                                   #
#Objetivo     => Archivo de variables y constantes globales para el WS         #
#Fecha inicio => 04 Febrero 2015                                               #
################################################################################

GLOBALS

# Constantes de tipos de rechazo de solicitud de portabilidad 
-- Rechazos
CONSTANT C_DIAGNOSTICO_1000 SMALLINT = 1000
CONSTANT C_DIAGNOSTICO_1001 SMALLINT = 1001
CONSTANT C_DIAGNOSTICO_1002 SMALLINT = 1002
CONSTANT C_DIAGNOSTICO_1003 SMALLINT = 1003
CONSTANT C_DIAGNOSTICO_1100 SMALLINT = 1100
---

CONSTANT C_SUBCUENTA_VIVIENDA     SMALLINT = 4
CONSTANT C_SUBCUENTA_PREDIAL      SMALLINT = 51
CONSTANT C_SUBCUENTA_CONSERVACION SMALLINT = 53

-- Indicadores de aceptado/rechazado
CONSTANT C_RESULTADO_OP_ACEPTADA_FOV  CHAR(2) = '01'
CONSTANT C_RESULTADO_OP_RECHAZADA_FOV CHAR(2) = '02'
--

-- Estados de solicitud de cancelacion
CONSTANT C_ESTADO_INI_SOLICITUD_CANCELACION  SMALLINT = 0
CONSTANT C_ESTADO_SOLICITUD_REGISTRADA       SMALLINT = 10
CONSTANT C_ESTADO_SOLICITUD_RECHAZADA        SMALLINT = 15
CONSTANT C_ESTADO_SOLICITUD_ACEPTADA         SMALLINT = 20
CONSTANT C_ESTADO_SOL_PAGO_CANCELADA         SMALLINT = 22
CONSTANT C_ESTADO_SOL_REST_REGISTRADA        SMALLINT = 25  
CONSTANT C_ESTADO_SOL_REST_SOLICITADA        SMALLINT = 30

-- Estados de pagos de servicio
CONSTANT C_ESTADO_PAGO_SERVICIO_ACTIVO       SMALLINT = 103
CONSTANT C_ESTADO_PAGO_SERVICIO_CANCELADO    SMALLINT = 107
--

-- Estados de restitución
CONSTANT C_ESTADO_REST_REGISTRADA            SMALLINT = 10 
--
--
CONSTANT C_ID_ESTATUS_VALIDACION_PORTAL CHAR(5) = "E0000" # Consulta del portal
CONSTANT C_ID_ESTATUS_VALIDACION_CRM    CHAR(5) = "E0003" # Pendiente de validación 1A
CONSTANT C_ID_ESTATUS_CANCELA_HPS       CHAR(5) = "E0004" # Firmada por trabajador 1B
--

END GLOBALS