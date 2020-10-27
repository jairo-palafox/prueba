--==============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 19/03/2015
--==============================================================================

################################################################################
#Modulo       => PRT                                                           #
#Programa     => PRTG01                                                        #
#Objetivo     => Archivo de variables y constantes globales para PRT           #
#Fecha inicio => 19 Marzo 2015                                                 #
################################################################################

GLOBALS

# Módulo
--
CONSTANT C_MODULO_COD_PRT CHAR(3) = 'prt' # Pendiente de definir
--

# Procesos
--
CONSTANT C_PROCESO_COD_MARCA                      SMALLINT = 2801 # Pendiente de definir
CONSTANT C_PROCESO_COD_TRANS_SDO_RECEPTORA        SMALLINT = 2802 # Pendiente de definir
CONSTANT C_PROCESO_COD_TRANS_SDO_SUBSEC_RECEPTORA SMALLINT = 2804 # Traspasos subsecuentes receptora
CONSTANT C_PROCESO_COD_TRANS_SDO_CEDENTE          SMALLINT = 2805 # Traspasos cedente
CONSTANT C_PROCESO_COD_TRANS_SDO_SUBSEC_CEDENTE   SMALLINT = 2806 # Traspasos subsecuentes cedente
CONSTANT C_PROCESO_COD_DEV_SDO_RECEPTORA          SMALLINT = 2807 # Devoluciones receptora
CONSTANT C_PROCESO_COD_DEV_SDO_CEDENTE            SMALLINT = 2808 # Devoluciones receptora
CONSTANT C_PROCESO_COD_REPORTES_PRT               SMALLINT = 2809 # Reportes portabilidad
CONSTANT C_PROCESO_COD_REENVIO_CRM                SMALLINT = 2815 # Reenvío de notificación a CRM

--

# Operaciones
--
CONSTANT C_OPERA_COD_CARGA          SMALLINT = 1
CONSTANT C_OPERA_COD_INTEGRACION    SMALLINT = 2
CONSTANT C_OPERA_COD_PRELIQUIDACION SMALLINT = 3
CONSTANT C_OPERA_COD_LIQUIDACION    SMALLINT = 4

CONSTANT C_OPERA_COD_PRELIQ_TANS_CED SMALLINT = 1
CONSTANT C_OPERA_COD_LIQ_TANS_CED    SMALLINT = 2

CONSTANT C_OPERA_COD_REENVIA_CRM SMALLINT = 1

--


# Subcuenta
--
CONSTANT C_SUBCUENTA_PRT SMALLINT = 60
--

# Movimientos
--
CONSTANT C_MOV_REC_ABONO_TRASPASO     SMALLINT = 1601
CONSTANT C_MOV_REC_CARGO_DISP         SMALLINT = 1672
CONSTANT C_MOV_REC_CARGO_TRASPASO     SMALLINT = 1602
CONSTANT C_MOV_REC_ABONO_SUBSECUENTE  SMALLINT = 1603
CONSTANT C_MOV_REC_CARGO_AMORTIZACION SMALLINT = 1604
--CONSTANT C_MOV_CED_ABONO_TRASPASO     SMALLINT = 1605
CONSTANT C_MOV_REC_CARGO_SUBSECUENTE  SMALLINT = 1606
CONSTANT C_MOV_CED_ABONO_SUBSECUENTE  SMALLINT = 1607
CONSTANT C_MOV_CED_CARGO_TRASPASO     SMALLINT = 1608
CONSTANT C_MOV_CED_CARGO_SUBSECUENTE  SMALLINT = 1610
CONSTANT C_MOV_CED_CARGO_SUB          SMALLINT = 1604
CONSTANT C_MOV_CED_ABONO_TRASPASO     SMALLINT = 1611

CONSTANT C_MOV_CED_ABONO_SUBSEC_PRT  SMALLINT = 1001
--

# Estados traspaso receptora
--
-- Estados no controlados en SP, es necesario modificar SP´s

CONSTANT C_ESTADO_TRASPASO_INTEGRADA    SMALLINT = 10 # Estado integrado traspaso tabla prt_traspaso_receptora
CONSTANT C_ESTADO_TRASPASO_PRELIQUIDADA SMALLINT = 20 # Estado preliquidado traspaso tabla prt_traspaso_receptora
CONSTANT C_ESTADO_TRASPASO_LIQUIDADA    SMALLINT = 30 # Estado liquidado traspaso tabla prt_traspaso_receptora
--
# utilizado para cedente y receptora
CONSTANT C_ESTADO_TRASPASO_RECHAZADO    SMALLINT = 5  # Estado rechazado
# Estados traspaso cedente
--
-- Estados no controlados en SP, es necesario modificar SP´s
CONSTANT C_ESTADO_TRASPASO_NOTIFICADA_CED   SMALLINT = 20 # Estado integrado traspaso tabla prt_traspaso_cedente
CONSTANT C_ESTADO_TRASPASO_PRELIQUIDADA_CED SMALLINT = 30 # Estado preliquidado traspaso tabla prt_traspaso_cedente
CONSTANT C_ESTADO_TRASPASO_LIQUIDADA_CED    SMALLINT = 40 # Estado liquidado traspaso tabla prt_traspaso_cedente
--

# Códigos cartera
CONSTANT C_CART_CRED_VIGENTE     CHAR(2) = "00" # Crédito vigente
CONSTANT C_CART_CRED_INEXISTENTE CHAR(2) = "01" # Crédito inexistente
CONSTANT C_CART_CRED_LIQUIDADO   CHAR(2) = "02" # Crédito liquidado
CONSTANT C_CART_SIN_ACCESO_BD    CHAR(2) = "05" # Sin acceso a BD
--

# tipo traspaso
--
CONSTANT C_TIPO_TRASPASO_CORRIENTE   CHAR(2) = '01' # traspaso de saldos corrientes
CONSTANT C_TIPO_TRASPASO_SUBSECUENTE CHAR(2) = '02' # Traspaso de saldos subsecuentes
CONSTANT C_TIPO_TRASPASO_DEVOLUCION  CHAR(2) = '03' # Devolución de saldos
--

# Tipo portabilidad
--
CONSTANT C_TIPO_PRT_CRED_NUEVO     SMALLINT = 1 # Crédito nuevo
CONSTANT C_TIPO_PRT_CRED_EXISTENTE SMALLINT = 2 # Crédito existente
CONSTANT C_TIPO_PRT_DESMARCA       SMALLINT = 3 # Desmarca de portabilidad
--

# Tipos registros para archivos de salida
--
CONSTANT C_REGISTRO_01   CHAR(2) = '01' # Registro encabezado
CONSTANT C_REGISTRO_02   CHAR(2) = '02' # Registro detalle
CONSTANT C_REGISTRO_09   CHAR(2) = '09' # Registro sumario

-- Datos para fecha de Originación de crédito
CONSTANT C_FECHA_INI_TIPO_PRT           = '03/19/2014' # Fecha decreto inicio de portabilidad 19 Marzo 2014
CONSTANT C_FECHA_FIN_TIPO_PRT           = '05/25/2015' # Fecha arranque portabilidad 25 Mayo 2015
CONSTANT C_PERIODO_ORINARIO_CREDITO SMALLINT = 90 # Periodo (Días) para consider un crédito como nuevo despues de la fecha de originación
CONSTANT C_FECHA_DEFECTO_ORIGINACION_CRED    = '01/01/1900'
CONSTANT C_CADENA_DEFECTO_ORIGINACION_CRED   = '19000101'
--

-- Tipo portabilidad
CONSTANT C_CREDITO_NUEVO         SMALLINT = 1
CONSTANT C_CREDITO_EXISTENTE     SMALLINT = 2
CONSTANT C_DESMARCA_PORTABILIDAD SMALLINT = 3
CONSTANT C_SIN_TIPO_PRT          SMALLINT = 0
--

-- Tipo solicitudes
CONSTANT C_FLUJO_CEDENTE   SMALLINT = 1
CONSTANT C_FLUJO_RECEPTORA SMALLINT = 2

-- Estados de reenvío
CONSTANT C_REENVIO_NO_ENVIADO SMALLINT = 0
CONSTANT C_REENVIO_ENVIADO    SMALLINT = 1
--

END GLOBALS
