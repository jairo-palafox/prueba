--===============================================================
-- Version: 1.0.0
-- Fecha ultima modificacion: 16-04-2012
--===============================================================

GLOBALS
DEFINE v_r_rpt_aceptadas ARRAY[12] OF RECORD -- registro de resumen por estado
          tpo_mandato      STRING,
          operacion        CHAR(20),
          total_mdt        INTEGER
       END RECORD
DEFINE v_r_rpt_canceladas ARRAY[12] OF RECORD -- registro de resumen por estado
         tpo_mandato      STRING,
         operacion        CHAR(20),
         total_mdt        INTEGER
      END RECORD


CONSTANT g_proceso_cod_pago_mandatos            INTEGER = 3103 # Pago de servicio

CONSTANT g_proceso_cod_rev_liq_pago_mandatos    INTEGER = 1311 # Reverso liquidación pago de mandatos
CONSTANT g_proceso_cod_rev_preliq_pago_mandatos INTEGER = 1312 # Reverso preliquidación Pago de mandatos    

CONSTANT g_proceso_cod_abonos_mandatos          INTEGER = 3101 # Traspaso Fondo Servicio
CONSTANT g_proceso_cod_originacion_deudor       INTEGER = 301 # Traspaso Fondo Servicio

CONSTANT g_opera_cod_carga          INTEGER = 1
CONSTANT g_opera_cod_integracion    INTEGER = 2
CONSTANT g_opera_cod_preliquidacion INTEGER = 3
CONSTANT g_opera_cod_liquidacion    INTEGER = 4

CONSTANT g_estado_abonado_pago_mdt      SMALLINT = 100
CONSTANT g_estado_preliquidado_pago_mdt SMALLINT = 102
CONSTANT g_estado_liquidado_pago_mdt    SMALLINT = 110

END GLOBALS

