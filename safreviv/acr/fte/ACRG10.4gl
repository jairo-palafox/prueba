DATABASE safre_viv
GLOBALS
   ######################################
   ## TRANSFERENCIA DE ACREDITADOS ACR ##
   ######################################
   CONSTANT g_proc_cod_acr_recurrente     = 201, -- recepci�n recurrente acr
            g_proc_cod_acr_liquidacion    = 220, -- liquidaci�n transferencia acr
            g_proc_cod_acr_rech_saldos    = 203, -- recepci�n rechazo de saldos acr
            g_proc_cod_acr_sdos_transf    = 204, -- recepci�n saldos transferidos acr
            g_proc_cod_acr_devol_solic    = 205, -- recepci�n devoluci�n solicitudes acr
            g_proc_cod_acr_no_atendidas   = 206, -- recepci�n solicitudes no atendidas acr
            g_proc_cod_acr_arch_liquida   = 207, -- generaci�n archivo liquidaci�n acr
            g_proc_cod_acr_arch_amort     = 208, -- generaci�n archivo amortizaci�n acr
            g_proc_cod_acr_arch_cc        = 209, -- generaci�n archivo cargo a cr�dito acr
            g_proc_cod_acr_arch_solic     = 210, -- generaci�n archivo solicitud saldos acr
            g_proc_cod_acr_arch_amort72   = 227, -- generaci�n archivo amortizaci�n fondo72 acr
            g_proc_cod_acr_conciliacion   = 211, -- conciliaci�n acr
            g_proc_cod_acr_sdos_reman     = 212, -- saldos remanentes acr
            g_proc_cod_acr_reverso        = 213, -- reverso transferencia acr
            g_proc_cod_acr_solic_desmarca = 214, -- solicitud desmarca acr
            g_proc_cod_acr_dse            = 202, -- devoluci�n de saldos excedentes acr
            g_proc_cod_acr_rech_dse       = 215, -- recepci�n rechazo devoluci�n saldos exc acr
            g_proc_cod_acr_conf_dse       = 216, -- recepci�n confirmaci�n saldos exc acr
            g_proc_cod_acr_fondo72        = 225, -- recepci�n fondo de ahorro 72
            g_proc_cod_acr_arch_solic_dse = 217, -- generaci�n solicitud devoluci�n sdo exc acr
            g_proc_cod_acr_concilia_dse   = 218, -- conciliaci�n dev sdo exc
            g_proc_cod_acr_reverso_dse    = 219, -- reverso dev sdo exc
            g_proc_cod_acr_agrupacion_dse = 221, -- acrupaci�n registros con devoluci�n acr
            g_proc_cod_acr_liquida_dse    = 222, -- afectaci�n cta ind dev sdos exc acr
            g_proc_cod_acr_liquida_f72    = 226 -- liquidaci�n fondo 72

   CONSTANT g_id_proceso_acr = 201 -- Transferencia de Acreditados
END GLOBALS
