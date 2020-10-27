DATABASE safre_viv
GLOBALS
   ######################################
   ## TRANSFERENCIA DE ACREDITADOS ACR ##
   ######################################
   CONSTANT g_proc_cod_acr_recurrente     = 201, -- recepción recurrente acr
            g_proc_cod_acr_liquidacion    = 220, -- liquidación transferencia acr
            g_proc_cod_acr_rech_saldos    = 203, -- recepción rechazo de saldos acr
            g_proc_cod_acr_sdos_transf    = 204, -- recepción saldos transferidos acr
            g_proc_cod_acr_devol_solic    = 205, -- recepción devolución solicitudes acr
            g_proc_cod_acr_no_atendidas   = 206, -- recepción solicitudes no atendidas acr
            g_proc_cod_acr_arch_liquida   = 207, -- generación archivo liquidación acr
            g_proc_cod_acr_arch_amort     = 208, -- generación archivo amortización acr
            g_proc_cod_acr_arch_cc        = 209, -- generación archivo cargo a crédito acr
            g_proc_cod_acr_arch_solic     = 210, -- generación archivo solicitud saldos acr
            g_proc_cod_acr_arch_amort72   = 227, -- generación archivo amortización fondo72 acr
            g_proc_cod_acr_conciliacion   = 211, -- conciliación acr
            g_proc_cod_acr_sdos_reman     = 212, -- saldos remanentes acr
            g_proc_cod_acr_reverso        = 213, -- reverso transferencia acr
            g_proc_cod_acr_solic_desmarca = 214, -- solicitud desmarca acr
            g_proc_cod_acr_dse            = 202, -- devolución de saldos excedentes acr
            g_proc_cod_acr_rech_dse       = 215, -- recepción rechazo devolución saldos exc acr
            g_proc_cod_acr_conf_dse       = 216, -- recepción confirmación saldos exc acr
            g_proc_cod_acr_fondo72        = 225, -- recepción fondo de ahorro 72
            g_proc_cod_acr_arch_solic_dse = 217, -- generación solicitud devolución sdo exc acr
            g_proc_cod_acr_concilia_dse   = 218, -- conciliación dev sdo exc
            g_proc_cod_acr_reverso_dse    = 219, -- reverso dev sdo exc
            g_proc_cod_acr_agrupacion_dse = 221, -- acrupación registros con devolución acr
            g_proc_cod_acr_liquida_dse    = 222, -- afectación cta ind dev sdos exc acr
            g_proc_cod_acr_liquida_f72    = 226 -- liquidación fondo 72

   CONSTANT g_id_proceso_acr = 201 -- Transferencia de Acreditados
END GLOBALS
