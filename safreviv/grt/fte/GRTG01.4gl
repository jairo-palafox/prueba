DATABASE safre_viv
GLOBALS
   #####################################
   ## CREDITOS EN GARANTÍA 43 BIS GRT ##
   #####################################
   CONSTANT g_proc_cod_grt_recurrente        = 1201, -- recepción recurrente crédito gtía 43bis
            g_proc_cod_grt_rech_saldos       = 1204, -- recepción rechazo de saldos 43bis
            g_proc_cod_grt_sdos_transf       = 1205, -- recepción saldos transferidos 43bis
            g_proc_cod_grt_devol_solic       = 1206, -- recepción devolución solicitudes 43bis
            g_proc_cod_grt_no_atendidas      = 1207, -- recepción solicitudes no atendidas 43bis
            g_proc_cod_grt_arch_solic        = 1209, -- generación archivo solicitud sdos 43bis
            g_proc_cod_grt_reverso           = 1210, -- reverso solicitud saldo garantía 43bis
            g_proc_cod_grt_solic_desmarca    = 1208, -- solicitud desmarca créditos 43bis
            g_proc_cod_grt_liquidacion       = 1227, -- liquidación solicitud de saldo en garant
            g_proc_cod_grt_uso_garantia      = 1202, -- recepción uso de garantía 43bis
            g_proc_cod_grt_uso_liquida       = 1217, -- liquidación uso garantía 43bis
            g_proc_cod_grt_uso_rech_saldos   = 1211, -- recepción rechazo de saldos uso 43bis
            g_proc_cod_grt_uso_sdos_transf   = 1212, -- recepción saldos transferidos uso 43bis
            g_proc_cod_grt_uso_devol_solic   = 1213, -- recepción devol solicitudes uso 43bis
            g_proc_cod_grt_uso_no_atendidas  = 1214, -- recepción solic no atendidas uso 43bis
            g_proc_cod_grt_uso_arch_liquida  = 1215, -- generación archivo liquidación uso 43bis
            g_proc_cod_grt_uso_arch_solic    = 1216, -- generación archivo solic sdos uso 43bis
            g_proc_cod_grt_uso_reverso       = 1219, -- reverso uso de garantía 43 bis
            g_proc_cod_grt_uso_cocilia       = 1218, -- conciliación uso garantía 43bis
            g_proc_cod_grt_dse               = 1203, -- devolución de saldos excedentes acr
            g_proc_cod_grt_rech_dse          = 1220, -- recepción rechazo devolución saldos exc acr
            g_proc_cod_grt_conf_dse          = 1221, -- recepción confirmación saldos exc acr
            g_proc_cod_grt_arch_solic_dse    = 1222, -- generación solicitud devolución sdo exc acr
            g_proc_cod_grt_concilia_dse      = 1225, -- conciliación dev sdo exc
            g_proc_cod_grt_reverso_dse       = 1226, -- reverso dev sdo exc
            g_proc_cod_grt_agrupacion_dse    = 1223, -- agrupación registros con devolución grt
            g_proc_cod_grt_liquida_dse       = 1224, -- liquidación devolución saldos exc grt
            g_proc_cod_grt_uso_arch_rechazos = 1228  -- generación archivo rechazo uso 43bis

   CONSTANT g_id_proceso_grt     = 1201, -- Solicitud de Saldos en Garantía
            g_id_proceso_grt_uso = 1202 -- Uso de Garantía 43 bis
END GLOBALS
