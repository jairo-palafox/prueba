DATABASE safre_viv
GLOBALS
   #####################################
   ## CREDITOS EN GARANT�A 43 BIS GRT ##
   #####################################
   CONSTANT g_proc_cod_grt_recurrente        = 1201, -- recepci�n recurrente cr�dito gt�a 43bis
            g_proc_cod_grt_rech_saldos       = 1204, -- recepci�n rechazo de saldos 43bis
            g_proc_cod_grt_sdos_transf       = 1205, -- recepci�n saldos transferidos 43bis
            g_proc_cod_grt_devol_solic       = 1206, -- recepci�n devoluci�n solicitudes 43bis
            g_proc_cod_grt_no_atendidas      = 1207, -- recepci�n solicitudes no atendidas 43bis
            g_proc_cod_grt_arch_solic        = 1209, -- generaci�n archivo solicitud sdos 43bis
            g_proc_cod_grt_reverso           = 1210, -- reverso solicitud saldo garant�a 43bis
            g_proc_cod_grt_solic_desmarca    = 1208, -- solicitud desmarca cr�ditos 43bis
            g_proc_cod_grt_liquidacion       = 1227, -- liquidaci�n solicitud de saldo en garant
            g_proc_cod_grt_uso_garantia      = 1202, -- recepci�n uso de garant�a 43bis
            g_proc_cod_grt_uso_liquida       = 1217, -- liquidaci�n uso garant�a 43bis
            g_proc_cod_grt_uso_rech_saldos   = 1211, -- recepci�n rechazo de saldos uso 43bis
            g_proc_cod_grt_uso_sdos_transf   = 1212, -- recepci�n saldos transferidos uso 43bis
            g_proc_cod_grt_uso_devol_solic   = 1213, -- recepci�n devol solicitudes uso 43bis
            g_proc_cod_grt_uso_no_atendidas  = 1214, -- recepci�n solic no atendidas uso 43bis
            g_proc_cod_grt_uso_arch_liquida  = 1215, -- generaci�n archivo liquidaci�n uso 43bis
            g_proc_cod_grt_uso_arch_solic    = 1216, -- generaci�n archivo solic sdos uso 43bis
            g_proc_cod_grt_uso_reverso       = 1219, -- reverso uso de garant�a 43 bis
            g_proc_cod_grt_uso_cocilia       = 1218, -- conciliaci�n uso garant�a 43bis
            g_proc_cod_grt_dse               = 1203, -- devoluci�n de saldos excedentes acr
            g_proc_cod_grt_rech_dse          = 1220, -- recepci�n rechazo devoluci�n saldos exc acr
            g_proc_cod_grt_conf_dse          = 1221, -- recepci�n confirmaci�n saldos exc acr
            g_proc_cod_grt_arch_solic_dse    = 1222, -- generaci�n solicitud devoluci�n sdo exc acr
            g_proc_cod_grt_concilia_dse      = 1225, -- conciliaci�n dev sdo exc
            g_proc_cod_grt_reverso_dse       = 1226, -- reverso dev sdo exc
            g_proc_cod_grt_agrupacion_dse    = 1223, -- agrupaci�n registros con devoluci�n grt
            g_proc_cod_grt_liquida_dse       = 1224, -- liquidaci�n devoluci�n saldos exc grt
            g_proc_cod_grt_uso_arch_rechazos = 1228  -- generaci�n archivo rechazo uso 43bis

   CONSTANT g_id_proceso_grt     = 1201, -- Solicitud de Saldos en Garant�a
            g_id_proceso_grt_uso = 1202 -- Uso de Garant�a 43 bis
END GLOBALS
