DATABASE safre_viv

GLOBALS

   ##################################
   ## ANUALIDADES GARANTIZADAS AGR ##
   ##################################

   CONSTANT g_proc_cod_agr_recurrente     = 301 -- recepci�n recurrente originaci�n agr
   CONSTANT g_proc_cod_agr_liquidacion    = 312 -- liquidaci�n anualidad garantizada
   CONSTANT g_proc_cod_agr_uso_anualid    = 302 -- recepci�n uso anualidades garantizadas
   CONSTANT g_proc_cod_agr_rech_saldos    = 304 -- recepci�n rechazo de saldos agr
   CONSTANT g_proc_cod_agr_sdos_transf    = 305 -- recepci�n saldos transferidos agr
   CONSTANT g_proc_cod_agr_devol_solic    = 306 -- recepci�n devoluci�n solicitudes agr
   CONSTANT g_proc_cod_agr_no_atendidas   = 307 -- recepci�n solicitudes no atendidas agr
   CONSTANT g_proc_cod_agr_sdos_reman     = 321 -- saldos remanentes agr
   CONSTANT g_proc_cod_agr_arch_liquida   = 309 -- generaci�n archivo liquidaci�n agr
   CONSTANT g_proc_cod_agr_arch_amort     = 325 -- generaci�n archivo amortizaci�n agr
   CONSTANT g_proc_cod_agr_arch_cc        = 326 -- generaci�n archivo cargo a cr�dito agr
   CONSTANT g_proc_cod_agr_arch_solic     = 310 -- generaci�n archivo solicitud saldos agr
   CONSTANT g_proc_cod_agr_arch_amort72   = 322 -- generaci�n archivo amortizaci�n fondo72 agr
   CONSTANT g_proc_cod_agr_conciliacion   = 313 -- conciliaci�n agr
   CONSTANT g_proc_cod_agr_reverso        = 311 -- reverso anualidades garantizadas
   CONSTANT g_proc_cod_agr_solic_desmarca = 308 -- solicitud desmarca agr
   CONSTANT g_proc_cod_agr_dse            = 303 -- devoluci�n de saldos excedentes agr
   CONSTANT g_proc_cod_agr_rech_dse       = 314 -- recepci�n rechazo devoluci�n saldos exc agr
   CONSTANT g_proc_cod_agr_conf_dse       = 315 -- recepci�n confirmaci�n saldos exc agr
   CONSTANT g_proc_cod_agr_arch_solic_dse = 316 -- generaci�n solicitud devoluci�n sdo exc agr
   CONSTANT g_proc_cod_agr_concilia_dse   = 319 -- conciliaci�n dev sdo exc agr
   CONSTANT g_proc_cod_agr_reverso_dse    = 320 -- reverso dev sdo exc agr
   CONSTANT g_proc_cod_agr_agrupacion_dse = 317 -- agrupaci�n registros con devoluci�n agr
   CONSTANT g_proc_cod_agr_liquida_dse    = 318 -- liquidaci�n devoluci�n saldos exc agr
   CONSTANT g_proc_cod_agr_recurr_marca   = 323 -- recepci�n recurrente marca y desmarca
   CONSTANT g_proc_cod_agr_solic_devol    = 324 -- recepci�n solicitud devolucion agr
   CONSTANT g_proc_cod_agr_arch_concilia  = 327 -- recepci�n solicitud devolucion agr
   CONSTANT g_proc_cod_agr_extrac_acred   = 330 -- generaci�n extractor de acreditados
   CONSTANT g_proc_cod_agr_gtia_edo_mcpio = 331 -- recepci�n uso garant�a estado y municipio
   CONSTANT g_proc_cod_agr_liq_gtia_ed_mc = 332  -- liquidaci�n de uso garant�a estado y municipio
   CONSTANT g_proc_cod_cre_trm            = 334 -- recepci�n homologaci�n trm safre

   CONSTANT g_id_proceso_agr              = 301 -- Anualidades Garantizadas

END GLOBALS
