DATABASE safre_viv

GLOBALS

   ##################################
   ## ANUALIDADES GARANTIZADAS AGR ##
   ##################################

   CONSTANT g_proc_cod_agr_recurrente     = 301 -- recepción recurrente originación agr
   CONSTANT g_proc_cod_agr_liquidacion    = 312 -- liquidación anualidad garantizada
   CONSTANT g_proc_cod_agr_uso_anualid    = 302 -- recepción uso anualidades garantizadas
   CONSTANT g_proc_cod_agr_rech_saldos    = 304 -- recepción rechazo de saldos agr
   CONSTANT g_proc_cod_agr_sdos_transf    = 305 -- recepción saldos transferidos agr
   CONSTANT g_proc_cod_agr_devol_solic    = 306 -- recepción devolución solicitudes agr
   CONSTANT g_proc_cod_agr_no_atendidas   = 307 -- recepción solicitudes no atendidas agr
   CONSTANT g_proc_cod_agr_sdos_reman     = 321 -- saldos remanentes agr
   CONSTANT g_proc_cod_agr_arch_liquida   = 309 -- generación archivo liquidación agr
   CONSTANT g_proc_cod_agr_arch_amort     = 325 -- generación archivo amortización agr
   CONSTANT g_proc_cod_agr_arch_cc        = 326 -- generación archivo cargo a crédito agr
   CONSTANT g_proc_cod_agr_arch_solic     = 310 -- generación archivo solicitud saldos agr
   CONSTANT g_proc_cod_agr_arch_amort72   = 322 -- generación archivo amortización fondo72 agr
   CONSTANT g_proc_cod_agr_conciliacion   = 313 -- conciliación agr
   CONSTANT g_proc_cod_agr_reverso        = 311 -- reverso anualidades garantizadas
   CONSTANT g_proc_cod_agr_solic_desmarca = 308 -- solicitud desmarca agr
   CONSTANT g_proc_cod_agr_dse            = 303 -- devolución de saldos excedentes agr
   CONSTANT g_proc_cod_agr_rech_dse       = 314 -- recepción rechazo devolución saldos exc agr
   CONSTANT g_proc_cod_agr_conf_dse       = 315 -- recepción confirmación saldos exc agr
   CONSTANT g_proc_cod_agr_arch_solic_dse = 316 -- generación solicitud devolución sdo exc agr
   CONSTANT g_proc_cod_agr_concilia_dse   = 319 -- conciliación dev sdo exc agr
   CONSTANT g_proc_cod_agr_reverso_dse    = 320 -- reverso dev sdo exc agr
   CONSTANT g_proc_cod_agr_agrupacion_dse = 317 -- agrupación registros con devolución agr
   CONSTANT g_proc_cod_agr_liquida_dse    = 318 -- liquidación devolución saldos exc agr
   CONSTANT g_proc_cod_agr_recurr_marca   = 323 -- recepción recurrente marca y desmarca
   CONSTANT g_proc_cod_agr_solic_devol    = 324 -- recepción solicitud devolucion agr
   CONSTANT g_proc_cod_agr_arch_concilia  = 327 -- recepción solicitud devolucion agr
   CONSTANT g_proc_cod_agr_extrac_acred   = 330 -- generación extractor de acreditados
   CONSTANT g_proc_cod_agr_gtia_edo_mcpio = 331 -- recepción uso garantía estado y municipio
   CONSTANT g_proc_cod_agr_liq_gtia_ed_mc = 332  -- liquidación de uso garantía estado y municipio
   CONSTANT g_proc_cod_cre_trm            = 334 -- recepción homologación trm safre

   CONSTANT g_id_proceso_agr              = 301 -- Anualidades Garantizadas

END GLOBALS
