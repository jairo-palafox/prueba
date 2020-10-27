--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================

-----------------------------------------------------------------------------------------
-- Modulo        => ACL                                                                   
-- Programa      => ACLS98                                                              
-- Objetivo      => Generacion del archivo de rechazos de Aclaraciones Con Cambio de NSS y NOMBRE
-- Autor         => GERARDO ALFONSO VEGA PAREDES
-- Fecha inicio  => 3 de nomvibre de 2015
-- Actualización => 10 de mayo 2016
-----------------------------------------------------------------------------------------
-- Modificación  => Funcionalidad que verifica que no exista reg acl ya pagados
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha         => 12-JUN-2017
-- Clave         => --xvi-77
-----------------------------------------------------------------------------------------

DATABASE safre_viv
GLOBALS "ACLG02.4gl"

GLOBALS
   DEFINE g_pid         LIKE bat_ctr_proceso.pid, --  ID del proceso
          g_proceso_cod LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod   LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN

   DEFINE p_usuario_cod    LIKE seg_usuario.usuario_cod,
          p_origen_archivo LIKE cta_his_pagos.origen_archivo

-- origen archivo = 6 Con cambio de nss
-- origen archivo = 7 Con cabmio en nombre          
          
   LET p_usuario_cod    = ARG_VAL(1)
   LET p_origen_archivo = ARG_VAL(2)
   
   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLS98.log")

   CALL f_rechazos (p_origen_archivo)

END MAIN

FUNCTION f_rechazos (p_origen_archivo)

   DEFINE p_origen_archivo LIKE cta_his_pagos.origen_archivo, 
          v_s_SqlQry STRING,
          v_extension CHAR(08) 
         
   DEFINE v_r_pag_det_acl   RECORD   
      entidad_receptora        LIKE cta_his_pagos.cve_ent_receptora,
      num_reg_patronal         LIKE cta_his_pagos.nrp,
      rfc_patron               LIKE cta_pag_complemento.rfc_patron,
      periodo_pago             LIKE cta_his_pagos.periodo_pago,
      f_pago_patron            LIKE cta_his_pagos.f_pago,
      folio_sua                LIKE cta_his_pagos.folio_sua,
      nss                      LIKE afi_derechohabiente.nss,
      rfc_trabajador           LIKE afi_derechohabiente.rfc,
      curp                     LIKE afi_derechohabiente.curp,
      num_cred_infonavit       LIKE cta_his_pagos.num_crd_ifv,
      f_inicio_dcto_cre_inf    LIKE cta_pag_complemento.f_ini_desc_crd_ifv,
      num_mov_periodo          LIKE cta_pag_complemento.num_mov_periodo,
      nombre_trabajador        LIKE afi_derechohabiente.nombre_imss,
      ultimo_salario_integrado LIKE cta_pag_complemento.ult_sdi,
      tipo_trabajador          LIKE cta_pag_complemento.tpo_trabajador,
      jornada_semana_reducida  LIKE cta_pag_complemento.jornada,
      localiza_trabajador      LIKE cta_his_pagos.localiza_trabajador,
      destino_ap_viv           LIKE cta_pag_complemento.destino_ap_viv,
      dias_cotizados_bim       LIKE cta_pag_complemento.dias_cot_bim,
      dias_incapacidad_bim     LIKE cta_pag_complemento.dias_incap_bim,
      dias_ausentismo_bim      LIKE cta_pag_complemento.dias_ausent_bim,
      imp_ap_pat_inf           LIKE cta_his_pagos.imp_ap_pat,
      imp_am_cre_inf           LIKE cta_his_pagos.imp_am_cre,
      imp_ren_viv_pgo_ext      LIKE cta_his_pagos.imp_ren_viv_pgo_ext,
      marca_cre_sua            LIKE cta_pag_complemento.marca_sua,
      marca_cre_BDNSAR         LIKE cta_pag_complemento.marca_bdnsar,
      diag_acl                 LIKE cta_his_pagos.tpo_aclaracion, 
      f_proceso                LIKE cta_his_pagos.f_proceso,
      id_derhab_nuevo          DECIMAL (9,0),
      ap_paterno_afore         LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_afore         LIKE afi_derechohabiente.ap_materno_af,
      nombre_afore             LIKE afi_derechohabiente.nombre_af,
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv ,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext ,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      folio                    DECIMAL(9,0),
      id_referencia            DECIMAL(9,0),
      id_derechohabiente       LIKE cta_his_pagos.id_derechohabiente    --xvi-77
   END RECORD
         
   DEFINE v_r_pag_sum_acl RECORD
      sum_ap_pat          LIKE acl_sum_sc_nss.suma_ap_pat,
      sum_am              LIKE acl_sum_sc_nss.suma_am,
      sum_aivs            LIKE acl_sum_sc_nss.suma_aivs,
      sum_int_viv_pag_ext LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
      sum_aivs_pag_ext    LIKE acl_sum_sc_nss.suma_aiv_pgo_ext
   END RECORD
           
   DEFINE v_c_fecha_hoy       VARCHAR(8),
          v_c_nss             CHAR(11),
          l_s_cadena_detalle  STRING,
          l_s_cadena_sum_acl  STRING,
          v_v_nom_archi       VARCHAR(100),
          v_v_ruta_nomarch    STRING,
          v_c_ruta_env_acr    LIKE seg_modulo.ruta_envio,
          v_ch_arch_solTransf BASE.CHANNEL,
          v_c_tipo_registro   VARCHAR (2),
          v_i_contador_reg    INTEGER,
          v_num_reg_detalle   INTEGER,
          v_s_fec_tmp         STRING,
          v_c_fecha_pag       VARCHAR(8),
          v_filler1           CHAR(7),
          v_filler2           CHAR (12)

   DEFINE v_cont SMALLINT   --xvi-77

   CASE p_origen_archivo
--      WHEN 5 LET v_extension = "disscnss"  -- ACL sin cambio de nss   	
      WHEN 6 LET v_extension = "disccnss"  -- ACL con cambio de nss
      WHEN 7 LET v_extension = "diccncno"  -- ACL con cambio nombre
   END CASE

   LET INT_FLAG = 0
   
   LET v_c_tipo_registro = 2
   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Rechazos", v_v_nom_archi[4,5], v_v_nom_archi[7,8],v_extension
   
   LET v_s_fec_tmp = TODAY  USING "yyyymmdd "
   LET v_c_fecha_hoy = v_s_fec_tmp
                                    
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'
   LET v_c_ruta_env_acr = "/safreviv_int/archivos"

   LET v_v_nom_archi    = v_c_fecha_hoy||"."|| v_extension
   LET v_v_ruta_nomarch = v_c_ruta_env_acr CLIPPED ||"/"|| v_v_nom_archi
   
   DISPLAY " ARCHIVO DE RECHAZOS GENERADO: ",v_v_ruta_nomarch
   
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()
   
   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   -- ===================================================================
   -- ENCABEZADO
   --
   -- el archivo de Aclaraciones sin cambio no tiene encabezado
    

   -- ===================================================================
   -- DETALLE    
   --se asigna en cero el número de registros en estado rechazados
   --LET v_r_pag_sum_fc.num_reg_detalle = 0              
   
   --se asigna en cero el importe de registros en estado rechazados
   --LET v_r_pag_sum_fc.tot_ap_fc = 0

   -- se inicializan variables
   LET v_r_pag_sum_acl.sum_ap_pat          = 0
   LET v_r_pag_sum_acl.sum_am              = 0
   LET v_r_pag_sum_acl.sum_aivs            = 0
   LET v_r_pag_sum_acl.sum_int_viv_pag_ext = 0
   LET v_r_pag_sum_acl.sum_aivs_pag_ext    = 0       
   
   --se consulta el detalle 
   LET v_s_SqlQry = "\n SELECT a.cve_ent_receptora,   ",
                    "\n        a.nrp, b.rfc_patron,   ",
                    "\n        a.periodo_pago,        ", 
                    "\n        a.f_pago,              ",
                    "\n        a.folio_sua,           ",
                    "\n        c.nss,                 ",
                    "\n        c.rfc,                 ",
                    "\n        c.curp,                ", 
                    "\n        a.num_crd_ifv,         ",
                    "\n        b.f_ini_desc_crd_ifv,  ", 
                    "\n        b.num_mov_periodo,     ",
                    "\n        c.nombre_imss,         ",
                    "\n        b.ult_sdi,             ",  
                    "\n        b.tpo_trabajador,      ",
                    "\n        b.jornada,             ",
                    "\n        a.localiza_trabajador, ",
                    "\n        b.destino_ap_viv,      ",
                    "\n        b.dias_cot_bim,        ",
                    "\n        b.dias_incap_bim,      ",
                    "\n        b.dias_ausent_bim,     ",
                    "\n        a.imp_ap_pat,          ", 
                    "\n        a.imp_am_cre,          ",
                    "\n        a.imp_ren_viv_pgo_ext, ", 
                    "\n        b.marca_sua,           ", 
                    "\n        b.marca_bdnsar,        ",
                    "\n        a.tpo_aclaracion,      ",
                    "\n        a.f_proceso,           ",
                    "\n        b.id_derhab_nuevo,     ", 
                    "\n        c.ap_paterno_af,       ",         
                    "\n        c.ap_materno_af,       ", 
                    "\n        c.nombre_af,           ",
                    "\n        a.aiv_ap_pat,          ",
                    "\n        a.valor_aiv,           ",
                    "\n        a.int_gen_pgo_ext,     ",
                    "\n        a.aiv_gen_pgo_ext,     ", 
                    "\n        a.folio,               ",
                    "\n        a.id_referencia,       ",
                    "\n        a.id_derechohabiente   ",
                    "\n FROM cta_his_pagos a, cta_pag_complemento b, afi_derechohabiente c ",
                    "\n WHERE a.folio = b.folio                          ",
                    "\n AND a.id_referencia = b.id_referencia            ",
                    "\n AND b.id_derechohabiente = c.id_derechohabiente  ",
                    "\n AND a.origen_archivo = ",p_origen_archivo,
                    "\n AND a.result_operacion in (2,3) ",
                    "\n AND a.ind_liquidacion  <> -1 "

   LET v_i_contador_reg = 0
   
   LET v_filler1 = 7 SPACES
   LET v_filler2 = 12 SPACES

   PREPARE con_det_fc FROM v_s_SqlQry
   DECLARE c_det_fc CURSOR FOR  con_det_fc
   FOREACH c_det_fc INTO v_r_pag_det_acl.*  

      --xvi-77
      SELECT count(*)
      INTO   v_cont
      FROM   cta_his_pagos
      WHERE  id_derechohabiente = v_r_pag_det_acl.id_derechohabiente
      AND    folio_sua          = v_r_pag_det_acl.folio_sua
      AND    periodo_pago       = v_r_pag_det_acl.periodo_pago
      AND    f_pago             = v_r_pag_det_acl.f_pago_patron
      AND    nrp                = v_r_pag_det_acl.num_reg_patronal
      AND    cve_ent_receptora  = v_r_pag_det_acl.entidad_receptora
      AND    imp_ap_pat         = v_r_pag_det_acl.imp_ap_pat_inf
      AND    imp_am_cre         = v_r_pag_det_acl.imp_am_cre_inf
      AND    origen_archivo     = p_origen_archivo
      AND    result_operacion   = 1
      AND    ind_liquidacion    = 5

      IF v_cont = 0 THEN    -- SI NO EXISTE, IMPRIME REGISTRO   --xvi-77             
             
         LET v_c_fecha_pag = v_r_pag_det_acl.f_pago_patron USING "yyyymmdd"
         
         -- se obtiene el nss de dispersion
         SELECT nss
         INTO   v_c_nss
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = v_r_pag_det_acl.id_derhab_nuevo
         
         -- si no se encontro, se ponen ceros
         IF v_c_nss IS NULL THEN
            LET v_c_nss = "00000000000"
            --INSERT INTO safre_tmp:tmp_excep_ccnss VALUES (v_r_pag_det_acl.folio,v_r_pag_det_acl.id_referencia,p_origen_archivo)
         ELSE
            LET l_s_cadena_detalle = v_c_tipo_registro,
                                     v_r_pag_det_acl.entidad_receptora        ,
                                     v_r_pag_det_acl.num_reg_patronal         ,
                                     v_filler2                                ,
                                     v_r_pag_det_acl.rfc_patron               ,
                                     v_r_pag_det_acl.periodo_pago             ,
                                     v_r_pag_det_acl.f_pago_patron            USING "yyyymmdd",
                                     v_r_pag_det_acl.folio_sua                USING "&&&&&&",
                                     v_r_pag_det_acl.nss                      ,
                                     v_r_pag_det_acl.rfc_trabajador           ,
                                     v_r_pag_det_acl.curp                     ,
                                     v_r_pag_det_acl.num_cred_infonavit       USING "&&&&&&&&&&",
                                     v_r_pag_det_acl.f_inicio_dcto_cre_inf    USING "yyyymmdd",
                                     v_r_pag_det_acl.num_mov_periodo          USING "&&",
                                     v_r_pag_det_acl.nombre_trabajador        ,
                                     (v_r_pag_det_acl.ultimo_salario_integrado * 100) USING "&&&&&&&",
                                     v_r_pag_det_acl.tipo_trabajador          USING "&",
                                     v_r_pag_det_acl.jornada_semana_reducida  USING "&",
                                     v_r_pag_det_acl.localiza_trabajador      USING "&",
                                     v_r_pag_det_acl.destino_ap_viv           USING "&",
                                     v_r_pag_det_acl.dias_cotizados_bim       USING "&&",
                                     v_r_pag_det_acl.dias_incapacidad_bim     USING "&&",
                                     v_r_pag_det_acl.dias_ausentismo_bim      USING "&&",
                                     v_filler1                                ,
                                     (v_r_pag_det_acl.imp_ap_pat_inf * 100)   USING "&&&&&&&",
                                     v_filler1                                ,
                                     (v_r_pag_det_acl.imp_am_cre_inf * 100)   USING "&&&&&&&",
                                     (v_r_pag_det_acl.imp_ren_viv_pgo_ext * 100) USING "&&&&&&&",
                                     v_r_pag_det_acl.marca_cre_sua            USING "&&",
                                     v_r_pag_det_acl.marca_cre_BDNSAR         USING "&",
                                     v_r_pag_det_acl.diag_acl                 USING "&&",
                                     v_r_pag_det_acl.f_proceso                USING "yyyymmdd",
                                     v_c_nss                           ,
                                     v_r_pag_det_acl.ap_paterno_afore  ,       
                                     v_r_pag_det_acl.ap_materno_afore   , 
                                     v_r_pag_det_acl.nombre_afore      ,
                                     (v_r_pag_det_acl.ap_int_viv * 1000000)   USING "&&&&&&&&&&&&&",
                                     (v_r_pag_det_acl.precio_ap_int_viv * 1000000) USING "&&&&&&&&&",
                                     (v_r_pag_det_acl.int_gen_pag_ext_viv * 100) USING "&&&&&&&",
                                     (v_r_pag_det_acl.num_int_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&"
         
            --se escribe el deatalle en el archivo
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])       
            
            --se suma el importe de registros en estado rechazado
            LET v_r_pag_sum_acl.sum_ap_pat          = v_r_pag_sum_acl.sum_ap_pat          + v_r_pag_det_acl.imp_ap_pat_inf
            LET v_r_pag_sum_acl.sum_am              = v_r_pag_sum_acl.sum_am              + v_r_pag_det_acl.imp_am_cre_inf
            LET v_r_pag_sum_acl.sum_aivs            = v_r_pag_sum_acl.sum_aivs            + v_r_pag_det_acl.ap_int_viv
            LET v_r_pag_sum_acl.sum_int_viv_pag_ext = v_r_pag_sum_acl.sum_int_viv_pag_ext + v_r_pag_det_acl.int_gen_pag_ext_viv
            LET v_r_pag_sum_acl.sum_aivs_pag_ext    = v_r_pag_sum_acl.sum_aivs_pag_ext    + v_r_pag_det_acl.num_int_gen_pgo_ext
            
            --se hace el conteo de registros en estado rechazado   
            LET v_i_contador_reg = v_i_contador_reg + 1
         END IF
         
      END IF  --xvi-77
         
   END FOREACH
      
   --se asigna el total de registros para el suamrio
   LET  v_num_reg_detalle =  v_i_contador_reg

   LET l_s_cadena_sum_acl  = "9",
                             v_i_contador_reg                             USING "&&&&&&&&&",
                             (v_r_pag_sum_acl.sum_ap_pat * 100)           USING "&&&&&&&&&&&&&",         
                             (v_r_pag_sum_acl.sum_am * 100)               USING "&&&&&&&&&&&&&",  --xvi-77
                             (v_r_pag_sum_acl.sum_aivs * 1000000)         USING "&&&&&&&&&&&&&&&&&&",
                             (v_r_pag_sum_acl.sum_int_viv_pag_ext * 100)  USING "&&&&&&&&&&&&&",
                             (v_r_pag_sum_acl.sum_aivs_pag_ext * 1000000) USING "&&&&&&&&&&&&&&&&&&"

     --se escribe el sumario en el archivo
     CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_acl])
END FUNCTION
