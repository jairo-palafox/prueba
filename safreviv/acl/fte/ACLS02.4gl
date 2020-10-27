--=============================================================================
-- Version: 1.0.0
-- Fecha ultima modificacion:
--=============================================================================
--------------------------------------------------------------------------------------------
-- Modulo         => ACL                                                                  --
-- Programa       => ACLS02                                                               --
-- Objetivo       => Generacion del archivo de rechazos de Aclaraciones Sin Cambio de NSS --
-- Autor          => Rubén Haro Castro                                                    --
-- Fecha inicio   => Septiembre 05, 2012                                                  --
-- Actualizado    => Gerardo Alfonso Vega Paredes                                         --
-- Fecha          => 16 de febrero de 2016                                                --
--------------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                                          --
-- Fec Mod.     => 21 de octubre de 2016                                                  --
-- Modificación => Agregar código de rechazo de tabla acl_pag_rechazo y acl_cat_rechazo   --
-- Clave cambio => xvi-141                                                                --
--------------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                                          --
-- Fec Mod.     => 16 de noviembre de 2016.                                               --
-- Modificación => Quitar concepto de sin historico y registros confirmados y sin nss dest--
-- Clave cambio => xvi-141-02                                                             --
------------------------------------------------------------------------------------------------
-- Actualizado  => Gerardo Alfonso Vega Paredes.                                              --
-- Fec Mod.     => 17 de septiembre de 2018.                                                  --
-- Modificación => Adecuar funcionalidad para que lea nueva tabla de rechazos cta_rechazo_acl --
-- Clave cambio => saci2018-67                                                                --
------------------------------------------------------------------------------------------------
DATABASE safre_viv

GLOBALS "ACLG02.4gl"  ---archivo de variables globales proceso_cod, opera_cod

GLOBALS
   DEFINE g_pid            LIKE bat_ctr_proceso.pid, --  ID del proceso
          g_proceso_cod    LIKE cat_proceso.proceso_cod, -- codigo del proceso
          g_opera_cod      LIKE cat_operacion.opera_cod -- codigo de operacion
END GLOBALS

MAIN
DEFINE p_usuario_cod         LIKE seg_usuario.usuario_cod -- clave del usuario firmado
       ,p_i_folio            LIKE dis_preliquida.folio_liquida
       ,p_nombre_archivo     LIKE glo_ctr_archivo.nombre_archivo -- nombre del archivo
       ,r_bandera            INTEGER
       ,v_i_contador_reg     INTEGER
       ,v_mensaje            VARCHAR(255)
       ,v_s_sql              STRING, -- cadena con un enunciado SQL
       v_marca_disposicion   SMALLINT,
       v_edo_maraca          SMALLINT,
       v_caus_marac          SMALLINT,
       v_cod_rechazo         SMALLINT 

   -- se recuperan los parametros de la linea de comandos
   LET p_usuario_cod    = ARG_VAL(1)
   LET g_pid            = ARG_VAL(2)
   LET g_proceso_cod    = ARG_VAL(3)
   LET g_opera_cod      = ARG_VAL(4)
   LET p_i_folio        = ARG_VAL(5)
   LET p_nombre_archivo = ARG_VAL(6)    

   CALL STARTLOG (p_usuario_cod CLIPPED|| ".ACLS02.log")

   -- en el caso contrario se invoca a la función que despliega los registros a seleccionar 
   --CALL f_rechazos (p_i_folio)  --xvi-141
   CALL f_rechazos02 (p_i_folio)

END MAIN

FUNCTION f_rechazos (p_folio)

   DEFINE p_folio    LIKE  tia_det_traspaso.folio,  --folio a desplegar
          v_s_SqlQry STRING                        --variable que almacena la consulta
        
   DEFINE v_r_pag_det_acl RECORD   
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
      diag_acl                 LIKE cta_his_pagos.tpo_aclaracion, --tpo_aclaracion ct_
      f_proceso                LIKE cta_his_pagos.f_proceso,
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv ,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext ,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext
   END RECORD
   
   DEFINE v_r_pag_sum_acl RECORD
      sum_ap_pat          LIKE acl_sum_sc_nss.suma_ap_pat,
      sum_am              LIKE acl_sum_sc_nss.suma_am,
      sum_aivs            DECIMAL(22,6),
      sum_int_viv_pag_ext LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
      sum_aivs_pag_ext    DECIMAL(22,6)
   END RECORD
   
   DEFINE v_imp_ap_fc         LIKE cta_his_pagos.imp_ap_pat,
          v_c_fecha_hoy       VARCHAR(8),
          v_c_nss             CHAR(11),
          l_s_cadena_detalle  STRING,
          l_s_cadena_cza_fc   STRING,
          l_s_cadena_sum_acl  STRING,
          v_v_nom_archi       VARCHAR(100),--STRING, -- nombre del archivo de salida
          v_v_ruta_nomarch    STRING, -- ruta y nombre del archivo de salida
          v_c_ruta_env_acr    LIKE seg_modulo.ruta_envio, -- ruta donde se colocara el archivo
          v_ch_arch_solTransf BASE.CHANNEL, -- manejador de apuntador hacia archivo
          v_c_tipo_registro   VARCHAR (2),
          v_i_contador_reg    INTEGER,
          v_num_reg_detalle   INTEGER,
          v_s_fec_tmp         STRING,
          v_c_fecha_pag       VARCHAR(8),
          v_folio_formato     VARCHAR(9),
          v_archivo_original  VARCHAR(40),
          v_longitud          SMALLINT,
          v_filler1           CHAR(7) ,
          v_filler2           CHAR(12),
          v_filler3           CHAR(17)

   -- se verifica si hay rechazados, de lo contrario no se genera archivo
                           
   SELECT COUNT(*)
   INTO   v_i_contador_reg
--   FROM cta_his_pagos          --saci2018-67 
   FROM   cta_rechazos_acl       --saci2018-67
   WHERE  folio = p_folio
   AND    result_operacion = 2

   -- si no hay rechazados, entonces no se genera archivo
   IF v_i_contador_reg < 1 THEN
      DISPLAY "No existen registros rechazados. No se generará archivo de rechazos."
      EXIT PROGRAM
   END IF

   LET INT_FLAG = 0
   
   LET v_c_tipo_registro = "2"
   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Rechazos", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.SINNSS'
   
   LET v_s_fec_tmp = TODAY  USING "yyyymmdd "
   LET v_c_fecha_hoy = v_s_fec_tmp
                                    
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
   INTO   v_c_ruta_env_acr
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

   -- el nombre del archivo es:
   -- el mismo nombre que el archivo original + _FOLIO_rechazoACL.SINNSS
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_folio

   DISPLAY "archivo ", v_archivo_original

   -- longitud del nombre del archivo original (menus 5 para quitar .fort)
   LET v_longitud = LENGTH(v_archivo_original CLIPPED) - 5

   LET v_folio_formato = p_folio USING "&&&&&&&&&"
 
   DISPLAy "longitud ", v_longitud
 
   LET v_v_nom_archi    = v_archivo_original[1,v_longitud] || "_" || v_folio_formato || "_rechazoACL.SINNSS"
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

   -- se inicializan variables
   LET v_r_pag_sum_acl.sum_ap_pat          = 0
   LET v_r_pag_sum_acl.sum_am              = 0
   LET v_r_pag_sum_acl.sum_aivs            = 0
   LET v_r_pag_sum_acl.sum_int_viv_pag_ext = 0
   LET v_r_pag_sum_acl.sum_aivs_pag_ext    = 0       
   
   --se consulta el detalle 
   LET v_s_SqlQry = "\n SELECT a.cve_ent_receptora,   ", 
                    "\n        a.nrp,                 ",
                    "\n        b.rfc_patron,          ",                
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
                    "\n        a.aiv_ap_pat,          ",
                    "\n        a.valor_aiv,           ",
                    "\n        a.int_gen_pgo_ext,     ",
                    "\n        a.aiv_gen_pgo_ext      ", 
--                    "\n FROM  cta_his_pagos a, cta_pag_complemento b, afi_derechohabiente c ",     --saci2018-67
                    "\n FROM  cta_rechazos_acl a, cta_pag_complemento b, afi_derechohabiente c ",    --saci2018-67
                    "\n WHERE a.folio=b.folio                                               ",
                    "\n AND   a.id_derechohabiente = b.id_derechohabiente                   ",
                    "\n AND   b.id_derechohabiente = c.id_derechohabiente                   ",
                    "\n AND a.folio = ",p_folio,
                    "\n AND a.id_referencia = b.id_referencia ",
                    "\n and a.result_operacion IN (2,3)            "                


   --DISPLAY " v_s_SqlQry ",v_s_SqlQry
   LET v_i_contador_reg = 0
   
   LET v_filler1 =  7 SPACES
   LET v_filler2 = 12 SPACES 
   LET v_filler3 = 17 SPACES
   
   PREPARE con_det_fc FROM v_s_SqlQry
   DECLARE c_det_fc CURSOR FOR  con_det_fc
   FOREACH c_det_fc INTO v_r_pag_det_acl.*  
             
      LET v_c_fecha_pag = v_r_pag_det_acl.f_pago_patron USING "yyyymmdd"
      
      -- se le quitan los decimales al monto de aportacion
      --LET v_imp_ap_fc   = v_r_pag_det_acl.imp_ap_pat_inf * 100
      
      LET l_s_cadena_detalle = v_c_tipo_registro,
                               v_r_pag_det_acl.entidad_receptora                ,
                               v_r_pag_det_acl.num_reg_patronal                 ,
                               v_filler2                                        ,
                               v_r_pag_det_acl.rfc_patron                       ,
                               v_r_pag_det_acl.periodo_pago                     ,
                               v_r_pag_det_acl.f_pago_patron                    USING "yyyymmdd",
                               v_r_pag_det_acl.folio_sua                        USING "&&&&&&",
                               v_r_pag_det_acl.nss                              ,
                               v_r_pag_det_acl.rfc_trabajador                   ,
                               v_r_pag_det_acl.curp                             ,
                               v_r_pag_det_acl.num_cred_infonavit               USING "&&&&&&&&&&",
                               v_r_pag_det_acl.f_inicio_dcto_cre_inf            USING "yyyymmdd",
                               v_r_pag_det_acl.num_mov_periodo                  USING "&&",
                               v_r_pag_det_acl.nombre_trabajador                ,
                               (v_r_pag_det_acl.ultimo_salario_integrado * 100) USING "&&&&&&&",
                               v_r_pag_det_acl.tipo_trabajador                  USING "&",
                               v_r_pag_det_acl.jornada_semana_reducida          USING "&",
                               v_r_pag_det_acl.localiza_trabajador              USING "&",
                               v_r_pag_det_acl.destino_ap_viv                   USING "&",
                               v_r_pag_det_acl.dias_cotizados_bim               USING "&&",
                               v_r_pag_det_acl.dias_incapacidad_bim             USING "&&",
                               v_r_pag_det_acl.dias_ausentismo_bim              USING "&&",
                               v_filler1                                        ,
                               (v_r_pag_det_acl.imp_ap_pat_inf * 100)           USING "&&&&&&&",
                               v_filler1                                        ,
                               (v_r_pag_det_acl.imp_am_cre_inf * 100)           USING "&&&&&&&",
                               (v_r_pag_det_acl.imp_ren_viv_pgo_ext * 100)      USING "&&&&&&&",
                               v_r_pag_det_acl.marca_cre_sua                    USING "&&",
                               v_r_pag_det_acl.marca_cre_BDNSAR                 USING "&",
                               v_r_pag_det_acl.diag_acl                         USING "&&",
                               v_r_pag_det_acl.f_proceso                        USING "yyyymmdd",
                               (v_r_pag_det_acl.ap_int_viv * 1000000)           USING "&&&&&&&&&&&&&&&",
                               (v_r_pag_det_acl.precio_ap_int_viv * 1000000)    USING "&&&&&&&&&&&",
                               (v_r_pag_det_acl.int_gen_pag_ext_viv * 100)      USING "&&&&&&&",
                               (v_r_pag_det_acl.num_int_gen_pgo_ext * 1000000)  USING "&&&&&&&&&&&&&",
                               v_filler3

      --se escribe el deatalle en el archivo
      CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])       

      --se suma el importe de registros en estado rechazado
      LET v_r_pag_sum_acl.sum_ap_pat          = v_r_pag_sum_acl.sum_ap_pat          + v_r_pag_det_acl.imp_ap_pat_inf
      LET v_r_pag_sum_acl.sum_am              = v_r_pag_sum_acl.sum_am              + v_r_pag_det_acl.imp_am_cre_inf 
      LET v_r_pag_sum_acl.sum_aivs            = v_r_pag_sum_acl.sum_aivs            + v_r_pag_det_acl.ap_int_viv
      LET v_r_pag_sum_acl.sum_int_viv_pag_ext = v_r_pag_sum_acl.sum_int_viv_pag_ext + v_r_pag_det_acl.int_gen_pag_ext_viv 
      LET v_r_pag_sum_acl.sum_aivs_pag_ext    = v_r_pag_sum_acl.sum_aivs_pag_ext    + v_r_pag_det_acl.num_int_gen_pgo_ext 

     -- DISPLAY "@ v_r_pag_sum_acl.sum_aivs: ",v_r_pag_sum_acl.sum_aivs
      --se hace el conteo de registros en estado rechazado   
      LET v_i_contador_reg = v_i_contador_reg + 1
   END FOREACH
      
   --se asigna el total de registros para el suamrio
   LET  v_num_reg_detalle =  v_i_contador_reg

   -- se quitan los decimales de los totales
   LET v_r_pag_sum_acl.sum_ap_pat          = v_r_pag_sum_acl.sum_ap_pat          * 100
   LET v_r_pag_sum_acl.sum_am              = v_r_pag_sum_acl.sum_am              * 100
   LET v_r_pag_sum_acl.sum_aivs            = v_r_pag_sum_acl.sum_aivs            * 1000000
   LET v_r_pag_sum_acl.sum_int_viv_pag_ext = v_r_pag_sum_acl.sum_int_viv_pag_ext * 100
   LET v_r_pag_sum_acl.sum_aivs_pag_ext    = v_r_pag_sum_acl.sum_aivs_pag_ext    * 1000000

  -- DISPLAY "@@ v_r_pag_sum_acl.sum_aivs: ",v_r_pag_sum_acl.sum_aivs
   LET l_s_cadena_sum_acl  = "9"                                                      ,
                             v_i_contador_reg                    USING "&&&&&&&&&"   ,
                             v_r_pag_sum_acl.sum_ap_pat          USING "&&&&&&&&&&&&&",         
                             v_r_pag_sum_acl.sum_am              USING "&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_aivs            USING "&&&&&&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_int_viv_pag_ext USING "&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_aivs_pag_ext    USING "&&&&&&&&&&&&&&&&&&"

     --se escribe el sumario en el archivo
     CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_acl])
END FUNCTION


FUNCTION f_rechazos02(p_folio)
   DEFINE 
      p_folio LIKE  tia_det_traspaso.folio,
      v_s_SqlQry STRING
          
   DEFINE v_r_pag_det_acl RECORD   
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
      diag_acl                 LIKE cta_his_pagos.tpo_aclaracion, --tpo_aclaracion ct_
      f_proceso                LIKE cta_his_pagos.f_proceso,
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv ,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext ,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      result_operacion         LIKE cta_his_pagos.result_operacion,
      descripcion              CHAR(16),
      desc_rechazo             CHAR(30)  --xvi-141
   END RECORD
         
   DEFINE v_r_pag_sum_acl RECORD -- registro para el sumario
      sum_ap_pat          LIKE acl_sum_sc_nss.suma_ap_pat,
      sum_am              LIKE acl_sum_sc_nss.suma_am,
      sum_aivs            DECIMAL(22,6),
      sum_int_viv_pag_ext LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
      sum_aivs_pag_ext    DECIMAL(22,6)
   END RECORD
   
   DEFINE 
      v_imp_ap_fc         LIKE cta_his_pagos.imp_ap_pat,
      v_c_fecha_hoy       VARCHAR(8),
      v_c_nss             CHAR(11),
      l_s_cadena_detalle  STRING,
      l_s_cadena_cza_fc   STRING,
      l_s_cadena_sum_acl  STRING,
      v_v_nom_archi       VARCHAR(100),
      v_v_ruta_nomarch    STRING, 
      v_c_ruta_env_acr    LIKE seg_modulo.ruta_envio,
      v_ch_arch_solTransf BASE.CHANNEL, 
      v_c_tipo_registro   VARCHAR (2),
      v_i_contador_reg    DECIMAL(9,0),
      v_registros_01      DECIMAL(9,0),
      v_num_reg_detalle   INTEGER,
      v_s_fec_tmp         STRING,
      v_c_fecha_pag       VARCHAR(8),
      v_folio_formato     VARCHAR(9),
      v_archivo_original  VARCHAR(40),
      v_longitud          SMALLINT,
      v_filler1           CHAR(7) ,
      v_filler2           CHAR(12),
      v_filler3           CHAR(17)

   -- se verifica si hay rechazados, de lo contrario no se genera archivo
                           
   SELECT COUNT(*)
   INTO   v_i_contador_reg
--   FROM   cta_his_pagos        --saci2018-67
   FROM   cta_rechazos_acl       --saci2018-67   
   WHERE  folio = p_folio
   AND    result_operacion = 2

   SELECT COUNT(*)
   INTO   v_registros_01
   FROM   cta_his_pagos        --saci2018-67   
--   FROM   cta_rechazos_acl       --sacie2018-67
   WHERE  folio = p_folio
   AND    result_operacion = 1
   AND    ind_liquidacion  = 4

   -- si no hay rechazados, entonces no se genera archivo   
   IF v_i_contador_reg < 1 AND v_registros_01 < 1 THEN
      DISPLAY "No existe registros rechazados. No se genera archivo de rechazos"
      EXIT PROGRAM   
   END IF

   LET INT_FLAG = 0
   
   LET v_c_tipo_registro = "2"
   LET v_v_nom_archi = TIME
   LET v_v_nom_archi = "Rechazos", v_v_nom_archi[4,5], v_v_nom_archi[7,8],'.SINNSS_EXT'
   
   LET v_s_fec_tmp = TODAY  USING "yyyymmdd "
   LET v_c_fecha_hoy = v_s_fec_tmp
                                    
   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio 
     INTO v_c_ruta_env_acr
     FROM seg_modulo
    WHERE modulo_cod = 'acl'

   -- el nombre del archivo es:
   -- el mismo nombre que el archivo original + _FOLIO_rechazoACL.SINNSS
   SELECT nombre_archivo
   INTO   v_archivo_original
   FROM   glo_ctr_archivo
   WHERE  proceso_cod = g_proceso_cod
   AND    folio = p_folio

   DISPLAY "archivo ", v_archivo_original

   -- longitud del nombre del archivo original (menus 5 para quitar .fort)
   LET v_longitud = LENGTH(v_archivo_original CLIPPED) - 5

   LET v_folio_formato = p_folio USING "&&&&&&&&&"
 
   DISPLAy "longitud ", v_longitud
 
   LET v_v_nom_archi    = v_archivo_original[1,v_longitud] || "_" || v_folio_formato || "_rechazoACL.SINNSS_EXT"
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

   -- se inicializan variables
   LET v_r_pag_sum_acl.sum_ap_pat          = 0
   LET v_r_pag_sum_acl.sum_am              = 0
   LET v_r_pag_sum_acl.sum_aivs            = 0
   LET v_r_pag_sum_acl.sum_int_viv_pag_ext = 0
   LET v_r_pag_sum_acl.sum_aivs_pag_ext    = 0       
   
   --se consulta el detalle 
   LET v_s_SqlQry = " SELECT a.cve_ent_receptora,   ", 
                    "        a.nrp,                 ",
                    "        b.rfc_patron,          ",                
                    "        a.periodo_pago,        ", 
                    "        a.f_pago,              ",
                    "        a.folio_sua,           ",
                    "        c.nss,                 ",      
                    "        c.rfc,                 ",      
                    "        c.curp,                ",      
                    "        a.num_crd_ifv,         ",
                    "        b.f_ini_desc_crd_ifv,  ",
                    "        b.num_mov_periodo,     ",
                    "        c.nombre_imss,         ",
                    "        b.ult_sdi,             ",
                    "        b.tpo_trabajador,      ",
                    "        b.jornada,             ",
                    "        a.localiza_trabajador, ",
                    "        b.destino_ap_viv,      ", 
                    "        b.dias_cot_bim,        ",
                    "        b.dias_incap_bim,      ",
                    "        b.dias_ausent_bim,     ",
                    "        a.imp_ap_pat,          ", 
                    "        a.imp_am_cre,          ",
                    "        a.imp_ren_viv_pgo_ext, ",
                    "        b.marca_sua,           ", 
                    "        b.marca_bdnsar,        ",
                    "        a.tpo_aclaracion,      ",
                    "        a.f_proceso,           ",
                    "        a.aiv_ap_pat,          ",
                    "        a.valor_aiv,           ",
                    "        a.int_gen_pgo_ext,     ",
                    "        a.aiv_gen_pgo_ext,     ",
                    "        result_operacion,      ",  --g-
                    "        CASE result_operacion  ",  --g-
                    "           WHEN 2 THEN ' '     ",  --xvi-141-02 
                    "           ELSE ' '            ",  --xvi-141-02
--                    "           WHEN 2 THEN 'SIN HISTORICO'  ",  --g- --xvi-141-02
--                    "           ELSE 'SIN NSS DESTINO'       ",       --xvi-141-02 
                    "         END,                           ", 
                    "         d.descripcion                  ",         --xvi-141
--                    " FROM  cta_his_pagos a                  ",         --xvi-141    --saci2018-67
                    " FROM  cta_rechazos_acl a                  ",         --xvi-141   --saci2018-67
                    "          JOIN cta_pag_complemento b    ",         --xvi-141
                    "             ON (a.folio = b.folio      ",         --xvi-141
                    "                 AND a.id_referencia = b.id_referencia                    ", --xvi-141
                    "                 AND a.id_derechohabiente = b.id_derechohabiente)         ", --xvi-141
                    "          JOIN afi_derechohabiente c                                      ", --xvi-141
                    "             ON (b.id_derechohabiente = c.id_derechohabiente)             ", --xvi-141
                    "       LEFT OUTER                                                         ", --xvi-141
                    "          JOIN acl_pag_rechazo r                                          ", --xvi-141
                    "             ON (a.folio = r.folio AND a.id_referencia = r.id_referencia) ", --xvi-141
                    "       LEFT OUTER                                                         ", --xvi-141
                    "          JOIN acl_cat_rechazo d                                          ", --xvi-141
                    "             ON (r.codigo_rechazo = d.codigo_rechazo)                     ", --xvi-141
                    " WHERE a.folio = ",p_folio,             --xvi-141
                    " AND   a.result_operacion IN (2,3)  "   --xvi-141

{xvi-141-02                    " UNION ALL ",
                    " SELECT a.cve_ent_receptora,   ", 
                    "        a.nrp,                 ",
                    "        b.rfc_patron,          ",                
                    "        a.periodo_pago,        ", 
                    "        a.f_pago,              ",
                    "        a.folio_sua,           ",
                    "        c.nss,                 ",      
                    "        c.rfc,                 ",      
                    "        c.curp,                ",      
                    "        a.num_crd_ifv,         ",
                    "        b.f_ini_desc_crd_ifv,  ",
                    "        b.num_mov_periodo,     ",
                    "        c.nombre_imss,         ",
                    "        b.ult_sdi,             ",
                    "        b.tpo_trabajador,      ",
                    "        b.jornada,             ",
                    "        a.localiza_trabajador, ",
                    "        b.destino_ap_viv,      ", 
                    "        b.dias_cot_bim,        ",
                    "        b.dias_incap_bim,      ",
                    "        b.dias_ausent_bim,     ",
                    "        a.imp_ap_pat,          ", 
                    "        a.imp_am_cre,          ",
                    "        a.imp_ren_viv_pgo_ext, ",
                    "        b.marca_sua,           ", 
                    "        b.marca_bdnsar,        ",
                    "        a.tpo_aclaracion,      ",
                    "        a.f_proceso,           ",
                    "        a.aiv_ap_pat,          ",
                    "        a.valor_aiv,           ",
                    "        a.int_gen_pgo_ext,     ",
                    "        a.aiv_gen_pgo_ext,     ",
                    "        result_operacion,      ",  --g-
                    "        'YA LIQUIDADO',        ",  --g-
                    "        d.descripcion          ",    --xvi-141                    
                    " FROM  cta_his_pagos a         ",    --xvi-141 
                    "    JOIN cta_pag_complemento b ",    --xvi-141
                    "       ON (a.folio = b.folio   ",    --xvi-141
                    "           AND a.id_referencia = b.id_referencia                     ",  --xvi-141
                    "           AND a.id_derechohabiente = b.id_derechohabiente)          ",  --xvi-141
                    "    JOIN afi_derechohabiente c                                       ",  --xvi-141
                    "       ON (b.id_derechohabiente = c.id_derechohabiente)              ",  --xvi-141
                    " LEFT OUTER                                                          ",  --xvi-141
                    "    JOIN acl_pag_rechazo r                                           ",  --xvi-141
                    "       ON (a.folio = r.folio  AND a.id_referencia = r.id_referencia) ",  --xvi-141
                    " LEFT OUTER                                                          ",  --xvi-141
                    "    JOIN acl_cat_rechazo d                                           ",  --xvi-141
                    "       ON (r.codigo_rechazo = d.codigo_rechazo)                      ",  --xvi-141
                    " WHERE a.folio = ",p_folio,       --xvi-141
                    " AND   a.result_operacion = 1 ",  --xvi-141
                    " AND   a.ind_liquidacion  = 4 "   --xvi-141
--xvi-141-02 }
                    
   LET v_i_contador_reg = 0
   
   LET v_filler1 =  7 SPACES
   LET v_filler2 = 12 SPACES 
   LET v_filler3 = 17 SPACES
   
   PREPARE cla_det FROM v_s_SqlQry
   DECLARE cur_det CURSOR FOR cla_det
   	
   FOREACH cur_det INTO v_r_pag_det_acl.*  
             
      LET v_c_fecha_pag = v_r_pag_det_acl.f_pago_patron USING "yyyymmdd"
      
      IF v_r_pag_det_acl.desc_rechazo IS NULL THEN            --xvi-141-02
         LET v_r_pag_det_acl.desc_rechazo = "13-SIN LQINFO Y HAY SALIDA"
      END IF
      
      LET l_s_cadena_detalle = v_c_tipo_registro,
                               v_r_pag_det_acl.entidad_receptora                ,
                               v_r_pag_det_acl.num_reg_patronal                 ,
                               v_filler2                                        ,
                               v_r_pag_det_acl.rfc_patron                       ,
                               v_r_pag_det_acl.periodo_pago                     ,
                               v_r_pag_det_acl.f_pago_patron                    USING "yyyymmdd",
                               v_r_pag_det_acl.folio_sua                        USING "&&&&&&",
                               v_r_pag_det_acl.nss                              ,
                               v_r_pag_det_acl.rfc_trabajador                   ,
                               v_r_pag_det_acl.curp                             ,
                               v_r_pag_det_acl.num_cred_infonavit               USING "&&&&&&&&&&",
                               v_r_pag_det_acl.f_inicio_dcto_cre_inf            USING "yyyymmdd",
                               v_r_pag_det_acl.num_mov_periodo                  USING "&&",
                               v_r_pag_det_acl.nombre_trabajador                ,
                               (v_r_pag_det_acl.ultimo_salario_integrado * 100) USING "&&&&&&&",
                               v_r_pag_det_acl.tipo_trabajador                  USING "&",
                               v_r_pag_det_acl.jornada_semana_reducida          USING "&",
                               v_r_pag_det_acl.localiza_trabajador              USING "&",
                               v_r_pag_det_acl.destino_ap_viv                   USING "&",
                               v_r_pag_det_acl.dias_cotizados_bim               USING "&&",
                               v_r_pag_det_acl.dias_incapacidad_bim             USING "&&",
                               v_r_pag_det_acl.dias_ausentismo_bim              USING "&&",
                               v_filler1                                        ,
                               (v_r_pag_det_acl.imp_ap_pat_inf * 100)           USING "&&&&&&&",
                               v_filler1                                        ,
                               (v_r_pag_det_acl.imp_am_cre_inf * 100)           USING "&&&&&&&",
                               (v_r_pag_det_acl.imp_ren_viv_pgo_ext * 100)      USING "&&&&&&&",
                               v_r_pag_det_acl.marca_cre_sua                    USING "&&",
                               v_r_pag_det_acl.marca_cre_BDNSAR                 USING "&",
                               v_r_pag_det_acl.diag_acl                         USING "&&",
                               v_r_pag_det_acl.f_proceso                        USING "yyyymmdd",
                               (v_r_pag_det_acl.ap_int_viv * 1000000)           USING "&&&&&&&&&&&&&&&",
                               (v_r_pag_det_acl.precio_ap_int_viv * 1000000)    USING "&&&&&&&&&&&",
                               (v_r_pag_det_acl.int_gen_pag_ext_viv * 100)      USING "&&&&&&&",
                               (v_r_pag_det_acl.num_int_gen_pgo_ext * 1000000)  USING "&&&&&&&&&&&&&",
                               v_r_pag_det_acl.result_operacion USING "&"," ",
                               v_r_pag_det_acl.descripcion,"  ", --xvi-141
                               v_r_pag_det_acl.desc_rechazo      --xvi-141
--                               v_filler3

      --se escribe el deatalle en el archivo
      CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])       

      --se suma el importe de registros en estado rechazado
      LET v_r_pag_sum_acl.sum_ap_pat          = v_r_pag_sum_acl.sum_ap_pat          + v_r_pag_det_acl.imp_ap_pat_inf
      LET v_r_pag_sum_acl.sum_am              = v_r_pag_sum_acl.sum_am              + v_r_pag_det_acl.imp_am_cre_inf 
      LET v_r_pag_sum_acl.sum_aivs            = v_r_pag_sum_acl.sum_aivs            + v_r_pag_det_acl.ap_int_viv
      LET v_r_pag_sum_acl.sum_int_viv_pag_ext = v_r_pag_sum_acl.sum_int_viv_pag_ext + v_r_pag_det_acl.int_gen_pag_ext_viv 
      LET v_r_pag_sum_acl.sum_aivs_pag_ext    = v_r_pag_sum_acl.sum_aivs_pag_ext    + v_r_pag_det_acl.num_int_gen_pgo_ext 

     -- DISPLAY "@ v_r_pag_sum_acl.sum_aivs: ",v_r_pag_sum_acl.sum_aivs
      --se hace el conteo de registros en estado rechazado   
      LET v_i_contador_reg = v_i_contador_reg + 1
   END FOREACH
      
   --se asigna el total de registros para el suamrio
   LET  v_num_reg_detalle =  v_i_contador_reg

   -- se quitan los decimales de los totales
   LET v_r_pag_sum_acl.sum_ap_pat          = v_r_pag_sum_acl.sum_ap_pat          * 100
   LET v_r_pag_sum_acl.sum_am              = v_r_pag_sum_acl.sum_am              * 100
   LET v_r_pag_sum_acl.sum_aivs            = v_r_pag_sum_acl.sum_aivs            * 1000000
   LET v_r_pag_sum_acl.sum_int_viv_pag_ext = v_r_pag_sum_acl.sum_int_viv_pag_ext * 100
   LET v_r_pag_sum_acl.sum_aivs_pag_ext    = v_r_pag_sum_acl.sum_aivs_pag_ext    * 1000000

  -- DISPLAY "@@ v_r_pag_sum_acl.sum_aivs: ",v_r_pag_sum_acl.sum_aivs
   LET l_s_cadena_sum_acl  = "9"                                                      ,
                             v_i_contador_reg                    USING "&&&&&&&&&"   ,
                             v_r_pag_sum_acl.sum_ap_pat          USING "&&&&&&&&&&&&&",         
                             v_r_pag_sum_acl.sum_am              USING "&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_aivs            USING "&&&&&&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_int_viv_pag_ext USING "&&&&&&&&&&&&&",
                             v_r_pag_sum_acl.sum_aivs_pag_ext    USING "&&&&&&&&&&&&&&&&&&"

     --se escribe el sumario en el archivo
     CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_acl])
END FUNCTION
