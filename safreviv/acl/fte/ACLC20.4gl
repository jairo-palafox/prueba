-----------------------------------------------------------------------------------------
-- Modulo        => ACL
-- Programa      => ACLC20
-- Objetivo      => Archivo y reporte de rechazos disscnss, disccnss y disccnom.
-- Autor         => GERARDO ALFONSO VEGA PAREDES.
-- Fecha inicio  => 24 de Septiembre de 2018.
-- Requerimiento => SACI2018-67
-- Clave cambio  => saci2018-67
-----------------------------------------------------------------------------------------
-- Modificación =>  Generar nuevos archivos con causal al final de la liena
-- Fehca        =>  21 de Octubre de 2018.
-- Autor        =>  GERARDO ALFONSO VEGA PAREDES.
-- Clave cambio =>  saci2018-67-02
-- Descripción  =>
-----------------------------------------------------------------------------------------

DATABASE safre_viv

GLOBALS

   DEFINE
      p_usuario     LIKE seg_usuario.usuario_cod,
      p_pid         DECIMAL (9,0),
      p_proceso_cod LIKE cat_proceso.proceso_cod,
      p_opera_cod   LIKE cat_operacion.opera_cod,
      p_nom_archivo LIKE glo_ctr_archivo.nombre_archivo,
      r_bandera     SMALLINT,
      p_tpo_archivo INTEGER,
      p_folio       INTEGER

   DEFINE  
      v_ventana ui.Window,
      v_forma   ui.form,
      p_ventana STRING, 
      g_usuario_cod LIKE seg_usuario.usuario_cod,
      v_i_cont_registros INTEGER
      
   DEFINE v_arr_causales DYNAMIC ARRAY OF RECORD -- Arreglo dinamico para las cuenstas causales encontradas
      v_causal           LIKE cta_his_pagos.tpo_aclaracion,
      v_causal_des       LIKE pag_tpo_aclaracion.aclaracion_descripcion,
      v_tot_causal       INTEGER,
      v_imp_ap           DECIMAL (22,2), 
      v_aiv_ap_pat       DECIMAL (18,6),
      v_amp_am           DECIMAL (22,2),
      v_int_gen_pgo_ext  DECIMAL (22,2),
      v_aiv_gen_pgo_ext  DECIMAL (18,6),
      v_imp_ap_am        DECIMAL (22,2),
      v_porcentaje       DECIMAL (16,4)
   END RECORD

   DEFINE g_total_causales RECORD
      total_registros INTEGER,
      imp_ap_pat DECIMAL(12,2),
      imp_am_cre DECIMAL(12,2),
      aiv_ap_pat DECIMAL(18,6)
   END RECORD        
   
   DEFINE v_arr_totales RECORD
      v_tot_causales_gral   DECIMAL (9,0),
      v_tot_imp_ap          DECIMAL (22,2),
      v_tot_aiv_ap_pat      DECIMAL (18,6),
      v_tot_imp_am          DECIMAL (22,2),
      v_tot_int_gen_pgo_ext DECIMAL (22,2),
      v_tot_aiv_gen_pgo_ext DECIMAL (18,6),
      v_tot_imp_apam        DECIMAL (22,2),
      v_tot_porcentajes     DECIMAL(16,4),
      v_aux                 DECIMAL
   END RECORD

   DEFINE g_arr_causales_rechazo DYNAMIC ARRAY OF RECORD
      causal_rechazo VARCHAR(60),
      total_registros INTEGER,
      imp_ap_pat decimal(12,2),
      imp_am_cre decimal(12,2),
      aiv_ap_pat decimal(18,6)
   END RECORD

   DEFINE g_total_causales RECORD
      total_registros INTEGER,
      imp_ap_pat DECIMAL(12,2),
      imp_am_cre DECIMAL(12,2),
      aiv_ap_pat DECIMAL(18,6)
   END RECORD
   
   DEFINE v_sql   STRING
   DEFINE v_sql02 STRING

   DEFINE v_origen_archivo SMALLINT

END GLOBALS

MAIN
   
   --Asignación de parametros generales
   LET p_usuario     = ARG_VAL(1)
   LET p_pid         = ARG_VAL(2)
   LET p_proceso_cod = ARG_VAL(3)
   LET p_opera_cod   = ARG_VAL(4)
   LET p_nom_archivo = ARG_VAL(5)
   LET p_tpo_archivo = ARG_VAL(6)
   LET p_folio       = ARG_VAL(7)

   DISPLAY "p_usuario     ",p_usuario 
   DISPLAY "p_pid         ",p_pid         
   DISPLAY "p_proceso_cod ",p_proceso_cod
   DISPLAY "p_opera_cod   ",p_opera_cod
   DISPLAY "p_nom_archivo ",p_nom_archivo
   DISPLAY "p_tpo_archivo ",p_tpo_archivo
   DISPLAY "p_folio       ",p_folio

   CALL fn_genera_archivo()
   CALL fn_genera_reporte()
   CALL fn_genera_archivo02()  --saci2018-67-02

   --Finaliza la operación
   CALL fn_actualiza_opera_fin(p_pid, p_proceso_cod, p_opera_cod)
   RETURNING r_bandera
  
   IF r_bandera = 0 THEN 
      DISPLAY "Se ha realizado la generación del archivo."
      EXIT PROGRAM 
   ELSE --Si ocurrió error 
      CALL fn_error_opera(p_pid, p_proceso_cod, p_opera_cod) RETURNING r_bandera
      CALL fn_desplega_inc_operacion(r_bandera)
      EXIT PROGRAM 
   END IF

END MAIN

FUNCTION fn_genera_archivo()

   DEFINE v_i_inicio_for    INTEGER
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)

   DEFINE reg_det_scnss RECORD
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
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv ,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      rechazo                  CHAR(32)
   END RECORD

   DEFINE reg_det_ccnss RECORD
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
      id_derhab_nuevo          DECIMAL (9,0),--LIKE cta_pag_complemento.id_derhab_nuevo,
      ap_paterno_afore         LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_afore         LIKE afi_derechohabiente.ap_materno_af,
      nombre_afore             LIKE afi_derechohabiente.nombre_af,
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      rechazo                  CHAR(32)
   END RECORD

   DEFINE reg_sum_scnss RECORD
      sum_ap_pat          LIKE acl_sum_sc_nss.suma_ap_pat,
      sum_am              LIKE acl_sum_sc_nss.suma_am,
      sum_aivs            DECIMAL(22,6),
      sum_int_viv_pag_ext LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
      sum_aivs_pag_ext    DECIMAL(22,6)
   END RECORD   
   
   DEFINE
      v_filler1          CHAR(7),
      v_filler2          CHAR(12),
      v_filler3          CHAR(17),
      v_ruta_envio       LIKE seg_modulo.ruta_envio,
      v_ruta_nomarch     STRING,
      v_c_fecha_pag      VARCHAR(8),
      l_s_cadena_detalle STRING,
      l_s_cadena_sum_acl STRING,
      v_c_tipo_registro  VARCHAR(2),
      v_i_contador_reg   INTEGER,
      v_num_reg_detalle  INTEGER,
      v_c_nss            CHAR(11),
      v_fecha            DATE,
      v_id_derechohabiente DECIMAL(9,0),
      v_existe_liq       SMALLINT,
      v_ch_arch_solTransf BASE.CHANNEL  -- manejador de apuntador hacia archivo

      WHENEVER ERROR CONTINUE
      DROP TABLE tmp_rechazos_acl
      WHENEVER ERROR STOP
      
      CREATE TEMP table tmp_rechazos_acl
        (
          rechazo    CHAR(32),
          imp_ap_pat DECIMAL(12,2),
          imp_am_cre DECIMAL(12,2),
          aiv_ap_pat DECIMAL(18,6)           
        );      

   LET v_i_inicio_for = 1

   LET v_nom_reporte = p_usuario     CLIPPED,"-ACLC20-",
                       p_pid         USING "&&&&&", "-",
                       p_proceso_cod USING "&&&&&","-",
                       1             USING "&&&&&"

   CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

   LET v_c_tipo_registro = "2"

   LET v_genero_reporte_pdf = FALSE

   LET v_fecha = TODAY

   IF p_tpo_archivo = 1 THEN
--      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disscnss_rechazoACL"
      LET v_nom_reporte = v_fecha USING "YYYYMMDD","-RECH.disscnss"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 5
      CALL llena_scnss()
   END IF
   IF p_tpo_archivo = 2 THEN
--      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disccnss_rechazoACL"
      LET v_nom_reporte = v_fecha USING "YYYYMMDD","-RECH.disccnss"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 6   
      CALL llena_ccnss()
   END IF
   IF p_tpo_archivo = 3 THEN
--      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disccnom_rechazoACL"
      LET v_nom_reporte = v_fecha USING "YYYYMMDD","-RECH.diccncno"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 7      
      CALL llena_ccnss()
   END IF

   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_filler1 =  7 SPACES
   LET v_filler2 = 12 SPACES
   LET v_filler3 = 17 SPACES
   LET reg_sum_scnss.sum_ap_pat          = 0
   LET reg_sum_scnss.sum_am              = 0
   LET reg_sum_scnss.sum_aivs            = 0
   LET reg_sum_scnss.sum_int_viv_pag_ext = 0
   LET reg_sum_scnss.sum_aivs_pag_ext    = 0

   LET v_id_derechohabiente = 0

   IF p_tpo_archivo = 1 THEN
   	   
      PREPARE cla_rechazo_scnss FROM v_sql
      DECLARE cur_rechazo_scnss CURSOR FOR cla_rechazo_scnss
      
      FOREACH cur_rechazo_scnss INTO reg_det_scnss.*

         LET v_existe_liq = 0
         LET v_id_derechohabiente = 0

         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = reg_det_scnss.nss
         IF v_id_derechohabiente IS NULL THEN
         	 LET v_id_derechohabiente = 0
         END IF

         SELECT COUNT(*)
         INTO   v_existe_liq
         FROM   cta_his_pagos
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    folio_sua          = reg_det_scnss.folio_sua
         AND    periodo_pago       = reg_det_scnss.periodo_pago
         AND    f_pago             = reg_det_scnss.f_pago_patron
         AND    nrp                = reg_det_scnss.num_reg_patronal
         AND    ind_liquidacion not in (1,-1)
         AND    result_operacion <> 2
         AND    f_proceso < "11/27/2018"
         
         IF v_existe_liq = 0 THEN

      	    INSERT INTO tmp_rechazos_acl VALUES (reg_det_scnss.rechazo,
      	                                         reg_det_scnss.imp_ap_pat_inf,
                                                 reg_det_scnss.imp_am_cre_inf,
                                                 reg_det_scnss.ap_int_viv)
            
            IF reg_det_scnss.imp_ap_pat_inf      IS NULL THEN LET reg_det_scnss.imp_ap_pat_inf      = 0 END IF
            IF reg_det_scnss.imp_am_cre_inf      IS NULL THEN LET reg_det_scnss.imp_am_cre_inf      = 0 END IF
            IF reg_det_scnss.ap_int_viv          IS NULL THEN LET reg_det_scnss.ap_int_viv          = 0 END IF
            IF reg_det_scnss.int_gen_pag_ext_viv IS NULL THEN LET reg_det_scnss.int_gen_pag_ext_viv = 0 END IF
            IF reg_det_scnss.num_int_gen_pgo_ext IS NULL THEN LET reg_det_scnss.num_int_gen_pgo_ext = 0 END IF
      	    
            LET v_c_fecha_pag = reg_det_scnss.f_pago_patron USING "yyyymmdd"
            
            LET l_s_cadena_detalle = v_c_tipo_registro,
                                     reg_det_scnss.entidad_receptora                ,
                                     reg_det_scnss.num_reg_patronal                 ,
                                     v_filler2                                      ,
                                     reg_det_scnss.rfc_patron                       ,
                                     reg_det_scnss.periodo_pago                     ,
                                     reg_det_scnss.f_pago_patron                    USING "yyyymmdd",
                                     reg_det_scnss.folio_sua                        USING "&&&&&&",
                                     reg_det_scnss.nss                              ,
                                     reg_det_scnss.rfc_trabajador                   ,
                                     reg_det_scnss.curp                             ,
                                     reg_det_scnss.num_cred_infonavit               USING "&&&&&&&&&&",
                                     reg_det_scnss.f_inicio_dcto_cre_inf            USING "yyyymmdd",
                                     reg_det_scnss.num_mov_periodo                  USING "&&",
                                     reg_det_scnss.nombre_trabajador                ,
                                     (reg_det_scnss.ultimo_salario_integrado * 100) USING "&&&&&&&",
                                     reg_det_scnss.tipo_trabajador                  USING "&",
                                     reg_det_scnss.jornada_semana_reducida          USING "&",
                                     reg_det_scnss.localiza_trabajador              USING "&",
                                     reg_det_scnss.destino_ap_viv                   USING "&",
                                     reg_det_scnss.dias_cotizados_bim               USING "&&",
                                     reg_det_scnss.dias_incapacidad_bim             USING "&&",
                                     reg_det_scnss.dias_ausentismo_bim              USING "&&",
                                     v_filler1                                        ,
                                     (reg_det_scnss.imp_ap_pat_inf * 100)           USING "&&&&&&&",
                                     v_filler1                                        ,
                                     (reg_det_scnss.imp_am_cre_inf * 100)           USING "&&&&&&&",
                                     (reg_det_scnss.imp_ren_viv_pgo_ext * 100)      USING "&&&&&&&",
                                     reg_det_scnss.marca_cre_sua                    USING "&&",
                                     reg_det_scnss.marca_cre_BDNSAR                 USING "&",
                                     reg_det_scnss.diag_acl                         USING "&&",
                                     TODAY                                          USING "yyyymmdd",
                                     (reg_det_scnss.ap_int_viv * 1000000)           USING "&&&&&&&&&&&&&&&",
                                     (reg_det_scnss.precio_ap_int_viv * 1000000)    USING "&&&&&&&&&&&",
                                     (reg_det_scnss.int_gen_pag_ext_viv * 100)      USING "&&&&&&&",
                                     (reg_det_scnss.num_int_gen_pgo_ext * 1000000)  USING "&&&&&&&&&&&&&",
                                     v_filler3
            
            --se escribe el deatalle en el archivo
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
            
            --se suma el importe de registros en estado rechazado
            LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          + reg_det_scnss.imp_ap_pat_inf
            LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              + reg_det_scnss.imp_am_cre_inf
            LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            + reg_det_scnss.ap_int_viv
            LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext + reg_det_scnss.int_gen_pag_ext_viv
            LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    + reg_det_scnss.num_int_gen_pgo_ext
            
            LET v_i_contador_reg = v_i_contador_reg + 1
         END  IF
      END FOREACH
   ELSE
      PREPARE cla_rechazo_ccnss FROM v_sql
      DECLARE cur_rechazo_ccnss CURSOR FOR cla_rechazo_ccnss
      
      FOREACH cur_rechazo_ccnss INTO reg_det_ccnss.*
         
         LET v_existe_liq = 0
         LET v_id_derechohabiente = 0         

         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = reg_det_ccnss.nss
         IF v_id_derechohabiente IS NULL THEN
         	  LET v_id_derechohabiente = 0
         END IF
         	
         SELECT COUNT(*)
         INTO   v_existe_liq
         FROM   cta_his_pagos
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    folio_sua          = reg_det_ccnss.folio_sua
         AND    periodo_pago       = reg_det_ccnss.periodo_pago
         AND    f_pago             = reg_det_ccnss.f_pago_patron
         AND    nrp                = reg_det_ccnss.num_reg_patronal
         AND    ind_liquidacion not in (1,-1)
         AND    result_operacion <> 2
         AND    f_proceso < "11/27/2018"
         
         IF v_existe_liq = 0 THEN      	 
      	    INSERT INTO tmp_rechazos_acl VALUES (reg_det_ccnss.rechazo,
      	                                         reg_det_ccnss.imp_ap_pat_inf,
                                                 reg_det_ccnss.imp_am_cre_inf,
                                                 reg_det_ccnss.ap_int_viv)
            
            IF reg_det_ccnss.imp_ap_pat_inf      IS NULL THEN LET reg_det_ccnss.imp_ap_pat_inf      = 0 END IF
            IF reg_det_ccnss.imp_am_cre_inf      IS NULL THEN LET reg_det_ccnss.imp_am_cre_inf      = 0 END IF
            IF reg_det_ccnss.ap_int_viv          IS NULL THEN LET reg_det_ccnss.ap_int_viv          = 0 END IF
            IF reg_det_ccnss.int_gen_pag_ext_viv IS NULL THEN LET reg_det_ccnss.int_gen_pag_ext_viv = 0 END IF
            IF reg_det_ccnss.num_int_gen_pgo_ext IS NULL THEN LET reg_det_ccnss.num_int_gen_pgo_ext = 0 END IF
            
            LET v_c_fecha_pag = reg_det_ccnss.f_pago_patron USING "yyyymmdd"

            IF v_origen_archivo = 6 THEN            
               -- se obtiene el nss de dispersion
               SELECT nss
               INTO   v_c_nss
               FROM   afi_derechohabiente
               WHERE  id_derechohabiente = reg_det_ccnss.id_derhab_nuevo
               
               -- si no se encontro, se ponen ceros
               IF v_c_nss IS NULL THEN
                  LET v_c_nss = "00000000000"
               END IF
            ELSE
               LET v_c_nss = "00000000000"
            END IF   
            
            LET l_s_cadena_detalle = v_c_tipo_registro,
                                     reg_det_ccnss.entidad_receptora        ,
                                     reg_det_ccnss.num_reg_patronal         ,
                                     v_filler2                              ,
                                     reg_det_ccnss.rfc_patron               ,
                                     reg_det_ccnss.periodo_pago             ,
                                     reg_det_ccnss.f_pago_patron            USING "yyyymmdd",
                                     reg_det_ccnss.folio_sua                USING "&&&&&&",
                                     reg_det_ccnss.nss                      ,
                                     reg_det_ccnss.rfc_trabajador           ,
                                     reg_det_ccnss.curp                     ,
                                     reg_det_ccnss.num_cred_infonavit       USING "&&&&&&&&&&",
                                     reg_det_ccnss.f_inicio_dcto_cre_inf    USING "yyyymmdd",
                                     reg_det_ccnss.num_mov_periodo          USING "&&",
                                     reg_det_ccnss.nombre_trabajador        ,
                                     (reg_det_ccnss.ultimo_salario_integrado * 100) USING "&&&&&&&",
                                     reg_det_ccnss.tipo_trabajador          USING "&",
                                     reg_det_ccnss.jornada_semana_reducida  USING "&",
                                     reg_det_ccnss.localiza_trabajador      USING "&",
                                     reg_det_ccnss.destino_ap_viv           USING "&",
                                     reg_det_ccnss.dias_cotizados_bim       USING "&&",
                                     reg_det_ccnss.dias_incapacidad_bim     USING "&&",
                                     reg_det_ccnss.dias_ausentismo_bim      USING "&&",
                                     v_filler1                                ,
                                     (reg_det_ccnss.imp_ap_pat_inf * 100)   USING "&&&&&&&",
                                     v_filler1                                ,
                                     (reg_det_ccnss.imp_am_cre_inf * 100)   USING "&&&&&&&",
                                     (reg_det_ccnss.imp_ren_viv_pgo_ext * 100) USING "&&&&&&&",
                                     reg_det_ccnss.marca_cre_sua            USING "&&",
                                     reg_det_ccnss.marca_cre_BDNSAR         USING "&",
                                     reg_det_ccnss.diag_acl                 USING "&&",
                                     TODAY                                  USING "yyyymmdd",
                                     v_c_nss                           ,
                                     reg_det_ccnss.ap_paterno_afore  ,
                                     reg_det_ccnss.ap_materno_afore   ,
                                     reg_det_ccnss.nombre_afore      ,
                                     (reg_det_ccnss.ap_int_viv * 1000000)          USING "&&&&&&&&&&&&&",
                                     (reg_det_ccnss.precio_ap_int_viv * 1000000)   USING "&&&&&&&&&",
                                     (reg_det_ccnss.int_gen_pag_ext_viv * 100)     USING "&&&&&&&",
                                     (reg_det_ccnss.num_int_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&"
            
            --se escribe el deatalle en el archivo                  
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
            
            
            LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          + reg_det_ccnss.imp_ap_pat_inf
            LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              + reg_det_ccnss.imp_am_cre_inf
            LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            + reg_det_ccnss.ap_int_viv
            LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext + reg_det_ccnss.int_gen_pag_ext_viv
            LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    + reg_det_ccnss.num_int_gen_pgo_ext
            
            --se hace el conteo de registros en estado rechazado
            LET v_i_contador_reg = v_i_contador_reg + 1
         END IF
      END FOREACH
   END IF   	

   --se asigna el total de registros para el suamrio
   LET  v_num_reg_detalle =  v_i_contador_reg

   -- se quitan los decimales de los totales
   LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          * 100
   LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              * 100
   LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            * 1000000
   LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext * 100
   LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    * 1000000

  -- DISPLAY "@@ reg_sum_scnss.sum_aivs: ",reg_sum_scnss.sum_aivs
   LET l_s_cadena_sum_acl  = "9"                                                      ,
                             v_i_contador_reg                  USING "&&&&&&&&&"   ,
                             reg_sum_scnss.sum_ap_pat          USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_am              USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_aivs            USING "&&&&&&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_int_viv_pag_ext USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_aivs_pag_ext    USING "&&&&&&&&&&&&&&&&&&"

     --se escribe el sumario en el archivo
     CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_acl])

END FUNCTION

FUNCTION llena_scnss()

   LET v_sql = " SELECT a.cve_ent_receptora,   ",
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
               "        a.aiv_ap_pat,          ",
               "        a.valor_aiv,           ",
               "        a.int_gen_pgo_ext,     ",
               "        a.aiv_gen_pgo_ext,     ",
               "        NVL(fn_busca_rechazo_scnss(a.cve_ent_receptora, ",               
               "                              a.nrp,                ",
               "                              b.rfc_patron,         ",
               "                              a.periodo_pago,       ",
               "                              a.f_pago,             ",
               "                              a.folio_sua,          ",
               "                              c.nss,                ",
               "                              c.rfc,                ",
               "                              c.curp,               ",
               "                              a.num_crd_ifv,        ",
               "                              b.f_ini_desc_crd_ifv, ",
               "                              b.num_mov_periodo,    ",
               "                              c.nombre_imss,        ",
               "                              b.ult_sdi,            ",
               "                              b.tpo_trabajador,     ",
               "                              b.jornada,            ",
               "                              a.localiza_trabajador,",
               "                              b.destino_ap_viv,     ",
               "                              b.dias_cot_bim,       ",
               "                              b.dias_incap_bim,     ",
               "                              b.dias_ausent_bim,    ",
               "                              a.imp_ap_pat,         ",
               "                              a.imp_am_cre,         ",
               "                              a.imp_ren_viv_pgo_ext,",
               "                              b.marca_sua,          ",
               "                              b.marca_bdnsar,       ",
               "                              a.tpo_aclaracion,     ",
               "                              a.aiv_ap_pat,         ",
               "                              a.valor_aiv,          ",
               "                              a.int_gen_pgo_ext,    ",
               "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",               
               " FROM cta_rechazos_acl a       ",       
               "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
               "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
               "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
               "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
               "       cta_pag_complemento b, ",
               "       afi_derechohabiente c  ",                  
               " WHERE a.folio=b.folio        ",
               " AND   a.id_derechohabiente = c.id_derechohabiente  ",
               " AND   a.id_referencia = b.id_referencia ",
               " AND   a.result_operacion IN (2,3)       ",
               " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
               " AND   a.origen_archivo = ",v_origen_archivo,
               " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32 ",
               " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32 "

END FUNCTION

FUNCTION llena_ccnss()

   IF v_origen_archivo = 6 THEN
      LET v_sql  = " SELECT a.cve_ent_receptora,   ",
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
                   "        b.id_derhab_nuevo,     ",
                   "        c.ap_paterno_af,       ",
                   "        c.ap_materno_af,       ",
                   "        c.nombre_af,           ",
                   "        a.aiv_ap_pat,          ",
                   "        a.valor_aiv,           ",
                   "        a.int_gen_pgo_ext,     ",
                   "        a.aiv_gen_pgo_ext,      ",
                   "        NVL(fn_busca_rechazo_ccnss(a.cve_ent_receptora, ", 
                   "                              a.nrp,                ", 
                   "                              b.rfc_patron,         ", 
                   "                              a.periodo_pago,       ", 
                   "                              a.f_pago,             ", 
                   "                              a.folio_sua,          ", 
                   "                              c.nss,                ", 
                   "                              c.rfc,                ", 
                   "                              c.curp,               ", 
                   "                              a.num_crd_ifv,        ",
                   "                              b.f_ini_desc_crd_ifv, ",
                   "                              b.num_mov_periodo,    ",
                   "                              c.nombre_imss,        ",
                   "                              b.ult_sdi,            ",
                   "                              b.tpo_trabajador,     ",
                   "                              b.jornada,            ",
                   "                              a.localiza_trabajador,",
                   "                              b.destino_ap_viv,     ",
                   "                              b.dias_cot_bim,       ",
                   "                              b.dias_incap_bim,     ",
                   "                              b.dias_ausent_bim,    ",
                   "                              a.imp_ap_pat,         ",
                   "                              a.imp_am_cre,         ",
                   "                              a.imp_ren_viv_pgo_ext,",
                   "                              b.marca_sua,          ",
                   "                              b.marca_bdnsar,       ",
                   "                              a.tpo_aclaracion,     ",
                   "                              b.id_derhab_nuevo,    ",
                   "                              c.ap_paterno_af,      ",
                   "                              c.ap_materno_af,      ",
                   "                              c.nombre_af,          ",
                   "                              a.aiv_ap_pat,         ",
                   "                              a.valor_aiv,          ",
                   "                              a.int_gen_pgo_ext,    ",
                   "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",
                   " FROM cta_rechazos_acl a       ",       
                   "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
                   "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
                   "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
                   "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                   "       cta_pag_complemento b, ",
                   "       afi_derechohabiente c  ",
                   " WHERE a.folio=b.folio       ",
                   " AND a.id_derechohabiente = c.id_derechohabiente  ",
                   " AND a.id_referencia = b.id_referencia ",
                   " AND a.result_operacion IN (2,3)   ",
                   " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
                   " AND a.origen_archivo = ",v_origen_archivo,
                   " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36",
                   " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36"

   END IF
   
   IF v_origen_archivo = 7 THEN   
      LET v_sql  = " SELECT a.cve_ent_receptora,   ",
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
                   "        b.id_derhab_nuevo,     ",
                   "        c.ap_paterno_af,       ",
                   "        c.ap_materno_af,       ",
                   "        c.nombre_af,           ",
                   "        a.aiv_ap_pat,          ",
                   "        a.valor_aiv,           ",
                   "        a.int_gen_pgo_ext,     ",
                   "        a.aiv_gen_pgo_ext,      ",
                   "        NVL(fn_busca_rechazo_ccnom(a.cve_ent_receptora, ",                   
                   "                              a.nrp,                ",
                   "                              b.rfc_patron,         ",
                   "                              a.periodo_pago,       ",
                   "                              a.f_pago,             ",
                   "                              a.folio_sua,          ",
                   "                              c.nss,                ",
                   "                              c.rfc,                ",
                   "                              c.curp,               ",
                   "                              a.num_crd_ifv,        ",
                   "                              b.f_ini_desc_crd_ifv, ",
                   "                              b.num_mov_periodo,    ",
                   "                              c.nombre_imss,        ",
                   "                              b.ult_sdi,            ",
                   "                              b.tpo_trabajador,     ",
                   "                              b.jornada,            ",
                   "                              a.localiza_trabajador,",
                   "                              b.destino_ap_viv,     ",
                   "                              b.dias_cot_bim,       ",
                   "                              b.dias_incap_bim,     ",
                   "                              b.dias_ausent_bim,    ",
                   "                              a.imp_ap_pat,         ",
                   "                              a.imp_am_cre,         ",
                   "                              a.imp_ren_viv_pgo_ext,",
                   "                              b.marca_sua,          ",
                   "                              b.marca_bdnsar,       ",
                   "                              a.tpo_aclaracion,     ",
                   "                              b.id_derhab_nuevo,    ",
                   "                              c.ap_paterno_af,      ",
                   "                              c.ap_materno_af,      ",
                   "                              c.nombre_af,          ",
                   "                              a.aiv_ap_pat,         ",
                   "                              a.valor_aiv,          ",
                   "                              a.int_gen_pgo_ext,    ",
                   "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",                   
                   " FROM cta_rechazos_acl a       ",       
                   "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
                   "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
                   "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
                   "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                   "       cta_pag_complemento b, ",
                   "       afi_derechohabiente c  ",
                   " WHERE a.folio=b.folio       ",
                   " AND a.id_derechohabiente = c.id_derechohabiente  ",
                   " AND a.id_referencia = b.id_referencia ",
                   " AND a.result_operacion IN (2,3)   ",
                   " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
                   " AND a.origen_archivo = ",v_origen_archivo,
                   " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36",
                   " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36"
   END IF

END FUNCTION

FUNCTION fn_genera_reporte()

   DEFINE v_manejador_rpt OM.SaxDocumentHandler
          
   DEFINE v_contador      INTEGER
   DEFINE v_contador_causales_reporte      INTEGER
   DEFINE v_contador_causales INTEGER
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE v_v_nom_reporte VARCHAR(80)
   DEFINE v_existe        SMALLINT
   DEFINE v_nom_reporte   VARCHAR(80)

   -- Proesos 101=ENCLARA, 102= ACL SIN CAMBIO NSS, 103=ACL CAMBIO NSS, 
   --         107=ACL CAMBIO NOMBRE, 1401=LQINFO, 1403= SOLO INFONAVIT
--===============================================================================
   LET v_v_nom_reporte = p_usuario     CLIPPED,"-ACLC20-",
                         p_pid         USING "&&&&&", "-",
                         p_proceso_cod USING "&&&&&","-",
                         1             USING "&&&&&"

    CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

    CALL fgl_report_loadCurrentSettings(r_ruta_bin CLIPPED ||"/ACLC20.4rp") RETURNING v_existe

    -- se indica la salida del reporte
    CALL fgl_report_selectDevice("PDF")

    CALL fgl_report_setOutputFileName(r_ruta_listados CLIPPED||"/"||v_v_nom_reporte)

    -- sin indica que no es necesario el preview
    CALL fgl_report_selectPreview(0)

    -- se asigna la configuración en el menejo del reporte
    LET v_manejador_rpt = fgl_report_commitCurrentSettings()

    --===============================================================================    

   LET v_sql="SELECT rechazo,  ",
                   " COUNT(*), ",
                   " NVL(sum(imp_ap_pat),0),",
                   " NVL(sum(imp_am_cre),0),",
                   " NVL(sum(aiv_ap_pat),0) ",
             " FROM tmp_rechazos_acl  ",
             " GROUP BY 1 ORDER BY 1 ASC" 

{ 
   #query que  obtiene las causales de aclaracion agrupadas
    LET v_sql="SELECT cat.codigo_rechazo||'-'||TRIM(cat.descripcion), ",
              "       COUNT(*), ",
              "       NVL(sum(a.imp_ap_pat),0),",
              "       NVL(sum(a.imp_am_cre),0),",
              "       NVL(sum(a.aiv_ap_pat),0) ",
              " FROM cta_rechazos_acl a ",        --saci2018-67
              "        LEFT OUTER JOIN acl_pag_rechazo acl ",
              "           ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
              "        LEFT OUTER JOIN acl_cat_rechazo cat ",
              "           ON (acl.codigo_rechazo = cat.codigo_rechazo), ", 
              "     cta_pag_complemento b, ",
              "     afi_derechohabiente c  ",                     
              " WHERE a.folio=b.folio       ",
              " AND   a.id_derechohabiente = c.id_derechohabiente  ",
              " AND   a.id_referencia = b.id_referencia ",              
              " AND   a.result_operacion in (2,3) ",
              " AND  (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",              
              " AND   a.origen_archivo = ",v_origen_archivo,
              " GROUP BY 1 ORDER BY 1 ASC"
}

    DISPLAY v_sql

    PREPARE prp_stm_obtiene_causales FROM v_sql
    DECLARE cur_obtiene_causales CURSOR FOR prp_stm_obtiene_causales

    LET v_contador_causales=1

    DISPLAY "ANTES DE ENTRAR folio:",p_folio

    FOREACH cur_obtiene_causales INTO g_arr_causales_rechazo[v_contador_causales].*
    DISPLAY "g_arr_causales_rechazo ",g_arr_causales_rechazo[v_contador_causales].*
       LET v_contador_causales=v_contador_causales+1
    END FOREACH
    
    LET g_total_causales.total_registros=0
    LET g_total_causales.imp_ap_pat=0
    LET g_total_causales.imp_am_cre=0
    LET g_total_causales.aiv_ap_pat=0

    FOR v_contador_causales_reporte = 1 TO g_arr_causales_rechazo.getLength()-1
       LET g_total_causales.total_registros=g_total_causales.total_registros + g_arr_causales_rechazo[v_contador_causales_reporte].total_registros
       LET g_total_causales.imp_ap_pat=g_total_causales.imp_ap_pat + g_arr_causales_rechazo[v_contador_causales_reporte].imp_ap_pat
       LET g_total_causales.imp_am_cre=g_total_causales.imp_am_cre + g_arr_causales_rechazo[v_contador_causales_reporte].imp_am_cre
       LET g_total_causales.aiv_ap_pat=g_total_causales.aiv_ap_pat + g_arr_causales_rechazo[v_contador_causales_reporte].aiv_ap_pat
    END FOR 

   #se genera el reporte
   START REPORT rpt_consulta_lqinfo TO XML HANDLER v_manejador_rpt
   FOR v_contador = 1 TO g_arr_causales_rechazo.getLength()-1
      OUTPUT TO REPORT rpt_consulta_lqinfo(g_arr_causales_rechazo[v_contador].*,p_folio) --xvi-141-02
   END FOR
   FINISH REPORT rpt_consulta_lqinfo
   
END FUNCTION

REPORT rpt_consulta_lqinfo(v_arr_causales_rechazo,v_folio) --xvi-141-01  --xvi-141-02

   DEFINE v_arr_causales_rechazo RECORD
      causal_rechazo VARCHAR(60),
      total_registros INTEGER,
      imp_ap_pat decimal(12,2),
      imp_am_cre decimal(12,2),
      aiv_ap_pat decimal(18,6)
   END RECORD

   DEFINE v_folio DECIMAL(9,0)  --xvi-141-02

   #para causales
   DEFINE v_total_causales RECORD
      total_registros INTEGER,
      imp_ap_pat DECIMAL(12,2),
      imp_am_cre DECIMAL(12,2),
      aiv_ap_pat DECIMAL(18,6)
   END RECORD

   DEFINE v_fecha_consulta DATE
   DEFINE v_fecha STRING
   DEFINE v_nombre_usuario VARCHAR(100)
   DEFINE g_nom_archivo    LIKE bat_ctr_operacion.nom_archivo
   
   FORMAT

      FIRST PAGE HEADER
         
         -- se envia folio, usuario y fecha
         LET v_fecha = TODAY USING "dd-mm-yyyy"
         LET v_fecha_consulta = v_fecha
         
         -- se obtiene el nombre del usuario
         SELECT USER
         INTO   g_usuario_cod
         FROM   seg_modulo
         WHERE  modulo_cod = "acl"

         SELECT usuario_desc
         INTO   v_nombre_usuario
         FROM   seg_usuario
         WHERE  usuario_cod = g_usuario_cod

         LET v_nombre_usuario = v_nombre_usuario CLIPPED

         IF p_tpo_archivo = 1 THEN
            LET g_nom_archivo = "Rechazos únicos sin cambio de nss" CLIPPED
         END IF
         IF p_tpo_archivo = 2 THEN
            LET g_nom_archivo = "Rechazos únicos con cambio de nss" CLIPPED
         END IF
         IF p_tpo_archivo = 3 THEN
            LET g_nom_archivo = "Rechazos únicos con cambio de nombre" CLIPPED
         END IF
         
         PRINTX g_usuario_cod, v_nombre_usuario, v_folio, g_nom_archivo, v_fecha  --xvi-141-02
         
      ON EVERY ROW
           -- causal nula se imprime en la función fn_busca_rechazo_xxxxx.sql
--         IF v_arr_causales_rechazo.causal_rechazo IS NULL THEN
--            LET v_arr_causales_rechazo.causal_rechazo = "13-SIN LQINFO Y HAY SALIDA"
--         END IF
         PRINTX v_arr_causales_rechazo.*

      ON LAST ROW
         LET v_total_causales.* = g_total_causales.*
         PRINTX v_total_causales.*
         
END REPORT

FUNCTION fn_genera_archivo02()  -- saci2018-67-02

   DEFINE v_i_inicio_for    INTEGER
   DEFINE v_genero_reporte_pdf BOOLEAN
   DEFINE r_ruta_listados LIKE seg_modulo.ruta_listados
   DEFINE r_ruta_bin      LIKE seg_modulo.ruta_bin
   DEFINE v_nom_reporte   VARCHAR(80)

   DEFINE reg_det_scnss RECORD
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
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv ,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      rechazo                  CHAR(32)
   END RECORD

   DEFINE reg_det_ccnss RECORD
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
      id_derhab_nuevo          DECIMAL (9,0),--LIKE cta_pag_complemento.id_derhab_nuevo,
      ap_paterno_afore         LIKE afi_derechohabiente.ap_paterno_af,
      ap_materno_afore         LIKE afi_derechohabiente.ap_materno_af,
      nombre_afore             LIKE afi_derechohabiente.nombre_af,
      ap_int_viv               LIKE cta_his_pagos.aiv_ap_pat,
      precio_ap_int_viv        LIKE cta_his_pagos.valor_aiv,
      int_gen_pag_ext_viv      LIKE cta_his_pagos.int_gen_pgo_ext,
      num_int_gen_pgo_ext      LIKE cta_his_pagos.aiv_gen_pgo_ext,
      rechazo                  CHAR(32)      
   END RECORD

   DEFINE reg_sum_scnss RECORD
      sum_ap_pat          LIKE acl_sum_sc_nss.suma_ap_pat,
      sum_am              LIKE acl_sum_sc_nss.suma_am,
      sum_aivs            DECIMAL(22,6),
      sum_int_viv_pag_ext LIKE acl_sum_sc_nss.suma_int_viv_pgo_ext,
      sum_aivs_pag_ext    DECIMAL(22,6)
   END RECORD   
   
   DEFINE
      v_filler1          CHAR(7) ,
      v_filler2          CHAR(12),
      v_filler3          CHAR(17),
      v_ruta_envio       LIKE seg_modulo.ruta_envio,
      v_ruta_nomarch     STRING,
      v_c_fecha_pag      VARCHAR(8),
      l_s_cadena_detalle STRING,
      l_s_cadena_sum_acl STRING,
      v_c_tipo_registro  VARCHAR(2),
      v_i_contador_reg   INTEGER,
      v_num_reg_detalle  INTEGER,
      v_c_nss            CHAR(11),
      v_fecha            DATE,
      v_id_derechohabiente DECIMAL(9,0),
      v_existe_liq       SMALLINT,      
      v_ch_arch_solTransf BASE.CHANNEL  -- manejador de apuntador hacia archivo

   LET v_i_inicio_for = 1

   CALL fn_rutas("acl") RETURNING r_ruta_bin, r_ruta_listados

   -- se obtienen la ruta envio del modulo
   SELECT ruta_envio
   INTO   v_ruta_envio
   FROM   seg_modulo
   WHERE  modulo_cod = 'acl'

   LET v_c_tipo_registro = "2"

   LET v_genero_reporte_pdf = FALSE

   LET v_fecha = TODAY

   IF p_tpo_archivo = 1 THEN
      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disscnss_rechazoACL_causal"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 5
      CALL llena_scnss02()
   END IF
   IF p_tpo_archivo = 2 THEN
      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disccnss_rechazoACL__causal"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 6   
      CALL llena_ccnss02()
   END IF
   IF p_tpo_archivo = 3 THEN
      LET v_nom_reporte = v_fecha USING "YYYYMMDD",".disccnom_rechazoACL_causal"
      LET v_ruta_nomarch = v_ruta_envio CLIPPED ||"/"|| v_nom_reporte
      LET v_origen_archivo = 7      
      CALL llena_ccnss02()
   END IF
   
   -- se crea el manejador de archivo
   LET v_ch_arch_solTransf = base.Channel.create()

   -- se crea archivo y se indica que se escribira en el mismo
   CALL v_ch_arch_solTransf.openFile(v_ruta_nomarch, "w" )
   CALL v_ch_arch_solTransf.setDelimiter("")

   LET v_filler1 =  7 SPACES
   LET v_filler2 = 12 SPACES
   LET v_filler3 = 17 SPACES
   LET reg_sum_scnss.sum_ap_pat          = 0
   LET reg_sum_scnss.sum_am              = 0
   LET reg_sum_scnss.sum_aivs            = 0
   LET reg_sum_scnss.sum_int_viv_pag_ext = 0
   LET reg_sum_scnss.sum_aivs_pag_ext    = 0

   IF p_tpo_archivo = 1 THEN   
      PREPARE cla_rechazo_scnss02 FROM v_sql02
      DECLARE cur_rechazo_scnss02 CURSOR FOR cla_rechazo_scnss02
      
      FOREACH cur_rechazo_scnss02 INTO reg_det_scnss.*

         LET v_existe_liq = 0
         LET v_id_derechohabiente = 0
         
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = reg_det_scnss.nss
         IF v_id_derechohabiente IS NULL THEN
         	  LET v_id_derechohabiente = 0
         END IF

         SELECT COUNT(*)
         INTO   v_existe_liq
         FROM   cta_his_pagos
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    folio_sua          = reg_det_scnss.folio_sua
         AND    periodo_pago       = reg_det_scnss.periodo_pago
         AND    f_pago             = reg_det_scnss.f_pago_patron
         AND    nrp                = reg_det_scnss.num_reg_patronal
         AND    ind_liquidacion not in (1,-1)
         AND    result_operacion <> 2
         AND    f_proceso < "11/27/2018"
         
         IF v_existe_liq = 0 THEN

            IF reg_det_scnss.imp_ap_pat_inf      IS NULL THEN LET reg_det_scnss.imp_ap_pat_inf      = 0 END IF
            IF reg_det_scnss.imp_am_cre_inf      IS NULL THEN LET reg_det_scnss.imp_am_cre_inf      = 0 END IF
            IF reg_det_scnss.ap_int_viv          IS NULL THEN LET reg_det_scnss.ap_int_viv          = 0 END IF
            IF reg_det_scnss.int_gen_pag_ext_viv IS NULL THEN LET reg_det_scnss.int_gen_pag_ext_viv = 0 END IF
            IF reg_det_scnss.num_int_gen_pgo_ext IS NULL THEN LET reg_det_scnss.num_int_gen_pgo_ext = 0 END IF
            
      	    
            LET v_c_fecha_pag = reg_det_scnss.f_pago_patron USING "yyyymmdd"
            
            LET l_s_cadena_detalle = v_c_tipo_registro,
                                     reg_det_scnss.entidad_receptora                ,
                                     reg_det_scnss.num_reg_patronal                 ,
                                     v_filler2                                      ,
                                     reg_det_scnss.rfc_patron                       ,
                                     reg_det_scnss.periodo_pago                     ,
                                     reg_det_scnss.f_pago_patron                    USING "yyyymmdd",
                                     reg_det_scnss.folio_sua                        USING "&&&&&&",
                                     reg_det_scnss.nss                              ,
                                     reg_det_scnss.rfc_trabajador                   ,
                                     reg_det_scnss.curp                             ,
                                     reg_det_scnss.num_cred_infonavit               USING "&&&&&&&&&&",
                                     reg_det_scnss.f_inicio_dcto_cre_inf            USING "yyyymmdd",
                                     reg_det_scnss.num_mov_periodo                  USING "&&",
                                     reg_det_scnss.nombre_trabajador                ,
                                     (reg_det_scnss.ultimo_salario_integrado * 100) USING "&&&&&&&",
                                     reg_det_scnss.tipo_trabajador                  USING "&",
                                     reg_det_scnss.jornada_semana_reducida          USING "&",
                                     reg_det_scnss.localiza_trabajador              USING "&",
                                     reg_det_scnss.destino_ap_viv                   USING "&",
                                     reg_det_scnss.dias_cotizados_bim               USING "&&",
                                     reg_det_scnss.dias_incapacidad_bim             USING "&&",
                                     reg_det_scnss.dias_ausentismo_bim              USING "&&",
                                     v_filler1                                        ,
                                     (reg_det_scnss.imp_ap_pat_inf * 100)           USING "&&&&&&&",
                                     v_filler1                                        ,
                                     (reg_det_scnss.imp_am_cre_inf * 100)           USING "&&&&&&&",
                                     (reg_det_scnss.imp_ren_viv_pgo_ext * 100)      USING "&&&&&&&",
                                     reg_det_scnss.marca_cre_sua                    USING "&&",
                                     reg_det_scnss.marca_cre_BDNSAR                 USING "&",
                                     reg_det_scnss.diag_acl                         USING "&&",
                                     TODAY                                          USING "yyyymmdd",
                                     (reg_det_scnss.ap_int_viv * 1000000)           USING "&&&&&&&&&&&&&&&",
                                     (reg_det_scnss.precio_ap_int_viv * 1000000)    USING "&&&&&&&&&&&",
                                     (reg_det_scnss.int_gen_pag_ext_viv * 100)      USING "&&&&&&&",
                                     (reg_det_scnss.num_int_gen_pgo_ext * 1000000)  USING "&&&&&&&&&&&&&",
                                     '                ',
                                     reg_det_scnss.rechazo
            
            --se escribe el deatalle en el archivo
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
            
            --se suma el importe de registros en estado rechazado
            LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          + reg_det_scnss.imp_ap_pat_inf
            LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              + reg_det_scnss.imp_am_cre_inf
            LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            + reg_det_scnss.ap_int_viv
            LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext + reg_det_scnss.int_gen_pag_ext_viv
            LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    + reg_det_scnss.num_int_gen_pgo_ext
            
            LET v_i_contador_reg = v_i_contador_reg + 1
         END IF
      END FOREACH
   ELSE
      PREPARE cla_rechazo_ccnss02 FROM v_sql02
      DECLARE cur_rechazo_ccnss02 CURSOR FOR cla_rechazo_ccnss02
      
      FOREACH cur_rechazo_ccnss02 INTO reg_det_ccnss.*

         LET v_existe_liq = 0
         LET v_id_derechohabiente = 0
         
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente
         WHERE  nss = reg_det_ccnss.nss
         IF v_id_derechohabiente IS NULL THEN
         	  LET v_id_derechohabiente = 0
         END IF         	

         SELECT COUNT(*)
         INTO   v_existe_liq
         FROM   cta_his_pagos
         WHERE  id_derechohabiente = v_id_derechohabiente
         AND    folio_sua          = reg_det_ccnss.folio_sua
         AND    periodo_pago       = reg_det_ccnss.periodo_pago
         AND    f_pago             = reg_det_ccnss.f_pago_patron
         AND    nrp                = reg_det_ccnss.num_reg_patronal
         AND    ind_liquidacion not in (1,-1)
         AND    result_operacion <> 2
         AND    f_proceso < "11/27/2018"
         
         IF v_existe_liq = 0 THEN

            IF reg_det_ccnss.imp_ap_pat_inf      IS NULL THEN LET reg_det_ccnss.imp_ap_pat_inf      = 0 END IF
            IF reg_det_ccnss.imp_am_cre_inf      IS NULL THEN LET reg_det_ccnss.imp_am_cre_inf      = 0 END IF
            IF reg_det_ccnss.ap_int_viv          IS NULL THEN LET reg_det_ccnss.ap_int_viv          = 0 END IF
            IF reg_det_ccnss.int_gen_pag_ext_viv IS NULL THEN LET reg_det_ccnss.int_gen_pag_ext_viv = 0 END IF
            IF reg_det_ccnss.num_int_gen_pgo_ext IS NULL THEN LET reg_det_ccnss.num_int_gen_pgo_ext = 0 END IF
            
            LET v_c_fecha_pag = reg_det_ccnss.f_pago_patron USING "yyyymmdd"
            
            IF v_origen_archivo = 6 THEN            
               -- se obtiene el nss de dispersion
               SELECT nss
               INTO   v_c_nss
               FROM   afi_derechohabiente
               WHERE  id_derechohabiente = reg_det_ccnss.id_derhab_nuevo
               
               -- si no se encontro, se ponen ceros
               IF v_c_nss IS NULL THEN
                  LET v_c_nss = "00000000000"
               END IF
            ELSE
               LET v_c_nss = "00000000000"
            END IF

            
            LET l_s_cadena_detalle = v_c_tipo_registro,
                                     reg_det_ccnss.entidad_receptora        ,
                                     reg_det_ccnss.num_reg_patronal         ,
                                     v_filler2                              ,
                                     reg_det_ccnss.rfc_patron               ,
                                     reg_det_ccnss.periodo_pago             ,
                                     reg_det_ccnss.f_pago_patron            USING "yyyymmdd",
                                     reg_det_ccnss.folio_sua                USING "&&&&&&",
                                     reg_det_ccnss.nss                      ,
                                     reg_det_ccnss.rfc_trabajador           ,
                                     reg_det_ccnss.curp                     ,
                                     reg_det_ccnss.num_cred_infonavit       USING "&&&&&&&&&&",
                                     reg_det_ccnss.f_inicio_dcto_cre_inf    USING "yyyymmdd",
                                     reg_det_ccnss.num_mov_periodo          USING "&&",
                                     reg_det_ccnss.nombre_trabajador        ,
                                     (reg_det_ccnss.ultimo_salario_integrado * 100) USING "&&&&&&&",
                                     reg_det_ccnss.tipo_trabajador          USING "&",
                                     reg_det_ccnss.jornada_semana_reducida  USING "&",
                                     reg_det_ccnss.localiza_trabajador      USING "&",
                                     reg_det_ccnss.destino_ap_viv           USING "&",
                                     reg_det_ccnss.dias_cotizados_bim       USING "&&",
                                     reg_det_ccnss.dias_incapacidad_bim     USING "&&",
                                     reg_det_ccnss.dias_ausentismo_bim      USING "&&",
                                     v_filler1                                ,
                                     (reg_det_ccnss.imp_ap_pat_inf * 100)   USING "&&&&&&&",
                                     v_filler1                                ,
                                     (reg_det_ccnss.imp_am_cre_inf * 100)   USING "&&&&&&&",
                                     (reg_det_ccnss.imp_ren_viv_pgo_ext * 100) USING "&&&&&&&",
                                     reg_det_ccnss.marca_cre_sua            USING "&&",
                                     reg_det_ccnss.marca_cre_BDNSAR         USING "&",
                                     reg_det_ccnss.diag_acl                 USING "&&",
                                     TODAY                                  USING "yyyymmdd",
                                     v_c_nss                           ,
                                     reg_det_ccnss.ap_paterno_afore  ,
                                     reg_det_ccnss.ap_materno_afore   ,
                                     reg_det_ccnss.nombre_afore      ,
                                     (reg_det_ccnss.ap_int_viv * 1000000)          USING "&&&&&&&&&&&&&",
                                     (reg_det_ccnss.precio_ap_int_viv * 1000000)   USING "&&&&&&&&&",
                                     (reg_det_ccnss.int_gen_pag_ext_viv * 100)     USING "&&&&&&&",
                                     (reg_det_ccnss.num_int_gen_pgo_ext * 1000000) USING "&&&&&&&&&&&&&",
                                     '                ',                                     
                                     reg_det_ccnss.rechazo
            
            --se escribe el deatalle en el archivo                  
            CALL v_ch_arch_solTransf.writeLine([l_s_cadena_detalle])
            
            LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          + reg_det_ccnss.imp_ap_pat_inf
            LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              + reg_det_ccnss.imp_am_cre_inf
            LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            + reg_det_ccnss.ap_int_viv
            LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext + reg_det_ccnss.int_gen_pag_ext_viv
            LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    + reg_det_ccnss.num_int_gen_pgo_ext
            
            --se hace el conteo de registros en estado rechazado
            LET v_i_contador_reg = v_i_contador_reg + 1
         END IF
      END FOREACH
   END IF   	

   --se asigna el total de registros para el suamrio
   LET  v_num_reg_detalle =  v_i_contador_reg

   -- se quitan los decimales de los totales
   LET reg_sum_scnss.sum_ap_pat          = reg_sum_scnss.sum_ap_pat          * 100
   LET reg_sum_scnss.sum_am              = reg_sum_scnss.sum_am              * 100
   LET reg_sum_scnss.sum_aivs            = reg_sum_scnss.sum_aivs            * 1000000
   LET reg_sum_scnss.sum_int_viv_pag_ext = reg_sum_scnss.sum_int_viv_pag_ext * 100
   LET reg_sum_scnss.sum_aivs_pag_ext    = reg_sum_scnss.sum_aivs_pag_ext    * 1000000

  -- DISPLAY "@@ reg_sum_scnss.sum_aivs: ",reg_sum_scnss.sum_aivs
   LET l_s_cadena_sum_acl  = "9"                                                      ,
                             v_i_contador_reg                  USING "&&&&&&&&&"   ,
                             reg_sum_scnss.sum_ap_pat          USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_am              USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_aivs            USING "&&&&&&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_int_viv_pag_ext USING "&&&&&&&&&&&&&",
                             reg_sum_scnss.sum_aivs_pag_ext    USING "&&&&&&&&&&&&&&&&&&"

     --se escribe el sumario en el archivo
     CALL v_ch_arch_solTransf.writeLine([l_s_cadena_sum_acl])

END FUNCTION

FUNCTION llena_scnss02()

   LET v_sql02 = " SELECT a.cve_ent_receptora,   ",
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
                 "        a.aiv_ap_pat,          ",
                 "        a.valor_aiv,           ",
                 "        a.int_gen_pgo_ext,     ",
                 "        a.aiv_gen_pgo_ext,     ",
                 "        NVL(fn_busca_rechazo_scnss(a.cve_ent_receptora, ",                 
                 "                              a.nrp,                ",
                 "                              b.rfc_patron,         ",
                 "                              a.periodo_pago,       ",
                 "                              a.f_pago,             ",
                 "                              a.folio_sua,          ",
                 "                              c.nss,                ",
                 "                              c.rfc,                ",
                 "                              c.curp,               ",
                 "                              a.num_crd_ifv,        ",
                 "                              b.f_ini_desc_crd_ifv, ",
                 "                              b.num_mov_periodo,    ",
                 "                              c.nombre_imss,        ",
                 "                              b.ult_sdi,            ",
                 "                              b.tpo_trabajador,     ",
                 "                              b.jornada,            ",
                 "                              a.localiza_trabajador,",
                 "                              b.destino_ap_viv,     ",
                 "                              b.dias_cot_bim,       ",
                 "                              b.dias_incap_bim,     ",
                 "                              b.dias_ausent_bim,    ",
                 "                              a.imp_ap_pat,         ",
                 "                              a.imp_am_cre,         ",
                 "                              a.imp_ren_viv_pgo_ext,",
                 "                              b.marca_sua,          ",
                 "                              b.marca_bdnsar,       ",
                 "                              a.tpo_aclaracion,     ",
                 "                              a.aiv_ap_pat,         ",
                 "                              a.valor_aiv,          ",
                 "                              a.int_gen_pgo_ext,    ",
                 "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",                 
                 " FROM cta_rechazos_acl a       ",       
                 "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
                 "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
                 "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
                 "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                 "       cta_pag_complemento b, ",
                 "       afi_derechohabiente c  ",                  
                 " WHERE a.folio=b.folio        ",
                 " AND   a.id_derechohabiente = c.id_derechohabiente  ",
                 " AND   a.id_referencia = b.id_referencia ",
                 " AND   a.result_operacion IN (2,3)       ",
                 " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
                 " AND   a.origen_archivo = ",v_origen_archivo,
                 " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32 ",
                 " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32 "
END FUNCTION

FUNCTION llena_ccnss02()

   IF v_origen_archivo = 6 THEN
      LET v_sql02 = " SELECT a.cve_ent_receptora,   ",
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
                    "        b.id_derhab_nuevo,     ",
                    "        c.ap_paterno_af,       ",
                    "        c.ap_materno_af,       ",
                    "        c.nombre_af,           ",
                    "        a.aiv_ap_pat,          ",
                    "        a.valor_aiv,           ",
                    "        a.int_gen_pgo_ext,     ",
                    "        a.aiv_gen_pgo_ext,      ",
                    "        NVL(fn_busca_rechazo_ccnss(a.cve_ent_receptora, ",                    
                    "                              a.nrp,                ",
                    "                              b.rfc_patron,         ",
                    "                              a.periodo_pago,       ",
                    "                              a.f_pago,             ",
                    "                              a.folio_sua,          ",
                    "                              c.nss,                ",
                    "                              c.rfc,                ",
                    "                              c.curp,               ",
                    "                              a.num_crd_ifv,        ",
                    "                              b.f_ini_desc_crd_ifv, ",
                    "                              b.num_mov_periodo,    ",
                    "                              c.nombre_imss,        ",
                    "                              b.ult_sdi,            ",
                    "                              b.tpo_trabajador,     ",
                    "                              b.jornada,            ",
                    "                              a.localiza_trabajador,",
                    "                              b.destino_ap_viv,     ",
                    "                              b.dias_cot_bim,       ",
                    "                              b.dias_incap_bim,     ",
                    "                              b.dias_ausent_bim,    ",
                    "                              a.imp_ap_pat,         ",
                    "                              a.imp_am_cre,         ",
                    "                              a.imp_ren_viv_pgo_ext,",
                    "                              b.marca_sua,          ",
                    "                              b.marca_bdnsar,       ",
                    "                              a.tpo_aclaracion,     ",
                    "                              b.id_derhab_nuevo,    ",
                    "                              c.ap_paterno_af,      ",
                    "                              c.ap_materno_af,      ",
                    "                              c.nombre_af,          ",                    
                    "                              a.aiv_ap_pat,         ",
                    "                              a.valor_aiv,          ",
                    "                              a.int_gen_pgo_ext,    ",
                    "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",                    
                    " FROM cta_rechazos_acl a       ",       
                    "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
                    "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
                    "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
                    "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                    "       cta_pag_complemento b, ",
                    "       afi_derechohabiente c  ",
                    " WHERE a.folio=b.folio       ",
                    " AND a.id_derechohabiente = c.id_derechohabiente  ",
                    " AND a.id_referencia = b.id_referencia ",
                    " AND a.result_operacion IN (2,3)   ",
                    " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
                    " AND a.origen_archivo = ",v_origen_archivo,
                    " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36",
                    " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36"   
   END IF
   	
   IF v_origen_archivo = 7 THEN
      LET v_sql02 = " SELECT a.cve_ent_receptora,   ",
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
                    "        b.id_derhab_nuevo,     ",
                    "        c.ap_paterno_af,       ",
                    "        c.ap_materno_af,       ",
                    "        c.nombre_af,           ",
                    "        a.aiv_ap_pat,          ",
                    "        a.valor_aiv,           ",
                    "        a.int_gen_pgo_ext,     ",
                    "        a.aiv_gen_pgo_ext,      ",
                    "        NVL(fn_busca_rechazo_ccnom(a.cve_ent_receptora, ",                    
                    "                              a.nrp,                ",
                    "                              b.rfc_patron,         ",
                    "                              a.periodo_pago,       ",
                    "                              a.f_pago,             ",
                    "                              a.folio_sua,          ",
                    "                              c.nss,                ",
                    "                              c.rfc,                ",
                    "                              c.curp,               ",
                    "                              a.num_crd_ifv,        ",
                    "                              b.f_ini_desc_crd_ifv, ",
                    "                              b.num_mov_periodo,    ",
                    "                              c.nombre_imss,        ",
                    "                              b.ult_sdi,            ",
                    "                              b.tpo_trabajador,     ",
                    "                              b.jornada,            ",
                    "                              a.localiza_trabajador,",
                    "                              b.destino_ap_viv,     ",
                    "                              b.dias_cot_bim,       ",
                    "                              b.dias_incap_bim,     ",
                    "                              b.dias_ausent_bim,    ",
                    "                              a.imp_ap_pat,         ",
                    "                              a.imp_am_cre,         ",
                    "                              a.imp_ren_viv_pgo_ext,",
                    "                              b.marca_sua,          ",
                    "                              b.marca_bdnsar,       ",
                    "                              a.tpo_aclaracion,     ",
                    "                              b.id_derhab_nuevo,    ",
                    "                              c.ap_paterno_af,      ",
                    "                              c.ap_materno_af,      ",
                    "                              c.nombre_af,          ",                    
                    "                              a.aiv_ap_pat,         ",
                    "                              a.valor_aiv,          ",
                    "                              a.int_gen_pgo_ext,    ",
                    "                              a.aiv_gen_pgo_ext),'13-SIN LQINFO Y HAY SALIDA')   ",                    
                    " FROM cta_rechazos_acl a       ",       
                    "         LEFT OUTER JOIN acl_pag_rechazo acl  ",
                    "            ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia) ",
                    "         LEFT OUTER JOIN acl_cat_rechazo cat  ",
                    "            ON (acl.codigo_rechazo = cat.codigo_rechazo), ",
                    "       cta_pag_complemento b, ",
                    "       afi_derechohabiente c  ",
                    " WHERE a.folio=b.folio       ",
                    " AND a.id_derechohabiente = c.id_derechohabiente  ",
                    " AND a.id_referencia = b.id_referencia ",
                    " AND a.result_operacion IN (2,3)   ",
                    " AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL) ",
                    " AND a.origen_archivo = ",v_origen_archivo,
                    " GROUP BY 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36",
                    " ORDER BY 7,6,2,4,5,1,3,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36"   
   END IF   	
END FUNCTION

         {
         and    cve_ent_receptora   = reg_det_ccnss.entidad_receptora
         and    nrp                 = reg_det_ccnss.num_reg_patronal
--         and    rfc_patron          = reg_det_ccnss.rfc_patron
         and    periodo_pago        = reg_det_ccnss.periodo_pago
         and    f_pago              = reg_det_ccnss.f_pago_patron
         and    folio_sua           = reg_det_ccnss.folio_sua
--         and    nss                 = reg_det_ccnss.nss
--         and    rfc                 = reg_det_ccnss.rfc_trabajador
--         and    curp                = reg_det_ccnss.curp
         and    num_crd_ifv         = reg_det_ccnss.num_cred_infonavit
--         and    f_ini_desc_crd_ifv  = reg_det_ccnss.f_inicio_dcto_cre_inf
--         and    num_mov_periodo     = reg_det_ccnss.num_mov_periodo
--         and    nombre_imss         = reg_det_ccnss.nombre_trabajador
--         and    ult_sdi             = reg_det_ccnss.ultimo_salario_integrado
--         and    tpo_trabajador      = reg_det_ccnss.tipo_trabajador
--         and    jornada             = reg_det_ccnss.jornada_semana_reducida
         and    localiza_trabajador = reg_det_ccnss.localiza_trabajador
         and    destino_ap_viv      = reg_det_ccnss.destino_ap_viv
--         and    dias_cot_bim        = reg_det_ccnss.dias_cotizados_bim
--         and    dias_incap_bim      = reg_det_ccnss.dias_incapacidad_bim
--         and    dias_ausent_bim     = reg_det_ccnss.dias_ausentismo_bim
         and    imp_ap_pat          = reg_det_ccnss.imp_ap_pat_inf
         and    imp_am_cre          = reg_det_ccnss.imp_am_cre_inf
         and    imp_ren_viv_pgo_ext = reg_det_ccnss.imp_ren_viv_pgo_ext
--         and    marca_sua           = reg_det_ccnss.marca_cre_sua
--         and    marca_bdnsar        = reg_det_ccnss.marca_cre_bdnsar
         and    tpo_aclaracion      = reg_det_ccnss.diag_acl
--         and    id_derhab_nuevo     = reg_det_ccnss.id_derhab_nuevo   
--         and    ap_paterno_af       = reg_det_ccnss.ap_paterno_afore
--         and    ap_materno_af       = reg_det_ccnss.ap_materno_afore
--         and    nombre_af           = reg_det_ccnss.nombre_afore
         and    aiv_ap_pat          = reg_det_ccnss.ap_int_viv
         and    valor_aiv           = reg_det_ccnss.precio_ap_int_viv
         and    int_gen_pgo_ext     = reg_det_ccnss.int_gen_pag_ext_viv
         and    aiv_gen_pgo_ext     = reg_det_ccnss.num_int_gen_pgo_ext
         }