






CREATE PROCEDURE "safreviv".sp_carga_inicial_aclaraciones(p_folio DECIMAL(9,0))
RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(11), DECIMAL(9,0)
   --TABLA tmp_det_aclaracion
   DEFINE tmp_det_acl_tpo_registro         char(1);
   DEFINE tmp_det_acl_cve_ent_receptora    char(3);
   DEFINE tmp_det_acl_f_pago_patron        date;
   DEFINE tmp_det_acl_nrp                  char(11);
   DEFINE tmp_det_acl_periodo_pago         char(6);
   DEFINE tmp_det_acl_folio_sua            char(6);
   DEFINE tmp_det_acl_nss                  char(11);
   DEFINE tmp_det_acl_rfc                  char(13);
   DEFINE tmp_det_acl_curp                 char(18);
   DEFINE tmp_det_acl_num_crd_ifv          char(10);
   DEFINE tmp_det_acl_f_ini_desc_cre_ifv   date;
   DEFINE tmp_det_acl_nombre_trabajador    char(50);
   DEFINE tmp_det_acl_ult_sdi              decimal(15,2);
   DEFINE tmp_det_acl_dias_cot_bim         decimal(4,0);
   DEFINE tmp_det_acl_dias_incap_bim       decimal(4,0);
   DEFINE tmp_det_acl_dias_ausent_bim      decimal(4,0);
   DEFINE tmp_det_acl_imp_ap_pat           decimal(12,2);
   DEFINE tmp_det_acl_imp_am_cre           decimal(12,2);
   DEFINE tmp_det_acl_ind_crd_ifv          char(1);
   DEFINE tmp_det_acl_tpo_aclaracion       char(2);
   DEFINE tmp_det_acl_aivs                 decimal(18,6);
   DEFINE tmp_det_acl_valor_aiv            decimal(18,6);
   DEFINE tmp_det_acl_int_pag_extemp       DECIMAL(12,2);
   DEFINE tmp_det_acl_aivs_pag_extemp      DECIMAL(18,6);

   --TABLA tmp_sum_aclaracion
   DEFINE tmp_sum_acl_tpo_registro         CHAR(1);
   DEFINE tmp_sum_acl_tot_ap               DECIMAL(9,0);
   DEFINE tmp_sum_acl_suma_ap_pat          DECIMAL(20,2);
   DEFINE tmp_sum_acl_suma_am              DECIMAL(20,2);
   DEFINE tmp_sum_acl_suma_aivs            DECIMAL(20,2);
   DEFINE tmp_sum_acl_suma_int_pag_extemp  DECIMAL(13,2);

   --TABLA acl_sum_aclaracion
   DEFINE acl_sum_acl_folio                DECIMAL(9,0);
   DEFINE acl_sum_acl_tpo_registro         CHAR(2);
   DEFINE acl_sum_acl_tot_ap               INTEGER;
   DEFINE acl_sum_acl_suma_ap_pat          DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_am              DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_aivs            DECIMAL(18,6);
   DEFINE acl_sum_acl_int_pag_extemp       DECIMAL(18,2); -- suma de intereses de pago extemporaneo
   
   DEFINE ld_ide_derechohabiente           DECIMAL(9,0);
   DEFINE contador                         DECIMAL(9,0);
   DEFINE v_tipo_trabajador   CHAR(1);

   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

   -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         DECIMAL(12,0); -- registros detalle
   DEFINE v_num_registros_detalle_sum     DECIMAL(12,0); -- registros de detalle indicados en sumario

   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;
   DEFINE v_err_suma_int_pag_extemp_no_coincide         SMALLINT; -- error por diferencia en intereses de pagos extemporaneos
   
   DEFINE tmp_det_acl_nrp_short            CHAR(2);

   DEFINE v_ind_liquidacion SMALLINT;
   DEFINE v_tpo_patron      CHAR(02);
   DEFINE v_result_operacion SMALLINT;
   DEFINE v_nss              CHAR(11); -- para rastreo de error

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt, v_nss, contador;
   END EXCEPTION

   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   LET v_err_suma_int_pag_extemp_no_coincide         = 5;
   LET v_nss = "00000000000";
   
   LET tmp_det_acl_nrp_short = "";

   LET v_tpo_patron = "";
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/historicoenaclara.trace';
   --TRACE 'Inicia el store procedure de registro historicos de EN ACLARACION';
   -- se inicia el contador
   LET contador = 1;

   -- Se asume que el archivo no se integro correctamente
   UPDATE glo_ctr_archivo
      SET folio = p_folio,
          estado = 3 -- reversado
    WHERE proceso_cod    = 110 -- en aclaracion
      AND opera_cod      = 1 -- archivo cargado
      AND estado         = 1; -- etapa de carga

--======================================================
--======================================================

--   SELECT  SUM(suma_ap_pat)/100
--          ,SUM(suma_am    )/100
--          ,SUM(suma_aivs  )/1000000
--          ,SUM(suma_int_pag_extemp)/100
--   INTO  acl_sum_acl_suma_ap_pat
--        ,acl_sum_acl_suma_am    
--        ,acl_sum_acl_suma_aivs
--        ,tmp_sum_acl_suma_int_pag_extemp
--   FROM safre_mig:tmp_sum_aclaracion;

   -- se obtiene la suma de los monto de la tabla de detalle
--   SELECT  SUM(imp_ap_pat) / 100 
--          ,SUM(imp_am_cre) / 100 
--          ,SUM(aivs      ) / 1000000
--          ,sum(int_pag_extemp) / 100
--   INTO  acl_det_acl_imp_ap_pat 
--        ,acl_det_acl_imp_am_cre 
--        ,acl_det_acl_aivs
--        ,acl_sum_acl_int_pag_extemp
--   FROM safre_mig:tmp_det_aclaracion;

--   -- se verifica que el numero de registros en la tabla de detalle sea el mismo
--   -- que el encontrado en el sumario
--   SELECT COUNT(*)
--   INTO v_num_registros_detalle
--   FROM safre_mig:tmp_det_aclaracion;

--   SELECT tot_ap
--   INTO v_num_registros_detalle_sum
--   FROM safre_mig:tmp_sum_aclaracion;

--   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
--      -- se rechaza el archivo
--      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
--      LET isam_err       = 0;
--      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";
--      RETURN v_si_resultado, isam_err, err_txt;
--   END IF

--   -- ========================================================================
--   -- se verifican las cifras de montos totales recibidos
--   -- ========================================================================

--   -- aportaciones patronales
--   IF ( acl_sum_acl_suma_ap_pat <> acl_det_acl_imp_ap_pat ) THEN
--      -- se rechaza el archivo
--      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
--      LET isam_err       = 0;
--      LET err_txt        = "El monto de aportaciones patronales de detalle cargado no corresponde con el reportado en archivo.";
--      RETURN v_si_resultado, isam_err, err_txt;
--   END IF

   -- amortizacion del credito
--   IF ( acl_sum_acl_suma_am <> acl_det_acl_imp_am_cre ) THEN
--      -- se rechaza el archivo
--      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
--      LET isam_err       = 0;
--      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo: " || acl_det_acl_imp_am_cre || " - " || acl_sum_acl_suma_am ;
--      RETURN v_si_resultado, isam_err, err_txt;
--   END IF

--   -- aplicacion de intereses de vivienda
--   IF ( acl_sum_acl_suma_aivs <> acl_det_acl_aivs ) THEN
--      -- se rechaza el archivo
--      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
--      LET isam_err       = 0;
--      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";
--      RETURN v_si_resultado, isam_err, err_txt;
--   END IF

--   -- INTERESES DE PAGO EXTEMPORANEO
--   IF ( tmp_sum_acl_suma_int_pag_extemp <> acl_sum_acl_int_pag_extemp ) THEN
--      -- se rechaza el archivo
--      LET v_si_resultado = v_err_suma_int_pag_extemp_no_coincide;
--      LET isam_err       = 0;
--      LET err_txt        = "El monto de intereses de pagos extemporáneos de detalle cargado no corresponde con el reportado en archivo";
--      RETURN v_si_resultado, isam_err, err_txt;
--   END IF
   
--========================================
--========================================

   CREATE TEMP TABLE tmp_nss_cuenta
   (
     nss CHAR(11)
   );

   SET PDQPRIORITY HIGH;

   SET INDEXES xie2cta_his_pagos DISABLED;
   SET INDEXES xie4cta_his_pagos DISABLED;

--=====================================================
--=====   PRIMER FOREACH DE NSS ENCONTRADOS       =====

   FOREACH SELECT afi.id_derechohabiente
                  ,tmp.tpo_registro 
                  ,tmp.cve_ent_receptora 
                  ,tmp.f_pago_patron 
                  ,tmp.nrp 
                  ,tmp.periodo_pago 
                  ,tmp.folio_sua 
                  ,tmp.nss  
                  ,tmp.rfc  
                  ,tmp.curp 
                  ,tmp.num_crd_ifv  
                  ,tmp.f_ini_desc_cre_ifv
                  ,tmp.nom_trabajador 
                  ,tmp.ult_sdi /100
                  ,tmp.dias_cot_bim   
                  ,tmp.dias_incap_bim 
                  ,tmp.dias_ausent_bim
                  ,tmp.imp_ap_pat /100
                  ,tmp.imp_am_cre /100
                  ,tmp.ind_crd_ifv 
                  ,tmp.tpo_aclaracion 
                  ,tmp.aivs      /1000000
                  ,tmp.valor_aiv /1000000
                  ,tmp.int_pag_extemp  / 100    -- campos agregados 6 sep 2012
                  ,tmp.aivs_pag_extemp / 1000000
           INTO ld_ide_derechohabiente 
                ,tmp_det_acl_tpo_registro
                ,tmp_det_acl_cve_ent_receptora
                ,tmp_det_acl_f_pago_patron
                ,tmp_det_acl_nrp
                ,tmp_det_acl_periodo_pago
                ,tmp_det_acl_folio_sua
                ,tmp_det_acl_nss
                ,tmp_det_acl_rfc
                ,tmp_det_acl_curp
                ,tmp_det_acl_num_crd_ifv
                ,tmp_det_acl_f_ini_desc_cre_ifv
                ,tmp_det_acl_nombre_trabajador
                ,tmp_det_acl_ult_sdi
                ,tmp_det_acl_dias_cot_bim
                ,tmp_det_acl_dias_incap_bim
                ,tmp_det_acl_dias_ausent_bim
                ,tmp_det_acl_imp_ap_pat
                ,tmp_det_acl_imp_am_cre
                ,tmp_det_acl_ind_crd_ifv
                ,tmp_det_acl_tpo_aclaracion
                ,tmp_det_acl_aivs
                ,tmp_det_acl_valor_aiv
                ,tmp_det_acl_int_pag_extemp  -- campos agregados 6 sep 2012
                ,tmp_det_acl_aivs_pag_extemp
           FROM safre_mig:tmp_det_aclaracion tmp,
                afi_derechohabiente afi 
           where tmp.nss = afi.nss

      --se asigna el tipo de patron con las posiciones 1,2 del nrp
      INSERT INTO tmp_nss_cuenta VALUES ( tmp_det_acl_nss );

     --Inicializacion de variables
      LET v_tpo_patron = tmp_det_acl_nrp;
      LET v_result_operacion = 1;
      
--======================================================================
      -- Regla Negocio de Adelantamientos--

      IF v_tpo_patron = "99" THEN

         LET v_tipo_trabajador = "S";
         LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI

      ELSE

         LET v_tipo_trabajador = "I";

         IF (tmp_det_acl_tpo_aclaracion = "13" OR
             tmp_det_acl_tpo_aclaracion = "17") THEN

            LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

         ELSE

            LET v_ind_liquidacion  = 1; --ACL normal SIN liquidar
            LET v_result_operacion = 2;
            
         END IF
      END IF

      LET  tmp_det_acl_nrp_short =  tmp_det_acl_nrp;
      -- Inserta a tabla cta_his_pagos para control de historicos
      INSERT INTO cta_his_pagos(folio              
                               ,origen_archivo     
                               ,id_referencia      
                               ,cve_ent_receptora  
                               ,nrp                
                               ,periodo_pago       
                               ,folio_sua          
                               ,f_pago             
                               ,id_derechohabiente 
                               ,localiza_trabajador
                               ,tpo_aclaracion     
                               ,imp_ap_pat         
                               ,imp_am_cre         
                               ,imp_ren_viv_pgo_ext
                               ,aiv_ap_pat         
                               ,valor_aiv          
                               ,int_gen_pgo_ext    
                               ,aiv_gen_pgo_ext    
                               ,result_operacion   
                               ,ind_liquidacion    
                               ,num_crd_ifv        
                               ,f_proceso          
                               ,tpo_patron         
                               ,folio_referencia)
                     VALUES (p_folio 
                            ,4                             -- origen_archivo=8
                            ,seq_cta_his_pagos.NEXTVAL 
                            ,tmp_det_acl_cve_ent_receptora 
                            ,tmp_det_acl_nrp 
                            ,tmp_det_acl_periodo_pago  
                            ,tmp_det_acl_folio_sua 
                            ,tmp_det_acl_f_pago_patron 
                            ,ld_ide_derechohabiente 
                            ,NULL 
                            ,tmp_det_acl_tpo_aclaracion 
                            ,tmp_det_acl_imp_ap_pat
                            ,tmp_det_acl_imp_am_cre 
                            ,NULL 
                            ,tmp_det_acl_aivs 
                            ,tmp_det_acl_valor_aiv  
                            ,tmp_det_acl_int_pag_extemp  -- INTERESES de pagos extemporaneos. 6 sep 2012
                            ,tmp_det_acl_aivs_pag_extemp -- AIVS de pagos extemporaneos
                            ,v_result_operacion 
                            ,v_ind_liquidacion                -- nace carga con ind_liquidacion = 1 Aclaraciones SIN Liquidar
                            ,tmp_det_acl_num_crd_ifv
                            ,TODAY 
                            ,v_tpo_patron  
                            ,0);

      -- Inserta a tabla cta_pag_complemento para control de complementos
      INSERT INTO cta_pag_complemento(folio
                                     ,origen_archivo
                                     ,id_referencia
                                     ,id_derechohabiente
                                     ,rfc_patron
                                     ,rfc
                                     ,curp
                                     ,num_mov_periodo
                                     ,f_ini_desc_crd_ifv
                                     ,ult_sdi
                                     ,tpo_trabajador
                                     ,jornada
                                     ,destino_ap_viv
                                     ,dias_cot_bim
                                     ,dias_incap_bim
                                     ,dias_ausent_bim
                                     ,marca_sua
                                     ,marca_bdnsar)
                     VALUES(p_folio
                           ,4                                   -- origen_archivo=8
                           ,seq_cta_his_pagos.CURRVAL
                           ,ld_ide_derechohabiente
                           ,NULL
                           ,tmp_det_acl_rfc
                           ,tmp_det_acl_curp
                           ,0
                           ,tmp_det_acl_f_ini_desc_cre_ifv
                           ,tmp_det_acl_ult_sdi
                           ,NULL
                           ,NULL
                           ,NULL
                           ,tmp_det_acl_dias_cot_bim
                           ,tmp_det_acl_dias_incap_bim
                           ,tmp_det_acl_dias_ausent_bim
                           ,NULL
                           ,NULL);
                           
      LET contador = contador + 1;
   END FOREACH

   CREATE INDEX nss_cuenta ON tmp_nss_cuenta(nss);

   UPDATE STATISTICS FOR TABLE tmp_nss_cuenta;

--=====================================================
--=====   SEGUNDO FOREACH DE NSS ENCONTRADOS       =====

   FOREACH SELECT tmp.tpo_registro 
                  ,tmp.cve_ent_receptora 
                  ,tmp.f_pago_patron 
                  ,tmp.nrp 
                  ,tmp.periodo_pago 
                  ,tmp.folio_sua 
                  ,tmp.nss  
                  ,tmp.rfc  
                  ,tmp.curp 
                  ,tmp.num_crd_ifv  
                  ,tmp.f_ini_desc_cre_ifv
                  ,tmp.nom_trabajador 
                  ,tmp.ult_sdi /100
                  ,tmp.dias_cot_bim   
                  ,tmp.dias_incap_bim 
                  ,tmp.dias_ausent_bim
                  ,tmp.imp_ap_pat /100
                  ,tmp.imp_am_cre /100
                  ,tmp.ind_crd_ifv 
                  ,tmp.tpo_aclaracion 
                  ,tmp.aivs      /1000000
                  ,tmp.valor_aiv /1000000
                  ,tmp.int_pag_extemp  / 100    -- campos agregados 6 sep 2012
                  ,tmp.aivs_pag_extemp / 1000000
           INTO tmp_det_acl_tpo_registro
                ,tmp_det_acl_cve_ent_receptora
                ,tmp_det_acl_f_pago_patron
                ,tmp_det_acl_nrp
                ,tmp_det_acl_periodo_pago
                ,tmp_det_acl_folio_sua
                ,tmp_det_acl_nss
                ,tmp_det_acl_rfc
                ,tmp_det_acl_curp
                ,tmp_det_acl_num_crd_ifv
                ,tmp_det_acl_f_ini_desc_cre_ifv
                ,tmp_det_acl_nombre_trabajador
                ,tmp_det_acl_ult_sdi
                ,tmp_det_acl_dias_cot_bim
                ,tmp_det_acl_dias_incap_bim
                ,tmp_det_acl_dias_ausent_bim
                ,tmp_det_acl_imp_ap_pat
                ,tmp_det_acl_imp_am_cre
                ,tmp_det_acl_ind_crd_ifv
                ,tmp_det_acl_tpo_aclaracion
                ,tmp_det_acl_aivs
                ,tmp_det_acl_valor_aiv
                ,tmp_det_acl_int_pag_extemp  -- campos agregados 6 sep 2012
                ,tmp_det_acl_aivs_pag_extemp
           FROM safre_mig:tmp_det_aclaracion tmp
           WHERE tmp.nss NOT IN (SELECT afi.nss FROM tmp_nss_cuenta afi)


     --Inicializacion de variables
      LET v_tpo_patron = tmp_det_acl_nrp;
      LET v_result_operacion = 1;

--======================================================================

      --Obtiene el identificador del derechohabiente
      SELECT id_derechohabiente
      INTO   ld_ide_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_det_acl_nss;

      LET v_nss = tmp_det_acl_nss;

      --Si no existe informacion en la tabla "afi_derechohabiente",
      --entonces se inserta el registro en dicha tabla (aperturar una cuenta)
      IF (ld_ide_derechohabiente IS NULL) THEN
                                                                       
           LET ld_ide_derechohabiente = fn_apertura_cuenta_pag(tmp_det_acl_nss 
                                                            ,tmp_det_acl_curp
                                                            ,tmp_det_acl_rfc 
                                                            ,1
                                                            ,tmp_det_acl_nombre_trabajador
                                                            ,v_tipo_trabajador
                                                            ,0                   -- Credito. se da por omision
                                                            ,4                   --4=CARGA INICIAL ENACLARA,
                                                            ,p_folio             -- folio del lote
                                                            ,"R");               -- origen afiliacion
         --TRACE "despues de derechohab. null";
      END IF

--======================================================================
      -- Regla Negocio de Adelantamientos--

      IF v_tpo_patron = "99" THEN

         LET v_tipo_trabajador = "S";
         LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI

      ELSE

         LET v_tipo_trabajador = "I";

         IF (tmp_det_acl_tpo_aclaracion = "13" OR
             tmp_det_acl_tpo_aclaracion = "17") THEN

            LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

         ELSE

            LET v_ind_liquidacion  = 1; --ACL normal SIN liquidar
            LET v_result_operacion = 2;
            
         END IF
      END IF

      LET  tmp_det_acl_nrp_short =  tmp_det_acl_nrp;
      -- Inserta a tabla cta_his_pagos para control de historicos
      INSERT INTO cta_his_pagos(folio              
                               ,origen_archivo     
                               ,id_referencia      
                               ,cve_ent_receptora  
                               ,nrp                
                               ,periodo_pago       
                               ,folio_sua          
                               ,f_pago             
                               ,id_derechohabiente 
                               ,localiza_trabajador
                               ,tpo_aclaracion     
                               ,imp_ap_pat         
                               ,imp_am_cre         
                               ,imp_ren_viv_pgo_ext
                               ,aiv_ap_pat         
                               ,valor_aiv          
                               ,int_gen_pgo_ext    
                               ,aiv_gen_pgo_ext    
                               ,result_operacion   
                               ,ind_liquidacion    
                               ,num_crd_ifv        
                               ,f_proceso          
                               ,tpo_patron         
                               ,folio_referencia)
                     VALUES (p_folio 
                            ,4                             -- origen_archivo=8
                            ,seq_cta_his_pagos.NEXTVAL 
                            ,tmp_det_acl_cve_ent_receptora 
                            ,tmp_det_acl_nrp 
                            ,tmp_det_acl_periodo_pago  
                            ,tmp_det_acl_folio_sua 
                            ,tmp_det_acl_f_pago_patron 
                            ,ld_ide_derechohabiente 
                            ,NULL 
                            ,tmp_det_acl_tpo_aclaracion 
                            ,tmp_det_acl_imp_ap_pat
                            ,tmp_det_acl_imp_am_cre 
                            ,NULL 
                            ,tmp_det_acl_aivs 
                            ,tmp_det_acl_valor_aiv  
                            ,tmp_det_acl_int_pag_extemp  -- INTERESES de pagos extemporaneos. 6 sep 2012
                            ,tmp_det_acl_aivs_pag_extemp -- AIVS de pagos extemporaneos
                            ,v_result_operacion 
                            ,v_ind_liquidacion                -- nace carga con ind_liquidacion = 1 Aclaraciones SIN Liquidar
                            ,tmp_det_acl_num_crd_ifv
                            ,TODAY 
                            ,v_tpo_patron  
                            ,0);

      -- Inserta a tabla cta_pag_complemento para control de complementos
      INSERT INTO cta_pag_complemento(folio
                                     ,origen_archivo
                                     ,id_referencia
                                     ,id_derechohabiente
                                     ,rfc_patron
                                     ,rfc
                                     ,curp
                                     ,num_mov_periodo
                                     ,f_ini_desc_crd_ifv
                                     ,ult_sdi
                                     ,tpo_trabajador
                                     ,jornada
                                     ,destino_ap_viv
                                     ,dias_cot_bim
                                     ,dias_incap_bim
                                     ,dias_ausent_bim
                                     ,marca_sua
                                     ,marca_bdnsar)
                     VALUES(p_folio
                           ,4                                   -- origen_archivo=8
                           ,seq_cta_his_pagos.CURRVAL
                           ,ld_ide_derechohabiente
                           ,NULL
                           ,tmp_det_acl_rfc
                           ,tmp_det_acl_curp
                           ,0
                           ,tmp_det_acl_f_ini_desc_cre_ifv
                           ,tmp_det_acl_ult_sdi
                           ,NULL
                           ,NULL
                           ,NULL
                           ,tmp_det_acl_dias_cot_bim
                           ,tmp_det_acl_dias_incap_bim
                           ,tmp_det_acl_dias_ausent_bim
                           ,NULL
                           ,NULL);
                           
      LET contador = contador + 1;
   END FOREACH

   FOREACH SELECT  tpo_registro  
                  ,tot_ap       
                  ,suma_ap_pat/100  
                  ,suma_am/100
                  ,suma_aivs/1000000
           INTO  tmp_sum_acl_tpo_registro 
                ,tmp_sum_acl_tot_ap       
                ,tmp_sum_acl_suma_ap_pat  
                ,tmp_sum_acl_suma_am      
                ,tmp_sum_acl_suma_aivs
           FROM safre_mig:tmp_sum_aclaracion

      --Actualizar la precision para los campos correspondientes
      LET acl_sum_acl_suma_ap_pat   = tmp_sum_acl_suma_ap_pat;
      LET acl_sum_acl_suma_am       = tmp_sum_acl_suma_am;
      LET acl_sum_acl_suma_aivs     = tmp_sum_acl_suma_aivs;

      INSERT INTO acl_sum_aclaracion (folio        
                                     ,tpo_registro 
                                     ,tot_ap       
                                     ,suma_ap_pat  
                                     ,suma_am      
                                     ,suma_aivs)
           VALUES (p_folio                  
                  ,tmp_sum_acl_tpo_registro 
                  ,tmp_sum_acl_tot_ap       
                  ,acl_sum_acl_suma_ap_pat  
                  ,acl_sum_acl_suma_am      
                  ,acl_sum_acl_suma_aivs);
   END FOREACH

--=====================================================
--=====================================================
   SET INDEXES xie2cta_his_pagos ENABLED;
   SET INDEXES xie4cta_his_pagos ENABLED;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE safre_mig:glo_ctr_archivo
      SET folio = p_folio,
          estado = 2 -- integrado
    WHERE proceso_cod    = 110 -- en aclaracion
      AND folio IS NULL;

   UPDATE safre_mig:bat_ctr_operacion
      SET folio = p_folio
    WHERE proceso_cod = 110
      AND opera_cod = 2;

   --UPDATE STATISTICS FOR TABLE acl_det_aclaracion  ;
   UPDATE STATISTICS FOR TABLE cta_his_pagos       ;
   UPDATE STATISTICS FOR TABLE acl_sum_aclaracion  ;

   --TRACE 'Finaliza el store procedure de registro historicos de EN ACLARACION';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt, v_nss, contador;
END PROCEDURE;


