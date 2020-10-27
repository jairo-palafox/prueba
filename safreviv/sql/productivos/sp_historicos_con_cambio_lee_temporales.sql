






CREATE PROCEDURE "safreviv".sp_historicos_con_cambio_lee_temporales(p_origen_archivo SMALLINT )

RETURNING DECIMAL(9),  CHAR(1),     CHAR(3),  CHAR(11), CHAR(13), CHAR(6), DATE,    DECIMAL(6), CHAR(11),   CHAR(13),   CHAR(18),   CHAR(10),    DATE,
          DECIMAL(2),  CHAR(50),    CHAR(9),  CHAR(1),  CHAR(1), CHAR(1), CHAR(1),    DECIMAL(2), DECIMAL(2), DECIMAL(2), DECIMAL(14),
          DECIMAL(14), DECIMAL(14), CHAR(2),  CHAR(1),  CHAR(2), DATE,    CHAR(11),   CHAR(40),   CHAR(40),   CHAR(40),   DECIMAL(24), DECIMAL(24),
          DECIMAL(14), DECIMAL(24);


   --variables
   DEFINE tmp_det_id_derechohabiente      DECIMAL(9);
   DEFINE tmp_det_cc_tpo_registro         CHAR(1);
   DEFINE tmp_det_cc_cve_ent_receptora    CHAR(3);
   DEFINE tmp_det_cc_nrp                  CHAR(11);
   DEFINE tmp_det_cc_rfc_patron           CHAR(13);
   DEFINE tmp_det_cc_periodo_pago         CHAR(6);
   DEFINE tmp_det_cc_f_pago_patron        DATE;
   DEFINE tmp_det_cc_folio_sua            DECIMAL(6);
   DEFINE tmp_det_cc_nss                  CHAR(11);
   DEFINE tmp_det_cc_rfc                  CHAR(13);
   DEFINE tmp_det_cc_curp                 CHAR(18);
   DEFINE tmp_det_cc_num_crd_ifv          CHAR(10);
   DEFINE tmp_det_cc_f_ini_desc_crd_ifv   DATE;
   DEFINE tmp_det_cc_num_mov_periodo      DECIMAL(2);
   DEFINE tmp_det_cc_nombre_trabajador    CHAR(50);
   DEFINE tmp_det_cc_ult_sdi              CHAR(9);
   DEFINE tmp_det_cc_tpo_trabajador       CHAR(1);
   DEFINE tmp_det_cc_jornada              CHAR(1);
   DEFINE tmp_det_cc_localiza_trabajador  CHAR(1);
   DEFINE tmp_det_cc_destino_ap_viv       CHAR(1);
   DEFINE tmp_det_cc_dias_cot_bim         DECIMAL(2);
   DEFINE tmp_det_cc_dias_incap_bim       DECIMAL(2);
   DEFINE tmp_det_cc_dias_ausent_bim      DECIMAL(2);
   DEFINE tmp_det_cc_imp_ap_pat           DECIMAL(14);
   DEFINE tmp_det_cc_imp_am_cre           DECIMAL(14);
   DEFINE tmp_det_cc_imp_ren_viv          DECIMAL(14);
   DEFINE tmp_det_cc_marca_sua            CHAR(2);
   DEFINE tmp_det_cc_marca_bdnsar         CHAR(1);
   DEFINE tmp_det_cc_diag_aclaracion      CHAR(2);
   DEFINE tmp_det_cc_f_proceso            DATE;
   DEFINE tmp_det_cc_nss_dispersion       CHAR(11);
   DEFINE tmp_det_cc_paterno_afore        CHAR(40);
   DEFINE tmp_det_cc_materno_afore        CHAR(40);
   DEFINE tmp_det_cc_nombre_afore         CHAR(40);
   DEFINE tmp_det_cc_aivs                 DECIMAL(24);
   DEFINE tmp_det_cc_valor_aiv            DECIMAL(24);
   DEFINE tmp_det_cc_int_gen_pgo_ext      DECIMAL(14);
   DEFINE tmp_det_cc_aiv_gen_pgo_ext      DECIMAL(24);

   
   IF p_origen_archivo = 6 THEN
     
   FOREACH SELECT
   	       afi.id_derechohabiente   ,
   	       tmp.tpo_registro         ,
           tmp.cve_ent_receptora    ,
           tmp.nrp                  ,
           tmp.rfc_patron           ,
           tmp.periodo_pago         ,
           tmp.f_pago_patron        ,
           tmp.folio_sua            ,
           tmp.nss                  ,
           tmp.rfc                  ,
           tmp.curp                 ,
           tmp.num_crd_ifv          ,
           tmp.f_ini_desc_crd_ifv   ,
           tmp.num_mov_periodo      ,
           tmp.nombre_trabajador    ,
           tmp.ult_sdi/100          ,
           tmp.tpo_trabajador       ,
           tmp.jornada              ,
           tmp.localiza_trabajador  ,
           tmp.destino_ap_viv       ,
           tmp.dias_cot_bim         ,
           tmp.dias_incap_bim       ,
           tmp.dias_ausent_bim      ,
           tmp.imp_ap_pat/100       ,
           tmp.imp_am_cre/100       ,
           tmp.imp_ren_viv/100      ,
           tmp.marca_sua            ,
           tmp.marca_bdnsar         ,
           tmp.diag_aclaracion      ,
           tmp.f_proceso            ,
           tmp.nss_dispersion       ,
           tmp.paterno_afore        ,
           tmp.materno_afore        ,
           tmp.nombre_afore         ,
           tmp.aivs/1000000         ,
           tmp.valor_aiv/1000000    ,
           tmp.int_gen_pgo_ext/100  ,
           tmp.aiv_gen_pgo_ext/1000000
      INTO tmp_det_id_derechohabiente    ,
           tmp_det_cc_tpo_registro       ,
           tmp_det_cc_cve_ent_receptora  ,
           tmp_det_cc_nrp                ,
           tmp_det_cc_rfc_patron         ,
           tmp_det_cc_periodo_pago       ,
           tmp_det_cc_f_pago_patron      ,
           tmp_det_cc_folio_sua          ,
           tmp_det_cc_nss                ,
           tmp_det_cc_rfc                ,
           tmp_det_cc_curp               ,
           tmp_det_cc_num_crd_ifv        ,
           tmp_det_cc_f_ini_desc_crd_ifv ,
           tmp_det_cc_num_mov_periodo    ,
           tmp_det_cc_nombre_trabajador  ,
           tmp_det_cc_ult_sdi            ,
           tmp_det_cc_tpo_trabajador     ,
           tmp_det_cc_jornada            ,
           tmp_det_cc_localiza_trabajador,
           tmp_det_cc_destino_ap_viv     ,
           tmp_det_cc_dias_cot_bim       ,
           tmp_det_cc_dias_incap_bim     ,
           tmp_det_cc_dias_ausent_bim    ,
           tmp_det_cc_imp_ap_pat         ,
           tmp_det_cc_imp_am_cre         ,
           tmp_det_cc_imp_ren_viv        ,
           tmp_det_cc_marca_sua          ,
           tmp_det_cc_marca_bdnsar       ,
           tmp_det_cc_diag_aclaracion    ,
           tmp_det_cc_f_proceso          ,
           tmp_det_cc_nss_dispersion     ,
           tmp_det_cc_paterno_afore      ,
           tmp_det_cc_materno_afore      ,
           tmp_det_cc_nombre_afore       ,
           tmp_det_cc_aivs               ,
           tmp_det_cc_valor_aiv          ,
           tmp_det_cc_int_gen_pgo_ext    ,
           tmp_det_cc_aiv_gen_pgo_ext
      FROM safre_tmp:tmp_det_cc_nss tmp  ,
           afi_derechohabiente afi
     WHERE tmp.nss = afi.nss 
      
      RETURN 
           tmp_det_id_derechohabiente    ,
           tmp_det_cc_tpo_registro       ,
           tmp_det_cc_cve_ent_receptora  ,
           tmp_det_cc_nrp                ,
           tmp_det_cc_rfc_patron         ,
           tmp_det_cc_periodo_pago       ,
           tmp_det_cc_f_pago_patron      ,
           tmp_det_cc_folio_sua          ,
           tmp_det_cc_nss                ,
           tmp_det_cc_rfc                ,
           tmp_det_cc_curp               ,
           tmp_det_cc_num_crd_ifv        ,
           tmp_det_cc_f_ini_desc_crd_ifv ,
           tmp_det_cc_num_mov_periodo    ,
           tmp_det_cc_nombre_trabajador  ,
           tmp_det_cc_ult_sdi            ,
           tmp_det_cc_tpo_trabajador     ,
           tmp_det_cc_jornada            ,
           tmp_det_cc_localiza_trabajador,
           tmp_det_cc_destino_ap_viv     ,
           tmp_det_cc_dias_cot_bim       ,
           tmp_det_cc_dias_incap_bim     ,
           tmp_det_cc_dias_ausent_bim    ,
           tmp_det_cc_imp_ap_pat         ,
           tmp_det_cc_imp_am_cre         ,
           tmp_det_cc_imp_ren_viv        ,
           tmp_det_cc_marca_sua          ,
           tmp_det_cc_marca_bdnsar       ,
           tmp_det_cc_diag_aclaracion    ,
           tmp_det_cc_f_proceso          ,
           tmp_det_cc_nss_dispersion     ,
           tmp_det_cc_paterno_afore      ,
           tmp_det_cc_materno_afore      ,
           tmp_det_cc_nombre_afore       ,
           tmp_det_cc_aivs               ,
           tmp_det_cc_valor_aiv          ,
           tmp_det_cc_int_gen_pgo_ext    ,
           tmp_det_cc_aiv_gen_pgo_ext    
           WITH RESUME;
           
       END FOREACH ;
   ELSE 
   	  FOREACH SELECT
   	  	   afi.id_derechohabiente   ,
   	       tmp.tpo_registro         ,
           tmp.cve_ent_receptora    ,
           tmp.nrp                  ,
           tmp.rfc_patron           ,
           tmp.periodo_pago         ,
           tmp.f_pago_patron        ,
           tmp.folio_sua            ,
           tmp.nss                  ,
           tmp.rfc                  ,
           tmp.curp                 ,
           tmp.num_crd_ifv          ,
           tmp.f_ini_desc_crd_ifv   ,
           tmp.num_mov_periodo      ,
           tmp.nombre_trabajador    ,
           tmp.ult_sdi/100          ,
           tmp.tpo_trabajador       ,
           tmp.jornada              ,
           tmp.localiza_trabajador  ,
           tmp.destino_ap_viv       ,
           tmp.dias_cot_bim         ,
           tmp.dias_incap_bim       ,
           tmp.dias_ausent_bim      ,
           tmp.imp_ap_pat/100       ,
           tmp.imp_am_cre/100       ,
           tmp.imp_ren_viv/100      ,
           tmp.marca_sua            ,
           tmp.marca_bdnsar         ,
           tmp.diag_aclaracion      ,
           tmp.f_proceso            ,
           tmp.nss_dispersion       ,
           tmp.paterno_afore        ,
           tmp.materno_afore        ,
           tmp.nombre_afore         ,
           tmp.aivs/1000000         ,
           tmp.valor_aiv/1000000    ,
           tmp.int_gen_pgo_ext/100  ,
           tmp.aiv_gen_pgo_ext/1000000
      INTO tmp_det_id_derechohabiente    ,
           tmp_det_cc_tpo_registro       ,
           tmp_det_cc_cve_ent_receptora  ,
           tmp_det_cc_nrp                ,
           tmp_det_cc_rfc_patron         ,
           tmp_det_cc_periodo_pago       ,
           tmp_det_cc_f_pago_patron      ,
           tmp_det_cc_folio_sua          ,
           tmp_det_cc_nss                ,
           tmp_det_cc_rfc                ,
           tmp_det_cc_curp               ,
           tmp_det_cc_num_crd_ifv        ,
           tmp_det_cc_f_ini_desc_crd_ifv ,
           tmp_det_cc_num_mov_periodo    ,
           tmp_det_cc_nombre_trabajador  ,
           tmp_det_cc_ult_sdi            ,
           tmp_det_cc_tpo_trabajador     ,
           tmp_det_cc_jornada            ,
           tmp_det_cc_localiza_trabajador,
           tmp_det_cc_destino_ap_viv     ,
           tmp_det_cc_dias_cot_bim       ,
           tmp_det_cc_dias_incap_bim     ,
           tmp_det_cc_dias_ausent_bim    ,
           tmp_det_cc_imp_ap_pat         ,
           tmp_det_cc_imp_am_cre         ,
           tmp_det_cc_imp_ren_viv        ,
           tmp_det_cc_marca_sua          ,
           tmp_det_cc_marca_bdnsar       ,
           tmp_det_cc_diag_aclaracion    ,
           tmp_det_cc_f_proceso          ,
           tmp_det_cc_nss_dispersion     ,
           tmp_det_cc_paterno_afore      ,
           tmp_det_cc_materno_afore      ,
           tmp_det_cc_nombre_afore       ,
           tmp_det_cc_aivs               ,
           tmp_det_cc_valor_aiv          ,
           tmp_det_cc_int_gen_pgo_ext    ,
           tmp_det_cc_aiv_gen_pgo_ext
      FROM safre_tmp:tmp_det_cc_nom tmp  ,
           afi_derechohabiente afi
     WHERE tmp.nss = afi.nss 
      
      RETURN
           tmp_det_id_derechohabiente    ,
           tmp_det_cc_tpo_registro       ,
           tmp_det_cc_cve_ent_receptora  ,
           tmp_det_cc_nrp                ,
           tmp_det_cc_rfc_patron         ,
           tmp_det_cc_periodo_pago       ,
           tmp_det_cc_f_pago_patron      ,
           tmp_det_cc_folio_sua          ,
           tmp_det_cc_nss                ,
           tmp_det_cc_rfc                ,
           tmp_det_cc_curp               ,
           tmp_det_cc_num_crd_ifv        ,
           tmp_det_cc_f_ini_desc_crd_ifv ,
           tmp_det_cc_num_mov_periodo    ,
           tmp_det_cc_nombre_trabajador  ,
           tmp_det_cc_ult_sdi            ,
           tmp_det_cc_tpo_trabajador     ,
           tmp_det_cc_jornada            ,
           tmp_det_cc_localiza_trabajador,
           tmp_det_cc_destino_ap_viv     ,
           tmp_det_cc_dias_cot_bim       ,
           tmp_det_cc_dias_incap_bim     ,
           tmp_det_cc_dias_ausent_bim    ,
           tmp_det_cc_imp_ap_pat         ,
           tmp_det_cc_imp_am_cre         ,
           tmp_det_cc_imp_ren_viv        ,
           tmp_det_cc_marca_sua          ,
           tmp_det_cc_marca_bdnsar       ,
           tmp_det_cc_diag_aclaracion    ,
           tmp_det_cc_f_proceso          ,
           tmp_det_cc_nss_dispersion     ,
           tmp_det_cc_paterno_afore      ,
           tmp_det_cc_materno_afore      ,
           tmp_det_cc_nombre_afore       ,
           tmp_det_cc_aivs               ,
           tmp_det_cc_valor_aiv          ,
           tmp_det_cc_int_gen_pgo_ext    ,
           tmp_det_cc_aiv_gen_pgo_ext    
           WITH RESUME;
     END FOREACH ;
   END IF
   
   
END PROCEDURE ;


