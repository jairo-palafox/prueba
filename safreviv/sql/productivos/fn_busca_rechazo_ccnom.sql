






CREATE FUNCTION "safreviv".fn_busca_rechazo_ccnom
                                     (v_cve_ent_receptora   char(3), 
                                      v_nrp                 char(11),                
                                      v_rfc_patron          char(13),         
                                      v_periodo_pago        char(6),       
                                      v_f_pago              date,             
                                      v_folio_sua           decimal(6,0),          
                                      v_nss                 char(11),                
                                      v_rfc                 char(13),                
                                      v_curp                char(18),               
                                      v_num_crd_ifv         decimal(10,0),        
                                      v_f_ini_desc_crd_ifv  date, 
                                      v_num_mov_periodo     decimal(12,0),    
                                      v_nombre_imss         char(50),        
                                      v_ult_sdi             decimal(7,2),            
                                      v_tpo_trabajador      char(1),     
                                      v_jornada             char(1),            
                                      v_localiza_trabajador char(1),
                                      v_destino_ap_viv      char(1),     
                                      v_dias_cot_bim        smallint,       
                                      v_dias_incap_bim      smallint,     
                                      v_dias_ausent_bim     smallint,    
                                      v_imp_ap_pat          decimal(12,2),         
                                      v_imp_am_cre          decimal(12,2),         
                                      v_imp_ren_viv_pgo_ext decimal(12,2),
                                      v_marca_sua           char(2),          
                                      v_marca_bdnsar        char(1),       
                                      v_tpo_aclaracion      char(2),
                                      v_id_derhab_nuevo     decimal(9,0),   
                                      v_ap_paterno_af       char(40),  
                                      v_ap_materno_af       char(40),  
                                      v_nombre_af           char(40),      
                                      v_aiv_ap_pat          decimal(18,6),         
                                      v_valor_aiv           decimal(18,6),          
                                      v_int_gen_pgo_ext     decimal(12,2),    
                                      v_aiv_gen_pgo_ext     decimal(18,6))

   RETURNING CHAR(32);

   DEFINE v_rechazo CHAR(32);

   LET v_rechazo = NULL; 

   FOREACH
   	  SELECT nvl(acl.codigo_rechazo||'-'||TRIM(cat.descripcion),"13-SIN LQINFO Y HAY SALIDA")
      INTO   v_rechazo
      FROM   cta_rechazos_acl a
               LEFT OUTER JOIN acl_pag_rechazo acl
                  ON (a.folio = acl.folio AND a.id_referencia = acl.id_referencia)
               LEFT OUTER JOIN acl_cat_rechazo cat
                  ON (acl.codigo_rechazo = cat.codigo_rechazo),
             cta_pag_complemento b,
             afi_derechohabiente c
      WHERE a.folio = b.folio
      AND   a.id_derechohabiente = c.id_derechohabiente
      AND   a.id_referencia = b.id_referencia
      AND   a.result_operacion IN (2,3)
      AND   (a.tpo_aclaracion = '' OR a.tpo_aclaracion IS NULL)
      AND   a.origen_archivo = 7
----      AND   a.origen_archivo = v_origen
      --AND   acl.codigo_rechazo is not null
      and a.cve_ent_receptora   = v_cve_ent_receptora
      and a.nrp                 = v_nrp
      and b.rfc_patron          = v_rfc_patron
      and a.periodo_pago        = v_periodo_pago
      and a.f_pago              = v_f_pago
      and a.folio_sua           = v_folio_sua
      and c.nss                 = v_nss
      and c.rfc                 = v_rfc
      and c.curp                = v_curp
      and a.num_crd_ifv         = v_num_crd_ifv
      and b.f_ini_desc_crd_ifv  = v_f_ini_desc_crd_ifv
      and b.num_mov_periodo     = v_num_mov_periodo
      and c.nombre_imss         = v_nombre_imss
      and b.ult_sdi             = v_ult_sdi
      and b.tpo_trabajador      = v_tpo_trabajador
      and b.jornada             = v_jornada
      and a.localiza_trabajador = v_localiza_trabajador
      and b.destino_ap_viv      = v_destino_ap_viv
      and b.dias_cot_bim        = v_dias_cot_bim
      and b.dias_incap_bim      = v_dias_incap_bim
      and b.dias_ausent_bim     = v_dias_ausent_bim
      and a.imp_ap_pat          = v_imp_ap_pat
      and a.imp_am_cre          = v_imp_am_cre
      and a.imp_ren_viv_pgo_ext = v_imp_ren_viv_pgo_ext
      and b.marca_sua           = v_marca_sua
      and b.marca_bdnsar        = v_marca_bdnsar
      and a.tpo_aclaracion      = v_tpo_aclaracion
      and b.id_derhab_nuevo     =   v_id_derhab_nuevo   
      and c.ap_paterno_af       = v_ap_paterno_af
      and c.ap_materno_af       = v_ap_materno_af
      and c.nombre_af           = v_nombre_af
      and a.aiv_ap_pat          = v_aiv_ap_pat
      and a.valor_aiv           = v_valor_aiv
      and a.int_gen_pgo_ext     = v_int_gen_pgo_ext
      and a.aiv_gen_pgo_ext     = v_aiv_gen_pgo_ext
      
      EXIT FOREACH;
   END FOREACH;

   RETURN v_rechazo;

END FUNCTION;


