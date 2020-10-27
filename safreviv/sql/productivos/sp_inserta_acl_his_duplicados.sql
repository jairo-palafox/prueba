






CREATE PROCEDURE "safreviv".sp_inserta_acl_his_duplicados(
    p_folio_duplicado    decimal(9,0) ,
    p_origen_archivo     smallint     ,
    p_id_derechohabiente decimal(9,0) ,
    p_id_referencia      decimal(9,0) ,
    p_folio_sua          decimal(6,0) ,
    p_periodo_pago       char(6)      ,
    p_f_pago             date         ,
    p_nrp                char(11)     ,
    p_cve_ent_receptora  char(3)      ,
    p_imp_ap_pat         decimal(12,2),
    p_imp_am_cre         decimal(12,2),
    p_int_gen_pgo_ext    decimal(12,2),
    p_ind_cta            smallint     ,
    p_folio_integracion  decimal(9,0)
    )

    -- se inserta el registro en la tabla de duplicados
    INSERT INTO acl_his_duplicados (
       folio              ,
       origen_archivo     ,
       id_derechohabiente ,
       id_referencia      ,
       folio_sua          ,
       periodo_pago       ,
       f_pago             ,
       nrp                ,
       cve_ent_receptora  ,
       imp_ap_pat         ,
       imp_am_cre         ,
       int_gen_pgo_ext    ,
       ind_cta            ,
       folio_integracion  
    )
    VALUES (
       p_folio_duplicado    ,
       p_origen_archivo     ,
       p_id_derechohabiente ,
       p_id_referencia      ,
       p_folio_sua          ,
       p_periodo_pago       ,
       p_f_pago             ,
       p_nrp                ,
       p_cve_ent_receptora  ,
       p_imp_ap_pat         ,
       p_imp_am_cre         ,
       p_int_gen_pgo_ext    ,
       p_ind_cta            ,
       p_folio_integracion
    );

END PROCEDURE;


