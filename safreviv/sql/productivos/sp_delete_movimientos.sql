






CREATE PROCEDURE "safreviv".sp_delete_movimientos()

   RETURNING INTEGER, INTEGER

   DEFINE v_nss           char(11)     ;
   DEFINE v_sua           decimal(6,0) ;
   DEFINE v_periodo       char(6)      ;
   DEFINE v_f_pago        date         ;
   DEFINE v_nrp           char(11)     ;
   DEFINE v_ent_recep     char(3)      ;
   DEFINE v_origen_arc    smallint     ;
   DEFINE v_causal        smallint     ;
   DEFINE v_ap_viv        decimal(12,2);
   DEFINE v_aiv_viv       decimal(18,6);
   DEFINE v_am_cre        decimal(12,2);
   DEFINE v_int_viv       decimal(12,2);
   DEFINE v_aiv_int       decimal(18,6);
   DEFINE v_id_referencia decimal(9,0) ;
   DEFINE v_id_derecho    decimal(9,0) ;

   DEFINE v_encontrados    integer;
   DEFINE v_no_encontrados integer;
   DEFINE v_cuantos        integer;
   DEFINE v_algo char(1);
   DEFINE v_id_refe        decimal(9,0) ;

--   set debug file to "/home/safreviv/acl/nss_no_encontrados_mov.trace";

   LET v_encontrados = 0;
   LET v_no_encontrados = 0;

--   trace "NSS no encontrados: =========================";

   FOREACH
      SELECT nss           ,
             sua           ,
             periodo       ,
             f_pago        ,
             nrp           ,
             ent_recep     ,
             origen_arc    ,
             causal        ,
             ap_viv        ,
             aiv_viv       ,
             am_cre        ,
             int_viv       ,
             aiv_int       ,
             id_referencia ,
             id_derecho
      INTO   v_nss           ,
             v_sua           ,
             v_periodo       ,
             v_f_pago        ,
             v_nrp           ,
             v_ent_recep     ,
             v_origen_arc    ,
             v_causal        ,
             v_ap_viv        ,
             v_aiv_viv       ,
             v_am_cre        ,
             v_int_viv       ,
             v_aiv_int       ,
             v_id_referencia ,
             v_id_derecho
      FROM   acl_duplicados

      LET v_cuantos = 0;

      SELECT id_referencia
      INTO   v_id_refe
      FROM   tmp_his_pagos
      WHERE  folio = 9272
      AND    id_derechohabiente = v_id_derecho
      AND    folio_sua          = v_sua
      AND    periodo_pago       = v_periodo
      AND    f_pago             = v_f_pago
      AND    nrp                = v_nrp
      AND    cve_ent_receptora  = v_ent_recep
      AND    imp_ap_pat         = v_ap_viv
      AND    imp_am_cre         = v_am_cre
      AND    int_gen_pgo_ext    = v_int_viv;

--      INSERT INTO tmp_movimiento
--      SELECT * 
--      FROM   cta_movimiento
--      WHERE  folio_liquida = 9272
--      AND    id_referencia = v_id_refe;

      DELETE FROM cta_movimiento
      WHERE  folio_liquida = 9272
      AND    id_referencia = v_id_refe;

      IF ( DBINFO('sqlca.sqlerrd2') > 0 ) THEN

         LET v_encontrados = v_encontrados + 1;

      ELSE
--         TRACE "NSS: " || v_nss;
         LET v_no_encontrados = v_no_encontrados + 1;

      END IF

   END FOREACH

   RETURN v_encontrados, v_no_encontrados;

END PROCEDURE;


