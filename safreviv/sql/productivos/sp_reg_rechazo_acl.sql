






CREATE PROCEDURE "safreviv".sp_reg_rechazo_acl(p_folio      DECIMAL(9,0), 
                                    p_id_refer   DECIMAL(9,0),
                                    p_id_derecho DECIMAL(9,0),
                                    p_sua        DECIMAL(6,0),
                                    p_pp         CHAR(06),
                                    p_f_pago     DATE,
                                    p_nrp        CHAR(11),
                                    p_cve_ent    CHAR(03),
                                    p_imp_viv    DECIMAL(16,2),
                                    p_am_cre     DECIMAL(16,2))

   RETURNING SMALLINT;

-- NOTA: quitar del query f_pago y cve_ent_recep ya que no formanparte de la busqueda 10-may2019.
-- Clave del cambio saci2018-49

-- SE QUITA VALIDACIÓN DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
   IF EXISTS (SELECT cve_ent_receptora
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
              --AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         6,          --Diferente ent rec en LQ o CI
         TODAY
         );
 
      RETURN 0;

   END IF
--============================================================================
   
   IF EXISTS (SELECT nrp
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              --AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         2,          --Diferente nrp en LQ o CI
         TODAY
         );

      RETURN 0;

   END IF

   IF EXISTS (SELECT periodo_pago
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              --AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         3,          --Diferente perido pago en LQ o CI
         TODAY
         );

      RETURN 0;

   END IF

 --no aplica validación porque ya está hecha en el main del programa
 --Se quita comentario del query ya que tiene que validar diferencia
 --de la f_pago 24-abr-2018

-- SE QUITA VALIDACIÓN DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.  
   IF EXISTS (SELECT f_pago
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
              --AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         4,          --Diferente fecha pago en LQ o CI
         TODAY
         );

      RETURN 0;

   END IF
--============================================================================

 --no aplica validación porque ya esta hecha en el main del programa
 --Se quita comentario del query ya que tiene que validar diferencia
 --del sua 24-abr-2018
 
   IF EXISTS (SELECT folio_sua
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              --AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         5,          --Diferente folio sua LQ o CI
         TODAY
         );

      RETURN 0;

   END IF

   IF EXISTS (SELECT imp_ap_pat
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              --AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         8,          --Diferente ap pat LQ o CI
         TODAY
         );

      RETURN 0;

   END IF


   IF EXISTS (SELECT imp_am_cre
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              --AND    imp_am_cre         = p_am_cre
              AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         10,          --Diferente am cre LQ o CI
         TODAY
         );

      RETURN 0;

   END IF

--========================== 19mar2019 ==================================================
   IF EXISTS (SELECT origen_archivo
              FROM   cta_his_pagos
              WHERE  id_derechohabiente = p_id_derecho
              AND    folio_sua          = p_sua
              AND    periodo_pago       = p_pp
--saci2018-49              AND    f_pago             = p_f_pago
              AND    nrp                = p_nrp
--saci2018-49              AND    cve_ent_receptora  = p_cve_ent
              AND    imp_ap_pat         = p_imp_viv
              AND    imp_am_cre         = p_am_cre) THEN
              --AND    origen_archivo     in (1,4) ) THEN

      INSERT INTO acl_pag_rechazo VALUES
         (
         p_folio,
         p_id_refer,
         99,          --Diferente origen_archivo
         TODAY
         );
--======================================================================================

      RETURN 0;

   END IF

   RETURN 1;

END PROCEDURE
;


