






CREATE PROCEDURE "safreviv".act_causales_lqinfo_43815();
    
    DEFINE tpo_aclaracion_1 CHAR(2);
    DEFINE nss_causal CHAR(11);
    DEFINE cve_ent_receptora_causal CHAR(3);
    DEFINE nrp_causal CHAR(11);
    DEFINE periodo_pago_causal CHAR(6);
    DEFINE folio_sua_causal DECIMAL(6,0);
    DEFINE f_pago_causal DATE;
    DEFINE imp_ap_pat_causal DECIMAL(12,2);
    DEFINE imp_am_cre_causal DECIMAL(12,2);

    DEFINE tpo_aclaracion_1_pagos CHAR(2);
    DEFINE nss_pagos CHAR(11);
    DEFINE cve_ent_receptora_pagos CHAR(3);
    DEFINE nrp_pagos CHAR(11);
    DEFINE periodo_pago_pagos CHAR(6);
    DEFINE folio_sua_pagos DECIMAL(6,0);
    DEFINE f_pago_pagos DATE;
    DEFINE imp_ap_pat_pagos DECIMAL(12,2);
    DEFINE imp_am_cre_pagos DECIMAL(12,2);
    DEFINE folio_pagos DECIMAL(9,0);
    DEFINE id_referencia_pagos DECIMAL(9,0);

    DEFINE v_contador DECIMAL(9,0);
    LET v_contador=0;

	FOREACH SELECT TPO_ACLARACION,
					nss,
                    cve_ent_receptora,
                    nrp,
                    periodo_pago,
                    folio_sua,
                    f_pago,
                    imp_ap_pat,
                    imp_am_cre
			   INTO tpo_aclaracion_1,
                    nss_causal,
                    cve_ent_receptora_causal,
                    nrp_causal,
                    periodo_pago_causal,
                    folio_sua_causal,
                    f_pago_causal,
                    imp_ap_pat_causal,
                    imp_am_cre_causal																		
                FROM SAFRE_TMP:ACL_CAUSALES_43815
INSERT INTO SAFRE_TMP:acl_causales_rechazos_43815
                        VALUES(nss_causal,
                                        cve_ent_receptora_causal,
                                        nrp_causal,
                                        periodo_pago_causal,
                                        folio_sua_causal,
                                        f_pago_causal,
                                        imp_ap_pat_causal,
                                        imp_am_cre_causal,
                                        tpo_aclaracion_1,
					0);
	FOREACH SELECT CTA.FOLIO,CTA.ID_REFERENCIA,CTA.CVE_ENT_RECEPTORA, 
     				CTA.NRP,CTA.PERIODO_PAGO,CTA.FOLIO_SUA,CTA.F_PAGO,CTA.IMP_AP_PAT,CTA.IMP_AM_CRE										
            INTO folio_pagos,
                 id_referencia_pagos,
                 cve_ent_receptora_pagos,
                 nrp_pagos,
                 periodo_pago_pagos,
                 folio_sua_pagos,
                 f_pago_pagos,
                 imp_ap_pat_pagos,
                 imp_am_cre_pagos                                      
			FROM CTA_HIS_PAGOS CTA JOIN 
				  AFI_DERECHOHABIENTE AFI ON
				--  SAFRE_TMP:TMP_DERECHOHABIENTE_FALTAN AFI ON
				  CTA.ID_DERECHOHABIENTE=AFI.ID_DERECHOHABIENTE 
				  WHERE  AFI.NSS=nss_causal
				  AND CTA.CVE_ENT_RECEPTORA=cve_ent_receptora_causal
				  AND CTA.NRP=nrp_causal
				  AND CTA.PERIODO_PAGO=periodo_pago_causal
				  AND CTA.FOLIO_SUA=folio_sua_causal 
				  AND CTA.F_PAGO=f_pago_causal
				  AND CTA.IMP_AP_PAT=imp_ap_pat_causal
				  AND CTA.IMP_AM_CRE=imp_am_cre_causal
				  AND CTA.FOLIO=43815																								
																			
                UPDATE CTA_HIS_PAGOS 
                SET TPO_ACLARACION=tpo_aclaracion_1
                WHERE folio=43815 AND
                               id_referencia=id_referencia_pagos AND
                               cve_ent_receptora=cve_ent_receptora_pagos AND
                                nrp=nrp_pagos AND
                                periodo_pago=periodo_pago_pagos AND
                                folio_sua=folio_sua_pagos AND
                                 f_pago=f_pago_pagos AND
                                 imp_ap_pat=imp_ap_pat_pagos AND
                                 imp_am_cre=imp_am_cre_pagos;
				LET v_contador=1;
UPDATE SAFRE_TMP:acl_causales_rechazos_43815 SET b_inserto=1  WHERE
                       NSS=nss_causal AND
           cve_ent_receptora=cve_ent_receptora_causal AND
                        NRP=nrp_causal AND
                        periodo_pago=periodo_pago_causal AND
                        folio_sua=folio_sua_causal AND
                        f_pago=f_pago_causal AND
                        imp_ap_pat=imp_ap_pat_causal AND
                        imp_am_cre=imp_am_cre_causal AND
                        tpo_aclaracion=tpo_aclaracion_1;		

	END FOREACH;

		END FOREACH;       
END PROCEDURE;


