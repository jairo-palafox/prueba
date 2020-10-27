






CREATE FUNCTION "safreviv".fn_existe_pgo_his(p_id_derechohabiente decimal(9,0),
                                  p_cve_entidad_receptora char (3),
                                  p_folio              decimal(9,0),
                                  p_nrp                char(11),    
                                  p_periodo_pago       char(6),     
                                  p_folio_sua          decimal(6,0),
                                  p_f_pago_patron      date,        
                                  p_imp_ap_pat         DECIMAL (12,2),
                                  p_imp_am_cre         DECIMAL(12,2),
                                  p_int_gen_pgo_ext    DECIMAL(12,2))
   RETURNING CHAR (1);
   
   DEFINE v_imp_ap_pat DECIMAL(12,2);
   LET v_imp_ap_pat = 0 ;
   --Se lee el registro de la tabla historica
   
   FOREACH SELECT imp_ap_pat
           INTO   v_imp_ap_pat
           FROM   safre_viv:cta_his_pagos h
           WHERE  h.id_derechohabiente = p_id_derechohabiente
           AND    h.folio              < p_folio
           AND    h.folio_sua          = p_folio_sua
           AND    h.periodo_pago       = p_periodo_pago
           AND    h.nrp                = p_nrp
           AND    h.imp_ap_pat         = p_imp_ap_pat 
           AND    h.imp_am_cre         = p_imp_am_cre
           AND    h.ind_liquidacion   <> -1     --SE AGREGO POR J-523 05-NOV-14

--         SE QUITA VALIDACIÓN DEL CAMPO f_pago DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--         el campo iba después de periodo_pago
--         AND    h.f_pago             = p_f_pago_patron
--         SE QUITA VALIDACIÓN DEL CAMPO cve_ent_receptora DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--         el campo iba después de nrp
--         AND    h.cve_ent_receptora  = p_cve_entidad_receptora
-----------------------------------------------------------------------------------------------------------------------------------------------------
           
--     SE QUITA VALIDACIÓN DEL CAMPO aiv_gen_pgo_ext DE ACUERDO
--     A REQUERIMIENTO JIRA PRODINF-241  22-ABR-2014
--           AND    h.int_gen_pgo_ext    = p_int_gen_pgo_ext           


      IF p_imp_ap_pat = v_imp_ap_pat THEN
         --Si se encontro el resultado y es igual al que se recibe se regresa TRUE
         RETURN "T";
      END IF;
      LET v_imp_ap_pat = NULL;
      
   END FOREACH;
   
   --No cumplio con ninguna de las condiciones anteriores se regresa falso
   RETURN "F";
   
END FUNCTION;


