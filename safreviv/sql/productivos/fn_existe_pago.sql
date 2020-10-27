






CREATE FUNCTION "safreviv".fn_existe_pago(p_id_derechohabiente decimal(9,0),
                               p_cve_entidad_receptora char(3),
                               p_nrp                char(11),  
                               p_periodo_pago       char(6), 
                               p_folio_sua          decimal(6,0),
                               p_f_pago_patron      date, 
                               p_monto_pesos        DECIMAL(12,2),  
                               p_amortizacion       DECIMAL(12,2),
                               p_int_gen_pgo_ext    DECIMAL(12,2),
                               p_folio              DECIMAL(9,0),   -- Folio del archivo que se esta procesando
                               p_ind_liquidacion    SMALLINT)       -- Indicador de liq que corresponde según 
                                                                    -- validaciones previas
                                                                    -- 1: ACL normal sin liquidar 
                                                                    -- 2: ACL adelantada liquidada SI 
                                                                    -- 3: ACL adelantada liquidada IMSS 
                                                                    -- 5: Salida Normal del ACL 
   RETURNING SMALLINT;
   
   DEFINE v_status ,
          v_ind_liquidacion SMALLINT;
          
   DEFINE v_folio DECIMAL(9,0);
   DEFINE v_id_referencia DECIMAL(9,0);
   
   --Se asume que no cumple con las condiciones
   --Se lee el registro de la tabla historica

   LET v_status = 0; -- El pago no existe previamente  

   FOREACH cur_act_ind FOR 
           SELECT folio,
                  ind_liquidacion,
                  id_referencia
             INTO v_folio,
                  v_ind_liquidacion,
                  v_id_referencia
             FROM cta_his_pagos
            WHERE id_derechohabiente = p_id_derechohabiente
              AND folio_sua          = p_folio_sua
              AND periodo_pago       = p_periodo_pago
              AND nrp                = p_nrp
              AND imp_ap_pat         = p_monto_pesos 
              AND imp_am_cre         = p_amortizacion
              AND ind_liquidacion    <> -1     --SE AGREGO POR J-523 05-NOV-14

--         SE QUITA VALIDACIÓN DEL CAMPO f_pago DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--         el campo iba después de periodo_pago
--         AND f_pago             = p_f_pago_patron
--         SE QUITA VALIDACIÓN DEL CAMPO cve_ent_receptora DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--         el campo iba después de nrp
--         AND cve_ent_receptora  = p_cve_entidad_receptora
-----------------------------------------------------------------------------------------------------------------------------------------------------
              
--     SE QUITA VALIDACIÓN DEL CAMPO aiv_gen_pgo_ext DE ACUERDO
--     A REQUERIMIENTO JIRA PRODINF-241  22-ABR-2014
--              AND int_gen_pgo_ext    = p_int_gen_pgo_ext
           ORDER BY folio

     IF v_folio <> p_folio THEN    -- El pago existe previamente

       IF v_ind_liquidacion = 0 THEN -- El pago existe liquidado, ap normal
          LET v_status = 1; -- Liquidado 

       ELSE 
          IF v_ind_liquidacion = 1 THEN -- El pago existe sin liq, acl normal
             LET v_status = 2; --  Sin liquidar

             UPDATE cta_his_pagos
             SET    ind_liquidacion  = p_ind_liquidacion, -- El pago se va a liquidar 
                    folio_referencia = p_folio            -- por salida normal del aclaratorio ind_liquidacion = 5 
             WHERE  folio         = v_folio
             AND    id_referencia = v_id_referencia;
             
             --WHERE CURRENT OF cur_act_ind;
          ELSE 
             IF v_ind_liquidacion = 2 THEN -- El pago existe liq, acl ade SI
                LET v_status = 1; -- Liquidado

             ELSE 

                IF v_ind_liquidacion = 3 THEN -- El pago existe liq, acl ade IMSS
                   LET v_status = 1; -- Liquidado

                   UPDATE cta_his_pagos
                   SET    ind_liquidacion  = 4,        -- El pago ya se concilió con PRC
                          folio_referencia = p_folio   -- El pago ya se concilió con PRC
                   WHERE  folio         = v_folio
                   AND    id_referencia = v_id_referencia;                         
                   
                   --WHERE  CURRENT OF cur_act_ind;

                ELSE -- ind_liquidacion IN (4,5,6) El pago existe y liquidado, ya sea conciliado o por salida del acl
                	 IF v_ind_liquidacion = 4 OR
                	 	  v_ind_liquidacion = 5 OR
                	 	  v_ind_liquidacion = 6 THEN
                      LET v_status = 1; -- Liquidado
                   END IF
                END IF

             END IF
          END IF
       END IF
     END IF
     
     IF v_status = 2 THEN
     	  EXIT FOREACH;
     END IF	

   END FOREACH;

   RETURN v_status;
   
END FUNCTION;


