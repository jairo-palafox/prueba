






CREATE FUNCTION "safreviv".fn_existe_pago_enaclara(p_id_derechohabiente decimal(9,0),
                               p_cve_entidad_receptora char (3),
                               p_nrp                char(11),  
                               p_periodo_pago       char(6), 
                               p_folio_sua          decimal(6,0),
                               p_f_pago_patron      date, 
                               p_monto_pesos        DECIMAL(12,2),  
                               p_amortizacion       DECIMAL(12,2),
                               p_folio              DECIMAL(9,0),   -- Folio del archivo que se esta procesando
                               p_ind_liquidacion    SMALLINT)       -- Indicador de liq que corresponde seg�n 
                                                                    -- validaciones previas
                                                                    -- 1: ACL normal sin liquidar 
                                                                    -- 2: ACL adelantada liquidada SI 
                                                                    -- 3: ACL adelantada liquidada IMSS 
                                                                    -- 5: Salida Normal del ACL 
   RETURNING SMALLINT;
   
   DEFINE v_status ,
          v_ind_liquidacion SMALLINT;
          
   DEFINE v_folio DECIMAL(9,0);
   
   --Se asume que no cumple con las condiciones
   --Se lee el registro de la tabla historica

   LET v_status = 0; -- El pago no existe previamente  

   FOREACH cur_act_ind FOR 
           SELECT folio,
                  ind_liquidacion
             INTO v_folio,
                  v_ind_liquidacion
             FROM cta_his_pagos
            WHERE id_derechohabiente = p_id_derechohabiente
              AND folio_sua          = p_folio_sua
              AND periodo_pago       = p_periodo_pago
              AND f_pago             = p_f_pago_patron
              AND nrp                = p_nrp
              AND cve_ent_receptora  = p_cve_entidad_receptora
              AND imp_ap_pat         = p_monto_pesos 
              AND imp_am_cre         = p_amortizacion

     IF v_folio <> p_folio THEN    -- El pago existe previamente

       IF v_ind_liquidacion = 0 THEN -- El pago existe liquidado, ap normal
          LET v_status = 1; -- Liquidado 

       ELSE 
          IF v_ind_liquidacion = 1 THEN -- El pago existe sin liq, acl normal
             LET v_status = 2; --  Sin liquidar

             UPDATE cta_his_pagos
             SET    ind_liquidacion  = p_ind_liquidacion, -- El pago se va a liquidar 
                    folio_referencia = p_folio            -- por salida normal del aclaratorio ind_liquidacion = 5 
             WHERE CURRENT OF cur_act_ind;
          ELSE 
             IF v_ind_liquidacion = 2 THEN -- El pago existe liq, acl ade SI
                LET v_status = 1; -- Liquidado

             ELSE 

                IF v_ind_liquidacion = 3 THEN -- El pago existe liq, acl ade IMSS
                   LET v_status = 1; -- Liquidado

                   UPDATE cta_his_pagos
                   SET    ind_liquidacion  = 4,        -- El pago ya se concili� con PRC
                          folio_referencia = p_folio   -- El pago ya se concili� con PRC
                   WHERE  CURRENT OF cur_act_ind;

                ELSE -- ind_liquidacion IN (4,5,6) El pago existe y liquidado, ya sea conciliado o por salida del acl
                   LET v_status = 1; -- Liquidado
                END IF

             END IF
          END IF
       END IF
     END IF

   END FOREACH;

   RETURN v_status;
   
END FUNCTION;


