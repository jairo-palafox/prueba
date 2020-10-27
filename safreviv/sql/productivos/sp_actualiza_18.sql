






CREATE PROCEDURE "safreviv".sp_actualiza_18()
                 
   DEFINE v_id_derecho DECIMAL(9,0);
   DEFINE v_id_referen DECIMAL(9,0);

   -- 1) Inferir todo a 2 Afore -- 5,713,482 registros
   UPDATE cta_his_pagos
   SET    destino_ap_viv = "2"
   WHERE  folio = 9272;
   
   UPDATE cta_pag_complemento
   SET    destino_ap_viv = "2"
   WHERE  folio = 9272;
   ------------------------------
   

   -- Regla definida por Gabriel Reyes imp_am_cre > 0 --- 186,941 registros    
   FOREACH 
      SELECT id_derechohabiente,
             id_referencia 
      INTO   v_id_derecho,
             v_id_referen
      FROM   cta_his_pagos
      WHERE  folio = 9272
      AND    imp_am_cre > 0
      

      UPDATE cta_his_pagos
      SET    destino_ap_viv = "1"
      WHERE  folio = 9272
      AND    id_derechohabiente = v_id_derecho
      AND    id_referencia      = v_id_referen;
      
      UPDATE cta_pag_complemento
      SET    destino_ap_viv = "1"
      WHERE  folio = 9272
      AND    id_derechohabiente = v_id_derecho
      AND    id_referencia      = v_id_referen;      
   
   END FOREACH;
   
   
   -- d) tpo_cre <> "002" y cruce con acreditados y f_credito <= f_pago --- 208,018 registros
                                                                        --- 208,015 al 6may13
   FOREACH
      SELECT det.id_derechohabiente,
             det.id_referencia
      INTO   v_id_derecho,
             v_id_referen
      FROM   cta_his_pagos det,
             cta_credito   cre
      WHERE  det.folio = 9272
      AND    det.id_derechohabiente = cre.id_derechohabiente
      AND    cre.f_credito <= det.f_pago
      AND    tpo_credito <> 2


      UPDATE cta_his_pagos
      SET    destino_ap_viv = "1"
      WHERE  folio = 9272
      AND    id_derechohabiente = v_id_derecho
      AND    id_referencia      = v_id_referen;
      
      UPDATE cta_pag_complemento
      SET    destino_ap_viv = "1"
      WHERE  folio = 9272
      AND    id_derechohabiente = v_id_derecho
      AND    id_referencia      = v_id_referen;          
      
      
   END FOREACH;
   
END PROCEDURE;


