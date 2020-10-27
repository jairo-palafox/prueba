






CREATE FUNCTION "safreviv".fn_existe_pgo_cta(p_id_derechohabiente decimal(9,0)
                                 ,p_nrp                char(11)    
                                 ,p_periodo_pago       char(6)     
                                 ,p_folio_sua          decimal(6,0)
                                 ,p_f_pago_patron      date        
                                 ,p_monto_pesos        DECIMAL (12,2))
   RETURNING SMALLINT;
   
   DEFINE v_monto_pesos DECIMAL(12,2);      
   LET v_monto_pesos = NULL ;
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_sinf.trace';
   
   --TRACE ("p_id_derechohabiente"||p_id_derechohabiente  );
   --TRACE ("p_nrp"||p_nrp  );
   --TRACE ("p_periodo_pago"||p_periodo_pago  );
   --TRACE ("p_folio_sua"||p_folio_sua  );
   --TRACE ("p_f_pago_patron"||p_f_pago_patron  );
   --TRACE ("p_monto_pesos"||p_monto_pesos  );
   --Se asume que no cumple con las condiciones
   --Se lee el registro de la tabla historica
   FOREACH SELECT m.monto_pesos
           INTO   v_monto_pesos
           FROM   cta_his_pagos h, safre_viv:cta_movimiento m
           WHERE  h.id_derechohabiente = p_id_derechohabiente
           AND    h.folio_sua          = p_folio_sua
           AND    h.periodo_pago       = p_periodo_pago
           AND    m.f_valor            = p_f_pago_patron
           AND    h.nrp                = p_nrp

      IF p_monto_pesos = v_monto_pesos AND v_monto_pesos IS NOT NULL THEN
         --Si se encontro el resultado y es igual al que se recibe se regresa TRUE
         RETURN 1;
      END IF;
      LET v_monto_pesos = NULL ;
   END FOREACH;
   
   --Si no cumplio con ninguna de las condiciones anteriores se regresa falso
   RETURN 0;
   
END FUNCTION;


