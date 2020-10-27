






CREATE PROCEDURE "safreviv".rv_carga_inicial_avances_pago(p_folio DECIMAL(10,0))

   SET PDQPRIORITY HIGH;
   -- Elimina los folios de la tabla detalle
   DELETE 
     FROM safre_viv:dis_det_avance_pago
    WHERE folio = p_folio;
   
   -- Elimina los folios de la tabla sumario
   DELETE 
     FROM safre_viv:dis_sum_avance_pago
    WHERE folio = p_folio;

   -- Elimina los folios de la tabla rechazos
   DELETE 
     FROM safre_viv:dis_rch_avance_pago
    WHERE folio = p_folio;
   
   -- Elimina los folios que cuentan con rechazos de la tabla de movimientos
   DELETE 
     FROM safre_viv:cta_movimiento
    WHERE folio_liquida = p_folio;

END PROCEDURE;


