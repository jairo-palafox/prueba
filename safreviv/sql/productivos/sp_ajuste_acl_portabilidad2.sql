






CREATE PROCEDURE "safreviv".sp_ajuste_acl_portabilidad2()
   DEFINE v_folio_liquida      DECIMAL(9,0);
   DEFINE v_id_referencia      DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_movimiento         SMALLINT;        
   DEFINE v_subcuenta          SMALLINT;         
   DEFINE v_monto_pesos        DECIMAL(12,2);
   DEFINE v_monto_acciones     DECIMAL(16,6);
   DEFINE v_f_valor            DATE;
   DEFINE v_h_registro         DATETIME HOUR TO SECOND;
   DEFINE v_origen             CHAR(20);

   LET v_h_registro = CURRENT HOUR TO SECOND;

   FOREACH
   	  SELECT folio_liquida fol,
             id_referencia idr,
             id_derechohabiente idd,
             movimiento  mov,
             subcuenta   sub,
             monto_pesos*-1 pes,
             monto_acciones*-1 acc,
             f_valor,
             origen
      INTO   v_folio_liquida,
             v_id_referencia,
             v_id_derechohabiente,
             v_movimiento,
             v_subcuenta,
             v_monto_pesos,
             v_monto_acciones,
             v_f_valor,
             v_origen
      from   cta_movimiento 
      where  folio_liquida = 46813
      and    id_referencia in (968,13460,14393,19461,47410,55539,75864,75997,90856)
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         11,                    -- fondo_inversion
         1192,                  -- movimiento CARGO POR AJUSTE OPERATIVO ó 1212 
         46991,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_valor,
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);
         
--      DELETE 
--      FROM   pag_ctr_pago
--      WHERE  folio = v_folio_liquida
--      AND    id_referencia = v_id_referencia;
      
--      UPDATE cta_his_pagos              
--      SET    ind_liquidacion = 1        
--      WHERE  folio           = v_folio_liquida
--      AND    id_referencia = v_id_referencia
--      AND    ind_liquidacion = 4;     
      
      
--      DELETE
--      FROM  acl_preliquida
--      WHERE  folio = v_folio_liquida
--      AND    id_referencia = v_id_referencia
--      AND    monto_pesos > 0;      

   END FOREACH


END PROCEDURE

;


