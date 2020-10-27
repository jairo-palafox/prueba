






CREATE PROCEDURE "safreviv".sp_ajuste_acl_portabilidad()
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
      where  folio_liquida = 46041
      and    id_referencia in (8017,102940,117380)
      and   id_derechohabiente in (42051397,21801495,37548439)
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         11,                    -- fondo_inversion
         1192,                  -- movimiento CARGO POR AJUSTE OPERATIVO ó 1212 
         46566,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_valor,
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);
         
      DELETE 
      FROM   pag_ctr_pago
      WHERE  folio = v_folio_liquida
      AND    id_referencia = v_id_referencia;
      
      UPDATE cta_his_pagos              
      SET    ind_liquidacion = 1        
      WHERE  folio           = v_folio_liquida
      AND    id_referencia = v_id_referencia
      AND    ind_liquidacion = 4;     
      
      
--      DELETE
--      FROM  acl_preliquida
--      WHERE  folio = v_folio_liquida
--      AND    id_referencia = v_id_referencia
--      AND    monto_pesos > 0;      

   END FOREACH

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
      where  folio_liquida = 46264
      and    id_referencia in (
                              2262,
                              4968,
                              30381,
                              40419,
                              44344,
                              44416,
                              48810,
                              59959,
                              60247,
                              67944,
                              71415,
                              79649,
                              81777,
                              92702,
                              105951)
      and   id_derechohabiente in (
                                  12861217,
                                  18565565,
                                  28777991,
                                  18333589,
                                  37446480,
                                  37548439,
                                  20118293,
                                  19739920,
                                  20375672,
                                  13740298,
                                  41896614,
                                  31242704,
                                  21801495,
                                  42051397,
                                  41896614)

      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         11,                    -- fondo_inversion
         1192,                  -- movimiento CARGO POR AJUSTE OPERATIVO ó 1212 
         46567,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_valor,
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

      DELETE 
      FROM   pag_ctr_pago
      WHERE  folio = v_folio_liquida
      AND    id_referencia = v_id_referencia;

      UPDATE cta_his_pagos              
      SET    ind_liquidacion = 1        
      WHERE  folio           = v_folio_liquida
      AND    id_referencia = v_id_referencia
      AND    ind_liquidacion = 4;

--      DELETE
--      FROM   acl_preliquida
--      WHERE  folio = v_folio_liquida
--      AND    id_referencia = v_id_referencia
--      AND    monto_pesos > 0;      

   END FOREACH

END PROCEDURE

;


