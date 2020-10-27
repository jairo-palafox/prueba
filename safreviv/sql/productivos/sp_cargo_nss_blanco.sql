






CREATE PROCEDURE "safreviv".sp_cargo_nss_blanco()

   DEFINE v_f_liquida          DATE;
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   DEFINE v_subcuenta          SMALLINT;
   DEFINE v_fondo_inversion    SMALLINT;
   DEFINE v_movimiento         SMALLINT;
   DEFINE v_folio_liquida      DECIMAL(9,0);
   DEFINE v_id_referencia      DECIMAL(9,0);
   DEFINE v_monto_acciones     DECIMAL(16,6);
   DEFINE v_monto_pesos        DECIMAL(12,2);
   DEFINE v_f_valor            DATE;
   DEFINE v_f_registro         DATE;
   DEFINE v_h_registro         DATETIME HOUR TO SECOND;
   DEFINE v_origen             CHAR(20);

   -- SECCIÓN DE CARGO

   FOREACH
      SELECT f_liquida,
             id_derechohabiente,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             id_referencia,
             monto_acciones*-1,    
             monto_pesos*-1,       
             f_valor,
             f_registro,
             h_registro,
             origen
      INTO   v_f_liquida,
             v_id_derechohabiente,
             v_subcuenta,
             v_fondo_inversion,
             v_movimiento,
             v_folio_liquida,
             v_id_referencia,
             v_monto_acciones,
             v_monto_pesos,
             v_f_valor,
             v_f_registro,
             v_h_registro,
             v_origen
      FROM   cta_movimiento15
      WHERE  folio_liquida = 38023
      AND    id_derechohabiente = 52112212

      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-38023";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         51725,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH

   -- SECCIÓN DE ABONO

   FOREACH
      SELECT mov.f_liquida,
             det.id_derechohabiente,
             mov.subcuenta,
             mov.fondo_inversion,
             mov.movimiento,
             mov.folio_liquida,
             mov.id_referencia,
             mov.monto_acciones,    
             mov.monto_pesos,       
             mov.f_valor,
             mov.f_registro,
             mov.h_registro,
             mov.origen
      INTO   v_f_liquida,
             v_id_derechohabiente,
             v_subcuenta,
             v_fondo_inversion,
             v_movimiento,
             v_folio_liquida,
             v_id_referencia,
             v_monto_acciones,
             v_monto_pesos,
             v_f_valor,
             v_f_registro,
             v_h_registro,
             v_origen
      FROM   cta_movimiento15 mov,
             cta_his_pagos  det 
      WHERE  mov.folio_liquida = 38023
      AND    mov.id_derechohabiente = 52112212
      AND    det.folio = mov.folio_liquida
      AND    det.id_referencia = mov.id_referencia

      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-38023";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         521 ,                  -- movimiento ABONO POR AJUSTE OPERATIVO
         51726,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH

END PROCEDURE
;


