






CREATE PROCEDURE "safreviv".sp_ajuste_port()

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

   -- SECCIÓN DE CARGO FOLIO 51593

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
      FROM   cta_movimiento 
      WHERE  folio_liquida = 51593
      AND    subcuenta = 60
      AND    movimiento = 51
      AND    id_derechohabiente  = 41896614


      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-51593";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         54078,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


   -- SECCIÓN DE ABONO FOLIO 51593

   FOREACH
      SELECT f_liquida,
             id_derechohabiente,
             subcuenta,
             fondo_inversion,
             movimiento,
             folio_liquida,
             id_referencia,
             monto_pesos,    -- OJO monto_acciones
             monto_pesos,       
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
      FROM   cta_movimiento 
      WHERE  folio_liquida = 51593
      AND    subcuenta = 60
      AND    movimiento = 51
      AND    id_derechohabiente  = 41896614


      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-51593";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         521,                   -- movimiento ABONO POR AJUSTE OPERATIVO
         54079,                 -- folio_liquida
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


