






CREATE PROCEDURE "safreviv".sp_ajuste_acl_normalizacion()

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
      WHERE  folio_liquida = 46264	
      AND    id_derechohabiente not in (16700757,13155041,13635468,41971173)

      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-46264";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         49138,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


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
      WHERE  folio_liquida = 46041	
      AND    id_derechohabiente not in (41139168)

      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-46041";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         49139,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH

END PROCEDURE


--det_f46264.unl
--cta_f46264.unl
--det_f46041.unl
--cta_f46041.unl
;


