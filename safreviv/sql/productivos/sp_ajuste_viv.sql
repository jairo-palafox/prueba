






CREATE PROCEDURE "safreviv".sp_ajuste_viv()

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

   -- SECCIÓN DE ABONO FOLIO 48138

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
      WHERE  folio_liquida = 49138
      AND    subcuenta = 4
      AND    movimiento = 1212
      AND    id_derechohabiente in (
                12861217,
                13740298,
                37446480,
                28777991,
                41896614,
                18333589,
                18565565,
                31242704,
                19739920,
                20118293,
                20375672)


      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-49138";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         521,                   -- movimiento ABONO POR AJUSTE OPERATIVO
         52551,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


   -- SECCIÓN DE ABONO FOLIO 48139

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
      WHERE  folio_liquida = 49139
      AND    subcuenta = 4
      AND    movimiento = 1212
      AND    id_derechohabiente in (
               37548439,
               42051397,
               21801495)


      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-49139";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         521,                   -- movimiento ABONO POR AJUSTE OPERATIVO
         52552,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


   -- SECCIÓN DE ABONO FOLIO 50650

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
      WHERE  folio_liquida = 50650
      AND    subcuenta = 4
      AND    movimiento = 1212
      AND    id_derechohabiente in (
               274379,
               953652,
               13740298,
               15178684,
               16935090,
               18208972,
               30295244,
               21801495,
               10263305)



      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-50650";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         521,                   -- movimiento ABONO POR AJUSTE OPERATIVO
         52550,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


   -- SECCIÓN DE CARGO FOLIO 51084

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
      WHERE  folio_liquida = 51084
      AND    subcuenta = 60
      AND    movimiento = 51
      AND    id_derechohabiente in (
               274379,
               953652,
               13740298,
               15178684,
               16935090,
               18208972,
               30295244,
               21801495,
               10263305)



      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-51084";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         52835,                 -- folio_liquida
         v_id_referencia,
         v_monto_acciones,
         v_monto_pesos,
         v_f_liquida,           -- v_f_valor
         TODAY,                 -- f_registro
         v_h_registro,
         v_origen);

   END FOREACH


   -- SECCIÓN DE CARGO FOLIO 52711

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
      WHERE  folio_liquida = 52711
      AND    subcuenta = 60
      AND    movimiento = 51
      AND    id_derechohabiente in (
                12861217,
                13740298,
                37446480,
               37548439,
                28777991,
                41896614,
                18333589,
               42051397,
                18565565,
                31242704,
                19739920,
                20118293,
                20375672,
               21801495)


      LET v_h_registro = CURRENT HOUR TO SECOND;
      LET v_origen = "ROP-FOLIO-52711";
      
      INSERT INTO cta_movimiento VALUES (
         TODAY,                 -- f_liquida
         v_id_derechohabiente,
         v_subcuenta,
         v_fondo_inversion,
         1212,                  -- movimiento CARGO POR AJUSTE OPERATIVO
         52836,                 -- folio_liquida
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


