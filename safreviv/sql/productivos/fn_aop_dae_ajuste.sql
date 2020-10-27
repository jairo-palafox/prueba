






CREATE FUNCTION "safreviv".fn_aop_dae_ajuste()
RETURNING SMALLINT, VARCHAR(100);

   DEFINE v_resultado SMALLINT;
   DEFINE v_mensaje   VARCHAR(100);
   DEFINE v_folio_liquida DECIMAL(9,0);

   DEFINE v_id_derechohabiente DECIMAL (9,0);
   DEFINE v_monto_pesos        DECIMAL (16,2); 
   DEFINE v_monto_aivs         DECIMAL(16,6);

   DEFINE v_f_liquida          DATE;
   DEFINE v_subcuenta          SMALLINT;
   DEFINE v_fondo_inversion    SMALLINT;
   DEFINE v_movimiento         SMALLINT;
   DEFINE v_id_referencia      DECIMAL(9,0);
   DEFINE v_f_valuacion        DATE;
   DEFINE v_f_registro         DATE;
   DEFINE v_h_registro         DATETIME HOUR TO SECOND ;
   DEFINE v_origen             CHAR(20);
   DEFINE v_precio             DECIMAL(19,14);

   --SET DEBUG FILE TO "/safreviv_int/BD/fn_aop_dae_ajuste.trace";
   --TRACE ON;

   LET v_resultado = 0;
   LET v_mensaje = "AJUSTE DE AMORTIZACION EXCEDENTE SE EJECUTO CORRECTAMENTE";
   LET v_folio_liquida = 65475;  --Producción
   --LET v_folio_liquida = 56952; --QA
   LET v_subcuenta       = 46;
   LET v_fondo_inversion = 11;
   LET v_movimiento      = 521; --ABONO REVERSO OPERATIVO

   FOREACH
      SELECT id_referencia,
             id_derechohabiente,
             monto_acciones,
             monto_pesos,
             f_valuacion
      INTO   v_id_referencia,
             v_id_derechohabiente,
             v_monto_aivs,
             v_monto_pesos,
             v_f_valuacion
      FROM   safre_tmp:cta_mov_ajuste_dae

      INSERT INTO cta_movimiento VALUES (TODAY,
                                         v_id_derechohabiente,
                                         v_subcuenta,
                                         v_fondo_inversion,
                                         v_movimiento,
                                         v_folio_liquida,
                                         v_id_referencia,
                                         (v_monto_aivs * - 1),
                                         (v_monto_pesos * - 1),
                                         v_f_valuacion,
                                         TODAY,
                                         CURRENT HOUR TO SECOND,
                                         "ROP-FOLIO-" ||v_folio_liquida)
                                         ;
   END FOREACH

   INSERT INTO cta_movimiento VALUES (TODAY, 35348258,46,11,1192,56952,59750,-1847.271963,-3780.83,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");
   INSERT INTO cta_movimiento VALUES (TODAY, 38888813,46,11,1192,56952,59692,-3168.480146,-6484.96,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");
   INSERT INTO cta_movimiento VALUES (TODAY, 38888813,46,11,1192,56952,59693,-3071.853876,-6484.96,"05/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");
   INSERT INTO cta_movimiento VALUES (TODAY, 45034246,46,11,1192,56952,59784,-1284.786328,-2620.72,"02/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");
   INSERT INTO cta_movimiento VALUES (TODAY, 45034246,46,11,1192,56952,59783,-33.51782764,-125.19 ,"02/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");
   INSERT INTO cta_movimiento VALUES (TODAY, 45034246,46,11,1192,56952,59785,-61.37336321,-68.37  ,"02/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-56952");

   RETURN v_resultado, v_mensaje;

END FUNCTION;


