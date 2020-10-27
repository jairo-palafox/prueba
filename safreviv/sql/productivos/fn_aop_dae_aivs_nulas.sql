






CREATE FUNCTION "safreviv".fn_aop_dae_aivs_nulas()
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
   DEFINE v_precio_fondo       DECIMAL(19,14);
   DEFINE v_movimiento_origen  SMALLINT;
   DEFINE v_mov_abono          SMALLINT;
   DEFINE v_mov_cargo          SMALLINT;
   DEFINE v_total_pesos        DECIMAL (16,2);

   --SET DEBUG FILE TO "/safreviv_int/BD/fn_aop_dae_ajuste.trace";
   --TRACE ON;

   LET v_resultado = 0;
   LET v_mensaje = "AJUSTE SE EJECUTO CORRECTAMENTE";
   LET v_folio_liquida = 70083; --Produccion 
  -- LET v_folio_liquida = 57875; --QA
   LET v_subcuenta       = 46;
   LET v_fondo_inversion = 11;
   LET v_mov_abono      = 521; --ABONO REVERSO OPERATIVO
   LET v_mov_cargo      = 1192; --CARGO POR AJUSTE OPERATIVO
   
   SELECT precio_fondo
   INTO   v_precio_fondo
   FROM   glo_valor_fondo d
   WHERE  f_valuacion = "07/01/1997"
   AND    fondo = 11;

   FOREACH
      SELECT id_referencia,
             id_derechohabiente,
             monto_pesos,
             f_valuacion,
             movimiento
      INTO   v_id_referencia,
             v_id_derechohabiente,
             v_monto_pesos,
             v_f_valuacion,
             v_movimiento_origen
      FROM   safre_tmp:cta_mov_ajuste_aivs
      ORDER BY id_derechohabiente, movimiento

      SELECT SUM (monto_pesos)
      INTO   v_total_pesos
      FROM   safre_tmp:cta_mov_ajuste_aivs
      WHERE  id_derechohabiente = v_id_derechohabiente;

	    --LET v_total_pesos = v_total_pesos + v_monto_pesos;

	    IF v_total_pesos > 0 THEN 
         LET v_monto_aivs = v_monto_pesos / v_precio_fondo;

         IF v_monto_aivs > 0 AND v_monto_pesos > 0 THEN  
            INSERT INTO cta_movimiento VALUES (TODAY,
                                               v_id_derechohabiente,
                                               v_subcuenta,
                                               v_fondo_inversion,
                                               v_mov_abono,
                                               v_folio_liquida,
                                               v_id_referencia,
                                               v_monto_aivs,
                                               v_monto_pesos,
                                               v_f_valuacion,
                                               TODAY,
                                               CURRENT HOUR TO SECOND,
                                               "ROP-FOLIO-" ||v_folio_liquida)
                                               ;
         END IF
      END IF
   END FOREACH

   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59745,NULL        ,145.33, "03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59745,NULL        ,145.33, "03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59746,15820.231191,7965.17,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59746,15820.231191,7965.17,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59747,15820.211329,7965.16,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11, 521,57873,59747,15820.211329,7965.16,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,2991259,  -68.841215, -145.33,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,2991259,  -68.841215, -145.33,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,3043954,-3773.012993,-7965.17,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,3043954,-3773.012993,-7965.17,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,3223166,-3891.689590,-7965.16,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   INSERT INTO cta_movimiento VALUES (TODAY,  9042753,46,11,1192,57873,3223166,-3891.689590,-7965.16,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873"); 
   
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,NULL       ,   10.73,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,NULL       ,   10.73,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,7882.875189, 3968.87,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,7882.875189, 3968.87,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,1099.427981,  553.54,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,521,57873,57873,1099.427981,  553.54,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873,   -5.199097,   -10.73,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873,   -5.199097,   -10.73,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873, -268.211375, -3968.87,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873, -268.211375, -3968.87,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873,-1923.069841,  -553.54,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");
   INSERT INTO cta_movimiento VALUES (TODAY, 19817697,46,11,1192,57873,57873,-1923.069841,  -553.54,"03/07/2016",TODAY,CURRENT HOUR TO SECOND,"ROP-FOLIO-57873");

   RETURN v_resultado, v_mensaje;

END FUNCTION;


