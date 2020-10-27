






CREATE PROCEDURE "safreviv".sp_dis_avances_pago3(p_folio   DECIMAL(9,0),
                                      p_usuario CHAR(20))

--Última modificación 01082017
--Declaración de variables
DEFINE v_f_liquida           DATE;
DEFINE v_id_derechohabiente  DECIMAL(9,0); 
DEFINE v_subcuenta           SMALLINT; 
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_id_referencia       DECIMAL(9,0);
DEFINE v_monto_acciones      DECIMAL(16,0);
DEFINE v_monto_pesos         DECIMAL(12,2);
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);

  SET PDQPRIORITY HIGH;
  --SET DEBUG FILE TO '/ds/safreviv_int/BD/VIV_AvancePago3.--TRACE';
  --TRACE 'Folio '||p_folio;
  --TRACE 'Usuario '||p_usuario;

  --TRACE 'Tabla sumario ';
  --Actualiza los valores de registro del archivo de avance de pagos
  -- en la tabla de sumario
  UPDATE safre_viv:dis_sum_avance_pago 
  SET    estado = 30
  WHERE  folio  = p_folio;

  --TRACE 'Tabla detalle ';
  --Actualiza los valores de registro del archivo de avance de pagos
  -- en la tabla de detalle
  UPDATE safre_viv:dis_det_avance_pago 
  SET    estado = 30
  WHERE  folio  = p_folio;

  --Realiza el registro de Cargo por dispersión de avance de pagos
  --en la tabla de movimientos
  INSERT INTO cta_movimiento
  SELECT TODAY,
         id_derechohabiente,
         subcuenta,
         fondo_inversion,
         502,
         folio_liquida,
         id_referencia,
         monto_acciones,
         monto_pesos * -1,
         f_valor,
         TODAY,
         CURRENT HOUR TO SECOND,
         origen
  FROM   cta_movimiento
  WHERE  folio_liquida = p_folio;

  INSERT INTO dis_his_transaccion
  SELECT id_derechohabiente,
         folio,
         num_credito,
         0,
         0,
         "",
         "",
         0,
         0,
         0,
         0,
         0,
         0,
         periodo_pago,
         f_pago,
         nrp,
         monto_aportacion,
         monto_amortizacion,
         0
  FROM   dis_det_avance_pago
  WHERE  folio = p_folio;

  {FOREACH
    SELECT *
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
    WHERE  folio_liquida = p_folio

    LET v_movimiento  = 502;
    LET v_monto_pesos = v_monto_pesos * -1;
    LET v_h_registro  = CURRENT HOUR TO SECOND;
 
    INSERT INTO safre_viv:cta_movimiento VALUES (TODAY,
                                                 v_id_derechohabiente,
                                                 v_subcuenta,
                                                 v_fondo_inversion,
                                                 v_movimiento,
                                                 p_folio,
                                                 v_id_referencia, 
                                                 v_monto_acciones,
                                                 v_monto_pesos,
                                                 v_f_valor,
                                                 TODAY,
                                                 v_h_registro,
                                                 v_origen);

  END FOREACH;}

  --TRACE 'Fin de sp ';

END PROCEDURE;


