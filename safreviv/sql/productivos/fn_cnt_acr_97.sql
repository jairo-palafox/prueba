






create procedure "safreviv".fn_cnt_acr_97(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                               p_f_liquida          DATE,          --Fecha de liquidación del proceso
                               p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                               p_cod_proceso        SMALLINT,      --Código Proceso
                               p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 02072015
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_deudor        DECIMAL(20,2);  --Monto Deudor por Originación
DEFINE v_deudor_mayor        DECIMAL(20,2);  --Monto Deudor por Originación Mayor
DEFINE v_ssv_menor           DECIMAL(20,2);  --Monto SSV Menor
DEFINE v_deudor_menor        DECIMAL(20,2);  --Monto Deudor por Originación Menor
DEFINE v_monto_cred_ssv      DECIMAL(20,2);  --Monto Incremento Cred X SSV 
DEFINE v_monto_taa_exc       DECIMAL(20,2);  --Monto Traspaso Excedente SSV
DEFINE v_ssv_mayor           DECIMAL(20,2);  --Monto SSV Mayor
DEFINE v_tpo_trabajador      CHAR(1);        --Tipo de Trabajador
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción
DEFINE v_movimiento          SMALLINT;       --Movimiento

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_deudor        = 0;
  LET v_deudor_mayor        = 0;
  LET v_ssv_menor           = 0;
  LET v_deudor_menor        = 0;
  LET v_ssv_mayor           = 0;
  LET v_tpo_trabajador      = 0;
  LET v_bnd_inserta         = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_cnt_acr_97.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso'||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Obtiene el Tipo de Trabajador 
  -- IMSS
  IF p_cod_proceso_cnt = 20 THEN
     LET v_tpo_trabajador = 'I';
  END IF

  -- Solo Infonavit
  IF p_cod_proceso_cnt = 21 THEN
     LET v_tpo_trabajador = 'S';
  END IF

  -- Obtiene saldo del Deudor por Originación
  -- Cuenta contable 2504070003 Devolución Saldo de la 
  -- Subcta de Viv GL - Dif Ca
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_deudor
  FROM   safre_viv:cre_saldo_deudor sdo,
         safre_viv:cre_ctr_contable cnt 
  WHERE  cnt.folio_liquida     = p_folio_liquida
--AND    cnt.folio_referencia  = sdo.folio_referencia
  AND    cnt.id_cre_acreditado = sdo.id_cre_acreditado 
  AND    cnt.tpo_trabajador   IN ('I','S')
  AND    cnt.tpo_deudor       IN (0,1,2,3,4,5)
  AND    sdo.movimiento        = 181;  
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_deudor = 0;
  END IF
    
  -- Cuenta Contable Incremento Cred X SSV 
  -- Insuficiente Cta Conex Cart
  -- 1502050010
  -- (Cargo) 
  SELECT SUM(monto_pesos)
  INTO   v_monto_cred_ssv
  FROM   cre_saldo_deudor sdo
  WHERE  sdo.folio_referencia  = p_folio_liquida
  AND    sdo.movimiento        = 252;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_cred_ssv = 0;
  END IF
    
  -- Cuenta Contable Traspaso Excedente SSV
  -- 2403012410 ---- Se cambia por la cuenta 2403011600 13/12/2012
  -- (Abono) 
  SELECT SUM(monto_pesos)
  INTO   v_monto_taa_exc
  FROM   cta_movimiento mov
  WHERE  mov.folio_liquida     = p_folio_liquida
  AND    mov.movimiento       IN(132,482,572,582);
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_taa_exc = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cta_contable       <> '0000000000'
    AND    cod_proceso         = p_cod_proceso
    AND    cod_proceso_cnt     = p_cod_proceso_cnt

    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento   IN (472,482,122,132,572,582,1572)
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
    END IF

    -- Cuenta Contable Deudor por Originación
    -- (Abono)
    IF v_cta_contable = '2504070003' THEN
       LET v_monto_pesos = v_monto_deudor;
    END IF

    -- Cuenta Contable Traspaso Excedente SSV
    -- (Abono) - Se cambia 2403012410 por la cuenta 2403011600 
    --IF v_cta_contable = '2403012410' THEN    
	IF v_cta_contable = '2403011600' AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 1 THEN    
       LET v_monto_pesos = v_monto_taa_exc;
    END IF
    
    -- Cuenta Contable Incremento Cred X SSV 
    -- Insuficiente Cta Conex Cart
    -- (Cargo) - Se cambia 1502050010 por la cuenta 2403011600 
    IF v_cta_contable = '2403011600' AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 2 THEN
       LET v_monto_pesos = v_monto_cred_ssv;
    END IF

    -- Saldo de la Subcuenta Solo Infonavit 97
    -- 2504080001 (Abono)
    IF v_cta_contable = '2504080001' THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta    IN (42,44)
       AND    movimiento   IN (582,122,132)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF
    END IF

    -- RISS
    -- Cuenta Contable Vivienda 97
    -- (Abono) 
    IF v_cta_contable = '2504070001' AND v_cod_transaccion_cnt = 12 AND v_cod_naturaleza_cta = 1 THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN (881)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF
    END IF

    -- Fortalecimiento de Crédito
    IF v_cod_transaccion_cnt = 61 THEN   
       IF v_cta_contable = '2203900001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (302)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF

       -- Saldo de la Subcuenta de Vivienda 97
       -- 2504070001 (Abono)
       IF v_cta_contable = '2504070001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (201)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF

       -- Saldo de la Subcuenta Solo Infonavit 97
       -- 2504080001 (Abono)
       IF v_cta_contable = '2504080001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta    IN (42,44)
          AND    movimiento   IN (201)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    END IF

    IF v_cod_subcta_cnt = 40 THEN
       SELECT SUM(importe)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_fondo72
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN (452)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF
    END IF

    -- Aportaciones Voluntarias 
    IF v_cod_transaccion_cnt = 66 THEN   
       -- Saldo de la Subcuenta de Aportaciones Voluntarias
       -- 2511010001 (Cargo)
       IF v_cta_contable = '2511010001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento    = 1242
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF

       -- Saldo de la Subcuenta de Vivienda 97
       -- 2504070001 (Abono)
       IF v_cta_contable = '2504070001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento    = 551
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    END IF

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos < 0 ) THEN
       LET v_monto_pesos = v_monto_pesos * (-1);
    END IF

    IF v_monto_pesos > 0 THEN
       --Insertar las cuentas contables en la tabla de transacciones
       --TRACE 'Inserta en transaccion';
       INSERT INTO safre_viv:cnt_transaccion VALUES(v_id_cuenta_contable,
                                                    0,
                                                    p_cod_proceso_cnt,
                                                    p_cod_proceso,
                                                    v_cod_transaccion_cnt,
                                                    v_cod_subcta_cnt,
                                                    v_cta_contable,
                                                    v_cod_naturaleza_cta,
                                                    p_folio_liquida,
                                                    v_monto_pesos,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos = 0;
  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


