






CREATE PROCEDURE "selefp".fn_rest_ret_ley73_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                       p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                       p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                       p_cod_proceso        SMALLINT,      --Código Proceso
                                       p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 23092019
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_tanto_adicional     DECIMAL(20,2);  --Monto Tanto Adicional
DEFINE v_monto_conexion      DECIMAL(20,2);  --Monto Conexión
DEFINE v_monto_restituir     DECIMAL(20,2);  --Monto Restitución
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento
DEFINE v_monto_sdo_inicial   DECIMAL(20,2);  --Monto saldo inicial
DEFINE v_monto_dev_viv_tra   DECIMAL(20,2);  --Monto dev sub viv trab no oblig
DEFINE v_monto_ret_ley73_a1  DECIMAL(20,2);  --Monto ret ley 73 anexo1

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_tanto_adicional     = 0;
  LET v_monto_conexion      = 0;
  LET v_movimiento          = 0;
  LET v_monto_sdo_inicial   = 0;
  LET v_monto_dev_viv_tra   = 0;
  LET v_monto_ret_ley73_a1  = 0;

  -- Obtiene saldo de la Cuenta Contable
  -- 2504090001 Cargo Inicial (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_sdo_inicial
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento   IN (621,631)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_sdo_inicial  = 0;
  END IF

  -- Obtiene saldo de la Cuenta Contable
  -- 2504070005 Devol Subcuenta Viv Trab No Obligados IMSS (Cargo)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_viv_tra
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento   IN (621,631)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_viv_tra  = 0;
  END IF

  -- Obtiene saldo del mov 1442 CARGO RETIRO LEY 73 ANEXO 1
  SELECT SUM(monto_pesos)
  INTO   v_monto_ret_ley73_a1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento   IN (1442,661,2072,1951)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_ret_ley73_a1  = 0;
  END IF

  --#######################################
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
--  WHERE  cod_transaccion_cnt  = p_transaccion
    WHERE  cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    --Saldo de la subcuebta Vivienda 97 2504070001
    --Saldo de la Subcuenta Vivienda 92 2504060001
    --(Cargo)
    {IF v_cod_naturaleza_cta = 2 THEN
       IF v_cta_contable       = '2504070001' OR 
          v_cta_contable       = '2504060001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  folio_liquida = p_folio_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (621,631)
          AND    f_liquida     = p_f_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    END IF}

    --Saldo de la subcuebta Vivienda 97 2504070001
    --Saldo de la Subcuenta Vivienda 92 2504060001
    --(Abono)
    IF v_cod_naturaleza_cta = 1 THEN
       IF v_cta_contable       = '2504070001' OR 
          v_cta_contable       = '2504060001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  folio_liquida = p_folio_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (621,631)
          AND    f_liquida     = p_f_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    END IF

    -- Cargo Inicial 
    -- 2504090001 (Abono)
    {IF v_cta_contable = '2504090001' THEN
       LET v_monto_pesos = v_monto_sdo_inicial; 
    END IF}

    --ANEXO UNO DEV DE LA SSV 8212020100 (Cargo)
    --DEV DE SSV ANEXO 1 8112020100 (Abono)
    IF v_cta_contable       = '8212020100' OR 
       v_cta_contable       = '8112020100' THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  folio_liquida = p_folio_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN (1442,661,2072,1951)
       AND    f_liquida     = p_f_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF
    END IF

    IF p_cod_proceso_cnt = 117 THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_ret_ley73_a1
       FROM   safre_viv:cta_movimiento
       WHERE  folio_liquida = p_folio_liquida
       AND    movimiento   IN (1442,661,2072,1951)
       AND    f_liquida     = p_f_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_ret_ley73_a1  = 0;
       ELSE
          LET v_monto_pesos = v_monto_ret_ley73_a1;
       END IF
    END IF

    -- Devol Subcuenta Viv Trab No Obligados IMSS 
    -- 2504070005 (Cargo)
    IF v_cta_contable = '2504070005' THEN
       LET v_monto_pesos = v_monto_dev_viv_tra; 
    END IF

    -- Transacción 149
    IF v_cod_transaccion_cnt = 149 THEN
       LET v_monto_pesos = v_monto_ret_ley73_a1;
    END IF
    -- Transacción 149

    -- Transacción 150
    IF v_cod_transaccion_cnt = 150 THEN
       LET v_monto_pesos = v_monto_ret_ley73_a1;
    END IF
    -- Transacción 150

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos < 0 ) THEN
       LET v_monto_pesos = v_monto_pesos * (-1);
    END IF

    --Insertar las cuentas contables en la tabla de transacciones
    IF v_monto_pesos > 0 THEN
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
       LET v_monto_pesos = 0;
    END IF
  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


