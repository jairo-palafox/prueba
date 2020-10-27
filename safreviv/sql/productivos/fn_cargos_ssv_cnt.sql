






create procedure "safreviv".fn_cargos_ssv_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                   p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                   p_cod_proceso        SMALLINT,      --Código Proceso
                                   p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 14062017
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_dev_ssv       DECIMAL(20,2);  --Monto Devolución a Patrones
DEFINE v_monto_dev_ssv1      DECIMAL(20,2);  --Monto Devolución a Patrones
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción
DEFINE v_folio_lote          DECIMAL(9,0);   --Folio Lote Devolución de Pagos Patrones
DEFINE v_monto_no_disp       DECIMAL(20,2);  --Monto no dispersados

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_dev_ssv       = 0;
  LET v_monto_dev_ssv1      = 0;
  LET v_bnd_inserta         = 0;
  LET v_folio_lote          = 0;
  LET v_monto_no_disp       = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_cargos_ssv_cnt.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Transacción 113 Liquidación Ajuste de Saldo TESOFE
  -- Cuenta Contable Vivienda SAR 97 (SHCP)
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento    = 1802
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (58,113)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento    = 1802
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Vivienda SAR 97 (SHCP)
    -- (Abono)
    IF v_cta_contable = '2203060006' THEN
       LET v_monto_pesos = v_monto_dev_ssv;
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
  -- Transacción 113 Liquidación Ajuste de Saldo TESOFE


  -- Transacción 114 Alta de Cuentas
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos = 0;

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (114)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Carga Inicial SACI
    --(Cargo)
    -- Cuenta Contable Vivienda 97
    --(Cargo y Abono)
    -- Cuenta Contable Conexión 8vo Transitorio
    --(Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1701)
    --AND    movimiento   IN (1701,1832)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
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
  -- Transacción 114 Alta de Cuentas


  -- Transacción 115 Alta de SSV
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Carga Inicial
  -- (Cargo)
  -- Cuenta Contable Vivienda SAR 97 (SHCP)
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1691 
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento   IN (1842,1852)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF


  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (115)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1842,1852)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Carga Inicial
    -- (Cargo)
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable        = '2504090001' OR
      (v_cta_contable        = '2504070001' AND
       v_cod_naturaleza_cta  = 1)           THEN
       LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 115 Alta de SSV


  -- Transacción 116 Con Unificación Sin Diferencia
  LET v_monto_pesos   = 0;
  LET v_monto_dev_ssv = 0;

  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento    = 1862
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt =  116
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1862)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv;
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
  -- Transacción 116 Con Unificación Sin Diferencia


  -- Transacción 117 Con Laudo
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Erogaciones Carácter Fortuito-Resul Ejercicios R
  -- (Cargo)
  -- Cuenta Contable Vivienda 97
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1721
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento   IN (1882,1892)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (117)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1882,1892)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Erogaciones Carácter Fortuito-Resul Ejercicios R
    -- (Cargo)
    IF v_cta_contable = '7104030200' THEN  
        LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF (v_cta_contable        = '2504070001'  AND 
        v_cod_naturaleza_cta  = 1)            THEN
        LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 117 Con Laudo


  -- Transacción 118 Con Deudor
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Dev SSV CONEXIÓN PSCD
  -- (Cargo)
  -- Cuenta Contable Vivienda 97
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1731
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento   IN (1902,1912)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (118)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1902,1912)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Dev SSV CONEXIÓN PSCD
    -- (Cargo)
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable        = '1502050005' OR
      (v_cta_contable        = '2504070001' AND
       v_cod_naturaleza_cta  = 1)           THEN
       LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 118 Con Deudor


  -- Transacción 119 Con Cargos por Crédito
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Carga Inicial
  -- (Cargo)
  -- Cuenta Contable Vivienda 97
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1741
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_pesos = 0;
  END IF


  -- Cuenta Contable Carga Inicial
  -- (Cargo)
  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento   IN (1922,1932)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (119)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1922,1932)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Carga Inicial
    -- (Cargo)
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable        = '2504090001' OR
      (v_cta_contable        = '2504070001' AND
       v_cod_naturaleza_cta  = 1)           THEN
       LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 119 Con Cargos por Crédito


  -- Transacción 120 Con Diferencia
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Carga Inicial
  -- (Cargo)
  -- Cuenta Contable Vivienda 97
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1751
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_pesos = 0;
  END IF

  -- Cuenta Contable Carga Inicial
  -- (Cargo)
  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento   IN (1942,1952)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (120)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento   IN (1942,1952)
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Carga Inicial
    -- (Cargo)
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable        = '2504090001' OR
      (v_cta_contable        = '2504070001' AND
       v_cod_naturaleza_cta  = 1)           THEN
       LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 120 Con Diferencia

  -- Transacción 121 Con Unificación con Diferencia
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  LET v_monto_pesos    = 0;
  LET v_monto_dev_ssv  = 0;
  LET v_monto_dev_ssv1 = 0;

  -- Cuenta Contable Erogaciones Carácter Fortuito-Resul Ejercicios R
  -- (Cargo)
  -- Cuenta Contable Vivienda 97
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento    = 1711
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv = 0;
  END IF

  -- Cuenta Contable Conexión 80vo Transitorio
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_ssv1
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    subcuenta    IN (4,8)
  AND    movimiento    = 1872
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_ssv1 = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt IN (121)
    AND    cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Vivienda 97
    -- Cuenta Contable Vivienda 92
    --(Cargo)
    -- Cuenta Contable Subcuenta TESOFE
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    movimiento    = 1872 
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Carga Inicial
    -- (Cargo)
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable        = '2504090001' OR
      (v_cta_contable        = '2504070001' AND
       v_cod_naturaleza_cta  = 1)           THEN
       LET v_monto_pesos = v_monto_dev_ssv;
    END IF

    -- Cuenta Contable Conexión 80vo Transitorio
    -- (Abono)
    IF v_cta_contable = '2203060003' THEN
       LET v_monto_pesos = v_monto_dev_ssv1;
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
  -- Transacción 121 Con Unificación con Diferencia

  RETURN v_bnd_proceso;

END PROCEDURE;


