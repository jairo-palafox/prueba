






create procedure "safreviv".fn_ret_sar_92_97_cnt14(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                        p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                        p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                        p_cod_proceso        SMALLINT,      --Código Proceso
                                        p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 01112016
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_devolucion          DECIMAL(20,2);  --Monto devolucion
DEFINE v_int_compl           DECIMAL(20,2);  --Monto intereses complementarios
DEFINE v_carga_inicial       DECIMAL(20,2);  --Monto carga inicial
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_devolucion          = 0;
  LET v_int_compl           = 0;
  LET v_carga_inicial       = 0;
  LET v_bnd_inserta         = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_sar_92_97_cnt14.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2504070004 Dev Sub Viv SAR 92-97
  --Afores (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_devolucion
  FROM   cta_movimiento
  WHERE  f_liquida       = p_f_liquida
  AND    movimiento NOT IN (1011,911,1662)
  AND    subcuenta      IN (SELECT cod_subcta_cnt
                            FROM   safre_viv:cnt_regla_contable
                            WHERE  cta_contable    <> '0000000000'
                            AND    cod_proceso      = p_cod_proceso
                            AND    cod_proceso_cnt  = p_cod_proceso_cnt)
  AND    folio_liquida   = p_folio_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_devolucion = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2504090001 Carga Inicial
  --Afores (Cargo)
  SELECT SUM(monto_pesos)
  INTO   v_carga_inicial
  FROM   safre_viv:cta_movimiento
  WHERE  folio_liquida = p_folio_liquida
  AND    movimiento   IN (1011)
  AND    f_liquida     = p_f_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_int_compl = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Obtiene saldo de las subcuentas de Vivienda 97 y 92-97 EN AFORE
    -- (Cargo)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida   = p_folio_liquida
    AND    subcuenta       = v_cod_subcta_cnt
    AND    movimiento NOT IN (832,842,852,862,1011,911,1662)
    AND    f_liquida       = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Dev Sub Viv SAR 92-97 Afores 
    -- (Abono y Cargo)
    IF v_cta_contable = '2504070004' THEN
       LET v_monto_pesos = v_devolucion;
    END IF

    -- Cuenta Contable Cuentas por Pagar
    -- (Abono)
    IF v_cta_contable = '2203120008' THEN
       LET v_monto_pesos = v_devolucion;
    END IF

    -- Cuenta Contable Carga Inicial
    -- (Cargo)
    IF v_cta_contable = '2504090001' THEN
       LET v_monto_pesos = v_carga_inicial;
    END IF

    -- Obtiene variación menor retiros de Vivienda 97 y 92-97 EN AFORE
    -- (Abono)
    IF (v_cta_contable       = '2504060001'  OR
        v_cta_contable       = '2504070001') AND
        v_cod_naturaleza_cta = 1             THEN
        SELECT SUM(monto_pesos)
        INTO   v_monto_pesos
        FROM   safre_viv:cta_movimiento
        WHERE  folio_liquida   = p_folio_liquida
        AND    subcuenta       = v_cod_subcta_cnt
        AND    movimiento      = 1011
        AND    f_liquida       = p_f_liquida;
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_monto_pesos = 0;
        END IF
    END IF

    -- RISS
    -- Cuenta Contable VIVIENDA VOL RISS
    -- (Cargo)
    IF v_cta_contable = '2505010002' THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN (1662)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF
    END IF
    
    -- Cuenta Contable Vivienda 97
    -- (Abono)
    IF v_cta_contable = '2504070001' AND v_cod_transaccion_cnt = 0 AND v_cod_naturaleza_cta = 1 THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN (911)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
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


