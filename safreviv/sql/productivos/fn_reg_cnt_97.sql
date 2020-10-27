






create procedure "safreviv".fn_reg_cnt_97(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                               p_f_liquida          DATE,          --Fecha de liquidación del proceso
                               p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                               p_cod_proceso        SMALLINT,      --Código Proceso
                               p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 12072013
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_uso_gar       DECIMAL(20,2);  --Monto en pesos uso de garantía y AG

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_uso_gar       = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reg_cnt_97.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del importe en pesos para la referencia a la cta contable
  --Uso de Garantía y AG
  --Cuenta Contable 2504070004
  --Proceso 32 - USO GARANTIA 97
  SELECT SUM(monto_pesos)
  INTO   v_monto_uso_gar
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = p_f_liquida
  AND    subcuenta    IN (SELECT cod_subcta_cnt
                          FROM   safre_viv:cnt_regla_contable
                          WHERE  cta_contable    <> '0000000000'
                          AND    cod_proceso      = p_cod_proceso
                          AND    cod_proceso_cnt  = p_cod_proceso_cnt
                          AND    cod_subcta_cnt  <> 45)
  AND    folio_liquida = p_folio_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_uso_gar = 0;
     LET v_bnd_proceso   = 10; --Estado incorrecto. No se encontro información
  END IF

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

    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Uso de Garantía y AG
    -- 2504070004
    -- (Abono)
    IF v_cta_contable = '2504070004' THEN
       LET v_monto_pesos = v_monto_uso_gar;
    END IF

    -- Saldo de la Subcuenta Solo Infonavit 97
    -- 2504080001 (Cargo)
    IF v_cta_contable = '2504080001' THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta    IN (42,44)
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
          AND    movimiento    = 1262
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
          AND    movimiento    = 571
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    END IF

    --Insertar las cuentas contables en la tabla de transacciones
    --TRACE 'Inserta en transaccion';
    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos < 0 ) THEN
       LET v_monto_pesos = v_monto_pesos * (-1);
    END IF
    
    IF v_monto_pesos > 0 THEN
    
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
  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


