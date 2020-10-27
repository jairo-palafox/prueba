






create procedure "safreviv".fn_cnt_dev_ssv_97(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidaci�n del proceso
                                   p_cod_proceso_cnt    SMALLINT,      --C�digo Proceso Contable
                                   p_cod_proceso        SMALLINT,      --C�digo Proceso
                                   p_transaccion        SMALLINT)      --C�digo Transacci�n contable
RETURNING SMALLINT;

--Declaraci�n de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --C�digo transacci�n contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --C�digo subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --C�digo naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_restituir     DECIMAL(20,2);  --Monto a restituir
DEFINE v_monto_intereses     DECIMAL(20,2);  --Monto intereses
DEFINE v_tpo_trabajador      CHAR(1);        --Tipo Trabajador
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserci�n

  --Inicializaci�n de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_restituir     = 0;
  LET v_monto_intereses     = 0;
  LET v_tpo_trabajador      = 0;
  LET v_bnd_inserta         = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_cnt_dev_ssv_97.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Obtiene saldo de la
  -- Cuenta contable 2504050001 Subcuenta de vivienda
  -- a restituir
  -- (Cargo)
     SELECT SUM(monto_pesos)
     INTO   v_monto_restituir
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    folio_liquida = p_folio_liquida
     {AND    subcuenta    IN (SELECT cod_subcta_cnt
                             FROM   safre_viv:cnt_regla_contable);}
     AND    subcuenta    IN (4,8,42,44);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_restituir = 0;
     END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
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
    AND    folio_liquida = p_folio_liquida;
    --AND    movimiento    = v_movimiento;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
    END IF

    IF v_cod_subcta_cnt = 44 THEN
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

    -- Cuenta Contable Subcuenta de vivienda a restituir
    -- (Cargo)
    IF v_cta_contable = '2504050001' THEN
       LET v_monto_pesos = v_monto_restituir;
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


