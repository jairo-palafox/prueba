






create procedure "safreviv".fn_ret_solo_inf97_cnt30(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                         p_f_liquida          DATE,          --Fecha de liquidaci�n del proceso
                                         p_cod_proceso_cnt    SMALLINT,      --C�digo Proceso Contable
                                         p_cod_proceso        SMALLINT,      --C�digo Proceso
                                         p_transaccion        SMALLINT)      --C�digo Transaccion contable 
RETURNING SMALLINT;

--�ltima modificaci�n 24042018
--Declaraci�n de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --C�digo transacci�n contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --C�digo subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --C�digo naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos
DEFINE v_tanto_adicional     DECIMAL(20,2);  --Monto Tanto Adicional
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

  --Inicializaci�n de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_tanto_adicional     = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_solo_inf97_cnt30.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del importe en pesos para la referencia a la cta contable
  --p_transaccion = 36 (Transacci�n Liquidaci�n del proceso de Devoluci�n del 
  --                    Saldo 97 en Adelante)
  --p_transaccion = 60 (El registro contable se deber� realizar en el momento
  --                    de la cancelaci�n de DAP's no cobrados)

  --Obtiene la suman del importe en pesos para la referencia a la cta contable
  --p_transaccion = 36 (Transacci�n Liquidaci�n del proceso de Devoluci�n del
  --                    Saldo 97 en Adelante) 
  IF p_transaccion = 36 THEN
     LET v_movimiento = 172;
  END IF

  --p_transaccion = 60 (El registro contable se deber� realizar en el momento
  --                    de la cancelaci�n de DAP's no cobrados)
  IF p_transaccion = 60 THEN
     LET v_movimiento = 131;
  END IF

  --p_transaccion = 132 (El registro contable se deber� realizar en el momento
  --                    de la conciliaci�n del Retiro Solo INFONAVIT)
  IF p_transaccion = 132 THEN
     LET v_movimiento = 1821;
  END IF

  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt  = p_transaccion
    AND    cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento    = v_movimiento
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
       LET v_monto_pesos1 = 0;
    ELSE
       LET v_monto_pesos1 = v_monto_pesos;
    END IF

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

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro informaci�n

  RETURN v_bnd_proceso;

END PROCEDURE;


