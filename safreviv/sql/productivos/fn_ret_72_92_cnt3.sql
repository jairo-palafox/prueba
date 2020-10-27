






create procedure "safreviv".fn_ret_72_92_cnt3(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                   p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                   p_cod_proceso        SMALLINT,      --Código Proceso
                                   p_transaccion        SMALLINT)      --Código Transaccion contable 
RETURNING SMALLINT;

--Última modificación 07022020
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos
DEFINE v_tanto_adicional     DECIMAL(20,2);  --Monto Tanto Adicional
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

--Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_pesos1        = 0;
  LET v_tanto_adicional     = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_72_92_cnt3.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del importe en pesos para la referencia a la cta contable
  --p_transaccion = 3 (Transacción Liquidación Retiro Fondo anterior 72 - 92)
  IF p_transaccion = 3 THEN
     LET v_movimiento = 182;
  END IF

  --p_transaccion = 60 (Cancelación DAP'S No Cobrado)
  IF p_transaccion = 60 THEN
     LET v_movimiento = 141;
  END IF

  SELECT SUM(importe)
  INTO   v_monto_pesos
  FROM   safre_viv:cta_fondo72
  WHERE  folio_liquida = p_folio_liquida
  AND    f_liquida     = p_f_liquida
  AND    movimiento    = v_movimiento;
  IF v_monto_pesos IS NULL THEN
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_pesos  = 0;
     LET v_monto_pesos1 = 0;
  ELSE
     LET v_monto_pesos1 = v_monto_pesos;
  END IF

  --Cuenta contable de Tanto Adicional
  SELECT SUM(importe)
  INTO   v_tanto_adicional
  FROM   safre_viv:cta_fondo72
  WHERE  folio_liquida = p_folio_liquida
  AND    f_liquida     = p_f_liquida
  AND    movimiento   IN (422,601);
  IF v_tanto_adicional IS NULL THEN
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_tanto_adicional = 0;
  END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt  = p_transaccion
    AND    cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    --Cuenta contable Devolución de Fondo de Ahorro 1972-1992
    IF v_cta_contable = '2501070002' THEN
       LET v_monto_pesos = v_monto_pesos1 + v_tanto_adicional;
    END IF

    --Cuenta contable de Tanto Adicional
    IF v_cta_contable = '2502010001' THEN
       LET v_monto_pesos = v_tanto_adicional;
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

    END IF
    LET v_monto_pesos = v_monto_pesos1;
  END FOREACH;

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información
  RETURN v_bnd_proceso;

END PROCEDURE;


