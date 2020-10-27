






create procedure "safreviv".fn_dev_pat_97_cnt27(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                     p_f_liquida          DATE,          --Fecha de liquidaci�n del proceso
                                     p_cod_proceso_cnt    SMALLINT,      --C�digo Proceso Contable
                                     p_cod_proceso        SMALLINT,      --C�digo Proceso
                                     p_transaccion        SMALLINT)      --C�digo Transacci�n contable
RETURNING SMALLINT;

--�ltima modificaci�n 24062014
--Declaraci�n de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --C�digo transacci�n contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --C�digo subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --C�digo naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_dev_pat       DECIMAL(20,2);  --Monto Devoluci�n a Patrones
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserci�n
DEFINE v_folio_lote          DECIMAL(9,0);   --Folio Lote Devoluci�n de Pagos Patrones
DEFINE v_monto_no_disp       DECIMAL(20,2);  --Monto no dispersados

  --Inicializaci�n de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_bnd_inserta         = 0;
  LET v_folio_lote          = 0;
  LET v_monto_no_disp       = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_dev_pat_97_cnt27.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Cuenta Contable Devoluci�n a Patrones
  -- (Cargo y Abono)
  SELECT SUM(monto_pesos)
  INTO   v_monto_dev_pat
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = p_f_liquida
  AND    subcuenta    IN (SELECT cod_subcta_cnt
                          FROM   safre_viv:cnt_regla_contable
                          WHERE  cta_contable    <> '0000000000'
                          AND    cod_proceso      = p_cod_proceso
                          AND    cod_proceso_cnt  = p_cod_proceso_cnt)
  AND    movimiento   IN (342,372,512,1452,1462)
  AND    folio_liquida = p_folio_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_dev_pat = 0;
  END IF;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cta_contable    <> '0000000000'
    AND    cod_proceso      = p_cod_proceso
    AND    cod_proceso_cnt  = p_cod_proceso_cnt

    -- Cuenta Contable Subcuentas SAR 97 y Amortizaci�n (Concentradora)
    --(Cargo y Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento   IN (342,372,512,1452,1462)
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Saldo de la Subcuenta de Vivienda 97
    -- (Cargo)
    -- Cuenta Contable Devoluciones de Aportaciones a
    -- Patrones (Abono)
    IF v_cod_transaccion_cnt <> 34 OR  
       v_cod_transaccion_cnt <> 35 THEN
       -- Cuenta Contable Cuenta de Cx HS Dev a Patrones
       -- (Cargo)
       IF v_cta_contable = '2504070010' THEN
          LET v_monto_pesos = v_monto_dev_pat;
       END IF
   
       -- Cuenta Contable Cuenta Devoluciones de Aportaciones
       -- a Patrones
       -- (Abono)
       IF v_cta_contable = '2504070004' THEN
          LET v_monto_pesos = v_monto_dev_pat;
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


