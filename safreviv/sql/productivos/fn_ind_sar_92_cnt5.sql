






create procedure "safreviv".fn_ind_sar_92_cnt5(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                    p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                    p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                    p_cod_proceso        SMALLINT,      --Código Proceso
                                    p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción
DEFINE v_aportaciones        DECIMAL(20,2);  --Aportaciones
DEFINE v_intereses           DECIMAL(20,2);  --Intereses

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_bnd_inserta         = 0;
  LET v_aportaciones        = 0;
  LET v_intereses           = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ind_sar_92_cnt5.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Cuenta Contable Convenios - SAR 92 Aport
  -- en Conv SAR (Cargo) Aportaciones
  SELECT SUM(monto_pesos)
  INTO   v_aportaciones
  FROM   safre_viv:cta_decreto
  WHERE  f_liquida     = p_f_liquida
  AND    subcuenta     = 48
  AND    movimiento    = 71
  AND    folio_liquida = p_folio_liquida;
  --IF STATUS = NOTFOUND THEN;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_aportaciones = 0;
  END IF

  -- Cuenta Contable Intereses Complementarios
  -- Subcta de Vivienda (Cargo) Intereses
  --SELECT SUM(monto_pesos)
  --INTO   v_intereses
  --FROM   safre_viv:cta_decreto
  --WHERE  f_liquida     = p_f_liquida
  --AND    movimiento    = 3
  --AND    folio_liquida = p_folio_liquida;
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     --LET v_intereses = 0;
  --END IF

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

    -- Cuenta Contable Convenios - SAR 92 Aport
    -- en Conv SAR (Cargo)
    -- Aportaciones
    IF v_cta_contable = '2508010011' THEN
       LET v_monto_pesos = v_aportaciones;
    END IF

    -- Cuenta Contable Intereses Complementarios 
    -- Subcta de Vivienda (Cargo)
    -- Intereses
    IF v_cta_contable = '5102020300' THEN
       LET v_monto_pesos = v_intereses;
    END IF

    -- Cuenta Contable SAR 92 Decreto (Abono)
    -- Se acumula aportaciones mas intereses
    IF v_cta_contable = '2503220001' THEN
       LET v_monto_pesos = v_aportaciones + v_intereses;
    END IF

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos < 0 ) THEN
       LET v_monto_pesos = v_monto_pesos * (-1);
    END IF

    --Insertar las cuentas contables en la tabla de transacciones
    --TRACE 'Inserta en transaccion';
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

       LET v_monto_pesos = 0;
    END IF

  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


