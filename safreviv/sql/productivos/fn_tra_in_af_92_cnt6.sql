






create procedure "safreviv".fn_tra_in_af_92_cnt6(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                      p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                      p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                      p_cod_proceso        SMALLINT,      --Código Proceso
                                      p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 28022018
--Declaración de variables
  DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
  DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
  DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
  DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
  DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
  DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
  DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
  DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_bnd_inserta         = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_tra_in_af_92_cnt6.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suma del monto en pesos para la referencia a la cta contable
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

    IF v_cod_transaccion_cnt = 6 THEN
       -- Cuentas Contables:
       -- 2503210001 (Cargo) Reserva Aportaciones SAR 92
       -- 2504060001 (Abono) Saldo 92-97 EN AFORE
       -- Consistentes mas No Consistentes
       IF v_cta_contable = '2503210001' OR
          v_cta_contable = '2504060001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    folio_liquida = p_folio_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento    IN(111,231); --Falta agregar el movimiento 592
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos = 0;
          END IF
       END IF

       -- Cuenta Contable SAR 92 Decreto
       -- No Consistentes:
       -- 8220010100 (Cargo) Aport Viv SAR92 Decreto No Consistente
       -- 8120010100 (Abono) Aport Viv SAR92 Decreto No Consistente
       IF v_cta_contable = '8220010100' OR
          v_cta_contable = '8120010100' THEN
          SELECT SUM(ctad.monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_decreto ctad,
                 safre_viv:afi_decreto afid
          WHERE  ctad.f_liquida        = p_f_liquida
          AND    ctad.id_decreto       = afid.id_decreto
          AND    ctad.subcuenta        = v_cod_subcta_cnt
          AND    ctad.movimiento       = 592
          AND    ctad.folio_liquida    = p_folio_liquida
          AND    afid.ind_consistencia = 0;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos = 0;
          END IF
       END IF
       
       -- Cuenta Contable SAR 92 Decreto (Cargo)
       -- Consistentes:
       -- 8220010200 (Cargo) Aport Viv SAR92 Decreto Consistente
       -- 8120010200 (Abono) Aport Viv SAR92 Decreto Consistente
       IF v_cta_contable = '8220010200' OR
          v_cta_contable = '8120010200' THEN
          SELECT SUM(ctad.monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_decreto ctad,
                 safre_viv:afi_decreto afid
          WHERE  ctad.f_liquida        = p_f_liquida
          AND    ctad.id_decreto       = afid.id_decreto
          AND    ctad.subcuenta        = v_cod_subcta_cnt
          AND    ctad.movimiento       = 362
          AND    ctad.folio_liquida    = p_folio_liquida
          AND    afid.ind_consistencia = 1;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos = 0;
          END IF
       END IF
    END IF

    -- Rechazo Pago Previo por el Proceso de Devolución SSV
    IF v_cod_transaccion_cnt = 131 THEN
       -- Cuentas Contables:
       -- 2504060001 (Cargo) Saldo 92-97 EN AFORE
       -- 2504090001 (Abono) Carga Inicial
       -- Consistentes mas No Consistentes
       --IF v_cta_contable = '2503210001' OR
          --v_cta_contable = '2504060001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    folio_liquida = p_folio_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento    = 1993;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos = 0;
          END IF
       --END IF
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


