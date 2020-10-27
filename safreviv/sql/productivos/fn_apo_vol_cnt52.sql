






create procedure "safreviv".fn_apo_vol_cnt52(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                  p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                  p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                  p_cod_proceso        SMALLINT,      --Código Proceso
                                  p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 29012015
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos_cnss    DECIMAL(20,2);  --Monto en pesos apo vol cambio nss
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_pesos_cnss    = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_apo_vol_cnt52.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --- 52 APORTACIONES VOLUNTARIAS
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt = p_transaccion
    AND    cta_contable       <> '0000000000'
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

    -- Transaccion Cambio NSS Aportaciones Voluntarias
    IF p_transaccion = 73 THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    folio_liquida = p_folio_liquida
       AND    movimiento    = 1522;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF

       {SELECT SUM(imp_ap_vol)
       INTO   v_monto_pesos_cnss
       FROM   safre_viv:pag_det_apvol
       WHERE  folio          = p_folio_liquida
       AND    ind_cambio_nss = 1
       AND    f_cambio_nss   = p_f_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       ELSE
          LET v_monto_pesos = v_monto_pesos_cnss;
       END IF}

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

  -- Transacción Cambio NSS Aportaciones Voluntarias
  {IF v_monto_pesos_cnss > 0  AND
     p_transaccion = 73 THEN

     UPDATE safre_viv:pag_det_apvol
     SET    ind_cambio_nss = 2
     WHERE  folio          = p_folio_liquida
     AND    ind_cambio_nss = 1
     AND    f_cambio_nss   = p_f_liquida;

     INSERT INTO safre_viv:cnt_ctr_esp_liquida VALUES(p_folio_liquida,
                                                      p_f_liquida,
                                                      p_cod_proceso,
                                                      0,
                                                      TODAY,
                                                      p_cod_proceso_cnt,
                                                      p_transaccion,
                                                      v_monto_pesos_cnss,
                                                      2); --1>Registrado, 2>Contabilizado

     LET v_monto_pesos_cnss = 0;
  END IF}

  IF p_transaccion = 65 THEN
     --Obtiene la suman del monto en pesos para la referencia a la cta contable
     FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
              cta_contable, cod_naturaleza_cta
       INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
              v_cta_contable, v_cod_naturaleza_cta
       FROM   safre_viv:cnt_regla_contable
       WHERE  cod_transaccion_cnt = 69
       AND    cta_contable       <> '0000000000'
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
  END IF

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

  RETURN v_bnd_proceso;

END PROCEDURE;


