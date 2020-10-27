






create procedure "safreviv".fn_sep_ctas_cnt36(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                   p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                   p_cod_proceso        SMALLINT,      --Código Proceso
                                   p_transaccion        SMALLINT)      --Código Transaccion contable 
RETURNING SMALLINT;

--Última modificación 22102015
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_deudor_homonimias   DECIMAL(20,2);  --Monto Deudor Homonimias
DEFINE v_cta_conexion_hs_ds  DECIMAL(20,2);  --Monto Cuenta Conexión HS Dis, Sep
DEFINE v_cta_conexion_hs     DECIMAL(20,2);  --Monto Cuenta Conexión HS
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_deudor_homonimias   = 0;
  LET v_cta_conexion_hs_ds  = 0;
  LET v_cta_conexion_hs     = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_sep_ctas_cnt36.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Cuenta contable 1502050011 Deudor por Homonimias
  --Falta obtener la información
  --SELECT SUM(importe)
  --INTO   v_deudor_homonimias
  --FROM   safre_viv:cta_fondo72
  --WHERE  folio_liquida = p_folio_liquida
  --AND    f_liquida     = p_f_liquida
  --AND    movimiento    = 422;
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     --LET v_tanto_adicional = 0;
  --END IF

  --Cuenta contable 2504050001 Cuenta de Conexión   
  --HS, Dispersión, Separación de Cuentas
  --Falta obtener la información
  --SELECT SUM(importe)
  --INTO   v_cta_conexion_hs_ds
  --FROM   safre_viv:cta_fondo72
  --WHERE  folio_liquida = p_folio_liquida
  --AND    f_liquida     = p_f_liquida
  --AND    movimiento    = 422;
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     --LET v_tanto_adicional = 0;
  --END IF

  --Cuenta contable 2203400001 Cuenta de Conexión   
  --HS
  --Falta obtener la información
  --SELECT SUM(importe)
  --INTO   v_cta_conexion_hs
  --FROM   safre_viv:cta_fondo72
  --WHERE  folio_liquida = p_folio_liquida
  --AND    f_liquida     = p_f_liquida
  --AND    movimiento    = 422;
  --IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     --LET v_tanto_adicional = 0;
  --END IF

  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    --WHERE  cod_transaccion_cnt  = p_transaccion
    WHERE  cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida = p_folio_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    f_liquida     = p_f_liquida
    AND    movimiento    = 381;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
    END IF

    --Cuenta contable Deudor por Homonimias
    IF v_cta_contable = '1502050011' THEN
       LET v_monto_pesos = v_deudor_homonimias;
    END IF

    --Cuenta contable Cuenta de Conexión HS, Dispersión,
    --Separación de Cuentas 
    IF v_cta_contable = '2504050001' THEN
       LET v_monto_pesos = v_cta_conexion_hs_ds;
    END IF

    --Cuenta contable Cuenta de Conexión HS
    IF v_cta_contable = '2203400001' THEN
       LET v_monto_pesos = v_cta_conexion_hs;
    END IF

    IF v_cod_transaccion_cnt = 108 OR
       v_cod_transaccion_cnt = 109 THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  folio_liquida = p_folio_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    f_liquida     = p_f_liquida
       AND    movimiento   IN (381);
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF

       IF v_cod_subcta_cnt = 4 AND 
          v_cta_contable   = '2504070001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  folio_liquida = p_folio_liquida
          AND    subcuenta     = 61
          AND    f_liquida     = p_f_liquida
          AND    movimiento   IN (381);
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF

       IF v_cod_subcta_cnt = 8 AND 
          v_cta_contable   = '2504060001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  folio_liquida = p_folio_liquida
          AND    subcuenta     = 62
          AND    f_liquida     = p_f_liquida
          AND    movimiento   IN (381);
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
          
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

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

  RETURN v_bnd_proceso;

END PROCEDURE;


