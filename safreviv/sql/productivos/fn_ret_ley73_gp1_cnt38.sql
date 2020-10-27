






create procedure "safreviv".fn_ret_ley73_gp1_cnt38(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
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
DEFINE v_cta_conexion        DECIMAL(20,2);  --Monto Cuenta Conexión SAP-FICO
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_cta_conexion        = 0;
  LET v_movimiento          = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_ley73_gp1_cnt38.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Cuenta contable 2203120023 Cuenta de Conexión   
  --SAP-FICO
  --Falta obtener la información
  --SELECT SUM(importe)
  --INTO   v_cta_conexion
  --FROM   safre_viv:cta_movimiento
  --WHERE  f_liquida     = p_f_liquida
  --AND    movimiento    = 422
  --AND    folio_liquida = p_folio_liquida;
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
    WHERE  cod_transaccion_cnt  = p_transaccion
    AND    cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    SELECT SUM(importe)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento    = v_movimiento
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
    END IF

    --Cuenta contable Cuenta de Conexión SAP-FICO
    IF v_cta_contable = '2203120023' THEN
       LET v_monto_pesos = v_cta_conexion;
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

  RETURN v_bnd_proceso;

END PROCEDURE;


