






create procedure "safreviv".fn_dev_amo_cnt53_esp(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                      p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                      p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                      p_cod_proceso        SMALLINT,      --Código Proceso
                                      p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 15032019
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_amort_exc     DECIMAL(20,2);  --Monto en pesos amortización excedente
DEFINE v_monto_rend_amort_exc DECIMAL(20,2);  --Monto en pesos rendimientos amort exced
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento
DEFINE v_monto_devolucion    DECIMAL(20,2);  --Monto en pesos devolucion 
DEFINE v_monto_intereses     DECIMAL(20,2);  --Monto en pesos intereses

DEFINE e_monto_acciones_his  DECIMAL(22,2);
DEFINE e_monto_pesos_his     DECIMAL(22,2);
DEFINE e_monto_pesos_cort    DECIMAL(22,2);
DEFINE e_precio_fondo        DECIMAL(19,14);

  --Inicialización de variables
  LET v_bnd_proceso          = 1; --Estado correcto
  LET v_id_cuenta_contable   = 0;
  LET v_cod_transaccion_cnt  = 0;
  LET v_cod_subcta_cnt       = 0;
  LET v_cta_contable         = '0000000000';
  LET v_cod_naturaleza_cta   = 0;
  LET v_monto_pesos          = 0;
  LET v_monto_amort_exc      = 0;
  LET v_monto_rend_amort_exc = 0;
  LET v_movimiento           = 0;
  LET v_monto_devolucion     = 0;
  LET v_monto_intereses      = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_dev_amo_cnt53.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suma del importe en pesos de la Amortización Excedente
  IF p_transaccion = 68 THEN 
     --Cuenta contable
     --2206030003 Amortización Excedente (Cargo)
     SELECT SUM(monto_pesos)
     INTO   v_monto_amort_exc
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    movimiento   <> 481
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amort_exc = 0;
     END IF
     IF v_monto_amort_exc IS NULL THEN
        LET v_monto_amort_exc = 0;
     END IF

     --Cuenta contable
     --5110010200 Rendimientos Amort Excedente (Cargo)
     SELECT SUM(monto_pesos)
     INTO   v_monto_rend_amort_exc
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    movimiento    = 481
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_rend_amort_exc = 0;
     END IF
     IF v_monto_rend_amort_exc IS NULL THEN
        LET v_monto_rend_amort_exc = 0;
     END IF
  END IF

  --Cancelación DAP's No Cobrados
  IF p_transaccion = 60 THEN
     SELECT SUM(pesos_liquidacion), SUM(rendimiento)
     INTO   v_monto_devolucion, v_monto_intereses
     FROM   ret_rendimiento_restitucion
     WHERE  folio_restitucion = p_folio_liquida
     AND    subcuenta         = 46;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_devolucion    = 0;
        LET v_monto_intereses     = 0;
     END IF
  END IF
  --Cancelación DAP's No Cobrados

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt  = p_transaccion
    AND    cta_contable       <> '0000000000'
    AND    cod_proceso         = p_cod_proceso
    AND    cod_proceso_cnt     = p_cod_proceso_cnt

    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento  NOT IN (521)
    AND    folio_liquida = p_folio_liquida;
    --AND    movimiento    = v_movimiento;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos  = 0;
    END IF

    -- Cuenta Contable Amortización Excedente
    -- (Cargo)
    IF p_transaccion = 68 THEN
       IF v_cta_contable = '2206030003' THEN
          LET v_monto_pesos = v_monto_amort_exc;
       END IF
    END IF

    -- Cuenta Contable Rendimientos Amortización Excedente
    -- (Cargo)
    IF v_cta_contable = '5110010200' THEN
       LET v_monto_pesos = v_monto_rend_amort_exc;
    END IF

    --Cancelación Devolución Amortizaciones Excedentes
    --Rendimientos
    IF p_transaccion = 60 THEN
       -- Cuenta Contable Dev Amort Liq en Exc
       -- SOPCPDD SACI HS
       -- (Cargo) 
       IF v_cta_contable = '2203150006' THEN
          LET v_monto_pesos = v_monto_devolucion;
       END IF

       -- Cuenta Contable Intereses Amortizaciones
       -- en Exceso
       -- (Cargo)
       IF v_cta_contable = '7102010050' THEN
          LET v_monto_pesos = v_monto_intereses;
       END IF
    END IF
    --Cancelación Devolución Amortizaciones Excedentes

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

  --Cancelación de Pago no realizados(Saldo Transferido)
  IF p_transaccion = 4 THEN

     LET e_precio_fondo       = 0;
     LET e_monto_acciones_his = 0;
     LET e_monto_pesos_his    = 0;
     LET e_monto_pesos_cort   = 0;
     LET v_monto_pesos        = 0;

     SELECT precio_fondo
     INTO   e_precio_fondo
     FROM   glo_valor_fondo
     WHERE  f_valuacion = p_f_liquida
     AND    fondo       = 11;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET e_precio_fondo = 0;
     END IF

     SELECT SUM(monto_pesos), SUM(monto_acciones)
     INTO   e_monto_pesos_his, e_monto_acciones_his
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    subcuenta     = 46
     AND    movimiento  NOT IN (521)
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET e_monto_acciones_his = 0;
        LET e_monto_pesos_his    = 0;
     END IF

     LET e_monto_pesos_cort = e_monto_acciones_his * e_precio_fondo;
     LET v_monto_pesos      = e_monto_pesos_cort   - e_monto_pesos_his;

     --Obtiene la suman del monto en pesos para la referencia a la cta contable
     FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
              cta_contable, cod_naturaleza_cta
       INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
              v_cta_contable, v_cod_naturaleza_cta
       FROM   safre_viv:cnt_regla_contable
       WHERE  cod_transaccion_cnt  = 9
       AND    cta_contable       <> '0000000000'
       AND    cod_proceso         = p_cod_proceso
       AND    cod_proceso_cnt     = p_cod_proceso_cnt
   
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

       --LET v_monto_pesos = 0;
     END FOREACH;
  END IF
  --Cancelación de Pago no realizados(Saldo Transferido)

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

  RETURN v_bnd_proceso;

END PROCEDURE;


