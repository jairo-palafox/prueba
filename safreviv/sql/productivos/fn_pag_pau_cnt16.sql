






create procedure "safreviv".fn_pag_pau_cnt16(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                  p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                  p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                  p_cod_proceso        SMALLINT,      --Código Proceso
                                  p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_aportaciones        DECIMAL(20,2);  --Monto aportaciones
DEFINE v_amortizaciones      DECIMAL(20,2);  --Monto aportaciones
DEFINE v_intereses           DECIMAL(20,2);  --Monto intereses
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserción

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_aportaciones        = 0;
  LET v_amortizaciones      = 0;
  LET v_intereses           = 0;
  LET v_bnd_inserta         = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_pag_pau_cnt16.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2206021102 Aportaciones por recibir
  --Afores (Cargo)
  SELECT SUM(imp_ap_pat)
  INTO   v_aportaciones
  FROM   safre_viv:cta_his_pagos 
  WHERE  folio          = p_folio_liquida;
  --AND    origen_archivo = 1;
  --IF STATUS = NOTFOUND THEN;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_aportaciones = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2206021103 Amortizaciones por recibir
  --Afores (Cargo)
  SELECT SUM(imp_am_cre)
  INTO   v_amortizaciones
  FROM   safre_viv:cta_his_pagos 
  WHERE  folio          = p_folio_liquida;
  --IF STATUS = NOTFOUND THEN;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_amortizaciones = 0;
  END IF

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

    -- Obtiene saldo de las subcuentas de Vivienda 97 Solo Infonavit
    -- y Amortización (Concentradora)
    -- (Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    folio_liquida = p_folio_liquida;
    --IF STATUS = NOTFOUND THEN;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    -- Cuenta Contable Aportaciones por recibir     
    -- (Cargo)
    IF v_cta_contable = '2206021102' THEN
       LET v_monto_pesos = v_aportaciones;
    END IF

    -- Cuenta Contable Aportaciones por recibir     
    -- (Cargo)
    IF v_cta_contable = '2206021103' THEN
       LET v_monto_pesos = v_amortizaciones;
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


