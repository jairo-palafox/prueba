






create procedure "safreviv".fn_aclara_cnt17(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                 p_f_liquida          DATE,          --Fecha de liquidaci�n del proceso
                                 p_cod_proceso_cnt    SMALLINT,      --C�digo Proceso Contable
                                 p_cod_proceso        SMALLINT,      --C�digo Proceso
                                 p_transaccion        SMALLINT)      --C�digo Transacci�n contable
RETURNING SMALLINT;

--�ltima modificaci�n 30112018
--Declaraci�n de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --C�digo transacci�n contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --C�digo subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --C�digo naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_acciones      DECIMAL(16,6);  --Monto en acciones
DEFINE v_bnd_inserta         SMALLINT;       --Estatus para inserci�n
DEFINE v_precio_fondo        DECIMAL(19,14); --Precio Fondo
DEFINE v_fondo_inversion     SMALLINT;       --Fondo Inversi�n

  --Inicializaci�n de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_acciones      = 0;
  LET v_bnd_inserta         = 0;
  LET v_precio_fondo        = 0;
  LET v_fondo_inversion     = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_aclara_cnt17.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --p_transaccion = 17 (LIQUIDACI�N DE LA SALIDA DEL ACLARATORIO MENSUAL, 
  --                    NO SE DEBER� GENERAR EVENTO CONTABLE POR LAS SALIDAS 
  --                    DEL ACLARATORIO CORRESPONDIENTE A LAS CAUSALES 13 Y 17 
  --                    DADO QUE YA FUERON CONTABILIZADAS EN EL REGISTRO 
  --                    DEL PAGO)
  --p_transaccion = 18 (LIQUIDACI�N DE ACTUALIZACI�N DE ESTATUS DE 
  --                    ACLARACIONES, 
  --                    EL REGISTRO CONTABLE SE DEBER� GENERAR POR LAS 
  --                    ACTUALIZACIONES CORRESPONDIENTES A LAS CAUSALES 13 Y 17)
  --p_transaccion = 19 (LIQUIDACI�N DE LA SALIDA DEL ACLARATORIO MANUAL, 
  --                    NO SE DEBER� GENERAR EVENTO CONTABLE POR LAS SALIDAS 
  --                    DEL ACLARATORIO CORRESPONDIENTE A LAS CAUSALES 13 Y 17 
  --                    DADO QUE YA FUERON CONTABILIZADAS EN EL REGISTRO 
  --                    DEL PAGO)
  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt = p_transaccion
    AND    cta_contable       <> '0000000000'
    AND    cod_proceso         = p_cod_proceso
    AND    cod_proceso_cnt     = p_cod_proceso_cnt

    -- Obtiene saldo de las subcuentas de Vivienda 97 Solo Infonavit
    -- y Amortizaci�n (Concentradora)
    -- (Abono)
    {SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    folio_liquida = p_folio_liquida;
    --IF STATUS = NOTFOUND THEN
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF}

    -- Obtiene saldo de las subcuentas de Vivienda 97
    -- y Amortizaci�n (Concentradora)
    -- (Abono)
    SELECT fondo_inversion, SUM(monto_acciones)
    INTO   v_fondo_inversion, v_monto_acciones
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida   = p_folio_liquida
    AND    subcuenta       = v_cod_subcta_cnt
    AND    movimiento NOT IN (1851,1861,1911,2082,2092,2142,1871,1881,1921,2102,2112,2152,1891,1901,1931,2122,2132,2162)
    AND    f_liquida       = p_f_liquida
    GROUP BY 1;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_acciones = 0;
       LET v_monto_pesos    = 0;
    END IF

    SELECT precio_fondo
    INTO   v_precio_fondo
    FROM   glo_valor_fondo
    WHERE  fondo       = v_fondo_inversion
    AND    f_valuacion = TODAY;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_precio_fondo = 0;
       LET v_monto_acciones = 0;
    END IF

    LET v_monto_pesos = v_monto_acciones * v_precio_fondo;

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

  --Inicializaci�n de variables
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --de portabilidad
  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cod_transaccion_cnt = 110
    AND    cta_contable       <> '0000000000'
    AND    cod_proceso         = p_cod_proceso
    AND    cod_proceso_cnt     = p_cod_proceso_cnt

    -- Obtiene saldo de las subcuentas de Vivienda 97 (Aclaratorio) (Cargo)
    -- y Portabilidad (Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  folio_liquida   = p_folio_liquida
    AND    subcuenta       = 60
    AND    movimiento NOT IN (1851,1861,1911,2082,2092,2142,1871,1881,1921,2102,2112,2152,1891,1901,1931,2122,2132,2162)
    AND    f_liquida     = p_f_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
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


