






create procedure "safreviv".fn_ci_his_ia_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                  p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                  p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                  p_cod_proceso        SMALLINT,      --Código Proceso
                                  p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 08082015
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(22,2);  --Monto en pesos
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento
DEFINE v_precio_fondo_o12    DECIMAL(19,14); --Precio Fondo del día 30/06/2014
DEFINE v_precio_fondo_o14    DECIMAL(19,14); --Precio Fondo del día 31/10/2014
DEFINE v_monto_aivs          DECIMAL(22,2);  --Monto en aivs
DEFINE v_monto_pes_n         DECIMAL(22,2);  --Monto en pesos Nov
DEFINE v_monto_pes_i         DECIMAL(22,2);  --Monto en pesos Ini
DEFINE v_monto_pes_int       DECIMAL(22,2);  --Monto en pesos intereses

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_movimiento          = 0;
  LET v_precio_fondo_o12    = 0;
  LET v_precio_fondo_o14    = 0;
  LET v_monto_aivs          = 0;
  LET v_monto_pes_n         = 0;
  LET v_monto_pes_i         = 0;
  LET v_monto_pes_int       = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ajuste_op_cnt.trace';

  -- Extrae precio accion del dia 01/10/2012
  SELECT precio_fondo
  INTO   v_precio_fondo_o12
  FROM   glo_valor_fondo
  WHERE  f_valuacion = '10012012'
  AND    fondo       = 11;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_precio_fondo_o12    = 0;
  END IF

  -- Extrae precio accion del dia 03/25/2015
  {SELECT precio_fondo
  INTO   v_precio_fondo_o14
  FROM   glo_valor_fondo
  WHERE  f_valuacion = '03252015'
  AND    fondo       = 11;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_precio_fondo_o14    = 0;
  END IF}

  -- Extrae precio accion del dia de la liquidación
  SELECT precio_fondo
  INTO   v_precio_fondo_o14
  FROM   glo_valor_fondo
  WHERE  f_valuacion = p_f_liquida
  AND    fondo       = 11;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_precio_fondo_o14    = 0;
  END IF

  --- 89 CARGA INICIAL HISTÓRICO INFONAVIT - AFORE92
  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  FOREACH
    --Extrae información de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
    WHERE  cta_contable       <> '0000000000'
    AND    cod_proceso         = p_cod_proceso
    AND    cod_proceso_cnt     = p_cod_proceso_cnt

    -- CONSISTENTES
    -- Cuenta Contable SAR 92 Decreto
    IF v_cod_transaccion_cnt = 100 THEN
       SELECT SUM(ctad.monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_decreto ctad,
              safre_viv:afi_decreto afid
       WHERE  ctad.f_liquida        = p_f_liquida
       AND    ctad.id_decreto       = afid.id_decreto
       AND    ctad.subcuenta        = v_cod_subcta_cnt
       AND    ctad.movimiento       = 999
       AND    ctad.folio_liquida    = p_folio_liquida
       AND    afid.ind_consistencia = 1;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       END IF
    END IF

    -- NO CONSISTENTES
    -- Cuenta Contable Saldo 92-97 EN AFORE
    IF v_cod_transaccion_cnt = 101 THEN
       SELECT SUM(ctad.monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_decreto ctad,
              safre_viv:afi_decreto afid
       WHERE  ctad.f_liquida        = p_f_liquida
       AND    ctad.id_decreto       = afid.id_decreto
       AND    ctad.subcuenta        = v_cod_subcta_cnt
       AND    ctad.movimiento       = 999
       AND    ctad.folio_liquida    = p_folio_liquida
       AND    afid.ind_consistencia = 0;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       END IF
    END IF

    -- RENDIMIENTOS 
    IF v_cod_transaccion_cnt = 9 THEN
       -- CONSISTENTES 
       IF (v_cta_contable = '5110010100'  OR
           v_cta_contable = '2503220001') THEN
           SELECT SUM(ctad.monto_acciones)
           INTO   v_monto_aivs
           FROM   safre_viv:cta_decreto ctad,
                  safre_viv:afi_decreto afid
           WHERE  ctad.f_liquida        = p_f_liquida
           AND    ctad.id_decreto       = afid.id_decreto
           AND    ctad.subcuenta        = v_cod_subcta_cnt
           AND    ctad.movimiento       = 999
           AND    ctad.folio_liquida    = p_folio_liquida
           AND    afid.ind_consistencia = 1;
           IF DBINFO('sqlca.sqlerrd2') == 0 THEN
              LET v_monto_aivs = 0;
           END IF
       END IF

       -- NO CONSISTENTES 
       IF (v_cta_contable = '8120010100'  OR
           v_cta_contable = '8220010100') THEN
           SELECT SUM(ctad.monto_acciones)
           INTO   v_monto_aivs
           FROM   safre_viv:cta_decreto ctad,
                  safre_viv:afi_decreto afid
           WHERE  ctad.f_liquida        = p_f_liquida
           AND    ctad.id_decreto       = afid.id_decreto
           AND    ctad.subcuenta        = v_cod_subcta_cnt
           AND    ctad.movimiento       = 999
           AND    ctad.folio_liquida    = p_folio_liquida
           AND    afid.ind_consistencia = 0;
           IF DBINFO('sqlca.sqlerrd2') == 0 THEN
              LET v_monto_aivs = 0;
           END IF
       END IF

       LET v_monto_pes_n   = v_monto_aivs * v_precio_fondo_o14; 
       LET v_monto_pes_i   = v_monto_aivs * v_precio_fondo_o12; 
       --LET v_monto_pes_int = v_monto_pes_n - v_monto_pes_i;
       LET v_monto_pesos   = v_monto_pes_n - v_monto_pes_i;
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

    LET v_monto_pesos   = 0;
    LET v_monto_aivs    = 0;
    LET v_monto_pes_i   = 0;
    LET v_monto_pes_int = 0;
  END FOREACH;

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

  RETURN v_bnd_proceso;

END PROCEDURE;


