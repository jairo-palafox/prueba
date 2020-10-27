






create procedure "safreviv".fn_ajuste_op_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                  p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                  p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                  p_cod_proceso        SMALLINT,      --Código Proceso
                                  p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 27072015
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

DEFINE v_monto_pes_sc97      DECIMAL(22,2);  --Monto en pesos sub97 cargo
DEFINE v_monto_pes_sa97      DECIMAL(22,2);  --Monto en pesos sub97 abono
DEFINE v_monto_pes_sc92      DECIMAL(22,2);  --Monto en pesos sub92 cargo
DEFINE v_monto_pes_sa92      DECIMAL(22,2);  --Monto en pesos sub92 abono
DEFINE v_monto_aiv_sc97      DECIMAL(22,2);  --Monto en aivs sub97 cargo
DEFINE v_monto_aiv_sa97      DECIMAL(22,2);  --Monto en aivs sub97 abono
DEFINE v_monto_aiv_sc92      DECIMAL(22,2);  --Monto en aivs sub92 cargo
DEFINE v_monto_aiv_sa92      DECIMAL(22,2);  --Monto en aivs sub92 abono
DEFINE v_monto_aiv_s97       DECIMAL(22,2);  --Monto en aivs sub97 
DEFINE v_monto_aiv_s92       DECIMAL(22,2);  --Monto en aivs sub92
DEFINE v_monto_pes_s97_n     DECIMAL(22,2);  --Monto en pesos sub97 Nov
DEFINE v_monto_pes_s92_n     DECIMAL(22,2);  --Monto en pesos sub92 Nov
DEFINE v_monto_pes_s97_i     DECIMAL(22,2);  --Monto en pesos sub97 Ini
DEFINE v_monto_pes_s92_i     DECIMAL(22,2);  --Monto en pesos sub92 Ini
DEFINE v_monto_pes_int_97    DECIMAL(22,2);  --Monto en pesos intereses 97
DEFINE v_monto_pes_int_92    DECIMAL(22,2);  --Monto en pesos intereses 92

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
  LET v_monto_pes_sc97      = 0; 
  LET v_monto_pes_sa97      = 0; 
  LET v_monto_pes_sc92      = 0; 
  LET v_monto_pes_sa92      = 0; 
  LET v_monto_aiv_sc97      = 0; 
  LET v_monto_aiv_sa97      = 0; 
  LET v_monto_aiv_sc92      = 0; 
  LET v_monto_aiv_sa92      = 0; 
  LET v_monto_aiv_s97       = 0; 
  LET v_monto_aiv_s92       = 0; 
  LET v_monto_pes_s97_n     = 0;
  LET v_monto_pes_s92_n     = 0;
  LET v_monto_pes_s97_i     = 0;
  LET v_monto_pes_s92_i     = 0;
  LET v_monto_pes_int_97    = 0;
  LET v_monto_pes_int_92    = 0;

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

  -- Extrae precio accion del dia 31/10/2014
  {SELECT precio_fondo
  INTO   v_precio_fondo_o14
  FROM   glo_valor_fondo
  WHERE  f_valuacion = '10312014'
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

  --- 79 CARGA ARCHIVO AJUSTE
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

    --Identifica Incremento al SSV
    IF (v_cod_transaccion_cnt = 86  OR
        v_cod_transaccion_cnt = 88) THEN
       --Abono
       SELECT SUM(monto_acciones)
       INTO   v_monto_aivs
       FROM   cta_movimiento
       WHERE  folio_liquida = p_folio_liquida
       AND    movimiento    = 721
       AND    subcuenta     = v_cod_subcta_cnt; 
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_aivs          = 0;
       END IF
    END IF
  
    --Identifica Disminución al SSV
    IF (v_cod_transaccion_cnt = 87  OR 
        v_cod_transaccion_cnt = 89) THEN
       --Cargo
       SELECT SUM(monto_acciones)
       INTO   v_monto_aivs
       FROM   cta_movimiento
       WHERE  folio_liquida = p_folio_liquida
       AND    movimiento    = 672
       AND    subcuenta     = v_cod_subcta_cnt; 
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_aivs          = 0;
       ELSE
         IF ( v_monto_aivs < 0 ) THEN
            LET v_monto_aivs = v_monto_aivs * (-1);
         END IF
       END IF
    END IF

    LET v_monto_pes_n   = v_monto_aivs * v_precio_fondo_o14; 
    LET v_monto_pes_i   = v_monto_aivs * v_precio_fondo_o12; 
    LET v_monto_pes_int = v_monto_pes_n - v_monto_pes_i;

    --Saldo Sub Vivienda 92
    IF v_cta_contable = '2504060001' THEN
       LET v_monto_pesos = v_monto_pes_n;
    END IF

    --Saldo Sub Vivienda 97
    IF v_cta_contable = '2504070001' THEN
       LET v_monto_pesos = v_monto_pes_n;
    END IF
      
    --Carga Inicial SAFRE
    IF v_cta_contable = '2504090001' THEN
       LET v_monto_pesos = v_monto_pes_i;
    END IF
      
    --Intereses Complementarios Sub Vivienda
    IF v_cta_contable = '5102020300' THEN
       LET v_monto_pesos = v_monto_pes_int;
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


