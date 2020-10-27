






create procedure "selefp".fn_ret_fnd_ah_cnt57(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidaci�n del proceso
                                     p_f_liquida          DATE,          --Fecha de liquidaci�n del proceso
                                     p_cod_proceso_cnt    SMALLINT,      --C�digo Proceso Contable
                                     p_cod_proceso        SMALLINT,      --C�digo Proceso
                                     p_transaccion        SMALLINT)      --C�digo Transaccion contable
RETURNING SMALLINT;

--�ltima modificaci�n 10042019
--Declaraci�n de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --C�digo transacci�n contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --C�digo subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --C�digo naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos
DEFINE v_tanto_adicional     DECIMAL(20,2);  --Monto Tanto Adicional
DEFINE v_monto_conexion      DECIMAL(20,2);  --Monto Conexi�n
DEFINE v_monto_restituir     DECIMAL(20,2);  --Monto Restituci�n
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento

  --Inicializaci�n de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_tanto_adicional     = 0;
  LET v_monto_conexion      = 0;
  LET v_movimiento          = 0;
	 
  --#######################################
  SELECT SUM(importe)
  INTO   v_monto_pesos
  FROM   safre_viv:cta_fondo72
  WHERE  folio_liquida = p_folio_liquida
  AND    f_liquida     = p_f_liquida
  AND    movimiento   IN (182,802,141);
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_pesos  = 0;
     LET v_monto_pesos1 = 0;
  ELSE
     LET v_monto_pesos1 = v_monto_pesos;
  END IF

  --Cuenta contable de Tanto Adicional
  SELECT SUM(importe)
  INTO   v_tanto_adicional
  FROM   safre_viv:cta_fondo72
  WHERE  folio_liquida = p_folio_liquida
  AND    f_liquida     = p_f_liquida
  AND    movimiento   IN(422,601);
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_tanto_adicional = 0;
  END IF

  FOREACH
    --Extrae informaci�n de la cuenta contable para el proceso que lo solicita
    SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
           cta_contable, cod_naturaleza_cta
    INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
           v_cta_contable, v_cod_naturaleza_cta
    FROM   safre_viv:cnt_regla_contable
--  WHERE  cod_transaccion_cnt  = p_transaccion
    WHERE  cta_contable        <> '0000000000'
    AND    cod_proceso          = p_cod_proceso
    AND    cod_proceso_cnt      = p_cod_proceso_cnt

    IF p_cod_proceso_cnt = 70 OR
       p_cod_proceso_cnt = 71 THEN
       --Cuenta contable de Tanto Adicional
       IF v_cta_contable = '2502010001' THEN
          LET v_monto_pesos = v_tanto_adicional;
       END IF

       --Cuenta contable 1502050005 RECURSOS EN TESOFE FONDO DE AHORRO
       IF v_cta_contable = '1502050005' THEN
          LET v_monto_pesos = v_monto_pesos + v_tanto_adicional;
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

       LET v_monto_pesos = v_monto_pesos1;
    END IF
  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


