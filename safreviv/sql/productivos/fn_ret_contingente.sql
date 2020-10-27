






create procedure "safreviv".fn_ret_contingente(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                    p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                    p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                    p_cod_proceso        SMALLINT,      --Código Proceso
                                    p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 17102018
--Declaración de variables
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos
DEFINE v_tanto_adicional     DECIMAL(20,2);  --Monto Tanto Adicional
DEFINE v_monto_conexion      DECIMAL(20,2);  --Monto Conexión
DEFINE v_monto_restituir     DECIMAL(20,2);  --Monto Restitución
DEFINE v_movimiento          SMALLINT;       --Tipo Movimiento
DEFINE v_carga_inicial       DECIMAL(20,2);  --Monto carga inicial

  --Inicialización de variables
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
  LET v_carga_inicial       = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_ret_contingente.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --- 46 RETIROS FONDO AHORRO CONTINGENTE
  --- 114 RESTITUCIÓN PAGOS VENCIDOS FONDO AHORRO CONT
  IF p_cod_proceso = 1515 OR
     p_cod_proceso = 1595 OR 
     p_cod_proceso = 1596 THEN
     --Obtiene la suman del importe en pesos para la referencia a la cta contable

     --Cuenta contable de Tanto Adicional
     SELECT SUM(importe)
     INTO   v_monto_restituir
     FROM   safre_viv:cta_fondo72
     WHERE  folio_liquida = p_folio_liquida
     AND    f_liquida     = p_f_liquida
     AND    movimiento   IN (141);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_restituir = 0;
     END IF

     IF v_monto_restituir > 0 THEN
        IF p_cod_proceso <> 1596 THEN
           --p_transaccion = 60 (Transacción Cancelación de DAP´s No Cobrados - Restitución)
           LET p_transaccion = 60;
        END IF

        SELECT SUM(importe)
        INTO   v_monto_pesos
        FROM   safre_viv:cta_fondo72
        WHERE  folio_liquida = p_folio_liquida
        AND    f_liquida     = p_f_liquida
        AND    movimiento   IN (141);
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_monto_pesos  = 0;
           LET v_monto_pesos1 = 0;
        ELSE
           LET v_monto_pesos1 = v_monto_pesos;
        END IF
 
        --Cuenta contable de Tanto Adicional
        {SELECT SUM(importe)
        INTO   v_tanto_adicional
        FROM   safre_viv:cta_fondo72
        WHERE  folio_liquida = p_folio_liquida
        AND    f_liquida     = p_f_liquida
        AND    movimiento    = 601;
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_tanto_adicional = 0;
        END IF}
        LET v_tanto_adicional = 0;
   
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

          --Cuenta contable de Devolución de Fondo Ahorro 1972-1992
          IF v_cta_contable = '2501070002' THEN
             LET v_monto_pesos = v_monto_restituir;
          END IF

          --Cuenta contable de Tanto Adicional
          IF v_cta_contable = '2502010001' THEN
             LET v_monto_pesos = v_tanto_adicional;
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
     ELSE
        -- 113 FONDO AHORRO TRASPASOS CONT
        --p_transaccion = 3 (Transacción Liquidación Retiro Fondo anterior 72 - 92)
        IF p_transaccion = 3 THEN
           LET v_movimiento = 182;
        END IF

        SELECT SUM(importe)
        INTO   v_monto_pesos
        FROM   safre_viv:cta_fondo72
        WHERE  folio_liquida = p_folio_liquida
        AND    f_liquida     = p_f_liquida
        AND    movimiento   IN (182,802);
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
        AND    movimiento    = 422;
        IF DBINFO('sqlca.sqlerrd2') == 0 THEN
           LET v_tanto_adicional = 0;
        END IF
  
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

          --Cuenta contable de Tanto Adicional
          IF v_cta_contable = '2502010001' THEN
             LET v_monto_pesos = v_tanto_adicional;
          END IF
  
          --Cuenta contable de Devolución de Fondo Ahorro 1972-1992
          IF v_cta_contable = '2504070005' THEN
             LET v_monto_pesos = v_monto_pesos + v_tanto_adicional;
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
        -- 113 FONDO AHORRO TRASPASOS CONT
     END IF 
  END IF
  --- 46 RETIROS FONDO AHORRO CONTINGENTE
  --- 114 RESTITUCIÓN PAGOS VENCIDOS FONDO AHORRO CONT

  --- 47 RETIROS LEY 73 CONTINGENTE
  IF p_cod_proceso = 1516 THEN
     SELECT SUM(monto_pesos)
     INTO   v_monto_conexion
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida       = p_f_liquida
     AND    folio_liquida   = p_folio_liquida
     AND    subcuenta      IN (SELECT cod_subcta_cnt
                               FROM   safre_viv:cnt_regla_contable)
     AND    movimiento NOT IN (1671);
     --AND    movimiento NOT IN (1671,911,1662); --RISS
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_restituir = 0;
     END IF

     --Obtiene la suman del monto en pesos para la referencia a la cta contable
     --Obtiene saldo de la Cuenta Contable 2504090001 Carga Inicial
     --Afores (Cargo)
     SELECT SUM(monto_pesos)
     INTO   v_carga_inicial
     FROM   safre_viv:cta_movimiento
     WHERE  folio_liquida = p_folio_liquida
     AND    movimiento   IN (1671)
     AND    f_liquida     = p_f_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_carga_inicial = 0;
     END IF

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
       WHERE  folio_liquida   = p_folio_liquida
       AND    subcuenta       = v_cod_subcta_cnt
       --AND    movimiento NOT IN (1671,911,1662) --RISS
       AND    movimiento NOT IN (1671)
       AND    f_liquida       = p_f_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos  = 0;
       END IF

       -- Cuenta Contable Subcuenta Devoluciones de SSV Ley 73
       -- (Cargo)
       IF v_cta_contable = '2504070005' THEN
          LET v_monto_pesos = v_monto_conexion;
       END IF

       -- Cuenta Contable Carga Inicial
       -- (Cargo)
       IF v_cta_contable = '2504090001' THEN
          LET v_monto_pesos = v_carga_inicial;
       END IF

       -- Obtiene variación menor retiros de Vivienda 97 y 92-97 EN AFORE
       -- (Abono)
       IF (v_cta_contable       = '2504060001'  OR
           v_cta_contable       = '2504070001') AND
           v_cod_naturaleza_cta = 1             THEN
           SELECT SUM(monto_pesos)
           INTO   v_monto_pesos
           FROM   safre_viv:cta_movimiento
           WHERE  folio_liquida   = p_folio_liquida
           AND    subcuenta       = v_cod_subcta_cnt
           AND    movimiento      = 1671
           AND    f_liquida       = p_f_liquida;
           IF DBINFO('sqlca.sqlerrd2') == 0 THEN
              LET v_monto_pesos = 0;
           END IF
       END IF

       -- RISS
       -- Cuenta Contable VIVIENDA VOL RISS
       -- (Cargo)
       {IF v_cta_contable = '2505010002' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (1662)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    
       -- Cuenta Contable Vivienda 97
       -- (Abono)
       IF v_cta_contable = '2504070001' AND v_cod_transaccion_cnt = 0 AND v_cod_naturaleza_cta = 1 THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (911)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF}

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

     --Obtiene la suman del monto en pesos para la referencia a la cta contable
     {FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
              cta_contable, cod_naturaleza_cta
       INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
              v_cta_contable, v_cod_naturaleza_cta
       FROM   safre_viv:cnt_regla_contable
       WHERE  cod_transaccion_cnt  = 0
       AND    cta_contable       <> '0000000000'
       AND    cod_proceso         = p_cod_proceso
       AND    cod_proceso_cnt     = p_cod_proceso_cnt

       -- RISS
       -- Cuenta Contable VIVIENDA VOL RISS
       -- (Cargo)
       IF v_cta_contable = '2505010002' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (1662)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    
       -- Cuenta Contable Vivienda 97
       -- (Abono)
       IF v_cta_contable = '2504070001' AND v_cod_transaccion_cnt = 0 AND v_cod_naturaleza_cta = 1 THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (911)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
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
     END FOREACH;}     
  END IF
  --- 47 RETIROS LEY 73 CONTINGENTE

  --- 48 RETIROS SOLO INFONAVIT CONTINGENTE
  IF p_cod_proceso = 1517 THEN
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
       WHERE  f_liquida       = p_f_liquida
       AND    subcuenta       = v_cod_subcta_cnt
       AND    movimiento NOT IN (1831,2062)
       AND    folio_liquida   = p_folio_liquida;
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

     --Obtiene la suman del monto en pesos para la referencia a la cta contable
     --Liquidación Traspaso Saldos Vivienda 97 a Vivienda 97 Solo INFONAVIT
     LET p_transaccion = 133;
     
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

       --Traspaso Saldos Vivienda 97 a Vivienda 97 Solo INFONAVIT
       -- Cuenta Contable Vivienda 97
       -- (Cargo)
       IF v_cta_contable = '2504070001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (2062)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
       END IF
    
       -- Cuenta Contable Vivienda 97 Solo INFONAVIT
       -- (Abono)
       IF v_cta_contable = '2504080001' THEN
          SELECT SUM(monto_pesos)
          INTO   v_monto_pesos
          FROM   safre_viv:cta_movimiento
          WHERE  f_liquida     = p_f_liquida
          AND    subcuenta     = v_cod_subcta_cnt
          AND    movimiento   IN (1831)
          AND    folio_liquida = p_folio_liquida;
          IF DBINFO('sqlca.sqlerrd2') == 0 THEN
             LET v_monto_pesos  = 0;
          END IF
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
  --- 48 RETIROS SOLO INFONAVIT CONTINGENTE

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información

  RETURN v_bnd_proceso;

END PROCEDURE;


