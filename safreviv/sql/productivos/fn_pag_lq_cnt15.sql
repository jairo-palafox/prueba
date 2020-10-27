






create procedure "safreviv".fn_pag_lq_cnt15(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                 p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                 p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                 p_cod_proceso        SMALLINT,      --Código Proceso
                                 p_transaccion        SMALLINT)      --Código Transacción contable
RETURNING SMALLINT;

--Última modificación 10112016
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
DEFINE v_monto_sin_int_acl   DECIMAL(20,2);  --Monto en pesos SIN INT ACL
DEFINE v_monto_sin_amo_acl   DECIMAL(20,2);  --Monto en pesos SIN AMO ACL
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos1
DEFINE v_monto_riss_acl      DECIMAL(20,2);  --Monto en pesos RISS ACL
DEFINE v_cta_concentradora   DECIMAL(20,2);  --Monto cuenta concentradora

DEFINE v_monto_pesos2        DECIMAL(20,2);  --Monto en pesos2
DEFINE v_monto_pesos3        DECIMAL(20,2);  --Monto en pesos3
DEFINE v_monto_pesos4        DECIMAL(20,2);  --Monto en pesos4
DEFINE v_monto_pesos5        DECIMAL(20,2);  --Monto en pesos5
DEFINE v_monto_pesos_pas     DECIMAL(20,2);  --Monto en pesos_pas

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
  LET v_monto_sin_int_acl   = 0;
  LET v_monto_sin_amo_acl   = 0;
  LET v_monto_pesos1        = 0;
  LET v_monto_riss_acl      = 0;
  LET v_cta_concentradora   = 0;

  LET v_monto_pesos2        = 0;
  LET v_monto_pesos3        = 0;
  LET v_monto_pesos4        = 0;
  LET v_monto_pesos5        = 0;
  LET v_monto_pesos_pas     = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_pag_lq_cnt15.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Transaccion Contable '||p_transaccion;

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2206020002 Aportaciones por recibir
  --Afores (Cargo)
  SELECT SUM(imp_ap_pat)
  INTO   v_aportaciones
  FROM   cta_his_pagos
  WHERE  folio            = p_folio_liquida;
  --AND    ind_liquidacion <> 1;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_aportaciones = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 5102020300 Intereses complementarios
  --Subcta de Vivienda 
  --Afores (Cargo)
  SELECT SUM(int_gen_pgo_ext)
  INTO   v_intereses
  FROM   cta_his_pagos
  WHERE  folio          = p_folio_liquida;
  --AND    ind_liquidacion <> 1;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_intereses = 0;
  END IF

  -- Cuenta Contable 2504070002
  -- Saldo de la Subcuenta de
  -- Vivienda 97 (Aclaratorio)
  -- (Abono)
  -- ind_liquidacion = 1 Apo/Amo
  -- sin liquidar
  SELECT SUM(a.imp_ap_pat + a.int_gen_pgo_ext)
  INTO   v_monto_sin_int_acl 
  FROM   cta_his_pagos a
  WHERE  a.folio           = p_folio_liquida
  --AND    a.nrp        NOT IN ('B0799994105')
  AND    a.nrp        NOT IN (SELECT b.nrp
                              FROM   cat_riss_nrp b
                              WHERE  b.id_nrp IN (0,2))
  AND    a.ind_liquidacion = 1;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_sin_int_acl  = 0;
  END IF

  -- Cuenta Contable 2505010003
  -- Saldo de la Subcuenta   
  -- EFIRISS (Aclaratorio) 
  -- (Abono)
  -- ind_liquidacion = 1 Apo/Amo
  -- sin liquidar
  SELECT SUM(a.imp_ap_pat + a.int_gen_pgo_ext)
  INTO   v_monto_riss_acl 
  FROM   cta_his_pagos a
  WHERE  a.folio           = p_folio_liquida
  --AND    nrp            IN ('B0799994105')
  AND    a.nrp            IN (SELECT b.nrp
                              FROM   cat_riss_nrp b
                              WHERE  b.id_nrp IN (0,2))
  AND    a.ind_liquidacion = 1;
  IF DBINFO('sqlca.sqlerrd2') == 0    OR 
     v_monto_riss_acl         IS NULL THEN
     LET v_monto_riss_acl  = 0;
  END IF

  -- Cuenta Contable 2206030002
  -- Saldo de la Subcuenta de
  -- Cuenta Concentradora - 
  -- Amortización (Aclaratorio)
  -- (Abono)
  -- ind_liquidacion = 1 Apo/Amo
  -- sin liquidar
  SELECT SUM(imp_am_cre)
  INTO   v_monto_sin_amo_acl 
  FROM   cta_his_pagos
  WHERE  folio           = p_folio_liquida
  AND    ind_liquidacion = 1;
  IF DBINFO('sqlca.sqlerrd2') == 0    OR
     v_monto_sin_amo_acl      IS NULL THEN
     LET v_monto_sin_amo_acl  = 0;
  END IF

  --Obtiene saldo de la subcuenta Cuenta Concentradora (Amortización)
  --Obtiene saldo de la Cuenta Contable 2206030001 Cuenta Concentradora
  -- (Abono)
  SELECT SUM(monto_pesos)
  INTO   v_cta_concentradora
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = p_f_liquida
  AND    subcuenta    IN (41,43)
  AND    movimiento   IN (1,3,4,41,63,83,451,681,691,54,103,64,123,143,711,
                          133,701,183,193,761,771)
  AND    folio_liquida = p_folio_liquida;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_cta_concentradora = 0;
  END IF

  --Obtiene la suman del monto en pesos para la referencia a la cta contable
  --Obtiene saldo de la Cuenta Contable 2206020003 Amortizaciones por recibir
  --Afores (Cargo)
  LET v_amortizaciones = v_cta_concentradora + v_monto_sin_amo_acl;

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

    -- Obtiene saldo de las subcuentas de Vivienda 97, 
    -- Vivienda 97 Solo Infonavit,
    -- Vivienda Vol RISS y Portabilidad
    -- (Abono)
    SELECT SUM(monto_pesos)
    INTO   v_monto_pesos
    FROM   safre_viv:cta_movimiento
    WHERE  f_liquida     = p_f_liquida
    AND    subcuenta     = v_cod_subcta_cnt
    AND    movimiento   IN (1,3,4,41,63,83,451,681,691,54,103,64,123,143,711,
                            133,701,183,193,761,771,1001)
    AND    folio_liquida = p_folio_liquida;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_pesos = 0;
    END IF

    --Obtiene saldo de la subcuenta de Vivienda 97 (Aportación)
    -- (Abono)
    IF v_cta_contable = '2504070001' THEN
       {IF v_monto_pesos >=  9000000000.00 OR
          v_monto_pesos <= -9000000000.00 THEN
          LET v_monto_pesos1 = 9000000000.00 - v_monto_pesos;
          LET v_monto_pesos  = 9000000000.00;
       END IF}

       IF ( v_monto_pesos < 0 ) THEN
           LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       LET v_monto_pesos_pas = v_monto_pesos;

       IF v_monto_pesos >=  9000000000.00 THEN --OR
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos1 < 0 ) THEN
           LET v_monto_pesos1 = v_monto_pesos1 * (-1);
       END IF

       IF v_monto_pesos1 >=  9000000001.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos1;
          LET v_monto_pesos1    = 9000000001.00;
          LET v_monto_pesos2    = v_monto_pesos1 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos2 < 0 ) THEN
           LET v_monto_pesos2 = v_monto_pesos2 * (-1);
       END IF

       IF v_monto_pesos2 >=  9000000002.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos2;
          LET v_monto_pesos2    = 9000000002.00;
          LET v_monto_pesos3    = v_monto_pesos2 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos3 < 0 ) THEN
           LET v_monto_pesos3 = v_monto_pesos3 * (-1);
       END IF

       IF v_monto_pesos3 >=  9000000003.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos3;
          LET v_monto_pesos3    = 9000000003.00;
          LET v_monto_pesos4    = v_monto_pesos3 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos4 < 0 ) THEN
           LET v_monto_pesos4 = v_monto_pesos4 * (-1);
       END IF

       IF v_monto_pesos4 >=  9000000004.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos4;
          LET v_monto_pesos4    = 9000000004.00;
          LET v_monto_pesos5    = v_monto_pesos4 - v_monto_pesos_pas;
       END IF
    END IF

    --Obtiene saldo de la subcuenta Cuenta Concentradora (Amortización)
    -- (Abono)
    IF v_cta_contable = '2206030001' THEN
       LET v_monto_pesos = v_cta_concentradora;

       {IF v_monto_pesos >=  9000000000.00 OR
          v_monto_pesos <= -9000000000.00 THEN
          LET v_monto_pesos1 = 9000000000.00 - v_monto_pesos;
          LET v_monto_pesos  = 9000000000.00;
       END IF}

       IF ( v_monto_pesos < 0 ) THEN
           LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       LET v_monto_pesos_pas = v_monto_pesos;

       IF v_monto_pesos >=  9000000000.00 THEN --OR
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos1 < 0 ) THEN
           LET v_monto_pesos1 = v_monto_pesos1 * (-1);
       END IF

       IF v_monto_pesos1 >=  9000000001.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos1;
          LET v_monto_pesos1    = 9000000001.00;
          LET v_monto_pesos2    = v_monto_pesos1 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos2 < 0 ) THEN
           LET v_monto_pesos2 = v_monto_pesos2 * (-1);
       END IF

       IF v_monto_pesos2 >=  9000000002.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos2;
          LET v_monto_pesos2    = 9000000002.00;
          LET v_monto_pesos3    = v_monto_pesos2 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos3 < 0 ) THEN
           LET v_monto_pesos3 = v_monto_pesos3 * (-1);
       END IF

       IF v_monto_pesos3 >=  9000000003.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos3;
          LET v_monto_pesos3    = 9000000003.00;
          LET v_monto_pesos4    = v_monto_pesos3 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos4 < 0 ) THEN
           LET v_monto_pesos4 = v_monto_pesos4 * (-1);
       END IF

       IF v_monto_pesos4 >=  9000000004.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos4;
          LET v_monto_pesos4    = 9000000004.00;
          LET v_monto_pesos5    = v_monto_pesos4 - v_monto_pesos_pas;
       END IF
    END IF

    --Obtiene saldo de la subcuenta Vivienda 97 Solo Infonavit
    -- (Abono)
    IF v_cta_contable = '2504080001' THEN
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    movimiento   IN(41,63,83,451,681)
       AND    folio_liquida = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       END IF
    END IF

    -- Cuenta Contable Aportaciones por recibir     
    -- (Cargo)
    IF v_cta_contable = '2206020002' THEN
       LET v_monto_pesos = v_aportaciones;
       
       {IF v_monto_pesos >=  9000000000.00 OR
          v_monto_pesos <= -9000000000.00 THEN
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_aportaciones;          
       END IF}

       IF ( v_monto_pesos < 0 ) THEN
           LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       LET v_monto_pesos_pas = v_monto_pesos;

       IF v_monto_pesos >=  9000000000.00 THEN --OR
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos1 < 0 ) THEN
           LET v_monto_pesos1 = v_monto_pesos1 * (-1);
       END IF

       IF v_monto_pesos1 >=  9000000001.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos1;
          LET v_monto_pesos1    = 9000000001.00;
          LET v_monto_pesos2    = v_monto_pesos1 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos2 < 0 ) THEN
           LET v_monto_pesos2 = v_monto_pesos2 * (-1);
       END IF

       IF v_monto_pesos2 >=  9000000002.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos2;
          LET v_monto_pesos2    = 9000000002.00;
          LET v_monto_pesos3    = v_monto_pesos2 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos3 < 0 ) THEN
           LET v_monto_pesos3 = v_monto_pesos3 * (-1);
       END IF

       IF v_monto_pesos3 >=  9000000003.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos3;
          LET v_monto_pesos3    = 9000000003.00;
          LET v_monto_pesos4    = v_monto_pesos3 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos4 < 0 ) THEN
           LET v_monto_pesos4 = v_monto_pesos4 * (-1);
       END IF

       IF v_monto_pesos4 >=  9000000004.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos4;
          LET v_monto_pesos4    = 9000000004.00;
          LET v_monto_pesos5    = v_monto_pesos4 - v_monto_pesos_pas;
       END IF
    END IF

    -- Cuenta Contable Aportaciones por recibir     
    -- (Cargo)
    IF v_cta_contable = '2206020003' THEN
       LET v_monto_pesos = v_amortizaciones;

       {IF v_monto_pesos >=  9000000000.00 OR
          v_monto_pesos <= -9000000000.00 THEN
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_amortizaciones;
       END IF}

       IF ( v_monto_pesos < 0 ) THEN
           LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       LET v_monto_pesos_pas = v_monto_pesos;

       IF v_monto_pesos >=  9000000000.00 THEN --OR
          LET v_monto_pesos  = 9000000000.00;
          LET v_monto_pesos1 = v_monto_pesos - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos1 < 0 ) THEN
           LET v_monto_pesos1 = v_monto_pesos1 * (-1);
       END IF

       IF v_monto_pesos1 >=  9000000001.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos1;
          LET v_monto_pesos1    = 9000000001.00;
          LET v_monto_pesos2    = v_monto_pesos1 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos2 < 0 ) THEN
           LET v_monto_pesos2 = v_monto_pesos2 * (-1);
       END IF

       IF v_monto_pesos2 >=  9000000002.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos2;
          LET v_monto_pesos2    = 9000000002.00;
          LET v_monto_pesos3    = v_monto_pesos2 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos3 < 0 ) THEN
           LET v_monto_pesos3 = v_monto_pesos3 * (-1);
       END IF

       IF v_monto_pesos3 >=  9000000003.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos3;
          LET v_monto_pesos3    = 9000000003.00;
          LET v_monto_pesos4    = v_monto_pesos3 - v_monto_pesos_pas;
       END IF

       IF ( v_monto_pesos4 < 0 ) THEN
           LET v_monto_pesos4 = v_monto_pesos4 * (-1);
       END IF

       IF v_monto_pesos4 >=  9000000004.00 THEN --OR
          LET v_monto_pesos_pas = v_monto_pesos4;
          LET v_monto_pesos4    = 9000000004.00;
          LET v_monto_pesos5    = v_monto_pesos4 - v_monto_pesos_pas;
       END IF       
    END IF

    -- Cuenta Contable Intereses Complementarios Subcta de Vivienda
    -- (Cargo)
    IF v_cta_contable = '5102020300' THEN
       LET v_monto_pesos = v_intereses;
    END IF

    -- Cuenta Contable Saldo de la Subcuenta de Vivienda 97
    -- (Aclaratorio)
    -- (Abono)
    IF v_cta_contable = '2504070002' THEN
       LET v_monto_pesos = v_monto_sin_int_acl;
    END IF

    -- Cuenta Contable Saldo de la Subcuenta EFIRISS
    -- (Aclaratorio)
    -- (Abono)
    IF v_cta_contable = '2505010003' THEN
       LET v_monto_pesos = v_monto_riss_acl;
    END IF
    
    -- Cuenta Contable Cuenta Concentradora - Amortización 
    -- (Aclaratorio)
    -- (Abono)
    IF v_cta_contable = '2206030002' THEN
       LET v_monto_pesos = v_monto_sin_amo_acl;
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

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos1 < 0 ) THEN
       LET v_monto_pesos1 = v_monto_pesos1 * (-1);
    END IF

    IF v_monto_pesos1 > 0 THEN
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
                                                    v_monto_pesos1,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos1 = 0;

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos2 < 0 ) THEN
       LET v_monto_pesos2 = v_monto_pesos2 * (-1);
    END IF

    IF v_monto_pesos2 > 0 THEN
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
                                                    v_monto_pesos2,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos2 = 0;

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos3 < 0 ) THEN
       LET v_monto_pesos3 = v_monto_pesos3 * (-1);
    END IF

    IF v_monto_pesos3 > 0 THEN
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
                                                    v_monto_pesos3,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos3 = 0;

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos4 < 0 ) THEN
       LET v_monto_pesos4 = v_monto_pesos4 * (-1);
    END IF

    IF v_monto_pesos4 > 0 THEN
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
                                                    v_monto_pesos4,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos4 = 0;

    -- si el monto es negativo, se obtiene el valor absoluto
    IF ( v_monto_pesos5 < 0 ) THEN
       LET v_monto_pesos5 = v_monto_pesos5 * (-1);
    END IF

    IF v_monto_pesos5 > 0 THEN
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
                                                    v_monto_pesos5,
                                                    p_f_liquida,
                                                    TODAY,
                                                    0,   -- 0>Registro Contable, 1>Reverso
                                                    10);
    END IF

    LET v_monto_pesos5 = 0;

  END FOREACH;

  RETURN v_bnd_proceso;

END PROCEDURE;


