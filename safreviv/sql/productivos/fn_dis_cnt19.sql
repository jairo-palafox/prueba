






create procedure "safreviv".fn_dis_cnt19(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                              p_f_liquida          DATE,          --Fecha de liquidación del proceso
                              p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                              p_cod_proceso        SMALLINT,      --Código Proceso
                              p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Declaración de variables
--Última modificación 10112016
DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_monto_pesos         DECIMAL(20,2);  --Monto en pesos
DEFINE v_monto_pesos1        DECIMAL(20,2);  --Monto en pesos1
DEFINE v_monto_pesos2        DECIMAL(20,2);  --Monto en pesos2
DEFINE v_monto_pesos3        DECIMAL(20,2);  --Monto en pesos3
DEFINE v_monto_pesos4        DECIMAL(20,2);  --Monto en pesos4
DEFINE v_monto_pesos5        DECIMAL(20,2);  --Monto en pesos5
DEFINE v_monto_pesos_pas     DECIMAL(20,2);  --Monto en pesos_pas
DEFINE v_monto_avance        DECIMAL(20,2);  --Monto en pesos avance de pagos
DEFINE v_monto_avance_apo    DECIMAL(20,2);  --Monto en pesos avance de pagos aportaciones
DEFINE v_monto_avance_amo    DECIMAL(20,2);  --Monto en pesos avance de pagos amortizaciones
DEFINE v_monto_pago_virtual  DECIMAL(20,2);  --Monto en pesos pago virtual
DEFINE v_monto_pago_real     DECIMAL(20,2);  --Monto en pesos pago real
DEFINE v_monto_pag_menor     DECIMAL(20,2);  --Monto en pesos apo pago menor avance
DEFINE v_monto_amo_menor     DECIMAL(20,2);  --Monto en pesos amo pago menor avance
DEFINE v_monto_menor         DECIMAL(20,2);  --Monto en pesos pago menor avance
DEFINE v_monto_pag_mayor     DECIMAL(20,2);  --Monto en pesos apo pago mayor avance
DEFINE v_monto_amo_mayor     DECIMAL(20,2);  --Monto en pesos amo pago mayor avance
DEFINE v_monto_mayor         DECIMAL(20,2);  --Monto en pesos pago mayor avance

DEFINE v_monto_pag_menor2    DECIMAL(20,2);  --Monto en pesos apo pago menor avance < 2
DEFINE v_monto_amo_menor2    DECIMAL(20,2);  --Monto en pesos amo pago menor avance < 2
DEFINE v_monto_fisca2        DECIMAL(20,2);  --Monto en pesos pago menor avance fiscal
DEFINE v_monto_pag_mayor2    DECIMAL(20,2);  --Monto en pesos apo pago mayor avance < 2
DEFINE v_monto_amo_mayor2    DECIMAL(20,2);  --Monto en pesos amo pago mayor avance < 2
DEFINE v_monto_resul2        DECIMAL(20,2);  --Monto en pesos pago mayor avance resul

DEFINE v_monto_pag_menor_pv  DECIMAL(20,2);  --Monto en pesos apo pago menor avance pago virtual
DEFINE v_monto_amo_menor_pv  DECIMAL(20,2);  --Monto en pesos amo pago menor avance pago virtual
DEFINE v_monto_menor_pv      DECIMAL(20,2);  --Monto en pesos pago menor avance pago virtual
DEFINE v_monto_pag_mayor_pv  DECIMAL(20,2);  --Monto en pesos apo pago mayor avance pago virtual
DEFINE v_monto_amo_mayor_pv  DECIMAL(20,2);  --Monto en pesos amo pago mayor avance pago virtual
DEFINE v_monto_mayor_pv      DECIMAL(20,2);  --Monto en pesos pago mayor avance pago virtual
DEFINE v_importe_reconocer   DECIMAL(20,2);  --Monto en pesos imprte a reconocer
DEFINE v_importe_quebranto   DECIMAL(20,2);  --Monto en pesos importe a quebrantar

DEFINE v_monto_dev_pagos     DECIMAL(20,2);  --Monto en pesos devolución de pagos
DEFINE v_monto_apo_subs      DECIMAL(20,2);  --Monto en pesos aportación subsecuente
DEFINE v_folio_avance        DECIMAL(9,0);   --Folio Avance de Pagos
DEFINE v_monto_sin_avance    DECIMAL(20,2);  --Monto en pesos pago sin avance

DEFINE v_monto_acl_ant1205   DECIMAL(20,2);  --Monto en pesos aclaratorio anterior 1205
DEFINE v_monto_amo_excedente DECIMAL(20,2);  --Monto en pesos amortizaciones excedentes

DEFINE v_monto_aj_cred_vig   DECIMAL(20,2);  --Monto en pesos amortizaciones ajuste cred vig

DEFINE v_monto_mjv_plus      DECIMAL(20,2);  --Monto en pesos mejoravit+

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_pesos1        = 0;
  LET v_monto_pesos2        = 0;
  LET v_monto_pesos3        = 0;
  LET v_monto_pesos4        = 0;
  LET v_monto_pesos5        = 0;
  LET v_monto_pesos_pas     = 0;
  LET v_monto_avance        = 0;
  LET v_monto_avance_apo    = 0;
  LET v_monto_avance_amo    = 0;
  LET v_monto_pago_virtual  = 0;
  LET v_monto_pago_real     = 0;
  LET v_monto_pag_menor     = 0;
  LET v_monto_amo_menor     = 0;
  LET v_monto_menor         = 0;
  LET v_monto_pag_mayor     = 0;
  LET v_monto_amo_mayor     = 0;
  LET v_monto_mayor         = 0;
  LET v_importe_reconocer   = 0;
  LET v_importe_quebranto   = 0;

  LET v_monto_pag_menor2    = 0;
  LET v_monto_amo_menor2    = 0;
  LET v_monto_fisca2        = 0;
  LET v_monto_pag_mayor2    = 0;
  LET v_monto_amo_mayor2    = 0;
  LET v_monto_resul2        = 0;

  LET v_monto_pag_menor_pv  = 0;
  LET v_monto_amo_menor_pv  = 0;
  LET v_monto_menor_pv      = 0;
  LET v_monto_pag_mayor_pv  = 0;
  LET v_monto_amo_mayor_pv  = 0;
  LET v_monto_mayor_pv      = 0;

  LET v_monto_sin_avance    = 0;
  LET v_monto_dev_pagos     = 0;
  LET v_monto_apo_subs      = 0;

  LET v_monto_acl_ant1205   = 0;
  LET v_monto_amo_excedente = 0;
  
  LET v_monto_aj_cred_vig   = 0;
  LET v_monto_mjv_plus      = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_dis_cnt19.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- Registro Contable Dispersión Normal
  IF p_transaccion = 0 THEN
     --Obtiene la suman del importe en pesos del Avance de Pagos
     --Cuentas Contables:
     --1504020001 Avance de Pagos (Abono)
     SELECT SUM(monto_apo_avance)
     INTO   v_monto_avance_apo
     FROM   dis_compensa_avance
     WHERE  folio_dis         = p_folio_liquida
     AND    edo_compensa_apo IN (0,1,2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_avance_apo = 0;
     END IF
     IF v_monto_avance_apo IS NULL THEN
        LET v_monto_avance_apo = 0;
     END IF

     SELECT SUM(monto_amo_avance)
     INTO   v_monto_avance_amo
     FROM   dis_compensa_avance
     WHERE  folio_dis         = p_folio_liquida
     AND    edo_compensa_amo IN (0,1,2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_avance_amo = 0;
     END IF
     IF v_monto_avance_amo IS NULL THEN
        LET v_monto_avance_amo = 0;
     END IF

     --LET v_monto_avance = v_monto_avance_apo + v_monto_avance_amo;

     SELECT SUM(monto_amo_avance)
     INTO   v_monto_pago_virtual
     FROM   dis_compensa_avance
     WHERE  folio_dis         = p_folio_liquida
     AND    edo_compensa_amo IN (3,4,5);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pago_virtual = 0;
     END IF
     IF v_monto_pago_virtual IS NULL THEN
        LET v_monto_pago_virtual = 0;
     END IF

     LET v_monto_avance = v_monto_avance_apo + v_monto_avance_amo + v_monto_pago_virtual;

     --1502190001 Cuenta Pago Virtual(Abono)
     {SELECT SUM(monto_amo_avance)
     INTO   v_monto_pago_virtual
     FROM   dis_compensa_avance
     WHERE  folio_dis         = p_folio_liquida
     AND    edo_compensa_amo IN (3,4,5);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pago_virtual = 0;
     END IF}

     --Obtiene la suma del importe en pesos del Pago sin Avance
     --Cuenta Contable 2403011600 Cuenta de Conexión
     --Recaudación ACV Amortización PAGO REAL (Abono)
     SELECT SUM(imp_ap_pat + imp_am_cre)
     INTO   v_monto_pago_real
     FROM   dis_interface_hs
     WHERE  folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pago_real    = 0;
     END IF
     IF v_monto_pago_real IS NULL THEN
        LET v_monto_pago_real = 0;
     END IF

     --Cuenta Contable 2403011600 Cuenta de Conexión
     --Recaudación ACV Amortización CARGO (PAGO MENOR AL AVANCE)
     ---APORTACIÓN
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_menor
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 1
     AND   (ava.monto_dif_apo          > 2
     OR     ava.monto_dif_apo          < -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_menor    = 0;
     END IF
     IF v_monto_pag_menor IS NULL THEN
        LET v_monto_pag_menor = 0;
     END IF

     ---AMORTIZACIÓN
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_menor
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 1
     AND   (ava.monto_dif_amo          > 2
     OR     ava.monto_dif_amo          < -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_menor    = 0;
     END IF
     IF v_monto_amo_menor IS NULL THEN
        LET v_monto_amo_menor = 0;
     END IF

     LET v_monto_menor = v_monto_pag_menor + v_monto_amo_menor;

     --Cuenta Contable 2403011600 Cuenta de Conexión
     --Recaudación ACV Amortización ABONO (PAGO MAYOR AL AVANCE)
     ---APORTACIÓN
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_mayor
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 2
     AND   (ava.monto_dif_apo          > 2
     OR     ava.monto_dif_apo          < -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_mayor    = 0;
     END IF
     IF v_monto_pag_mayor IS NULL THEN
        LET v_monto_pag_mayor = 0;
     END IF

     ---AMORTIZACIÓN
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_mayor
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 2
     AND   (ava.monto_dif_amo          > 2
     OR     ava.monto_dif_amo          < -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_mayor    = 0;
     END IF
     IF v_monto_amo_mayor IS NULL THEN
        LET v_monto_amo_mayor = 0;
     END IF

     LET v_monto_mayor = v_monto_pag_mayor + v_monto_amo_mayor;

     --Cuenta Contable 7104030200 EROGACIONES DE    
     --CARACTER FORTUITO - RESUL EJERCICIOS R CARGO (PAGO MENOR AL AVANCE)
     ---APORTACIÓN MONTO MENOR IGUAL A 2
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_menor2
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 1
     AND    (ava.monto_dif_apo        <= 2
     AND    ava.monto_dif_apo         >= -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_menor2   = 0;
     END IF
     IF v_monto_pag_menor2 IS NULL THEN
        LET v_monto_pag_menor2 = 0;
     END IF
  
     ---APORTACIÓN MONTO MENOR IGUAL A 2
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_menor2
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 1
     AND    (ava.monto_dif_amo        <= 2
     AND    ava.monto_dif_amo         >= -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_menor2    = 0;
     END IF
     IF v_monto_amo_menor2 IS NULL THEN
        LET v_monto_amo_menor2 = 0;
     END IF

     LET v_monto_resul2 = v_monto_pag_menor2 + v_monto_amo_menor2;

     --Cuenta Contable 6103010200 OTROS PRODUCTOS  
     --Y BENEFICIOS FISCALIZACION ABONO (PAGO MAYOR AL AVANCE)
     ---APORTACIÓN MONTO MAYOR IGUAL A 2
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_mayor2
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 2
     AND    (ava.monto_dif_apo        <= 2
     AND    ava.monto_dif_apo         >= -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_mayor2    = 0;
     END IF
     IF v_monto_pag_mayor2 IS NULL THEN
        LET v_monto_pag_mayor2 = 0;
     END IF

     ---AMORTIZACIÓN MONTO MAYOR IGUAL A 2
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_mayor2
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 2
     AND    (ava.monto_dif_amo        <= 2
     AND    ava.monto_dif_amo         >= -2);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_mayor2    = 0;
     END IF
     IF v_monto_amo_mayor2 IS NULL THEN
        LET v_monto_amo_mayor2 = 0;
     END IF

     LET v_monto_fisca2 = v_monto_pag_mayor2 + v_monto_amo_mayor2;

     --Obtiene la suma del importe en pesos del Mandato
     --Aportaciones Subsecuentes
     --Cuenta Contable 2504070004 Apoyo Infonavit
     --(Aportaciones Subsecuentes) (Abono)
     SELECT SUM(imp_ap_pat)
     INTO   v_monto_apo_subs
     FROM   safre_viv:dis_interface_ef
     WHERE  folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_apo_subs = 0;
     END IF
     IF v_monto_apo_subs IS NULL THEN
        LET v_monto_apo_subs = 0;
     END IF

     --Obtiene la suma del importe en pesos de la Devolución
     --de Pagos(Restitución)
     --Cuenta Contable 2504050001 Subcuenta de vivienda
     --a restituir (Abono)
     SELECT SUM(monto_aportacion)
     INTO   v_monto_dev_pagos
     FROM   safre_viv:dse_devolucion
     WHERE  folio_referencia = p_folio_liquida
     AND    subcuenta       IN (SELECT cod_subcta_cnt
                                FROM   safre_viv:cnt_regla_contable
                                WHERE  cta_contable    <> '0000000000'
                                AND    cod_proceso      = p_cod_proceso
                                AND    cod_proceso_cnt  = p_cod_proceso_cnt);
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_dev_pagos = 0;
     END IF
     IF v_monto_dev_pagos IS NULL THEN
        LET v_monto_dev_pagos = 0;
     END IF

     --Cuenta Contable 1504020003 
     --Importes a Quebrantar por Dispersión 
     --CARGO (PAGO VIRTUAL MENOR AL AVANCE)
     ---APORTACIÓN
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_menor_pv
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 4;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_menor_pv = 0;
     END IF
     IF v_monto_pag_menor_pv IS NULL THEN
        LET v_monto_pag_menor_pv = 0;
     END IF

     ---AMORTIZACIÓN
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_menor_pv
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 4;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_menor_pv = 0;
     END IF
     IF v_monto_amo_menor_pv IS NULL THEN
        LET v_monto_amo_menor_pv = 0;
     END IF

     LET v_importe_quebranto = v_monto_pag_menor_pv + v_monto_amo_menor_pv;

     --Cuenta Contable 2508010002 
     --Importes a Reconocer como Ingresos por Pagos Virtuales
     -- ABONO (PAGO VIRTUAL MAYOR AL AVANCE)
     ---APORTACIÓN
     SELECT SUM(ava.monto_dif_apo)
     INTO   v_monto_pag_mayor_pv
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_apo      = 5;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_pag_mayor_pv = 0;
     END IF
     IF v_monto_pag_mayor_pv IS NULL THEN
        LET v_monto_pag_mayor_pv = 0;
     END IF

     ---AMORTIZACIÓN
     SELECT SUM(ava.monto_dif_amo)
     INTO   v_monto_amo_mayor_pv
     FROM   dis_det_avance_pago ava,
            dis_compensa_avance comp
     WHERE  ava.id_dis_det_avance_pago = comp.id_dis_det_avance_pago
     AND    comp.folio_dis             = p_folio_liquida
     AND    comp.edo_compensa_amo      = 5;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_mayor_pv = 0;
     END IF
     IF v_monto_amo_mayor_pv IS NULL THEN
        LET v_monto_amo_mayor_pv = 0;
     END IF

     LET v_importe_reconocer = v_monto_pag_mayor_pv + v_monto_amo_mayor_pv;

     --Obtiene la suma del importe en pesos de las Salidas de
     --Aclaratorio Anteriores 1205
     --Cuenta Contable 2504090001 Salidas Aclaratorio Anteriores
     --1205 (Entrada SACI) (Abono)
     SELECT SUM(monto_pesos)
     INTO   v_monto_acl_ant1205
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    movimiento    = 1202
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_acl_ant1205 = 0;
     END IF
     IF v_monto_acl_ant1205 IS NULL THEN
        LET v_monto_acl_ant1205 = 0;
     END IF

     --Obtiene la suma del importe en pesos de la Devolución 
     --de Amortizaciones Excedentes
     --Cuenta Contable 2203150006 Devolución Amortizaciones 
     --Excedentes (Abono)
     SELECT SUM(monto_pesos)
     INTO   v_monto_amo_excedente
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    movimiento    = 501
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_amo_excedente = 0;
     END IF
     IF v_monto_amo_excedente IS NULL THEN
        LET v_monto_amo_excedente = 0;
     END IF

     --Obtiene la suma del importe en pesos de los montos
     --de Mejoravit+
     --REGLA CONTABLE
     ----Transacción 111
     ------2206020003 Amortizaciones por Recibir (Cargo)
     ------2206030001 Cuenta Concentradora (Abono)
     ----Transacción 112
     ------2206030001 Cuenta Concentradora (Cargo)
     ------2403011600 Cuenta de Conexión Recaudación ACV Amortización (Abono)
     SELECT SUM(monto_pesos)
     INTO   v_monto_mjv_plus
     FROM   safre_viv:cta_movimiento
     WHERE  f_liquida     = p_f_liquida
     AND    movimiento   IN (1752,1762,1772,1782)
     AND    folio_liquida = p_folio_liquida;
     IF DBINFO('sqlca.sqlerrd2') == 0 THEN
        LET v_monto_mjv_plus = 0;
     END IF
     IF v_monto_mjv_plus IS NULL THEN
        LET v_monto_mjv_plus = 0;
     END IF

     FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
              cta_contable, cod_naturaleza_cta
       INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
              v_cta_contable, v_cod_naturaleza_cta
       FROM   safre_viv:cnt_regla_contable
       WHERE  cod_transaccion_cnt NOT IN (83,84)
       AND    cta_contable            <> '0000000000'
       AND    cod_proceso              = p_cod_proceso
       AND    cod_proceso_cnt          = p_cod_proceso_cnt

       -- Obtiene saldo de las subcuentas:
       -- Cuenta Contable 2504070001 SAR 97
       -- Cuenta Contable 2504080001 Solo Infonavit 97
       -- Cuenta Contable 2206030001 Concentradora
       -- (Amortización)
       -- (Cargo)
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida       = p_f_liquida
       AND    subcuenta       = v_cod_subcta_cnt
       AND    movimiento NOT IN (481,871)
       AND    folio_liquida   = p_folio_liquida;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       END IF

       -- Cuenta Contable Avance de pagos
       -- (Abono)
       IF v_cta_contable = '1504020001' THEN
          LET v_monto_pesos = v_monto_avance;
       END IF

       -- Cuenta Pago Virtual
       -- (Abono)
       {IF v_cta_contable = '1502190001' THEN
          LET v_monto_pesos = v_monto_pago_virtual;
       END IF}

       -- Cuenta Salidas Aclaratorio Anteriores
       -- 1205 (Entrada SACI)
       -- (Abono)
       IF v_cta_contable = '2504090001' THEN
          LET v_monto_pesos = v_monto_acl_ant1205;
       END IF

       IF v_cod_transaccion_cnt = 24 THEN
          --Cuenta Contable 2403011600 Cuenta de Conexión
          --Recaudación ACV Amortización
          IF v_cta_contable = '2403011600' THEN
             --PAGO REAL (Abono)
             IF v_cod_naturaleza_cta = 1 THEN
                LET v_monto_pesos = v_monto_pago_real;

                IF ( v_monto_pesos < 0 ) THEN
                   LET v_monto_pesos = v_monto_pesos * (-1);
                END IF

                IF v_monto_pesos >=  9000000000.00 THEN --OR
                   --v_monto_pesos <= -9000000000.00 THEN
                   LET v_monto_pesos  = 9000000000.00;
                   LET v_monto_pesos1 = v_monto_pesos - v_monto_pago_real;
                END IF

                IF ( v_monto_pesos1 < 0 ) THEN
                    LET v_monto_pesos1 = v_monto_pesos1 * (-1);
                END IF

                IF v_monto_pesos1 >=  9000000001.00 THEN --OR
                   --v_monto_pesos1 <= -9000000000.00 THEN
                   LET v_monto_pesos_pas = v_monto_pesos1;
                   LET v_monto_pesos1    = 9000000001.00;
                   LET v_monto_pesos2    = v_monto_pesos1 - v_monto_pesos_pas;
                END IF

                IF ( v_monto_pesos2 < 0 ) THEN
                    LET v_monto_pesos2 = v_monto_pesos2 * (-1);
                END IF

                IF v_monto_pesos2 >=  9000000002.00 THEN --OR
                   --v_monto_pesos2 <= -9000000000.00 THEN
                   LET v_monto_pesos_pas = v_monto_pesos2;
                   LET v_monto_pesos2    = 9000000002.00;
                   LET v_monto_pesos3    = v_monto_pesos2 - v_monto_pesos_pas;
                END IF

                IF ( v_monto_pesos3 < 0 ) THEN
                    LET v_monto_pesos3 = v_monto_pesos3 * (-1);
                END IF

                IF v_monto_pesos3 >=  9000000003.00 THEN --OR
                   --v_monto_pesos3 <= -9000000000.00 THEN
                   LET v_monto_pesos_pas = v_monto_pesos3;
                   LET v_monto_pesos3    = 9000000003.00;
                   LET v_monto_pesos4    = v_monto_pesos3 - v_monto_pesos_pas;
                END IF

                IF ( v_monto_pesos4 < 0 ) THEN
                    LET v_monto_pesos4 = v_monto_pesos4 * (-1);
                END IF

                IF v_monto_pesos4 >=  9000000004.00 THEN --OR
                   --v_monto_pesos4 <= -9000000000.00 THEN
                   LET v_monto_pesos_pas = v_monto_pesos4;
                   LET v_monto_pesos4    = 9000000004.00;
                   LET v_monto_pesos5    = v_monto_pesos4 - v_monto_pesos_pas;
                END IF
             END IF
          END IF

          --Cuenta Contable 2504070001 Vivienda 97
          --(Cargo)
          IF v_cta_contable = '2504070001' THEN
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

          --Cuenta Contable 2206030001 Cuenta Concentradora
          --(Cargo)
          IF v_cta_contable = '2206030001' THEN
             IF p_cod_proceso = 922 THEN
                SELECT SUM(monto_pesos)
                INTO   v_monto_pesos
                FROM   safre_viv:cta_movimiento
                WHERE  f_liquida      = p_f_liquida
                AND    subcuenta      = v_cod_subcta_cnt
                AND    movimiento    <> 871
                AND    folio_liquida  = p_folio_liquida;
                IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                   LET v_monto_pesos = 0;
                END IF
             ELSE
                SELECT SUM(monto_pesos)
                INTO   v_monto_pesos
                FROM   safre_viv:cta_movimiento
                WHERE  f_liquida       = p_f_liquida
                AND    subcuenta      IN (41,43)
                AND    movimiento NOT IN (481,871)
                AND    folio_liquida   = p_folio_liquida;
                IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                   LET v_monto_pesos = 0;
                END IF
             END IF

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
       END IF

       -- Cuenta Contable 2504080001 Solo Infonavit 97
       --(Abono)
       IF v_cta_contable = '2504080001' THEN 
          IF v_cod_naturaleza_cta = 1 THEN
             SELECT SUM(monto_pesos)
             INTO   v_monto_pesos
             FROM   safre_viv:cta_movimiento
             WHERE  f_liquida     = p_f_liquida
             AND    subcuenta     = v_cod_subcta_cnt
             AND    movimiento   IN (1502,1512)
             AND    folio_liquida = p_folio_liquida;
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF
       END IF

       IF v_cod_transaccion_cnt = 63 THEN
          --Cuenta Contable 2403011600 Cuenta de Conexión
          --Recaudación ACV Amortización
          IF v_cta_contable = '2403011600' THEN
             --PAGO MENOR AL AVANCE (Abono)
             IF v_cod_naturaleza_cta = 1 THEN
                LET v_monto_pesos = v_monto_mayor;
             END IF

             --PAGO MAYOR AL AVANCE (Cargo)
             IF v_cod_naturaleza_cta = 2 THEN
                LET v_monto_pesos = v_monto_menor;
             END IF
          END IF
       END IF


       --Cuenta Contable 7104030200 Erogaciones de 
       --caracter fortuito - Resul Ejercicios R
       --Cargo (Pago menor al avances)
       IF v_cta_contable = '7104030200' THEN
          LET v_monto_pesos = v_monto_resul2;
       END IF

       --Cuenta Contable 6103010200 Otros productos
       --y beneficios fiscalizacion
       --Abono (Pago mayor al avacnes)
       IF v_cta_contable = '6103010200' THEN
          LET v_monto_pesos = v_monto_fisca2;
       END IF
    
       --Cuenta Contable 2504070004 Apoyo Infonavit
       --(Aportaciones Subsecuentes) (Abono)
       IF v_cta_contable = '2504070004' THEN
          LET v_monto_pesos = v_monto_apo_subs;
       END IF

       -- Transacción 28 Liquidación de la Dispersión,
       -- se deberá registrar el importe de las
       -- aportaciones por las cuales se identifique
       -- que el crédito ya se encuentra liquidado.
       -- El importe de las amortizaciones deberá
       -- enviarse a Hipotecaria Social (HS)
       IF v_cod_transaccion_cnt = 28 THEN
          --Obtiene la suma del importe en pesos de la Devolución
          --de Pagos(Restitución)
          --Cuenta Contable 2504070001 Saldo de la Subcuenta
          --de Vivienda 97 (Cargo)
          --Cuenta Contable 2504080001 Subcuenta Solo Infonavit
          --97 (Cargo)
          SELECT SUM(monto_aportacion)
          INTO   v_monto_pesos
          FROM   safre_viv:dse_devolucion
          WHERE  folio_referencia = p_folio_liquida
          AND    subcuenta        = v_cod_subcta_cnt;

          --Cuenta Contable 2504050001 Subcuenta de vivienda
          --a restituir (Abono)
          IF v_cta_contable = '2504050001' THEN
             LET v_monto_pesos = v_monto_dev_pagos;
          END IF
       END IF

       -- Transacción 30 Liquidación de la Dispersión,
       -- se deberá registrar el importe de los
       -- servicios cobrados que serán entregados a
       -- los beneficiarios del pago
       IF v_cod_transaccion_cnt = 30 THEN
          --Obtiene la suma del importe en pesos de la Dispersión
          --Mandatos de Servicios
          --Cuenta Contable 2203060060 Dispersión de Predial
          --Tipo de Movimiento 312 (Abono)
          IF v_cta_contable = '2203060060' THEN
             SELECT SUM(monto_pesos)
             INTO   v_monto_pesos
             FROM   safre_viv:cta_movimiento
             WHERE  f_liquida     = p_f_liquida
             AND    movimiento    = 312
             AND    folio_liquida = p_folio_liquida;
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF

          --Cuenta Contable 2203060061 Dispersión de Cuota
          --Mantenimiento Tipo de Movimiento 332 (Abono)
          IF v_cta_contable = '2203060061' THEN
             SELECT SUM(monto_pesos)
             INTO   v_monto_pesos
             FROM   safre_viv:cta_movimiento
             WHERE  f_liquida     = p_f_liquida
             AND    movimiento    = 332
             AND    folio_liquida = p_folio_liquida;
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF

          --Cuenta Contable 22030600XX Luz (Abono)
          --Cuenta Contable 22030600XX Agua (Abono)
          --Tipo de Movimiento Servicios 322
          IF v_cta_contable = '22030600XX' THEN
             SELECT SUM(monto_pesos)
             INTO   v_monto_pesos
             FROM   safre_viv:cta_movimiento
             WHERE  f_liquida     = p_f_liquida
             AND    movimiento    = 322
             AND    folio_liquida = p_folio_liquida;
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF

          --Cuenta Contable 2206030001 Cuenta Concentradora
          --(Cargo)
          IF v_cta_contable = '2206030001' THEN
             SELECT SUM(monto_pesos)
             INTO   v_monto_pesos
             FROM   safre_viv:cta_movimiento
             WHERE  f_liquida     = p_f_liquida
             AND    subcuenta     = v_cod_subcta_cnt
             AND    movimiento   IN (SELECT movto_tpo_mandato
                                     FROM   mdt_tpo_mandato)
             AND    folio_liquida = p_folio_liquida;
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF
       END IF

       --Cuenta Contable Importes a Reconocer como Ingresos
       --por Pagos Virtuales
       --(Abono)
       IF v_cta_contable = '2508010002' THEN
          LET v_monto_pesos = v_importe_reconocer;
       END IF

       --Cuenta Contable Importes a Quebrantar por Dispersión
       --(Cargo)
       IF v_cta_contable = '1504020003' THEN
          LET v_monto_pesos = v_importe_quebranto;
       END IF

       {-- Transacción 31 Liquidación de la Dispersión,
       -- se deberá registrar el importe de las
       -- aportaciones cobradas que tengan asociadas
       -- el mandato de aportaciones subsecuentes
       IF v_cod_transaccion_cnt = 31 THEN
          --Cuenta Contable 2504070001 SAR 97
          --(Cargo)
          IF v_cta_contable = '2504070001' THEN
             SELECT SUM(imp_ap_pat)
             INTO   v_monto_pesos
             FROM   safre_viv:dis_interface_ef
             WHERE  folio_liquida = p_folio_liquida
             AND    id_derechohabiente IN (SELECT id_derechohabiente
                                           FROM   afi_derechohabiente
                                           WHERE  tipo_trabajador = 'I');
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF

          --Cuenta Contable 2504080001 Solo Infonavit 97
          --(Cargo)
          IF v_cta_contable = '2504080001' THEN
             SELECT SUM(imp_ap_pat)
             INTO   v_monto_pesos
             FROM   safre_viv:dis_interface_ef
             WHERE  folio_liquida = p_folio_liquida
             AND    id_derechohabiente IN (SELECT id_derechohabiente
                                           FROM   afi_derechohabiente
                                           WHERE  tipo_trabajador = 'S');
             IF DBINFO('sqlca.sqlerrd2') == 0 THEN
                LET v_monto_pesos = 0;
             END IF
          END IF
       END IF}

       IF v_cod_transaccion_cnt = 68 THEN
          -- Cuenta Contable 2206030001 Concentradora
          -- (Amortización)
          -- (Cargo)
          -- Cuenta Contable 2203150006 Devolución Amortización Excedente
          -- (Abono)
          LET v_monto_pesos = v_monto_amo_excedente;
       END IF

       IF v_cod_transaccion_cnt = 111 OR 
          v_cod_transaccion_cnt = 112 THEN
          --Transacción 111
          ----2206020003 Amortizaciones por Recibir (Cargo)
          ----2206030001 Cuenta Concentradora (Abono)
          --Transacción 112
          ----2206030001 Cuenta Concentradora (Cargo)
          ----2403011600 Cuenta de Conexión Recaudación ACV Amortización (Abono)
          LET v_monto_pesos = v_monto_mjv_plus;
       END IF
       
       -- si el monto es negativo, se obtiene el valor absoluto
       IF ( v_monto_pesos < 0 ) THEN
          LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       --Insertar las cuentas contables en la tabla de transacciones
       --TRACE 'Inserta en transaccion';
       IF v_monto_pesos > 0 THEN
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

       LET v_monto_pesos1    = 0;
       LET v_monto_pesos_pas = 0;

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

       LET v_monto_pesos2    = 0;
       LET v_monto_pesos_pas = 0;

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
  ELSE
     -- Registro Contable Dispersión Especial
     --Transacción 83-Reverso de Amortización no enviada a HS (100% de amortizaciones)
     --Transacción 84-Envío de Amortización a HS (importe correcto)
     -- si el monto es negativo, se obtiene el valor absoluto
     FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT id_cuenta_contable, cod_transaccion_cnt, cod_subcta_cnt,
              cta_contable, cod_naturaleza_cta
       INTO   v_id_cuenta_contable, v_cod_transaccion_cnt, v_cod_subcta_cnt,
              v_cta_contable, v_cod_naturaleza_cta
       FROM   safre_viv:cnt_regla_contable
       WHERE  cod_transaccion_cnt = p_transaccion
       AND    cta_contable       <> '0000000000'
       AND    cod_proceso         = p_cod_proceso
       AND    cod_proceso_cnt     = p_cod_proceso_cnt

       -- Obtiene saldo de las subcuentas:
       -- Cuenta Contable 2504070001 SAR 97
       -- Cuenta Contable 2504080001 Solo Infonavit 97
       -- Cuenta Contable 2206030001 Concentradora
       -- (Amortización)
       -- (Cargo)
       SELECT SUM(monto_pesos)
       INTO   v_monto_pesos
       FROM   safre_viv:cta_movimiento
       WHERE  f_liquida     = p_f_liquida
       AND    subcuenta     = v_cod_subcta_cnt
       AND    folio_liquida = p_folio_liquida
       AND    movimiento   <> 871;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_pesos = 0;
       END IF

       IF ( v_monto_pesos < 0 ) THEN
          LET v_monto_pesos = v_monto_pesos * (-1);
       END IF

       --Insertar las cuentas contables en la tabla de transacciones
       --TRACE 'Inserta en transaccion';
       IF v_monto_pesos > 0 THEN
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
  END IF

  RETURN v_bnd_proceso;

END PROCEDURE;


