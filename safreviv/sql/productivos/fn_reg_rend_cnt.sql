






create procedure "safreviv".fn_reg_rend_cnt(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                 p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                 p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                 p_cod_proceso        SMALLINT,      --Código Proceso
                                 p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 29122017
--Declaración de variables
  DEFINE v_id_cuenta_contable  SMALLINT;       --Id cuenta contable
  DEFINE v_cod_transaccion_cnt SMALLINT;       --Código transacción contable
  DEFINE v_cod_subcta_cnt      SMALLINT;       --Código subcuenta
  DEFINE v_cta_contable        CHAR(10);       --Cuenta contable
  DEFINE v_cod_naturaleza_cta  SMALLINT;       --Código naturaleza cuenta contable
  DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
  DEFINE v_monto_acciones_ant  DECIMAL(22,2);  --Monto en acciones día anterior
  DEFINE v_monto_acciones_ant0 DECIMAL(22,2);  --Monto en acciones día anterior no consistentes
  DEFINE v_monto_acciones_ant1 DECIMAL(22,2);  --Monto en acciones dia anterior consistentes
  DEFINE v_monto_acciones_hoy  DECIMAL(22,2);  --Monto en acciones día
  DEFINE v_monto_acciones_hoy0 DECIMAL(22,2);  --Monto en acciones día no consistentes
  DEFINE v_monto_acciones_hoy1 DECIMAL(22,2);  --Monto en acciones dia consistentes
  DEFINE v_monto_acciones_post DECIMAL(22,2);  --Monto en acciones día posterior
  DEFINE v_monto_acciones_cort DECIMAL(20,6);  --Monto en acciones corte
  DEFINE v_monto_acciones_cor0 DECIMAL(20,6);  --Monto en acciones corte no consistentes
  DEFINE v_monto_acciones_cor1 DECIMAL(22,2);  --Monto en acciones corte consistentes
  DEFINE v_monto_acciones      DECIMAL(22,2);  --Monto en acciones
  DEFINE v_monto_acciones0     DECIMAL(22,2);  --Monto en acciones no consistentes
  DEFINE v_monto_acciones1     DECIMAL(22,2);  --Monto en acciones consistentes
  DEFINE v_monto_pesos         DECIMAL(22,2);  --Monto en pesos a registrar
  DEFINE v_poliza_inversa      SMALLINT;       --Indicador poliza inversa
  DEFINE v_precio_fondo_hoy    DECIMAL(19,14); --Precio Fondo del día
  DEFINE v_precio_fondo_ant    DECIMAL(19,14); --Precio Fondo del día anterior
  DEFINE v_cantidad_basica     DECIMAL(22,2);  --Monto en acciones cantidad basica

  DEFINE v_monto_pesos_hoy     DECIMAL(22,2);  --Monto en pesos día
  DEFINE v_monto_pesos_ant     DECIMAL(22,2);  --Monto en pesos día anterior
  DEFINE v_monto_pesos_hoy0    DECIMAL(22,2);  --Monto en pesos día no consistentes
  DEFINE v_monto_pesos_ant0    DECIMAL(22,2);  --Monto en pesos día anterior no consistentes
  DEFINE v_monto_pesos0        DECIMAL(22,2);  --Monto en pesos no consistentes
  DEFINE v_monto_pesos_cons    DECIMAL(22,2);  --Monto en pesos consistentes

  DEFINE v_tot_cambio_nss      SMALLINT;       --Total de Registros con cambio de NSS apo vol
  DEFINE v_folio_cnss          DECIMAL(9,0);
  DEFINE v_f_cambio_nss        DATE;
  DEFINE r_bnd_proceso_cnt     SMALLINT; --Estatus del proceso

  DEFINE e_folio_liquida       DECIMAL(9,0);
  DEFINE e_f_liquida           DATE;
  DEFINE e_cod_proceso         SMALLINT;
  DEFINE e_cod_proceso_cnt     SMALLINT;
  DEFINE e_precio_fondo        DECIMAL(19,14);
  DEFINE e_id_cuenta_contable  SMALLINT;
  DEFINE e_cod_transaccion_cnt SMALLINT;
  DEFINE e_cod_subcta_cnt      SMALLINT;
  DEFINE e_cta_contable        CHAR(10);
  DEFINE e_cod_naturaleza_cta  SMALLINT;
  DEFINE e_monto_acciones_his  DECIMAL(22,2);
  DEFINE e_monto_pesos_his     DECIMAL(22,2);
  DEFINE e_monto_pesos_cort    DECIMAL(22,2);

  --Inicialización de variables
  LET v_bnd_proceso         = 1; --Estado correcto
  LET v_id_cuenta_contable  = 0;
  LET v_cod_transaccion_cnt = 0;
  LET v_cod_subcta_cnt      = 0;
  LET v_cta_contable        = '0000000000';
  LET v_cod_naturaleza_cta  = 0;
  LET v_monto_acciones      = 0;
  LET v_monto_pesos         = 0;
  LET v_monto_acciones0     = 0;
  LET v_monto_acciones1     = 0;
  LET v_monto_acciones_ant  = 0;
  LET v_monto_acciones_ant0 = 0;
  LET v_monto_acciones_ant1 = 0;
  LET v_monto_acciones_hoy  = 0;
  LET v_monto_acciones_hoy0 = 0;
  LET v_monto_acciones_hoy1 = 0;
  LET v_monto_acciones_post = 0;
  LET v_monto_acciones_cort = 0;
  LET v_monto_acciones_cor0 = 0;
  LET v_monto_acciones_cor1 = 0;
  LET v_poliza_inversa      = 0;
  LET v_precio_fondo_hoy    = 0;
  LET v_precio_fondo_ant    = 0;
  LET v_cantidad_basica     = 0;

  LET v_monto_pesos_hoy     = 0;
  LET v_monto_pesos_ant     = 0;
  LET v_monto_pesos_hoy0    = 0;
  LET v_monto_pesos_ant0    = 0;
  LET v_monto_pesos0        = 0;
  LET v_monto_pesos_cons    = 0;

  LET v_tot_cambio_nss      = 0;
  LET v_folio_cnss          = 0;
  LET v_f_cambio_nss        = '';
  LET r_bnd_proceso_cnt     = 1;

  LET e_folio_liquida       = 0;
  LET e_f_liquida           = 0;
  LET e_cod_proceso         = 0;
  LET e_cod_proceso_cnt     = 0;
  LET e_precio_fondo        = 0;
  LET e_id_cuenta_contable  = 0;
  LET e_cod_transaccion_cnt = 0;
  LET e_cod_subcta_cnt      = 0;
  LET e_cta_contable        = 0;
  LET e_cod_naturaleza_cta  = 0;
  LET e_monto_acciones_his  = 0;
  LET e_monto_pesos_his     = 0;
  LET e_monto_pesos_cort    = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_reg_rend_cnt.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  -- TRANSACCION 9 Registro de Rendimientos
  ------- CONSISTENTES -------
  -- Monto acciones dia corte
  SELECT SUM(monto_acciones)
  INTO   v_monto_acciones_cort
  FROM   safre_sdo@vivws_tcp:cta_saldo_diario_global
  WHERE  f_saldo          = p_f_liquida
  --AND    subcuenta   NOT IN (40,41)
  AND    subcuenta        = 48
  AND    folio            = p_folio_liquida
  AND    ind_consistencia = 1;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_acciones_cort = 0;
  END IF

  -- Extrae precio accion del dia
  SELECT precio_fondo
  INTO   v_precio_fondo_hoy
  FROM   glo_valor_fondo
  WHERE  f_valuacion = p_f_liquida + 1
  AND    fondo       = 11;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_precio_fondo_hoy    = 0;
  END IF

  -- Extrae precio acción del dia posterior
  SELECT precio_fondo
  INTO   v_precio_fondo_ant
  FROM   glo_valor_fondo
  WHERE  f_valuacion = p_f_liquida
  AND    fondo       = 11;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_precio_fondo_ant    = 0;
  END IF

  LET v_monto_pesos_hoy  = v_monto_acciones_cort * v_precio_fondo_hoy;
  LET v_monto_pesos_ant  = v_monto_acciones_cort * v_precio_fondo_ant;
  LET v_cantidad_basica  = v_monto_pesos_hoy     - v_monto_pesos_ant;
  LET v_monto_pesos_cons = v_cantidad_basica;
  ------- CONSISTENTES -------

  ------- NO CONSISTENTES ----
  -- Monto acciones día corte no consistentes
  SELECT SUM(monto_acciones)
  INTO   v_monto_acciones_cor0
  FROM   safre_sdo@vivws_tcp:cta_saldo_diario_global
  WHERE  f_saldo          = p_f_liquida
  AND    subcuenta        = 48
  AND    folio            = p_folio_liquida
  AND    ind_consistencia = 0;
  IF DBINFO('sqlca.sqlerrd2') == 0 THEN
     LET v_monto_acciones_cor0 = 0;
  END IF

  LET v_monto_pesos_hoy0 = v_monto_acciones_cor0 * v_precio_fondo_hoy;
  LET v_monto_pesos_ant0 = v_monto_acciones_cor0 * v_precio_fondo_ant;
  LET v_monto_pesos0     = v_monto_pesos_hoy0    - v_monto_pesos_ant0;
  ------- NO CONSISTENTES ----

  IF v_cantidad_basica < 0 THEN
     LET v_poliza_inversa = 1;
  ELSE
     LET v_poliza_inversa = 0;
  END IF

  LET v_cantidad_basica = 0;

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
    AND    cod_transaccion_cnt = 9

    LET v_monto_acciones_cort = 0;
    LET v_monto_pesos_hoy     = 0;
    LET v_monto_pesos_ant     = 0;
    LET v_monto_pesos         = 0;

    SELECT SUM(monto_acciones)
    INTO   v_monto_acciones_cort
    FROM   safre_sdo@vivws_tcp:cta_saldo_diario_global
    WHERE  f_saldo          = p_f_liquida
    AND    subcuenta        = v_cod_subcta_cnt
    AND    folio            = p_folio_liquida
    AND    ind_consistencia = 1;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       LET v_monto_acciones_cort = 0;
    END IF

    IF v_cod_subcta_cnt = 44 THEN
       SELECT SUM(monto_acciones)
       INTO   v_monto_acciones_cort
       FROM   safre_sdo@vivws_tcp:cta_saldo_diario_global
       WHERE  f_saldo          = p_f_liquida
       AND    subcuenta       IN (42,44)
       AND    folio            = p_folio_liquida
       AND    ind_consistencia = 1;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          LET v_monto_acciones_cort = 0;
       END IF
    END IF

    IF v_poliza_inversa = 1 THEN
       IF v_cod_naturaleza_cta = 1 THEN
          LET v_cod_naturaleza_cta = 2;
       ELSE
          LET v_cod_naturaleza_cta = 1;
       END IF
    END IF

    LET v_monto_pesos_hoy = v_monto_acciones_cort * v_precio_fondo_hoy;
    LET v_monto_pesos_ant = v_monto_acciones_cort * v_precio_fondo_ant;
    LET v_monto_pesos     = v_monto_pesos_hoy     - v_monto_pesos_ant;

    IF v_cod_subcta_cnt = 48 THEN
       -- Monto acciones día corte no consistentes
       IF v_cta_contable = '8120010100' OR 
          v_cta_contable = '8220010100' THEN
          LET v_monto_pesos = v_monto_pesos0;
       END IF

       -- Monto acciones día corte consistentes
       IF v_cta_contable = '8120010200' OR 
          v_cta_contable = '8220010200' THEN
          LET v_monto_pesos = v_monto_pesos_cons;
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

    END IF
  END FOREACH;

  SELECT SUM(importe)
  INTO   v_cantidad_basica
  FROM   cnt_transaccion
  WHERE  folio_liquida       = p_folio_liquida
  AND    id_cuenta_contable IN (7,8,9);
  --AND    id_cuenta_contable IN (5,6,7,8);
  --AND    id_cuenta_contable IN (4,5,6,7);

  -- si el monto es negativo, se obtiene el valor absoluto
  IF ( v_cantidad_basica < 0 ) THEN
     LET v_cantidad_basica = v_cantidad_basica * (-1);
  END IF

  --Insertar las cuentas contables en la tabla de transacciones
  IF v_cantidad_basica > 0 THEN
     LET v_cod_naturaleza_cta = 2;

     IF v_poliza_inversa = 1 THEN
        LET v_cod_naturaleza_cta = 1;
     END IF

     --TRACE 'Inserta en transaccion';
     INSERT INTO safre_viv:cnt_transaccion VALUES(1,
                                                  0,
                                                  p_cod_proceso_cnt,
                                                  p_cod_proceso,
                                                  9,
                                                  0,
                                                  '5110010100',
                                                  2,
                                                  p_folio_liquida,
                                                  v_cantidad_basica,
                                                  p_f_liquida,
                                                  TODAY,
                                                  0,   -- 0>Registro Contable, 1>Reverso
                                                  10);
  END IF;
  -- TRANSACCION 9 Registro de Rendimientos


  --Genera Registro Contable Cambio NSS Aportaciones Voluntarias
  SELECT COUNT(*)
  INTO   v_tot_cambio_nss
  FROM   safre_viv:pag_det_apvol
  WHERE  ind_cambio_nss = 1;
  IF v_tot_cambio_nss > 0 THEN
     FOREACH
       --Extrae información de la cuenta contable para el proceso que lo solicita
       SELECT folio, f_cambio_nss
       INTO   v_folio_cnss, v_f_cambio_nss
       FROM   safre_viv:pag_det_apvol
       WHERE  ind_cambio_nss = 1

       EXECUTE PROCEDURE fn_apo_vol_cnt52(v_folio_cnss,
                                          v_f_cambio_nss,
                                          52,
                                          1407,
                                          73)
                                     INTO r_bnd_proceso_cnt;
     END FOREACH;
  END IF;

  RETURN v_bnd_proceso;

END PROCEDURE;


