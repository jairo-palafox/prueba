






create procedure "safreviv".fn_canc_amo_cnt74(p_folio_liquida      DECIMAL(9,0),  --Folio de liquidación del proceso
                                   p_f_liquida          DATE,          --Fecha de liquidación del proceso
                                   p_cod_proceso_cnt    SMALLINT,      --Código Proceso Contable
                                   p_cod_proceso        SMALLINT,      --Código Proceso
                                   p_transaccion        SMALLINT)      --Código Transaccion contable
RETURNING SMALLINT;

--Última modificación 24102019
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

DEFINE v_nombre_tabla        VARCHAR(20);
DEFINE v_anio                SMALLINT;
DEFINE v_existe_his          SMALLINT;
DEFINE v_sel_his             LVARCHAR(5000);
DEFINE v_sel_act             LVARCHAR(5000);

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

  LET v_existe_his           = 0;
  LET v_sel_his              = "";
  LET v_sel_act              = "";

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_dev_amo_cnt53.trace';
  --TRACE 'Folio Liquida '||p_folio_liquida;
  --TRACE 'Fecha Liquida '||p_f_liquida;
  --TRACE 'Proceso Contable '||p_cod_proceso_cnt;
  --TRACE 'Proceso '||p_cod_proceso;
  --TRACE 'Transaccion Contable '||p_transaccion;

  SET PDQPRIORITY HIGH;

  DROP TABLE IF EXISTS tmp_canc_amo_cnt74;
  CREATE TABLE tmp_canc_amo_cnt74 (f_liquida           DATE,
                                   id_derechohabiente  DECIMAL(9,0),
                                   subcuenta           SMALLINT,
                                   movimiento          SMALLINT,
                                   folio_liquida       DECIMAL(9,0),
                                   monto_pesos         DECIMAL(12,2)
                                  )
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  --Extrae movimientos de abono por registro de pagos
  FOREACH
    SELECT a.tabla, a.anio
    INTO   v_nombre_tabla, v_anio
    FROM   cat_tab_movimiento a

    LET v_sel_his = v_sel_his || " SELECT a.f_liquida,               "||
                                 "        a.id_derechohabiente,      "||
                                 "        a.subcuenta,               "||
                                 "        a.movimiento,              "||
                                 "        a.folio_liquida,           "||
                                 "        a.monto_pesos              "||
                                 " FROM   "|| v_nombre_tabla || " a, "||
                                 "        glo_folio b                "||
                                 " WHERE  a.folio_liquida = b.folio  "||
                                 " AND    b.proceso_cod   = 1540     "||
                                 " AND    b.status        = 2        "||
                                 " UNION ALL ";
    LET v_existe_his = 1;
  END FOREACH;

  LET v_sel_act = " SELECT a.f_liquida,               "||
                  "        a.id_derechohabiente,      "||
                  "        a.subcuenta,               "||
                  "        a.movimiento,              "||
                  "        a.folio_liquida,           "||
                  "        a.monto_pesos              "||
                  " FROM   cta_movimiento a, "||
                  "        glo_folio b                "||
                  " WHERE  a.folio_liquida = b.folio  "||
                  " AND    b.proceso_cod   = 1540     "||
                  " AND    b.status        = 2        "||
                  " INTO TEMP tmp_cta_proc_1540; ";

  IF v_existe_his = 1 THEN
     LET v_sel_his = v_sel_his|| v_sel_act ;
  ELSE
     LET v_sel_his = v_sel_act ;
  END IF

  EXECUTE IMMEDIATE v_sel_his;

  UPDATE statistics FOR TABLE tmp_cta_proc_1540;

  INSERT INTO tmp_canc_amo_cnt74
  SELECT *
  FROM   tmp_cta_proc_1540;

  CREATE INDEX xie1tmp_canc_amo_cnt74 ON tmp_canc_amo_cnt74
  (f_liquida,subcuenta,folio_liquida) IN dis_ix_dbs;

  UPDATE statistics FOR TABLE tmp_canc_amo_cnt74;

  FOREACH
    SELECT unique folio_liquida, f_liquida
    INTO   p_folio_liquida, p_f_liquida
    FROM   tmp_canc_amo_cnt74
    
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
      FROM   safre_viv:tmp_canc_amo_cnt74
      WHERE  f_liquida     = p_f_liquida
      AND    subcuenta     = v_cod_subcta_cnt
      AND    folio_liquida = p_folio_liquida;
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
                                                      TODAY,
                                                      TODAY,
                                                      0,   -- 0>Registro Contable, 1>Reverso
                                                      10);
      END IF

      LET v_monto_pesos = 0;
    END FOREACH;
  END FOREACH;

  --LET v_bnd_proceso = 10; --Estado incorrecto. No se encontro información
  RETURN v_bnd_proceso;

END PROCEDURE;


