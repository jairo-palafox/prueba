






CREATE FUNCTION "safreviv".fn_aju_1175464(p_folio DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 14122017
--Declaración de variables
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_cadena              CHAR(300);
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER;
DEFINE isam_err              INTEGER;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_derechohabiente_pag DECIMAL(9,0);
DEFINE v_bnd_transaccion     SMALLINT;

DEFINE d_id_derechohabiente  DECIMAL(9,0);
DEFINE d_nss                 CHAR(11);
DEFINE d_folio_sua           DECIMAL(6,0);
DEFINE d_periodo_pago        CHAR(6);
DEFINE d_f_pago              DATE;
DEFINE d_nrp                 CHAR(11);
DEFINE d_aiv_ap_pat          DECIMAL(18,6);
DEFINE d_aivs                DECIMAL(18,6);
DEFINE d_id_dis_interface_ef DECIMAL(9,0);
DEFINE d_folio_liquida       DECIMAL(9,0);

DEFINE v_f_liquida           DATE;
DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_subcuenta           SMALLINT;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_id_referencia       DECIMAL(9,0);
DEFINE v_monto_acciones      DECIMAL(16,6);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_monto_acciones_abo  DECIMAL(16,6);
DEFINE v_monto_pesos_abo     DECIMAL(22,2);
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);

DEFINE v_id_cuenta_contable  SMALLINT;
DEFINE v_folio_cnt           DECIMAL(9,0);
DEFINE v_cod_proceso_cnt     SMALLINT;
DEFINE v_cod_proceso         SMALLINT;
DEFINE v_cod_transaccion_cnt SMALLINT;
DEFINE v_cod_subcta_cnt      SMALLINT;
DEFINE v_cta_contable        CHAR(10);
DEFINE v_cod_naturaleza_cta  SMALLINT;
DEFINE v_importe             DECIMAL(22,2);
DEFINE v_importe_viv         DECIMAL(22,2);
DEFINE v_importe_si          DECIMAL(22,2);
DEFINE v_f_emision           DATE;
DEFINE v_tpo_transaccion     SMALLINT;
DEFINE v_estado              SMALLINT;
DEFINE v_estado_op           SMALLINT;

DEFINE v_proceso_cod         SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_bnd_transaccion      = 0;
LET v_derechohabiente_pag  = 0;

LET d_id_derechohabiente   = 0;
LET d_nss                  = "";
LET d_folio_sua            = 0;
LET d_periodo_pago         = "";
LET d_f_pago               = "";
LET d_nrp                  = "";
LET d_aivs                 = 0;
LET d_id_dis_interface_ef  = 0;
LET d_folio_liquida        = 0;

LET v_subcuenta            = 0;
LET v_fondo_inversion      = 0;
LET v_movimiento           = 0;
LET v_monto_acciones       = 0;
LET v_monto_pesos          = 0;
LET v_monto_acciones_abo   = 0;
LET v_monto_pesos_abo      = 0;
LET v_f_valor              = "";
LET v_f_registro           = "";
LET v_h_registro           = "";
LET v_origen               = "";

LET v_id_cuenta_contable   = 0;
LET v_folio_cnt            = 0;
LET v_cod_proceso_cnt      = 0;
LET v_cod_proceso          = 0;
LET v_cod_transaccion_cnt  = 0;
LET v_cod_subcta_cnt       = 0;
LET v_cta_contable         = "";
LET v_cod_naturaleza_cta   = 0;
LET v_folio_liquida        = 0;
LET v_importe              = 0;
LET v_importe_viv          = 0;
LET v_importe_si           = 0;
LET v_f_liquida            = "";
LET v_f_emision            = "";
LET v_tpo_transaccion      = 0;
LET v_estado               = 0;

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT a.id_derechohabiente,
           c.nss,
           b.folio_sua,
           b.periodo_pago,
           b.f_pago,
           b.nrp,
           a.aiv_ap_pat,
           MAX(b.id_dis_interface_ef),
           MAX(b.folio_liquida)
    INTO   d_id_derechohabiente,
           d_nss,
           d_folio_sua,
           d_periodo_pago,
           d_f_pago,
           d_nrp,
           d_aivs,
           d_id_dis_interface_ef,
           d_folio_liquida
    FROM   dis_interface_ef a,
           dis_interface_ef b,
           afi_derechohabiente c
    WHERE  a.id_derechohabiente = b.id_derechohabiente
    AND    a.id_derechohabiente = c.id_derechohabiente
    AND    b.id_derechohabiente = c.id_derechohabiente
    AND    a.folio_sua          = b.folio_sua
    AND    a.f_pago             = b.f_pago
    AND    a.nrp                = b.nrp
    AND    a.periodo_pago       = b.periodo_pago
    AND    a.folio_liquida     <> b.folio_liquida
    GROUP BY 1,2,3,4,5,6,7
    ORDER BY 3,5

    LET v_bnd_proceso = 0; --Estado correcto
    LET v_proceso_cod = 0;

    SELECT g.proceso_cod
    INTO   v_proceso_cod
    FROM   glo_folio g
    WHERE  g.folio = d_folio_liquida;
    IF v_proceso_cod <> 932 THEN
       CONTINUE FOREACH;
    END IF;

    --Ajuste Operativo Abono Subcuenta 4-VIVIENDA 97
    SELECT d.subcuenta,
           d.fondo_inversion,
           d.movimiento,
           d.monto_acciones,
           d.monto_pesos,
           d.f_valor,
           d.f_registro,
           d.h_registro,
           d.origen
    INTO   v_subcuenta,
           v_fondo_inversion,
           v_movimiento,
           v_monto_acciones,
           v_monto_pesos,
           v_f_valor,
           v_f_registro,
           v_h_registro,
           v_origen
    FROM   cta_movimiento16 d
    WHERE  d.folio_liquida      = d_folio_liquida
    AND    d.id_referencia      = d_id_dis_interface_ef
    AND    d.id_derechohabiente = d_id_derechohabiente;
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       SELECT d.subcuenta,
              d.fondo_inversion,
              d.movimiento,
              d.monto_acciones,
              d.monto_pesos,
              d.f_valor,
              d.f_registro,
              d.h_registro,
              d.origen
       INTO   v_subcuenta,
              v_fondo_inversion,
              v_movimiento,
              v_monto_acciones,
              v_monto_pesos,
              v_f_valor,
              v_f_registro,
              v_h_registro,
              v_origen
       FROM   cta_movimiento15 d
       WHERE  d.folio_liquida      = d_folio_liquida
       AND    d.id_referencia      = d_id_dis_interface_ef
       AND    d.id_derechohabiente = d_id_derechohabiente;
    END IF
 
    LET v_f_liquida          = TODAY;
    LET v_folio_liquida      = p_folio;
    LET v_movimiento         = 521;
    LET v_monto_acciones_abo = v_monto_acciones * -1;
    LET v_monto_pesos_abo    = v_monto_pesos    * -1;
    LET v_estado_op          = 110;

    INSERT INTO cta_movimiento VALUES (v_f_liquida,
                                       d_id_derechohabiente,
                                       v_subcuenta,
                                       v_fondo_inversion,
                                       v_movimiento,
                                       v_folio_liquida,
                                       d_id_dis_interface_ef,
                                       v_monto_acciones_abo,
                                       v_monto_pesos_abo,
                                       v_f_valor,
                                       v_f_registro,
                                       v_h_registro,
                                       v_origen);

    INSERT INTO dis_ap_cargo_dup VALUES (0,
                                         d_folio_liquida,
                                         d_id_dis_interface_ef,
                                         d_id_derechohabiente,
                                         d_folio_sua,
                                         d_periodo_pago,
                                         d_f_pago,
                                         d_nrp,
                                         d_nss,
                                         d_aivs,
                                         0,
                                         v_folio_liquida,
                                         v_f_liquida,
                                         v_estado_op);

    LET d_id_derechohabiente  = 0;
    LET d_nss                 = "";
    LET d_folio_sua           = 0;
    LET d_periodo_pago        = "";
    LET d_f_pago              = "";
    LET d_nrp                 = "";
    LET d_aivs                = 0;
    LET d_id_dis_interface_ef = 0;
    LET d_folio_liquida       = 0;

    LET v_subcuenta           = 0;
    LET v_fondo_inversion     = 0;
    LET v_movimiento          = 0;
    LET v_monto_acciones      = 0;
    LET v_monto_pesos         = 0;
    LET v_monto_acciones_abo  = 0;
    LET v_monto_pesos_abo     = 0;
    LET v_f_valor             = "";
    LET v_f_registro          = "";
    LET v_h_registro          = "";
    LET v_origen              = "";

  END FOREACH;

  --Registro Contable Especial
  --Abono Subcuenta 4-VIVIENDA 97
  SELECT SUM(monto_pesos) 
  INTO   v_importe_viv
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = v_f_liquida
  AND    folio_liquida = v_folio_liquida
  AND    subcuenta     = 4
  AND    movimiento    = 521;

  --Abono Subcuenta 44-VIVIENDA 97 Solo INFONAVIT
  SELECT SUM(monto_pesos) 
  INTO   v_importe_si
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = v_f_liquida
  AND    folio_liquida = v_folio_liquida
  AND    subcuenta     = 44
  AND    movimiento    = 521;

  --Cargo Subcuenta APOYO INFONAVIT (APORTACIONES SUBSECUENTES) 
  SELECT SUM(monto_pesos) 
  INTO   v_importe
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = v_f_liquida
  AND    folio_liquida = v_folio_liquida
  AND    movimiento    = 521;

  IF (v_importe_viv < 0) THEN
     LET v_importe_viv = v_importe_viv * (-1);
  END IF

  LET v_folio_cnt           = 0;
  LET v_cod_proceso_cnt     = 19;
  LET v_cod_proceso         = 932;
  LET v_cod_transaccion_cnt = 24;
  LET v_f_emision           = TODAY;
  LET v_tpo_transaccion     = 0;
  LET v_estado              = 10;

  --Abono Subcuenta 4-VIVIENDA 97
  LET v_id_cuenta_contable  = 1;
  LET v_cod_subcta_cnt      = 4;
  LET v_cta_contable        = '2504070001';
  LET v_cod_naturaleza_cta  = 2;

  INSERT INTO cnt_transaccion VALUES (v_id_cuenta_contable,
                                      v_folio_cnt,
                                      v_cod_proceso_cnt,
                                      v_cod_proceso,
                                      v_cod_transaccion_cnt,
                                      v_cod_subcta_cnt,
                                      v_cta_contable,
                                      v_cod_naturaleza_cta,
                                      v_folio_liquida,
                                      v_importe_viv,
                                      v_f_liquida,
                                      v_f_emision,
                                      v_tpo_transaccion,
                                      v_estado);

  IF (v_importe_si < 0) THEN
     LET v_importe_si = v_importe_si * (-1);
  END IF

  LET v_folio_cnt           = 0;
  LET v_cod_proceso_cnt     = 19;
  LET v_cod_proceso         = 932;
  LET v_cod_transaccion_cnt = 24;
  LET v_f_emision           = TODAY;
  LET v_tpo_transaccion     = 0;
  LET v_estado              = 10;

  --Abono Subcuenta 44-VIVIENDA 97 Solo INFONAVIT
  LET v_id_cuenta_contable  = 2;
  LET v_cod_subcta_cnt      = 44;
  LET v_cta_contable        = '2504080001';
  LET v_cod_naturaleza_cta  = 2;

  INSERT INTO cnt_transaccion VALUES (v_id_cuenta_contable,
                                      v_folio_cnt,
                                      v_cod_proceso_cnt,
                                      v_cod_proceso,
                                      v_cod_transaccion_cnt,
                                      v_cod_subcta_cnt,
                                      v_cta_contable,
                                      v_cod_naturaleza_cta,
                                      v_folio_liquida,
                                      v_importe_si,
                                      v_f_liquida,
                                      v_f_emision,
                                      v_tpo_transaccion,
                                      v_estado);


  --Cargo Subcuenta APOYO INFONAVIT (APORTACIONES SUBSECUENTES) 
  LET v_id_cuenta_contable  = 3;
  LET v_cta_contable        = '2504070004';
  LET v_cod_naturaleza_cta  = 1;
  LET v_cod_transaccion_cnt = 31;

  INSERT INTO cnt_transaccion VALUES (v_id_cuenta_contable,
                                      v_folio_cnt,
                                      v_cod_proceso_cnt,
                                      v_cod_proceso,
                                      v_cod_transaccion_cnt,
                                      v_cod_subcta_cnt,
                                      v_cta_contable,
                                      v_cod_naturaleza_cta,
                                      v_folio_liquida,
                                      v_importe,
                                      v_f_liquida,
                                      v_f_emision,
                                      v_tpo_transaccion,
                                      v_estado);

  UPDATE glo_folio
  SET    status      = 2
  WHERE  folio       = v_folio_liquida
  AND    proceso_cod = 932;

  LET v_char = "  Ajuste Operativo finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END FUNCTION;


