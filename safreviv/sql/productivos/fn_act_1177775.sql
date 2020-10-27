






CREATE FUNCTION "safreviv".fn_act_1177775(p_folio DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70),DECIMAL(9,0)

--Última modificación 15112017
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

DEFINE v_f_liquida           DATE;
DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_subcuenta           SMALLINT;
DEFINE v_fondo_inversion     SMALLINT;
DEFINE v_movimiento          SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_id_referencia       DECIMAL(9,0);
DEFINE v_monto_acciones      DECIMAL(22,2);
DEFINE v_monto_pesos         DECIMAL(22,2);
DEFINE v_monto_acciones_abo  DECIMAL(22,2);
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
DEFINE v_f_emision           DATE;
DEFINE v_tpo_transaccion     SMALLINT;
DEFINE v_estado              SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_bnd_transaccion      = 0;
LET v_derechohabiente_pag  = 0;

  SET PDQPRIORITY HIGH;

  FOREACH
    SELECT tmp.f_liquida,
           tmp.id_derechohabiente,
           tmp.subcuenta,
           tmp.fondo_inversion,
           tmp.movimiento,
           tmp.folio_liquida,
           tmp.id_referencia,
           tmp.monto_acciones,
           tmp.monto_pesos,
           tmp.f_valor,
           tmp.f_registro,
           tmp.h_registro,
           tmp.origen
    INTO   v_f_liquida,
           v_id_derechohabiente,
           v_subcuenta,
           v_fondo_inversion,
           v_movimiento,
           v_folio_liquida,
           v_id_referencia,
           v_monto_acciones,
           v_monto_pesos,
           v_f_valor,
           v_f_registro,
           v_h_registro,
           v_origen
    FROM   safre_viv:tmp_dis_apo_subs_44 tmp

    LET v_bnd_proceso = 0; --Estado correcto

    {UPDATE cta_movimiento
    SET    subcuenta          = 4
    WHERE  folio_liquida      = v_folio_liquida
    AND    id_referencia      = v_id_referencia
    AND    id_derechohabiente = v_id_derechohabiente
    AND    subcuenta          = v_subcuenta
    AND    movimiento         = v_movimiento;}

    --Abono Subcuenta 44-VIVIENDA 97 SOLO INFONAVIT
    LET v_f_liquida          = TODAY;
    LET v_folio_liquida      = p_folio;
    LET v_subcuenta          = 44;
    LET v_movimiento         = 521;
    LET v_monto_acciones_abo = v_monto_acciones * -1;
    LET v_monto_pesos_abo    = v_monto_pesos    * -1;

    INSERT INTO cta_movimiento VALUES (v_f_liquida,
                                       v_id_derechohabiente,
                                       v_subcuenta,
                                       v_fondo_inversion,
                                       v_movimiento,
                                       v_folio_liquida,
                                       v_id_referencia,
                                       v_monto_acciones_abo,
                                       v_monto_pesos_abo,
                                       v_f_valor,
                                       v_f_registro,
                                       v_h_registro,
                                       v_origen);

    --Cargo Subcuenta 4-VIVIENDA 97
    LET v_f_liquida     = TODAY;
    LET v_folio_liquida = p_folio;
    LET v_subcuenta     = 4;
    LET v_movimiento    = 1212;

    INSERT INTO cta_movimiento VALUES (v_f_liquida,
                                       v_id_derechohabiente,
                                       v_subcuenta,
                                       v_fondo_inversion,
                                       v_movimiento,
                                       v_folio_liquida,
                                       v_id_referencia,
                                       v_monto_acciones,
                                       v_monto_pesos,
                                       v_f_valor,
                                       v_f_registro,
                                       v_h_registro,
                                       v_origen);

  END FOREACH;

  --Registro Contable Especial
  SELECT SUM(monto_pesos) 
  INTO   v_importe
  FROM   safre_viv:cta_movimiento
  WHERE  f_liquida     = v_f_liquida
  AND    folio_liquida = v_folio_liquida
  AND    movimiento    = 521;

  LET v_folio_cnt           = 0;
  LET v_cod_proceso_cnt     = 19;
  LET v_cod_proceso         = 901;
  LET v_cod_transaccion_cnt = 24;
  LET v_f_emision           = TODAY;
  LET v_tpo_transaccion     = 0;
  LET v_estado              = 10;

  --Cargo Subcuenta 4-VIVIENDA 97
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
                                      v_importe,
                                      v_f_liquida,
                                      v_f_emision,
                                      v_tpo_transaccion,
                                      v_estado);

  --Abono Subcuenta 44-VIVIENDA 97 SOLO INFONAVIT
  LET v_id_cuenta_contable  = 2;
  LET v_cod_subcta_cnt      = 44;
  LET v_cta_contable        = '2504080001';
  LET v_cod_naturaleza_cta  = 1;

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
  AND    proceso_cod = 901;

  LET v_char = "  Ajuste Operativo finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END FUNCTION;


