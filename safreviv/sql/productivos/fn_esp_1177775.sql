






CREATE FUNCTION "safreviv".fn_esp_1177775(p_folio DECIMAL(9,0))
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
DEFINE v_f_valor             DATE;
DEFINE v_f_registro          DATE;
DEFINE v_h_registro          DATETIME HOUR TO SECOND;
DEFINE v_origen              CHAR(20);

ON EXCEPTION
   SET sql_err, isam_err, error_info
       LET v_status = sql_err;
       RETURN  v_status ,isam_err , error_info, v_derechohabiente_pag;
END EXCEPTION

--Inicialización de variables
LET v_bnd_proceso          = 0; --Estado correcto
LET v_bnd_transaccion      = 0;
LET v_derechohabiente_pag  = 0;

  DROP TABLE IF EXISTS tmp_dis_apo_subs_44;
  
  --Crea tabla de respaldo de los registros con mov 72 - sub 44
  CREATE TABLE tmp_dis_apo_subs_44(f_liquida          DATE NOT NULL,
                                   id_derechohabiente DECIMAL(9,0) NOT NULL,
                                   subcuenta          SMALLINT NOT NULL,
                                   fondo_inversion    SMALLINT NOT NULL,
                                   movimiento         SMALLINT NOT NULL,
                                   folio_liquida      DECIMAL(9,0) NOT NULL,
                                   id_referencia      DECIMAL(9,0) NOT NULL,
                                   monto_acciones     DECIMAL(22,2),
                                   monto_pesos        DECIMAL(22,2),
                                   f_valor            DATE,
                                   f_registro         DATE,
                                   h_registro         DATETIME HOUR TO SECOND,
                                   origen             CHAR(20));

  SET PDQPRIORITY HIGH;

  --Obtiene los movimientos de cargo pago 43 bis
  FOREACH
    SELECT cta.f_liquida,
           cta.id_derechohabiente,
           cta.subcuenta,
           cta.fondo_inversion,
           cta.movimiento,
           cta.folio_liquida,
           cta.id_referencia,
           cta.monto_acciones,
           cta.monto_pesos,
           cta.f_valor,
           cta.f_registro,
           cta.h_registro,
           cta.origen
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
    FROM   safre_viv:cta_movimiento cta
    WHERE  cta.subcuenta  = 44
    AND    cta.movimiento = 72

    LET v_bnd_proceso = 0; --Estado correcto

    INSERT INTO tmp_dis_apo_subs_44 VALUES(v_f_liquida,
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

  --Obtiene los movimientos de saldo inicial anual
  SELECT UNIQUE a.id_derechohabiente
  FROM   safre_viv:cta_movimiento a
  WHERE  a.subcuenta  = 44
  AND    a.movimiento = 72
  INTO TEMP tmp_as_sub44;
  
  FOREACH
    SELECT cta.f_liquida,
           cta.id_derechohabiente,
           cta.subcuenta,
           cta.fondo_inversion,
           cta.movimiento,
           cta.folio_liquida,
           cta.id_referencia,
           cta.monto_acciones,
           cta.monto_pesos,
           cta.f_valor,
           cta.f_registro,
           cta.h_registro,
           cta.origen
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
    FROM   safre_viv:cta_movimiento cta,
           tmp_as_sub44 tmp
    WHERE  cta.id_derechohabiente = tmp.id_derechohabiente 
    AND    cta.subcuenta          = 44
    AND    cta.movimiento         = 1099

    LET v_bnd_proceso = 0; --Estado correcto

    INSERT INTO tmp_dis_apo_subs_44 VALUES(v_f_liquida,
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

  LET v_char = "  Respaldo finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END FUNCTION;


