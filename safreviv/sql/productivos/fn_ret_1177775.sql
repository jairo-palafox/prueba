






CREATE FUNCTION "safreviv".fn_ret_1177775(p_folio DECIMAL(9,0))
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

  SET PDQPRIORITY HIGH;

  {FOREACH
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

    UPDATE cta_movimiento
    SET    subcuenta          = 44
    WHERE  folio_liquida      = v_folio_liquida
    AND    id_referencia      = v_id_referencia
    AND    id_derechohabiente = v_id_derechohabiente
    AND    subcuenta          = v_subcuenta
    AND    movimiento         = v_movimiento;

  END FOREACH;}

  DELETE 
  FROM   cta_movimiento
  WHERE  folio_liquida = p_folio;

  DELETE 
  FROM   cnt_transaccion
  WHERE  folio_liquida = p_folio;

  UPDATE glo_folio
  SET    status      = 3
  WHERE  folio       = p_folio
  AND    proceso_cod = 901;

  DROP TABLE IF EXISTS tmp_dis_apo_subs_44;

  LET v_char = "  Retorno Ajuste Operativo finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END FUNCTION;


