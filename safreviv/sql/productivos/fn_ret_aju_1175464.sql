






CREATE FUNCTION "safreviv".fn_ret_aju_1175464(p_folio DECIMAL(9,0))
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
DEFINE d_id_dis_interface_ef DECIMAL(9,0);
DEFINE d_folio_liquida       DECIMAL(9,0);

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

  SET PDQPRIORITY HIGH;

  LET v_folio_liquida      = p_folio;
  LET v_movimiento         = 521;

  DELETE
  FROM   cta_movimiento
  WHERE  folio_liquida = v_folio_liquida
  AND    movimiento    = 521;

  DELETE 
  FROM   dis_ap_cargo_dup
  WHERE  folio_liq_ajuste = v_folio_liquida;

  DELETE 
  FROM   cnt_transaccion
  WHERE  folio_liquida = v_folio_liquida;

  UPDATE glo_folio
  SET    status      = 3
  WHERE  folio       = v_folio_liquida
  AND    proceso_cod = 932;

  LET v_char = " Retorno Ajuste Operativo finalizo correctamente";
  RETURN v_bnd_proceso , 0 , v_char, '';

END FUNCTION;


