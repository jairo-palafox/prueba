






CREATE PROCEDURE "safreviv".fn_eje_reg_cnt_proc_73(p_folio_liq   DECIMAL(9,0))  --Folio de Liquidación
RETURNING SMALLINT;

--Última modificación 03122018
--Declaración de variables
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(20);
DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_folio_liquida       DECIMAL(9,0);
DEFINE v_f_liquida           DATE;
DEFINE v_cod_proceso_cnt     SMALLINT;
DEFINE v_cod_proceso_op      SMALLINT;
DEFINE v_cod_trans_cnt       SMALLINT;
DEFINE r_bnd_proceso_cnt     SMALLINT;

--ON EXCEPTION
--   SET sql_err, isam_err, error_info
--       LET v_status = sql_err;
--       RETURN  v_status,isam_err,error_info,v_derechohabiente_pag;
--END EXCEPTION

  LET v_folio_liquida   = 0;
  LET v_f_liquida       = ""; 
  LET v_cod_proceso_cnt = 105;
  LET v_cod_proceso_op  = 1535;
  LET v_cod_trans_cnt   = 0;
  LET r_bnd_proceso_cnt = 1; --Estado correcto

  SET PDQPRIORITY HIGH;

  DELETE
  FROM   cnt_transaccion
  WHERE  folio_liquida = 105781;

  SELECT MAX(folio), f_actualiza
  INTO   v_folio_liquida, v_f_liquida
  FROM   glo_folio
  WHERE  folio       = 105781
  AND    proceso_cod = 1535
  GROUP BY 2;

  SELECT UNIQUE(f_liquida)
  INTO   v_f_liquida
  FROM   cta_movimiento
  WHERE  folio_liquida = v_folio_liquida;

  EXECUTE PROCEDURE fn_rest_ret_ley73_cnt(v_folio_liquida,
                                          v_f_liquida,
                                          v_cod_proceso_cnt,
                                          v_cod_proceso_op,
                                          v_cod_trans_cnt) 
  INTO r_bnd_proceso_cnt;

  UPDATE cnt_transaccion
  SET    f_liquida     = TODAY
  WHERE  folio_liquida = 105781;

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  --LET error_info = "Registro Contable finalizo correctamente";
  --RETURN v_bnd_transaccion,v_status,error_info,'';
  RETURN r_bnd_proceso_cnt;

END PROCEDURE;


