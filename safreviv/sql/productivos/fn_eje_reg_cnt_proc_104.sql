






CREATE PROCEDURE "safreviv".fn_eje_reg_cnt_proc_104(p_folio_liq   DECIMAL(9,0))  --Folio de Liquidación
RETURNING SMALLINT;

--Última modificación 24042018
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
  LET v_cod_proceso_cnt = 104;
  LET v_cod_proceso_op  = 2609;
  LET v_cod_trans_cnt   = 132;
  LET r_bnd_proceso_cnt = 1; --Estado correcto

  SET PDQPRIORITY HIGH;

  SELECT MAX(folio), f_actualiza
  INTO   v_folio_liquida, v_f_liquida
  FROM   glo_folio
  WHERE  proceso_cod = 2609
  GROUP BY 2;

  SELECT UNIQUE(f_liquida)
  INTO   v_f_liquida
  FROM   cta_movimiento
  WHERE  folio_liquida = v_folio_liquida;

  EXECUTE PROCEDURE fn_ret_solo_inf97_cnt30(v_folio_liquida,
                                            v_f_liquida,
                                            v_cod_proceso_cnt,
                                            v_cod_proceso_op,
                                            v_cod_trans_cnt) 
  INTO r_bnd_proceso_cnt;

  --TRACE 'Finaliza fn_dis_transaccion con valor '||v_bnd_proceso;
  --LET error_info = "Registro Contable finalizo correctamente";
  --RETURN v_bnd_transaccion,v_status,error_info,'';
  RETURN r_bnd_proceso_cnt;

END PROCEDURE;


