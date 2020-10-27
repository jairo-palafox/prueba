






CREATE PROCEDURE "safreviv".sp_dis_mov_tmp(p_id_derechohabiente DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 11082015 
--Declaración de variables
  DEFINE v_sel_his           LVARCHAR(3000);
  DEFINE v_sel_act           LVARCHAR(800);
  DEFINE v_nombre_tabla      VARCHAR(20)  ;
  DEFINE v_existe_his        SMALLINT;

  DEFINE v_bnd_proceso       SMALLINT;       --Estatus del proceso
  DEFINE v_status            SMALLINT;
  DEFINE sql_err             INTEGER ;
  DEFINE isam_err            INTEGER ;
  DEFINE error_info          CHAR(70);
  DEFINE v_char              CHAR(70);

  DEFINE v_lock              VARCHAR(100);
   
  DEFINE v_tmp_id_derechohabiente  DECIMAL(9,0); 
  DEFINE v_tmp_folio_liquida DECIMAL(9,0);
  DEFINE v_tmp_id_referencia DECIMAL(9,0);
  DEFINE v_tmp_f_liquida     DATE; 
  DEFINE v_tmp_mov           SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

  --SET DEBUG FILE TO '/ds/safreviv_int/dis/ERR_sp_dis_his_mov.TRACE';
  --TRACE ON;

  DROP TABLE IF EXISTS tmp_movimiento;

  LET v_bnd_proceso             = 0; --Estado correcto 
  LET v_status                  = 0;
  LET sql_err                   = 0;
  LET isam_err                  = 0;
  LET error_info                = "";
   
  LET v_existe_his              = 0 ;
  LET v_sel_his                 = "";
  LET v_lock                    = "";
   
  LET v_tmp_id_derechohabiente  = 0;
  LET v_tmp_folio_liquida       = 0;
  LET v_tmp_id_referencia       = 0;
  LET v_tmp_f_liquida           = "";  
  LET v_tmp_mov                 = 0;
   
  FOREACH 
    SELECT tabla
    INTO   v_nombre_tabla
    FROM   cat_tab_movimiento
   
    LET v_sel_his = v_sel_his || " SELECT id_derechohabiente, folio_liquida, id_referencia, f_liquida, movimiento "||
                                 " FROM " || v_nombre_tabla ||
                                 " WHERE  id_derechohabiente = "||p_id_derechohabiente||
                                 " AND    subcuenta          = 4 "||
                                 " AND    movimiento        IN ( SELECT b.movimiento "||
                                 "                               FROM   cat_movimiento b "||
                                 "                               WHERE  b.categoria = 2 )"||
                                 " UNION ALL ";
    LET v_existe_his = 1 ;

  END FOREACH;

  LET v_sel_act = " SELECT id_derechohabiente, folio_liquida, id_referencia, f_liquida, movimiento "||
                  " FROM   cta_movimiento " ||
                  " WHERE  id_derechohabiente = "||p_id_derechohabiente||
                  " AND    subcuenta          = 4"||
                  " AND    movimiento        IN ( SELECT b.movimiento "||
                  "                               FROM cat_movimiento b "||
                  "                               WHERE b.categoria = 2 )"||
                  " INTO TEMP tmp_movimiento ";

  IF v_existe_his = 1 THEN 
	 LET v_sel_his = v_sel_his|| v_sel_act ;
  ELSE
    LET v_sel_his = v_sel_act ;
  END IF 

  EXECUTE IMMEDIATE v_sel_his;

  --LET v_lock = "LOCK TABLE tmp_cta_movimiento IN EXCLUSIVE MODE";
  --EXECUTE IMMEDIATE v_lock;

  INSERT INTO tmp_cta_movimiento
  SELECT * 
  FROM tmp_movimiento;
   
  --LET v_lock = "UNLOCK TABLE tmp_cta_movimiento";
  --EXECUTE IMMEDIATE v_lock;

  --TRACE 'Finaliza Carga de movimientos temporales';
  LET v_char = "Termina Carga de movimientos temporale";
  RETURN v_bnd_proceso, 0, v_char;

END PROCEDURE;


