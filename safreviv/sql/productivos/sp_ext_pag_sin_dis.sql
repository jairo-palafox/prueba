






CREATE PROCEDURE "safreviv".sp_ext_pag_sin_dis(p_folio               DECIMAL(9,0),
                                    p_id_derechohabiente  DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 03082018
--Declaración de variables
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --ID Derechohabiente Registro Pago
DEFINE v_subcuenta           SMALLINT;       --Subcuenta Registro Pago
DEFINE v_folio_liquida       DECIMAL(9,0);   --Folio Liquida Registro Pago
DEFINE v_id_referencia       DECIMAL(9,0);   --Id referencia Registro Pago
DEFINE v_acc                 DECIMAL(16,6);  --Monto acciones Registro Pago
DEFINE v_pes                 DECIMAL(12,2);  --Monto pesos Registro Pago
DEFINE v_f_liquida           DATE;           --Fecha Liquida Registro Pago
DEFINE v_anio_val            SMALLINT;       --Año
DEFINE v_anio_pres           SMALLINT;       --Año presente
DEFINE v_folio_dis           DECIMAL(9,0);   --Folio Dispersión de Pago
DEFINE v_tot_reg             SMALLINT;       --Total de registros

DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(20);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);

DEFINE v_sel_his             LVARCHAR(5000);
DEFINE v_sel_act             LVARCHAR(5000);
DEFINE v_nombre_tabla        VARCHAR(20);
DEFINE v_anio                SMALLINT;
DEFINE v_existe_his          SMALLINT;

DEFINE v_tot_reg_cta_dis     SMALLINT;
DEFINE v_id_derecho_val      DECIMAL(9,0);
DEFINE v_val_pag_sin_dis     SMALLINT;
DEFINE v_val_inconsis        SMALLINT;

DEFINE v_saldo_aivs4         DECIMAL(16,6);
DEFINE v_saldo_pesos4        DECIMAL(16,6);
DEFINE v_resultado4          SMALLINT;
DEFINE v_saldo_aivs41        DECIMAL(16,6);
DEFINE v_saldo_pesos41       DECIMAL(16,6);
DEFINE v_resultado41         SMALLINT;

DEFINE v_valida              SMALLINT;
DEFINE v_edo_credito         SMALLINT;       --Tipo de credito del derechohabiente
DEFINE v_tipo_trabajador     SMALLINT;       --Codigo de proceso del tipo de trabajador
DEFINE v_tpo_credito         SMALLINT;
DEFINE v_num_credito_crd     DECIMAL(10,0);
DEFINE v_f_otorga            DATE;
DEFINE v_f_liquida_cred      DATE;
DEFINE v_tpo_dscto           SMALLINT;     --25072017

DEFINE p_anio                SMALLINT;                

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--SET DEBUG FILE TO '/safreviv_req/SACI2018-115/sp_ext_pag_sin_dis.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_id_derechohabiente  = 0;
LET v_subcuenta           = 0;
LET v_folio_liquida       = 0;
LET v_id_referencia       = 0;
LET v_acc                 = 0;
LET v_pes                 = 0;
LET v_f_liquida           = "";
LET v_anio_val            = 0;
LET v_anio_pres           = YEAR(TODAY);
LET v_folio_dis           = 0;

LET v_tot_reg             = 0;

LET v_bnd_proceso         = 0;
LET v_char                = "";

LET v_status              = 0;

LET sql_err               = 0;
LET isam_err              = 0;
LET error_info            = "";

LET v_existe_his          = 0;
LET v_sel_his             = "";

LET v_tot_reg_cta_dis     = 0;
LET v_id_derecho_val      = 0;
LET v_val_pag_sin_dis     = 0;
LET v_val_inconsis        = 0;

LET v_saldo_aivs4         = 0;
LET v_saldo_pesos4        = 0;
LET v_resultado4          = 0;
LET v_saldo_aivs41        = 0;
LET v_saldo_pesos41       = 0;
LET v_resultado41         = 0;

LET v_valida              = 0;
LET v_edo_credito         = 0;
LET v_tipo_trabajador     = 0;
LET v_tpo_credito         = 0;
LET v_num_credito_crd     = 0;
LET v_f_otorga            = '';
LET v_f_liquida_cred      = '';
LET v_tpo_dscto           = 0;

  SET PDQPRIORITY HIGH;

  DROP TABLE IF EXISTS tmp_pag_sin_dis;
  CREATE TABLE tmp_pag_sin_dis (id_derechohabiente  DECIMAL(9,0),
                                subcuenta           SMALLINT,
                                folio_liquida       DECIMAL(9,0),
                                id_referencia       DECIMAL(9,0),
                                monto_acciones      DECIMAL(16,6),
                                monto_pesos         DECIMAL(12,2),
                                f_liquida           DATE)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  {DROP TABLE IF EXISTS tmp_dis_fra_psd;
  CREATE TABLE tmp_dis_fra_psd (folio_act DECIMAL(9,0))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  DROP TABLE IF EXISTS tmp_dis_reg_psd_f;
  CREATE TABLE tmp_dis_reg_psd_f (id_derechohabiente  DECIMAL(9,0),
                                  subcuenta           SMALLINT,
                                  folio_liquida       DECIMAL(9,0),
                                  monto_acciones      DECIMAL(16,6),
                                  monto_pesos         DECIMAL(12,2))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  DROP TABLE IF EXISTS tmp_dis_info_inconsis;
  CREATE TABLE tmp_dis_info_inconsis (folio_liquida       DECIMAL(9,0),
                                      id_referencia       DECIMAL(9,0))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;
  
  SELECT a.id_referencia,
         a.folio_liquida
  FROM   dis_info_inconsistente a
  WHERE  a.tpo_inconsistente IN (0,10)
  INTO TEMP tmp_dis_info_inconsis_p;

  INSERT INTO tmp_dis_info_inconsis
  SELECT *
  FROM   tmp_dis_info_inconsis_p;

  DROP TABLE IF EXISTS tmp_dis_info_inconsis_p;

  CREATE INDEX x1tmp_dis_info_inconsis ON tmp_dis_info_inconsis
  (folio_liquida, id_referencia) IN dis_ix_dbs;

  UPDATE statistics FOR TABLE tmp_dis_info_inconsis;}

  --Extrae movimientos de abono por registro de pagos
  FOREACH
    SELECT a.tabla, a.anio
    INTO   v_nombre_tabla, v_anio
    FROM   cat_tab_movimiento a

    LET v_sel_his = v_sel_his || " SELECT a.id_derechohabiente,      "||
                                 "        a.subcuenta,               "||
                                 "        a.folio_liquida,           "||
                                 "        a.id_referencia,           "||
                                 "        a.monto_acciones * -1 acc, "||
                                 "        a.monto_pesos * -1 pes,    "||
                                 "        a.f_liquida,               "||
                                          v_anio || " anio           "||        
                                 " FROM   "|| v_nombre_tabla || " a  "||
                                 --" WHERE  1 = 1                      "||
                                 " WHERE  a.folio_liquida = 9272     "||
                                 " AND    a.subcuenta    IN (4,41)   "||
                                 " AND    a.movimiento   IN (1,41)   "||
                                 " UNION ALL ";
    LET v_existe_his = 1;
  END FOREACH;

  LET v_sel_act = " SELECT a.id_derechohabiente,         "||
                  "        a.subcuenta,                  "||
                  "        a.folio_liquida,              "||
                  "        a.id_referencia,              "||
                  "        a.monto_acciones * -1 acc,    "||
                  "        a.monto_pesos * -1 pes,       "||
                  "        a.f_liquida,                  "||
                           v_anio_pres || " anio         "||       
                  " FROM   cta_movimiento a              "||
                  --" WHERE  1 = 1                         "||
                  " WHERE  a.folio_liquida      = 9272   "|| 
                  " AND    a.subcuenta         IN (4,41) "||
                  " AND    a.movimiento        IN (1,41) "||
                  " INTO TEMP tmp_mov_pag_sin_dis; ";

  IF v_existe_his = 1 THEN
     LET v_sel_his = v_sel_his|| v_sel_act ;
  ELSE
     LET v_sel_his = v_sel_act ;
  END IF

  EXECUTE IMMEDIATE v_sel_his;

  CREATE INDEX xie1tmp_mov_pag_sin_dis2 ON tmp_mov_pag_sin_dis
  (folio_liquida) IN dis_ix_dbs;
  
  UPDATE statistics FOR TABLE tmp_mov_pag_sin_dis;

  {SELECT UNIQUE a.folio, YEAR(a.f_actualiza) anio_dis, b.folio_liquida
  FROM   glo_folio a, tmp_mov_pag_sin_dis b
  WHERE  a.folio_referencia = b.folio_liquida
  AND    a.proceso_cod      = 901
  AND    a.status           = 2
  INTO TEMP tmp_dis_fol_ref_psd;
  
  CREATE INDEX xie1tmp_dis_fol_ref_psd1 ON tmp_dis_fol_ref_psd
  (folio) IN dis_ix_dbs;

  CREATE INDEX xie1tmp_dis_fol_ref_psd2 ON tmp_dis_fol_ref_psd
  (anio_dis) IN dis_ix_dbs;

  CREATE INDEX xie1tmp_dis_fol_ref_psd3 ON tmp_dis_fol_ref_psd
  (folio_liquida) IN dis_ix_dbs;

  UPDATE statistics FOR TABLE tmp_dis_fol_ref_psd;

  LET v_sel_his    = "";
  LET v_existe_his = 0;
  LET v_sel_act    = "";
  LET v_anio_val   = 0;
  LET v_folio_dis  = 0;

  FOREACH
    SELECT a.tabla, b.folio 
    INTO   v_nombre_tabla, v_folio_dis
    FROM   tmp_dis_fol_ref_psd b, 
    OUTER  cat_tab_movimiento a 
    WHERE  a.anio = b.anio_dis
    ORDER BY 1

    IF v_nombre_tabla IS NULL THEN
       INSERT INTO tmp_dis_fra_psd VALUES (v_folio_dis);
    END IF
  END FOREACH;

  CREATE INDEX xie1tmp_dis_fra_psd ON tmp_dis_fra_psd (folio_act) IN dis_ix_dbs;

  UPDATE statistics FOR TABLE tmp_dis_fra_psd;

  LET v_sel_his    = "";
  LET v_existe_his = 0;
  LET v_sel_act    = "";
  LET v_anio_val   = 0;
  LET v_folio_dis  = 0;
  
  FOREACH
    SELECT a.tabla 
    INTO   v_nombre_tabla
    FROM   cat_tab_movimiento a

    LET v_sel_his = v_sel_his || " SELECT a.id_derechohabiente,                           "||
                                 "        a.subcuenta,                                    "||
                                 "        a.folio_liquida,                                "||
                                 "        a.monto_acciones,                               "||
                                 "        a.monto_pesos                                   "||
                                 " FROM   "|| v_nombre_tabla || " a, tmp_dis_fol_ref_psd b"||
                                 " WHERE  a.folio_liquida      = b.folio                  "||
                                 " UNION ALL ";
    LET v_existe_his = 1;
  END FOREACH;

  LET v_sel_act = " SELECT a.id_derechohabiente,                           "||
                  "        a.subcuenta,                                    "||
                  "        a.folio_liquida,                                "||
                  "        a.monto_acciones,                               "||
                  "        a.monto_pesos                                   "||
                  " FROM   cta_movimiento a, tmp_dis_fra_psd b             "||
                  " WHERE  a.folio_liquida      = b.folio_act              "||
                  " INTO TEMP tmp_dis_reg_psd; ";

  IF v_existe_his = 1 THEN
     LET v_sel_his = v_sel_his|| v_sel_act ;
  ELSE
     LET v_sel_his = v_sel_act ;
  END IF

  EXECUTE IMMEDIATE v_sel_his;

  UPDATE statistics FOR TABLE tmp_dis_reg_psd;

  INSERT INTO tmp_dis_reg_psd_f
  SELECT *
  FROM   tmp_dis_reg_psd;

  UPDATE statistics FOR TABLE tmp_dis_reg_psd_f;

  DROP TABLE IF EXISTS tmp_dis_reg_psd;
  
  CREATE INDEX xie1tmp_dis_reg_psd_f1 ON tmp_dis_reg_psd_f
  (folio_liquida, id_derechohabiente, subcuenta) IN cta_3ix_dbs;

  UPDATE statistics FOR TABLE tmp_dis_reg_psd_f;

  DELETE
  FROM tmp_dis_fol_ref_psd
  WHERE folio NOT IN (SELECT folio_liquida 
                      FROM   tmp_dis_reg_psd_f);

  LET v_folio_dis = 0;
  
  FOREACH
    SELECT a.id_derechohabiente,
           a.subcuenta,
           a.folio_liquida,
           a.id_referencia,
           a.acc,
           a.pes,
           a.f_liquida,
           a.anio,
           b.folio
           --NVL(b.folio,0)
    INTO   v_id_derechohabiente,
           v_subcuenta,
           v_folio_liquida,
           v_id_referencia,
           v_acc,
           v_pes,
           v_f_liquida,
           v_anio_val,
           v_folio_dis
    FROM   tmp_mov_pag_sin_dis a,
    OUTER  tmp_dis_fol_ref_psd b
    WHERE  a.folio_liquida = b.folio_liquida

    LET v_tot_reg        = 0;
    LET v_id_derecho_val = 0;

    LET v_saldo_aivs4    = 0;
    LET v_saldo_pesos4   = 0;
    LET v_resultado4     = 0;
    LET v_saldo_aivs41   = 0;
    LET v_saldo_pesos41  = 0;
    LET v_resultado41    = 0;

    IF v_folio_dis  = 0    OR
       v_folio_dis IS NULL THEN
       LET v_folio_dis = 0;
    END IF

    IF v_subcuenta = 4 THEN
       EXECUTE FUNCTION fn_saldo_dia(NULL, v_id_derechohabiente, 4, TODAY) INTO v_resultado4, v_saldo_aivs4, v_saldo_pesos4;
       IF v_saldo_aivs4 <= 0 THEN
          CONTINUE FOREACH;
       END IF
    END IF

    IF v_subcuenta = 41 THEN   
       EXECUTE FUNCTION fn_saldo_dia(NULL, v_id_derechohabiente, 41, TODAY) INTO v_resultado41, v_saldo_aivs41, v_saldo_pesos41;
       IF v_saldo_aivs41 <= 0 THEN
          CONTINUE FOREACH;
       END IF
    END IF 
    
    --Identifica dispersión de pago
    SELECT COUNT(*)
    INTO   v_val_pag_sin_dis
    FROM   tmp_dis_reg_psd_f a
    WHERE  a.folio_liquida      = v_folio_dis  
    AND    a.id_derechohabiente = v_id_derechohabiente
    AND    a.subcuenta          = v_subcuenta;
    IF v_val_pag_sin_dis  = 0    OR 
       v_val_pag_sin_dis IS NULL THEN

       --Valida si es un pago inconsistente por numero de crédito en cero y 
       --si ya fue liquidado
       SELECT COUNT(*)
       INTO   v_val_inconsis
       FROM   tmp_dis_info_inconsis a
       WHERE  a.folio_liquida      = v_folio_dis
       AND    a.id_referencia      = v_id_referencia;
       IF v_val_inconsis  = 0    OR 
          v_val_inconsis IS NULL THEN
          
          --No tiene dispersión de pago
          INSERT INTO tmp_pag_sin_dis VALUES(v_id_derechohabiente,
                                             v_subcuenta,
                                             v_folio_liquida,
                                             v_id_referencia,
                                             v_acc,
                                             v_pes,
                                             v_f_liquida);
       END IF
    END IF
  END FOREACH;}


  FOREACH
    SELECT a.id_derechohabiente,
           a.subcuenta,
           a.folio_liquida,
           a.id_referencia,
           a.acc,
           a.pes,
           a.f_liquida,
           a.anio
    INTO   v_id_derechohabiente,
           v_subcuenta,
           v_folio_liquida,
           v_id_referencia,
           v_acc,
           v_pes,
           v_f_liquida,
           v_anio_val
    FROM   tmp_mov_pag_sin_dis a

    --No tiene dispersión de pago
    INSERT INTO tmp_pag_sin_dis VALUES(v_id_derechohabiente,
                                       v_subcuenta,
                                       v_folio_liquida,
                                       v_id_referencia,
                                       v_acc,
                                       v_pes,
                                       v_f_liquida);
  END FOREACH;

  CREATE INDEX x1tmp_pag_sin_dis ON tmp_pag_sin_dis
  (id_derechohabiente) IN dis_ix_dbs;

  CREATE INDEX x2tmp_pag_sin_dis ON tmp_pag_sin_dis
  (folio_liquida) IN dis_ix_dbs;

  CREATE INDEX x3tmp_pag_sin_dis ON tmp_pag_sin_dis
  (subcuenta) IN dis_ix_dbs;

  UPDATE STATISTICS FOR TABLE tmp_pag_sin_dis;
  
  --TRACE 'Finaliza fn_dis_ext_ava_pag con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado identificación Pagos sin Dispersión";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


