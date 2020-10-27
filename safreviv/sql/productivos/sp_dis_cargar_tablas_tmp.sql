






CREATE PROCEDURE "safreviv".sp_dis_cargar_tablas_tmp()
 
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 15062018
--Declaración de variables
DEFINE v_nss                 CHAR(11);
DEFINE v_num_credito         DECIMAL(10,0);
DEFINE v_periodo_pago        CHAR(6);
DEFINE v_f_pago              DATE;
--DEFINE v_ent_recaudadora		CHAR(3);
DEFINE v_nrp                 CHAR(11);
DEFINE v_aportacion          DECIMAL(12,2);
DEFINE v_amortizacion        DECIMAL(12,2);
DEFINE v_folio_sua           DECIMAL(6,0);

DEFINE v_pp_par              CHAR(6);
DEFINE v_pp_non              CHAR(6);

DEFINE v_id_derechohabiente  DECIMAL(9,0);
DEFINE v_id_derecho_h_nuevo  DECIMAL(9,0);

DEFINE v_estado              SMALLINT;
DEFINE v_desc_resultado      CHAR(40);

DEFINE v_folio_liquida       DECIMAL(9,0);

DEFINE v_bnd_transaccion     SMALLINT;
DEFINE v_bnd_proceso         SMALLINT;       --Estatus del proceso
DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER;
DEFINE isam_err              INTEGER;
DEFINE error_info            CHAR(70);
DEFINE v_char                CHAR(70);
DEFINE v_cnt_disp            SMALLINT;
DEFINE v_cnt_no_disp         SMALLINT;

DEFINE v_id_dis_arh_num_cred DECIMAL(9,0);

DEFINE v_folio_glo           DECIMAL(9,0);
DEFINE v_bnd_existe_inconsis SMALLINT;
DEFINE v_bnd_existe_inconsis_0  SMALLINT;
DEFINE v_bnd_existe_inconsis_10 SMALLINT;
DEFINE v_folio_dii           DECIMAL(9,0);

DEFINE v_bnd_existe          SMALLINT;
DEFINE v_num_pagos           SMALLINT;
DEFINE v_bnd_existe_folio_disp SMALLINT;
DEFINE v_bnd_mov_div_disp    DECIMAL(6,0);
DEFINE v_bnd_reg_duplicado   INTEGER;
DEFINE v_bnd_existe_nrp      SMALLINT;
DEFINE v_bnd_existe_info_inc SMALLINT;

DEFINE v_status_pag          SMALLINT;
DEFINE isam_err_pag          INTEGER;
DEFINE error_info_pag        CHAR(70);
DEFINE v_bnd_nss_ok          SMALLINT;
DEFINE v_bnd_folio_sua_ok    SMALLINT;
DEFINE v_bnd_periodo_pago_ok SMALLINT;
DEFINE v_bnd_f_pago_ok       SMALLINT;
DEFINE v_bnd_nrp_ok          SMALLINT;
DEFINE v_bnd_pago_ok         SMALLINT;

DEFINE v_tmp_id              DECIMAL(9,0); 
DEFINE v_tmp_f_liquida       DATE; 
DEFINE v_tmp_mov             SMALLINT;
DEFINE v_folio_cta_hp        DECIMAL(9,0); 
DEFINE v_id_referencia_cta_hp DECIMAL(9,0);
DEFINE v_id_dh_aux           DECIMAL(9,0);
DEFINE v_ind_liquidacion     SMALLINT;
DEFINE v_origen_archivo      SMALLINT;

DEFINE v_tot_pagos           SMALLINT;

DEFINE v_bnd_transaccion_mov SMALLINT;
DEFINE v_status_mov          SMALLINT;
DEFINE error_info_mov        CHAR(70);

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

  --SET DEBUG FILE TO '/safreviv_int/dis/ERR_carga_tablas_tmp.TRACE';
  --TRACE ON;

  --#Inicialización de variables
  LET v_nss                    = "";
  LET v_num_credito            = "";
  LET v_periodo_pago           = "";
  LET v_f_pago                 = "";
  LET v_nrp                    = "";
  LET v_aportacion             = 0;
  LET v_amortizacion           = 0;
  LET v_folio_sua              = "";
  
  LET v_pp_par                 = "";
  LET v_pp_non                 = "";

  LET v_f_pago                 = "";
  LET v_f_pago                 = "";
  LET v_status_pag             = 0;
  LET isam_err_pag             = 0;
  LET error_info_pag           = "";
  LET v_bnd_nss_ok             = 0;
  LET v_bnd_folio_sua_ok       = 0;
  LET v_bnd_periodo_pago_ok    = 0;
  LET v_bnd_f_pago_ok          = 0;
  LET v_bnd_nrp_ok             = 0;
  LET v_bnd_pago_ok            = 0;

  LET v_id_derechohabiente     = 0;
  LET v_id_derecho_h_nuevo     = 0;
  LET v_estado                 = 0;
  LET v_desc_resultado         = "";

  LET v_bnd_proceso            = 0; --Estado correcto
  LET v_folio_liquida          = 0;
  LET v_status                 = 0;
   
  LET sql_err                  = 0;
  LET isam_err                 = 0;
  LET error_info               = "";

  LET v_cnt_disp               = 0;
  LET v_cnt_no_disp            = 0;
  LET v_id_dis_arh_num_cred    = 0;

  LET v_folio_cta_hp           = "";
  LET v_id_referencia_cta_hp   = "";
  LET v_folio_glo              = "";
  LET v_bnd_existe_inconsis    = 0;
  LET v_bnd_existe_inconsis_0  = 0;
  LET v_bnd_existe_inconsis_10 = 0;
  LET v_folio_dii              = 0;
  LET v_bnd_existe             = 0;
  LET v_num_pagos              = 0;
  LET v_bnd_existe_folio_disp  = 0;
  LET v_bnd_mov_div_disp       = 0;
  LET v_bnd_reg_duplicado      = 0;
  LET v_bnd_existe_nrp         = 0;
  LET v_bnd_existe_info_inc    = 0;
   
  LET v_tmp_id                 = 0;
  LET v_tmp_f_liquida          = "";  
  LET v_tmp_mov                = 0;
  LET v_id_dh_aux              = 0;
  LET v_ind_liquidacion        = 0;
  LET v_origen_archivo         = 0;
   
  LET v_tot_pagos              = 0;
   
  LET v_bnd_transaccion_mov    = 0;
  LET v_status_mov             = 0;
  LET error_info_mov           = "";
   
  DROP TABLE IF EXISTS tmp_dis_pagos_0;
  CREATE TABLE tmp_dis_pagos_0 (id_derechohabiente DECIMAL(9,0),
                                folio_sua          DECIMAL(6,0),
                                periodo_pago       CHAR(06),
                                f_pago             DATE,
                                nrp                CHAR(11),
                                folio              DECIMAL(9,0),
                                id_referencia      DECIMAL(9,0),
                                ind_liquidacion    SMALLINT,
                                origen_archivo     SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs; 

  DROP TABLE IF EXISTS tmp_cta_movimiento;
  CREATE TABLE tmp_cta_movimiento(id_derechohabiente DECIMAL(9,0),
                                  folio_liquida      DECIMAL(9,0),
                                  id_referencia      DECIMAL(9,0),
                                  f_liquida          DATE,
                                  movimiento         SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;
   
  SET PDQPRIORITY HIGH;

  --SET INDEXES FOR tmp_dis_pagos DISABLED;
  --SET INDEXES FOR tmp_cta_movimiento DISABLED;

  DROP TABLE IF EXISTS tmp_id_nss;
   
  SELECT DISTINCT NVL((afi.id_derechohabiente),"999999999") as id_derechohabiente,
         TRIM(cred_cero.nss) as nss
  FROM   safre_tmp:tmp_dis_cred_cero1 cred_cero,
  OUTER  afi_derechohabiente afi
  WHERE  cred_cero.nss = afi.nss
  INTO TEMP tmp_id_nss;

  DROP TABLE IF EXISTS tmp_dis_id_der_nuevo;
  
  SELECT UNIQUE a.id_derechohabiente id_der_viejo, b.id_derechohabiente, b.nss
  FROM   cta_pag_complemento a,
         tmp_id_nss b
  WHERE  a.id_derhab_nuevo = b.id_derechohabiente
  INTO TEMP tmp_dis_id_der_nuevo;

  FOREACH
    SELECT id_derechohabiente, nss
    INTO   v_id_derechohabiente, v_nss
    FROM   tmp_id_nss	   

    LET v_estado = 0;
    LET v_tmp_id = 0;

    FOREACH
      SELECT cta.id_derechohabiente, 
             cta.folio_sua,
             cta.periodo_pago,
             cta.f_pago,
             cta.nrp,
             cta.folio, 
             cta.id_referencia,
             cta.ind_liquidacion,
             cta.origen_archivo
      INTO   v_id_dh_aux,  
             v_folio_sua,	         
             v_periodo_pago,       	   
             v_f_pago,            	  
             v_nrp,
             v_folio_cta_hp,
             v_id_referencia_cta_hp,
             v_ind_liquidacion,
             v_origen_archivo
      FROM   cta_his_pagos cta
      WHERE  cta.id_derechohabiente  = v_id_derechohabiente
      AND    cta.ind_liquidacion   NOT IN (1,6)
		  
      INSERT INTO tmp_dis_pagos_0 VALUES(v_id_dh_aux, 
                                         v_folio_sua,	         
                                         v_periodo_pago,       	   
                                         v_f_pago,            	  
                                         v_nrp,
                                         v_folio_cta_hp,
                                         v_id_referencia_cta_hp,
                                         v_ind_liquidacion,
                                         v_origen_archivo);
    END FOREACH;
	  
    EXECUTE PROCEDURE sp_dis_mov_tmp(v_id_derechohabiente) INTO v_bnd_transaccion_mov, v_status_mov, error_info_mov;

  END FOREACH;

  FOREACH
    SELECT a.id_der_viejo, a.nss, a.id_derechohabiente
    INTO   v_id_derechohabiente, v_nss, v_id_derecho_h_nuevo
    FROM   tmp_dis_id_der_nuevo a

    LET v_estado = 0;
    LET v_tmp_id = 0;

    FOREACH
      SELECT cta.id_derechohabiente, 
             cta.folio_sua,
             cta.periodo_pago,
             cta.f_pago,
             cta.nrp,
             cta.folio, 
             cta.id_referencia,
             cta.ind_liquidacion,
             cta.origen_archivo
      INTO   v_id_dh_aux,  
             v_folio_sua,	         
             v_periodo_pago,       	   
             v_f_pago,            	  
             v_nrp,
             v_folio_cta_hp,
             v_id_referencia_cta_hp,
             v_ind_liquidacion,
             v_origen_archivo
      FROM   cta_his_pagos cta,
             cta_pag_complemento com
      WHERE  cta.folio               = com.folio
      AND    cta.id_referencia       = com.id_referencia
      AND    cta.id_derechohabiente  = v_id_derechohabiente
      AND    cta.ind_liquidacion   NOT IN (1,6)
      AND    cta.id_derechohabiente  = com.id_derechohabiente
      AND    com.id_derhab_nuevo     = v_id_derecho_h_nuevo
		  
      INSERT INTO tmp_dis_pagos_0 VALUES(v_id_derecho_h_nuevo, 
                                         v_folio_sua,	         
                                         v_periodo_pago,       	   
                                         v_f_pago,            	  
                                         v_nrp,
                                         v_folio_cta_hp,
                                         v_id_referencia_cta_hp,
                                         v_ind_liquidacion,
                                         v_origen_archivo);
    END FOREACH;
	  
    --EXECUTE PROCEDURE sp_dis_mov_tmp(v_id_derecho_h_nuevo) INTO v_bnd_transaccion_mov, v_status_mov, error_info_mov;

  END FOREACH;
   
  CREATE INDEX xie1tmp_dis_pagos_0 ON tmp_dis_pagos_0(id_derechohabiente, folio_sua, periodo_pago, f_pago, nrp, ind_liquidacion) IN dis_ix_dbs;
  CREATE INDEX xie2tmp_dis_pagos_0 ON tmp_dis_pagos_0(folio_sua, periodo_pago, f_pago, nrp) IN dis_ix_dbs;
  CREATE INDEX xie3tmp_dis_pagos_0 ON tmp_dis_pagos_0(periodo_pago, f_pago, nrp) IN dis_ix_dbs;
  CREATE INDEX xie4tmp_dis_pagos_0 ON tmp_dis_pagos_0(folio_sua, f_pago, nrp) IN dis_ix_dbs;
  CREATE INDEX xie5tmp_dis_pagos_0 ON tmp_dis_pagos_0(folio_sua, periodo_pago, nrp) IN dis_ix_dbs;
  CREATE INDEX xie6tmp_dis_pagos_0 ON tmp_dis_pagos_0(folio_sua, periodo_pago, f_pago) IN dis_ix_dbs;
   
  CREATE INDEX xie1tmp_cta_movimientos ON tmp_cta_movimiento(id_derechohabiente, f_liquida) IN dis_ix_dbs;
   
  SET INDEXES FOR tmp_dis_pagos_0 ENABLED;
  SET INDEXES FOR tmp_cta_movimiento ENABLED;
   
  UPDATE STATISTICS FOR TABLE tmp_dis_pagos_0;
  UPDATE STATISTICS FOR TABLE tmp_cta_movimiento;
   
  --TRACE 'Finaliza sp_dis_cargar_tablas_tmp '||v_bnd_proceso;
  LET v_char = "Termina Carga de Tablas Temporales";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


