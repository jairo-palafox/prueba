






CREATE PROCEDURE "safreviv".fn_val_mov_dis_as(p_folio DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 03052018
--Declaración de variables
DEFINE v_id_derechohabiente  DECIMAL(9,0);   --ID Derechohabiente
DEFINE v_folio_sua           DECIMAL(6,0);   --Folio SUA
DEFINE v_periodo_pago        CHAR(6);        --Periodo de Pago
DEFINE v_f_pago              DATE;           --Fecha de pago
DEFINE v_nrp                 CHAR(11);       --Número de registro patronal
DEFINE v_nss                 CHAR(11);       --Número de Seguridad Social
DEFINE v_imp_apo_pat         DECIMAL(12,2);  --Importe Aportaciòn Patronal
DEFINE v_imp_amo_cred        DECIMAL(12,2);  --Importe Amortización Crédito
DEFINE v_apl_apo_pat         DECIMAL(15,6);  --Num Aplic Int Viv Apo Pat INFONAV
DEFINE v_valor_apl_apo       DECIMAL(15,6);  --Valor Aplic Int Viv
DEFINE v_afore               SMALLINT;       --AFORE
DEFINE v_f_liquida           DATE;           --Fecha Liquidación
DEFINE v_edo_dispersion      SMALLINT;       --Estado Dispersión de Pagos
DEFINE v_edo_reg_pagos       SMALLINT;       --Estado Registro de Pagos
DEFINE v_tot_dispersion      SMALLINT;       --Total Dispersión de Pagos
DEFINE v_tot_reg_pagos       SMALLINT;       --Total Registro de Pagos
DEFINE v_tot_cta_mov         SMALLINT;       --Total Registros Cuenta Individual

DEFINE v_num_credito         DECIMAL(10,0);  --Número de Crédito
DEFINE v_monto_aportacion    DECIMAL(12,2);  --Importe aportaciones patronales 
DEFINE v_monto_amortizacion  DECIMAL(12,6);  --Importe amortizaciones de credito
DEFINE v_estado              SMALLINT;       --Estado de avance
DEFINE v_desc_edo_avance     CHAR(50);       --Descripción del estado de avance
DEFINE v_folio               DECIMAL(9,0);   --Número de Folio
DEFINE v_f_actualiza         DATE;           --Fecha proceso del folio   
DEFINE v_tipo_interface      CHAR(4);        --Tipo de interface

DEFINE v_bnd_proceso         SMALLINT;
DEFINE v_char                CHAR(20);

DEFINE v_status              SMALLINT;
DEFINE sql_err               INTEGER ;
DEFINE isam_err              INTEGER ;
DEFINE error_info            CHAR(70);

DEFINE v_sel_his             LVARCHAR(3000);
DEFINE v_sel_act             LVARCHAR(800);
DEFINE v_nombre_tabla        VARCHAR(20);
DEFINE v_existe_his          SMALLINT;

DEFINE v_folio_pag           DECIMAL(9,0);
DEFINE v_id_ref_pag          DECIMAL(9,0);
DEFINE v_id_der_pag          DECIMAL(9,0);   --ID Derechohabiente

ON EXCEPTION
   SET sql_err, isam_err, error_info
   LET v_status = sql_err;
   RETURN  v_status ,isam_err , error_info;
END EXCEPTION

--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/fn_val_mov_dis_as.TRACE';
--TRACE ON;

--Inicialización de variables
LET v_id_derechohabiente  = 0;
LET v_folio_sua           = 0;
LET v_periodo_pago        = "";
LET v_f_pago              = "";
LET v_nrp                 = "";
LET v_nss                 = "";
LET v_imp_apo_pat         = 0;
LET v_imp_amo_cred        = 0;
LET v_apl_apo_pat         = 0;
LET v_afore               = 0;
LET v_f_liquida           = "";
LET v_edo_dispersion      = 0;
LET v_edo_reg_pagos       = 0;
LET v_tot_dispersion      = 0;
LET v_tot_reg_pagos       = 0;
LET v_tot_cta_mov         = 0;

LET v_num_credito         = 0.00;
LET v_monto_aportacion    = 0.00;
LET v_monto_amortizacion  = 0.00;
LET v_estado              = 0;
LET v_desc_edo_avance     = "";
LET v_folio               = 0.00;
LET v_tipo_interface      = "";

LET v_bnd_proceso         = 0;
LET v_char                = "";

LET v_status              = 0;

LET sql_err               = 0;
LET isam_err              = 0;
LET error_info            = "";

LET v_existe_his          = 0;
LET v_sel_his             = "";

LET v_folio_pag           = 0;
LET v_id_ref_pag          = 0;
LET v_id_der_pag          = 0;

  SET PDQPRIORITY HIGH;

  DROP TABLE IF EXISTS tmp_dis_as_cp;
  CREATE TABLE tmp_dis_as_cp (id_derechohabiente  DECIMAL(9,0),
                              folio_sua           DECIMAL(6,0),
                              periodo_pago        CHAR(6),
                              f_pago              DATE,   
                              nrp                 CHAR(11),
                              nss                 CHAR(11),     
                              imp_apo_pat         DECIMAL(12,2),
                              imp_amo_cred        DECIMAL(12,2),
                              apl_apo_pat         DECIMAL(15,6),
                              valor_apl_apo       DECIMAL(15,6),
                              afore               SMALLINT,
                              f_liquida           DATE,
                              edo_dispersion      SMALLINT,
                              edo_reg_pagos       SMALLINT)
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  SELECT UNIQUE a.id_derechohabiente,
         a.folio_sua,
         a.periodo_pago,
         a.f_pago,
         a.reg_pat_imss,
         a.nss,
         a.imp_apo_pat,
         a.imp_amo_cred,
         a.apl_apo_pat,
         a.valor_apl_apo,
         a.afore,
         a.f_liquida
  FROM   dis_ap_subsecuente a
  WHERE (a.imp_apo_pat <> 0 AND
         a.apl_apo_pat <> 0)
  INTO TEMP tmp_dis_ap_subs_cp;

  SELECT UNIQUE id_derechohabiente
  FROM   tmp_dis_ap_subs_cp
  INTO TEMP tmp_dis_ap_su_cp_u;

  CREATE INDEX xie1tmp_dis_ap_su_cp_u1 ON tmp_dis_ap_su_cp_u
  (id_derechohabiente) IN dis_ix_dbs;

  UPDATE statistics FOR TABLE tmp_dis_ap_su_cp_u; 

  SELECT a.* 
  FROM   cta_his_pagos a,
         tmp_dis_ap_su_cp_u b
  WHERE  a.id_derechohabiente   = b.id_derechohabiente
  AND    a.ind_liquidacion NOT IN (1,6)
  AND    a.destino_ap_viv       = 2
  AND   (a.imp_ap_pat          <> 0 AND 
         a.aiv_ap_pat          <> 0)
  INTO TEMP tmp_cta_his_pag_cp;

  CREATE INDEX xie1tmp_cta_his_pag_cp ON tmp_cta_his_pag_cp
  (id_derechohabiente, folio_sua, periodo_pago, f_pago, nrp) IN dis_ix_dbs;
   
  UPDATE statistics FOR TABLE tmp_cta_his_pag_cp;

  --Extrae movimientos de cargos de créditos 43 Bis liquidados
  FOREACH
    SELECT a.tabla
    INTO   v_nombre_tabla
    FROM   cat_tab_movimiento a

    LET v_sel_his = v_sel_his || " SELECT a.* "||
                                 --" FROM   tmp_dis_ap_su_cp_u a, "|| v_nombre_tabla || " b "||
                                 " FROM   "|| v_nombre_tabla || " a, tmp_dis_ap_su_cp_u b"||
                                 " WHERE  a.id_derechohabiente = b.id_derechohabiente "||
                                 " AND    a.subcuenta         IN (4,44) "||
                                 " AND    a.movimiento         = 1 "||
                                 " UNION ALL ";
    LET v_existe_his = 1;
  END FOREACH;

  LET v_sel_act = " SELECT a.* "||
                  " FROM   cta_movimiento a, tmp_dis_ap_su_cp_u b " ||
                  " WHERE  a.id_derechohabiente = b.id_derechohabiente "||
                  " AND    a.subcuenta         IN (4,44) "||
                  " AND    a.movimiento         = 1 "||
                  " INTO TEMP tmp_mov_dis_val_as; ";

  IF v_existe_his = 1 THEN
     LET v_sel_his = v_sel_his|| v_sel_act ;
  ELSE
     LET v_sel_his = v_sel_act ;
  END IF

  EXECUTE IMMEDIATE v_sel_his;

  CREATE INDEX xie1tmp_mov_dis_val_as ON tmp_mov_dis_val_as
  (folio_liquida, id_referencia) IN dis_ix_dbs;
   
  UPDATE statistics FOR TABLE tmp_mov_dis_val_as;
  
  FOREACH
    SELECT ap.id_derechohabiente,
           ap.folio_sua,
           ap.periodo_pago,
           ap.f_pago,
           ap.reg_pat_imss,
           ap.nss,
           ap.imp_apo_pat,
           ap.imp_amo_cred,
           ap.apl_apo_pat,
           ap.valor_apl_apo,
           ap.afore,
           ap.f_liquida,
           0,
           0
    INTO   v_id_derechohabiente,
           v_folio_sua,
           v_periodo_pago,
           v_f_pago,
           v_nrp,
           v_nss,
           v_imp_apo_pat,
           v_imp_amo_cred,
           v_apl_apo_pat,
           v_valor_apl_apo,
           v_afore,
           v_f_liquida,
           v_edo_dispersion,
           v_edo_reg_pagos
    FROM   tmp_dis_ap_subs_cp ap

    LET v_edo_dispersion = 0;
    LET v_edo_reg_pagos  = 0;
    LET v_tot_dispersion = 0;
    LET v_tot_reg_pagos  = 0;

    SELECT COUNT(*)
    INTO   v_tot_dispersion
    FROM   dis_interface_ef a
    WHERE  a.id_derechohabiente = v_id_derechohabiente
    AND    a.folio_sua          = v_folio_sua
    AND    a.periodo_pago       = v_periodo_pago
    AND    a.f_pago             = v_f_pago
    AND    a.nrp                = v_nrp
    AND    a.ind_liquidacion   IN (0,1);
    IF v_tot_dispersion  = 0    OR 
       v_tot_dispersion IS NULL THEN
       LET v_edo_dispersion = 1; --Sin Dispersión

       SELECT COUNT(*)
       INTO   v_tot_dispersion
       FROM   dse_devolucion b
       WHERE  b.id_derechohabiente = v_id_derechohabiente
       AND    b.num_credito       IS NOT NULL
       AND    b.tpo_transferencia IN ('15','19')
       AND    b.origen_devolucion IN ('2','02','3','03')
       AND    b.periodo_pago       = v_periodo_pago
       AND    b.modulo_cod        IN ('dis','mao','mjv','ucm');
       IF v_tot_dispersion  = 0    OR
          v_tot_dispersion IS NULL THEN
          LET v_edo_dispersion = 2; --Sin Restitución o Devolución
       ELSE
          LET v_edo_dispersion = 4; --Con Restitución o Devolución
       END IF
    ELSE
       LET v_edo_dispersion = 3; --Con Dispersión
    END IF

    SELECT COUNT(*)
    INTO   v_tot_reg_pagos
    FROM   tmp_cta_his_pag_cp pag
    WHERE  pag.id_derechohabiente             = v_id_derechohabiente
    AND    pag.folio_sua                      = v_folio_sua
    AND    fn_bimestre_pago(pag.periodo_pago) = v_periodo_pago
    AND    pag.f_pago                         = v_f_pago
    AND    pag.nrp                            = v_nrp;
    IF v_tot_reg_pagos  = 0    OR 
       v_tot_reg_pagos IS NULL THEN
       LET v_edo_reg_pagos = 0; --Sin registro de pago
    ELSE
       FOREACH
         SELECT a.folio,
                a.id_referencia
         INTO   v_folio_pag,
                v_id_ref_pag
         FROM   tmp_cta_his_pag_cp a
         WHERE  a.id_derechohabiente             = v_id_derechohabiente
         AND    a.folio_sua                      = v_folio_sua
         AND    fn_bimestre_pago(a.periodo_pago) = v_periodo_pago
         AND    a.f_pago                         = v_f_pago
         AND    a.nrp                            = v_nrp
         ORDER BY a.folio desc, a.id_referencia

         LET v_tot_cta_mov = 0;

         SELECT COUNT(*)
         INTO   v_tot_cta_mov
         FROM   tmp_mov_dis_val_as a
         WHERE  a.folio_liquida = v_folio_pag
         AND    a.id_referencia = v_id_ref_pag;
         IF v_tot_cta_mov  = 0    OR 
            v_tot_cta_mov IS NULL THEN
            LET v_edo_reg_pagos = 0; --Sin registro en la cuenta individual
         ELSE
            LET v_edo_reg_pagos = 1; --Con registro en la cuenta individual
         END IF         
       END FOREACH;
    END IF

    IF v_edo_dispersion = 1 OR
       v_edo_dispersion = 2 THEN
        INSERT INTO tmp_dis_as_cp VALUES (v_id_derechohabiente,
                                          v_folio_sua,
                                          v_periodo_pago,
                                          v_f_pago,
                                          v_nrp,
                                          v_nss,
                                          v_imp_apo_pat,
                                          v_imp_amo_cred,
                                          v_apl_apo_pat,
                                          v_valor_apl_apo,
                                          v_afore,
                                          v_f_liquida,
                                          v_edo_dispersion,
                                          v_edo_reg_pagos);
    END IF
  END FOREACH;

  CREATE INDEX xpktmp_dis_as_cp ON tmp_dis_as_cp
  (id_derechohabiente,periodo_pago,edo_dispersion,edo_reg_pagos) IN dis_ix_dbs;

  UPDATE STATISTICS FOR TABLE tmp_dis_as_cp;
  
  --TRACE 'Finaliza fn_dis_ext_ava_pag con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado identificación control de pagos AS";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


