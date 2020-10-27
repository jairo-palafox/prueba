






create procedure "safreviv".fn_compensa_int_hs()

define v_folio_dis           decimal(9,0);
define v_id_derechohabiente  decimal(9,0);
define v_id_dis_det_avance   decimal(9,0);
define v_num_credito         decimal(10,0);
define v_periodo_pago        char(8);
define v_nrp                 char(11);
define v_monto_apo_avance    decimal(12,2);
define v_monto_amo_avance    decimal(12,2);
define v_monto_apo_pag       decimal(12,2);
define v_monto_amo_pag       decimal(12,2);
define v_edo_compensa_apo    smallint;
define v_edo_compensa_amo    smallint;
define v_folio_liquida       decimal(9,0);
define v_imp_ap_pat          decimal(12,2);
define v_imp_am_cre          decimal(12,2);
define v_aiv_ap_pat          decimal(18,6);
define v_monto_aportacion    decimal(12,2);
define v_monto_amortizacion  decimal(12,2);
define v_monto_dif_apo       decimal(12,2);
define v_monto_dif_amo       decimal(12,2);
define tmp_id_dis_det_avance decimal(9,0);

let v_folio_dis           = 0;
let v_id_derechohabiente  = 0; 
let v_id_dis_det_avance   = 0;  
let v_num_credito         = 0;
let v_periodo_pago        = "";
let v_nrp                 = "";
let v_monto_apo_avance    = 0;
let v_monto_amo_avance    = 0; 
let v_monto_apo_pag       = 0;  
let v_monto_amo_pag       = 0;  
let v_edo_compensa_apo    = 0; 
let v_edo_compensa_amo    = 0; 
let v_folio_liquida       = 0; 
let v_imp_ap_pat          = 0;  
let v_imp_am_cre          = 0;  
let v_aiv_ap_pat          = 0;
let v_monto_aportacion    = 0;
let v_monto_amortizacion  = 0;
let v_monto_dif_apo       = 0;
let v_monto_dif_amo       = 0;
let tmp_id_dis_det_avance = 0;

SET PDQPRIORITY HIGH; 

{create table tmp_compensa_avance (id_derechohabiente  decimal(9,0),
                                  id_dis_det_avance   decimal(9,0),
                                  folio_dis           decimal(9,0),
                                  folio_liquida       decimal(9,0),
                                  num_credito         decimal(10,0),
                                  periodo_pago        char(8),
                                  nrp                 char(11),
                                  monto_apo_avance    decimal(12,2),
                                  monto_amo_avance    decimal(12,2),
                                  monto_apo_pag       decimal(12,2),
                                  monto_amo_pag       decimal(12,2),
                                  edo_compensa_apo    smallint,
                                  edo_compensa_amo    smallint,
                                  monto_aportacion    decimal(12,2),
                                  monto_amortizacion  decimal(12,2),
                                  monto_dif_apo       decimal(12,2),
                                  monto_dif_amo       decimal(12,2),
                                  imp_ap_pat          decimal(12,2),
                                  imp_am_cre          decimal(12,2),
                                  aiv_ap_pat          decimal(18,6));}
FOREACH
  SELECT comp.folio_dis,
         comp.id_derechohabiente,
         comp.num_credito,
         comp.periodo_pago,
         comp.nrp,
         comp.id_dis_det_avance_pago,
         comp.monto_apo_avance,
         comp.monto_amo_avance,
         comp.monto_apo_pag,
         comp.monto_amo_pag, 
         comp.edo_compensa_apo,
         comp.edo_compensa_amo,
         ava.monto_aportacion,
         ava.monto_amortizacion,
         ava.monto_dif_apo,
         ava.monto_dif_amo
  INTO   v_folio_dis,
         v_id_derechohabiente,
         v_num_credito,
         v_periodo_pago,
         v_nrp,
         v_id_dis_det_avance,
         v_monto_apo_avance,
         v_monto_amo_avance,
         v_monto_apo_pag,
         v_monto_amo_pag,
         v_edo_compensa_apo,
         v_edo_compensa_amo,
         v_monto_aportacion,
         v_monto_amortizacion,
         v_monto_dif_apo,
         v_monto_dif_amo
  FROM   dis_compensa_avance comp,
         dis_det_avance_pago ava
  WHERE  comp.folio_dis              = 20164
  AND    comp.id_dis_det_avance_pago = ava.id_dis_det_avance_pago

  FOREACH
    SELECT hs.folio_liquida,
           hs.imp_ap_pat,
           hs.imp_am_cre,
           hs.aiv_ap_pat
    INTO   v_folio_liquida,
           v_imp_ap_pat,
           v_imp_am_cre,
           v_aiv_ap_pat
    FROM   dis_interface_hs hs
    WHERE  hs.folio_liquida      = 20191
    AND    hs.id_derechohabiente = v_id_derechohabiente
    AND    hs.periodo_pago       = v_periodo_pago
    AND    hs.nrp                = v_nrp
    IF DBINFO('sqlca.sqlerrd2') == 0 THEN
       CONTINUE FOREACH;
    ELSE
       SELECT tmp.id_dis_det_avance
       INTO   tmp_id_dis_det_avance
       FROM   tmp_compensa_avance tmp
       WHERE  tmp.id_dis_det_avance = v_id_dis_det_avance;
       IF DBINFO('sqlca.sqlerrd2') == 0 THEN
          INSERT INTO tmp_compensa_avance VALUES(v_id_derechohabiente,
                                                 v_id_dis_det_avance,
                                                 v_folio_dis,
                                                 v_folio_liquida,
                                                 v_num_credito,
                                                 v_periodo_pago,
                                                 v_nrp,
                                                 v_monto_apo_avance,
                                                 v_monto_amo_avance,
                                                 v_monto_apo_pag,
                                                 v_monto_amo_pag,
                                                 v_edo_compensa_apo,
                                                 v_edo_compensa_amo,
                                                 v_monto_aportacion,
                                                 v_monto_amortizacion,
                                                 v_monto_dif_apo,
                                                 v_monto_dif_amo,
                                                 v_imp_ap_pat,
                                                 v_imp_am_cre,
                                                 v_aiv_ap_pat);                              
       END IF
    END IF 
  END FOREACH;

END FOREACH;

END PROCEDURE;


