






create procedure "safreviv".fn_compensa_ava()

define v_id_dis_det_avance   decimal(9,0);
define v_folio               decimal(9,0);
define v_periodo_pago        char(8);
define v_id_derechohabiente  decimal(9,0);
define v_tpo_avance          smallint;
define v_tpo_registro        smallint;
define v_num_credito         decimal(10,0);
define v_f_pago              date;
define v_nrp                 char(11);
define v_monto_aportacion    decimal(12,2);
define v_monto_amortizacion  decimal(12,2);
define v_monto_dif_apo       decimal(12,2);
define v_monto_dif_amo       decimal(12,2);
define v_f_presentacion      date;
define v_estado              smallint;


define v_folio_dis           decimal(9,0);


define v_monto_apo_avance    decimal(12,2);
define v_monto_amo_avance    decimal(12,2);
define v_monto_apo_pag       decimal(12,2);
define v_monto_amo_pag       decimal(12,2);
define v_edo_compensa_apo    smallint;
define v_folio_liquida       decimal(9,0);
define v_imp_ap_pat          decimal(12,2);
define v_imp_am_cre          decimal(12,2);
define v_aiv_ap_pat          decimal(18,6);
define tmp_id_dis_det_avance decimal(9,0);

let v_id_dis_det_avance   = 0;  
let v_folio               = 0;
let v_periodo_pago        = "";
let v_id_derechohabiente  = 0; 
let v_tpo_avance          = 0;
let v_tpo_registro        = 0;
let v_num_credito         = 0;
let v_f_pago              = "";
let v_nrp                 = "";
let v_monto_aportacion    = 0;
let v_monto_amortizacion  = 0;
let v_monto_dif_apo       = 0;
let v_monto_dif_amo       = 0;
let v_f_presentacion      = "";
let v_estado              = 0;

let v_folio_dis           = 0;
let v_monto_apo_avance    = 0;
let v_monto_amo_avance    = 0; 
let v_monto_apo_pag       = 0;  
let v_monto_amo_pag       = 0;  
let v_edo_compensa_apo    = 0; 
let v_folio_liquida       = 0; 
let v_imp_ap_pat          = 0;  
let v_imp_am_cre          = 0;  
let v_aiv_ap_pat          = 0;
let tmp_id_dis_det_avance = 0;

SET PDQPRIORITY HIGH; 

{create table tmp_dis_det_avance (id_dis_det_avance_pago decimal(9,0),
                                  folio                  decimal(9,0),
                                  periodo_pago           char(6) not null ,
                                  id_derechohabiente     decimal(9,0) not null ,
                                  tpo_avance             smallint not null ,
                                  tpo_registro           smallint,
                                  num_credito            decimal(10,0) not null ,
                                  f_pago                 date,
                                  nrp                    char(11) not null ,
                                  monto_aportacion       decimal(12,2),
                                  monto_amortizacion     decimal(12,2),
                                  monto_dif_apo          decimal(12,2),
                                  monto_dif_amo          decimal(12,2),
                                  f_presentacion         date,
                                  estado                 smallint not null
                                 );}

FOREACH
  SELECT ava.id_dis_det_avance_pago,
         ava.folio,
         ava.periodo_pago,
         ava.id_derechohabiente,
         ava.tpo_avance,
         ava.tpo_registro,
         ava.num_credito,
         ava.f_pago,
         ava.nrp, 
         ava.monto_aportacion,
         ava.monto_amortizacion,
         ava.monto_dif_apo,
         ava.monto_dif_amo,
         ava.f_presentacion,
         ava.estado
  INTO   v_id_dis_det_avance,
         v_folio,
         v_periodo_pago,
         v_id_derechohabiente,
         v_tpo_avance,
         v_tpo_registro,
         v_num_credito,
         v_f_pago,
         v_nrp,
         v_monto_aportacion,
         v_monto_amortizacion,
         v_monto_dif_apo,
         v_monto_dif_amo,
         v_f_presentacion,
         v_estado
  FROM   dis_compensa_avance comp,
         dis_det_avance_pago ava
  WHERE  comp.folio_dis              = 20164
  AND    comp.id_dis_det_avance_pago = ava.id_dis_det_avance_pago

         INSERT INTO tmp_dis_det_avance VALUES(v_id_dis_det_avance,
                                               v_folio,
                                               v_periodo_pago,
                                               v_id_derechohabiente,
                                               v_tpo_avance,
                                               v_tpo_registro,
                                               v_num_credito,
                                               v_f_pago,
                                               v_nrp,
                                               v_monto_aportacion,
                                               v_monto_amortizacion,
                                               v_monto_dif_apo,
                                               v_monto_dif_amo,
                                               v_f_presentacion,
                                               v_estado);


END FOREACH;

END PROCEDURE;


