






CREATE PROCEDURE "safreviv".fn_ext_ava_vs_comp(p_folio DECIMAL(9,0))
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 07082018
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

--SET DEBUG FILE TO '/ds/safreviv_int/dis/respaldo/fn_ext_ava_vs_comp.TRACE';
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

  DROP TABLE IF EXISTS tmp_dis_ava_vs_comp_int;
  CREATE TABLE tmp_dis_ava_vs_comp_int (folio               DECIMAL(9,0),
                                        nss                 CHAR(11),     
                                        numero_credito      DECIMAL(10,0),
                                        periodo_pago        CHAR(6),
                                        nrp                 CHAR(11),
                                        monto_aportacion    DECIMAL(12,2),
                                        monto_amortizacion  DECIMAL(12,2),
                                        monto_apo_pag       DECIMAL(12,2),
                                        monto_amo_pag       DECIMAL(12,2),
                                        f_pago              DATE,   
                                        edo_compensa_apo    SMALLINT,
                                        edo_compensa_amo    SMALLINT,
                                        estado              CHAR(55))
  FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

  IF p_folio = 0 THEN
     SELECT av.id_dis_det_avance_pago,
            av.folio,
            av.num_credito,
            av.periodo_pago,
            av.nrp,
            av.monto_aportacion,
            av.monto_amortizacion,
            av.estado,
            af.id_derechohabiente,
            af.nss
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.id_derechohabiente = af.id_derechohabiente
     INTO TEMP tmp_dis_ava_vs_compensa;
  ELSE
     SELECT av.id_dis_det_avance_pago,
            av.folio,
            av.num_credito,
            av.periodo_pago,
            av.nrp,
            av.monto_aportacion,
            av.monto_amortizacion,
            av.estado,
            af.id_derechohabiente,
            af.nss
     FROM   dis_det_avance_pago av,
            afi_derechohabiente af
     WHERE  av.folio              = p_folio
     AND    av.id_derechohabiente = af.id_derechohabiente
     INTO TEMP tmp_dis_ava_vs_compensa;
  END IF

  CREATE INDEX xdis_ava_vs_comp ON tmp_dis_ava_vs_compensa(id_dis_det_avance_pago) in dis_ix_dbs;
  UPDATE STATISTICS FOR TABLE tmp_dis_ava_vs_compensa;

  INSERT INTO tmp_dis_ava_vs_comp_int 
  SELECT av.folio,
         av.nss,
         av.num_credito,
         av.periodo_pago,
         av.nrp,
         av.monto_aportacion,
         av.monto_amortizacion,
         ca.monto_apo_pag,
         ca.monto_amo_pag,
         ca.f_pago,
         ca.edo_compensa_apo,
         ca.edo_compensa_amo,
         av.estado || '-' || ce.desc_edo_avance estado
  FROM   tmp_dis_ava_vs_compensa av,
  OUTER  dis_compensa_avance ca,
         cat_edo_avance_pago ce
  WHERE  av.id_dis_det_avance_pago = ca.id_dis_det_avance_pago
  AND    ca.id_dis_det_avance_pago = av.id_dis_det_avance_pago
  AND    av.estado                 = ce.cod_edo_avance;

  CREATE INDEX xtmp_dis_ava_vs_comp_int ON tmp_dis_ava_vs_comp_int(folio, nss) in dis_ix_dbs;
  UPDATE STATISTICS FOR TABLE tmp_dis_ava_vs_comp_int;
   
  --TRACE 'Finaliza fn_ext_ava_vs_comp con valor '||v_bnd_proceso;
   
  LET v_char = "Terminado identificación ava pag vs ava comp";
  RETURN v_bnd_proceso , 0 , v_char;

END PROCEDURE;


