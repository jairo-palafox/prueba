






CREATE PROCEDURE "safreviv".sp_dis_transaccion23 (p_id_derechohabiente    DECIMAL(9,0) ,
                                       p_folio_liquida         DECIMAL(9,0) ,
                                       p_num_credito           DECIMAL(10,0),
                                       p_tpo_originacion       SMALLINT     ,
                                       p_tpo_credito           SMALLINT     ,
                                       p_f_otorga              DATE         ,
                                       p_f_liquida_cred        DATE         ,
                                       p_edo_credito           SMALLINT     ,
                                       p_origen_num_credito    SMALLINT     ,
                                       p_destino_ap_viv        CHAR(1)      ,
                                       p_folio_reg_pagos       DECIMAL(9,0) ,
                                       p_id_ref_reg_pagos      DECIMAL(9,0) , --Identificador de referencia del histórico de pagos
                                       p_folio_sua             DECIMAL(6,0) ,
                                       p_periodo_pago          CHAR(6)      ,
                                       p_f_pago                DATE         ,
                                       p_nrp                   CHAR(11)     ,
                                       p_imp_ap_pat            DECIMAL(12,2), -- importe del aporte patronal
                                       p_imp_am_cre            DECIMAL(12,2), -- importe de la amortización del crédito
                                       p_aiv_ap_pat            DECIMAL(18,6)) -- Aivs del aporte patronal
RETURNING SMALLINT, SMALLINT, CHAR(70);

--Última modificación 25042019
--Declaracion de variables
DEFINE v_status          SMALLINT;
DEFINE sql_err           INTEGER;
DEFINE isam_err          INTEGER;
DEFINE error_info        CHAR(70);
DEFINE v_char            CHAR(20);
DEFINE v_bnd_proceso     SMALLINT; --Estatus del proceso
DEFINE v_bnd_transaccion SMALLINT;

ON EXCEPTION
   SET sql_err, isam_err, error_info
      LET v_status = sql_err;
   RETURN v_status ,isam_err , error_info;
END EXCEPTION
   
  LET v_bnd_proceso = 0; --Estado correcto
  
  INSERT INTO dis_pre_his_trans(id_derechohabiente,
                                folio_liquida,
                                num_credito,
                                tpo_originacion,
                                tpo_credito,
                                f_otorga,
                                f_liquida_cred,
                                edo_credito,
                                origen_num_credito,
                                destino_ap_viv,
                                folio_reg_pagos,
                                id_ref_reg_pagos,
                                folio_sua,
                                periodo_pago,
                                f_pago,
                                nrp,
                                imp_ap_pat,
                                imp_am_cre,
                                aiv_ap_pat) 
  VALUES                       (p_id_derechohabiente,
                                p_folio_liquida,
                                p_num_credito,
                                p_tpo_originacion,
                                p_tpo_credito,
                                p_f_otorga,
                                p_f_liquida_cred,
                                p_edo_credito,
                                p_origen_num_credito,
                                p_destino_ap_viv,
                                p_folio_reg_pagos,
                                p_id_ref_reg_pagos,
                                p_folio_sua,
                                p_periodo_pago,
                                p_f_pago,
                                p_nrp,
                                p_imp_ap_pat,
                                p_imp_am_cre,
                                p_aiv_ap_pat);
										   
  LET error_info = "Terminada transacción 23 correctamente";
RETURN v_bnd_proceso, 0, error_info;

END PROCEDURE;


