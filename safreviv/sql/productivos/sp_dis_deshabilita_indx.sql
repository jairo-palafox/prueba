






CREATE PROCEDURE "safreviv".sp_dis_deshabilita_indx(p_proceso_cod_reg_pago SMALLINT,     --Codigo proceso de registro de pagos
                                         p_folio_reg_pag        DECIMAL(9,0), --Folio de registro de pagos
                                         p_folio_disp           DECIMAL(9,0), --Folio dispersión de pagos 
                                         p_masivo               SMALLINT,     --Indicador de proceso masivo (0 No Masivo, 1 Masivo)
                                         p_tpo_pre_liquida      SMALLINT,     --Tipo de pre liquidación
                                         p_acc_ind              SMALLINT)     --Acción índices

DEFINE v_tot_registros          DECIMAL(10,0);  --Total de registros del sumario
DEFINE v_limite_reg             DECIMAL(10,0);  --Límite registros
DEFINE v_masivo_habilita        SMALLINT;
DEFINE v_tpo_pre_hab            SMALLINT;
                                     
--Última modificación 25042019
--#Inicialización de variables
  LET v_tot_registros   = 0;
  LET v_limite_reg      = 1000000;
  LET v_masivo_habilita = 0;
  LET v_tpo_pre_hab     = 0;

  --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_dis_deshabilita_indx.--TRACE';
  --TRACE 'Folio '||p_folio_reg_pag;

  SET PDQPRIORITY HIGH;
  
  --Deshabilita Índices
  IF p_acc_ind = 0 THEN
     DROP TABLE IF EXISTS dis_preliquida;
     DROP TABLE IF EXISTS dis_pre_his_trans;
     DROP TABLE IF EXISTS dis_pre_interface_hs;
 
     INSERT INTO dis_masivo VALUES (p_folio_disp,
                                    p_masivo,
                                    p_tpo_pre_liquida);

     SET INDEXES FOR dis_info_inconsistente DISABLED;
     SET INDEXES FOR dis_interface_ef_ad DISABLED;
     SET INDEXES FOR dis_compensa_avance DISABLED;
     SET INDEXES FOR dis_cta_ind_pau DISABLED;
     SET INDEXES FOR dis_crd_tramite DISABLED;
     SET INDEXES FOR dis_crd_ceros DISABLED;

     CREATE TABLE dis_preliquida(f_liquida          DATE NOT NULL,
                                 id_derechohabiente DECIMAL(9,0) NOT NULL,
                                 subcuenta          SMALLINT NOT NULL,
                                 fondo_inversion    SMALLINT NOT NULL,
                                 movimiento         SMALLINT NOT NULL,
                                 folio_liquida      DECIMAL(9,0) NOT NULL,
                                 id_referencia      DECIMAL(9,0) NOT NULL,
                                 monto_acciones     DECIMAL(22,2),
                                 monto_pesos        DECIMAL(22,2),
                                 f_valor            DATE,
                                 f_registro         DATE,
                                 h_registro         DATETIME HOUR TO SECOND,
                                 origen             CHAR(20))
     FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

     CREATE TABLE dis_pre_interface_hs (id_dis_interface_hs DECIMAL(9,0),
                                        id_derechohabiente  DECIMAL(9,0),
                                        folio_sua           DECIMAL(6,0),
                                        periodo_pago        CHAR(6),
                                        f_pago              DATE,
                                        nrp                 CHAR(11),
                                        folio_liquida       DECIMAL(9,0) NOT NULL,
                                        num_crd_ifv         DECIMAL(10,0),
                                        imp_ap_pat          DECIMAL(12,2),
                                        imp_am_cre          DECIMAL(12,2),
                                        aiv_ap_pat          DECIMAL(18,6),
                                        tipo_hs             SMALLINT)
     FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;

     CREATE TABLE dis_pre_his_trans (id_derechohabiente DECIMAL(9,0) NOT NULL,
                                     folio_liquida      DECIMAL(9,0) NOT NULL,
                                     num_credito        DECIMAL(10,0),
                                     tpo_originacion    SMALLINT,
                                     tpo_credito        SMALLINT,
                                     f_otorga           DATE,
                                     f_liquida_cred     DATE,
                                     edo_credito        SMALLINT,
                                     origen_num_credito SMALLINT,
                                     destino_ap_viv     CHAR(1),
                                     folio_reg_pagos    DECIMAL(9,0),
                                     id_ref_reg_pagos   DECIMAL(9,0),
                                     folio_sua          DECIMAL(6,0),
                                     periodo_pago       CHAR(6),
                                     f_pago             DATE,
                                     nrp                CHAR(11),
                                     imp_ap_pat         DECIMAL(12,2),
                                     imp_am_cre         DECIMAL(12,2),
                                     aiv_ap_pat         DECIMAL(18,6))
     FRAGMENT BY ROUND ROBIN dis_1_dbs, dis_2_dbs;
  ELSE --Habilita Índices
     CREATE INDEX xdis_pre_interface_hs2 ON dis_pre_interface_hs (folio_liquida) IN dis_ix_dbs;
     UPDATE STATISTICS FOR TABLE dis_pre_interface_hs;

     CREATE INDEX xdis_pre_his_trans2 ON dis_pre_his_trans (folio_liquida) IN dis_ix_dbs;
     UPDATE STATISTICS FOR TABLE dis_pre_his_trans;
  
     CREATE INDEX xdis_preliquida2 ON dis_preliquida (folio_liquida) IN dis_ix_dbs;
     UPDATE STATISTICS FOR TABLE dis_preliquida;

     INSERT INTO dis_interface_hs
     SELECT *
     FROM   dis_pre_interface_hs
     WHERE  folio_liquida = p_folio_disp;

     INSERT INTO dis_his_transaccion
     SELECT *
     FROM   dis_pre_his_trans
     WHERE  folio_liquida = p_folio_disp;

     SET INDEXES FOR dis_info_inconsistente ENABLED;
     SET INDEXES FOR dis_interface_ef_ad ENABLED;
     SET INDEXES FOR dis_compensa_avance ENABLED;
     SET INDEXES FOR dis_cta_ind_pau ENABLED;
     SET INDEXES FOR dis_crd_tramite ENABLED;
     SET INDEXES FOR dis_crd_ceros ENABLED;       --Req SACI2018-175

     UPDATE STATISTICS FOR TABLE dis_info_inconsistente;
     UPDATE STATISTICS FOR TABLE dis_interface_ef_ad;
     UPDATE STATISTICS FOR TABLE dis_interface_hs;
     UPDATE STATISTICS FOR TABLE dis_compensa_avance;
     UPDATE STATISTICS FOR TABLE dis_cta_ind_pau;
     UPDATE STATISTICS FOR TABLE dis_crd_tramite;
     UPDATE STATISTICS FOR TABLE dis_crd_ceros;       --Req SACI2018-175
     UPDATE STATISTICS FOR TABLE dis_his_hs;
     UPDATE STATISTICS FOR TABLE dis_his_transaccion; --25072017
  END IF;
END PROCEDURE;


