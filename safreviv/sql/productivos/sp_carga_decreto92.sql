






CREATE PROCEDURE "safreviv".sp_carga_decreto92(p_folio DECIMAL(9,0),
                                    p_fecha DATE)

   DEFINE v_movimiento   SMALLINT;
   DEFINE v_hora         DATETIME HOUR TO SECOND;
   DEFINE v_subcuenta    SMALLINT;
   DEFINE v_fecha_corte  DATE;

   DEFINE v_id_decreto           DECIMAL(9,0) ;
   DEFINE v_id_cta_bd92     INTEGER;

   DEFINE v_nss                  CHAR(11)     ;
   DEFINE v_rfc                  CHAR(13)     ;
   DEFINE v_nci                  CHAR(30)     ;
   DEFINE v_tip_entidad          CHAR(2)      ;
   DEFINE v_cve_icefa            CHAR(3)      ;
   DEFINE v_curp                 CHAR(18)     ;
   DEFINE v_nombre               CHAR(120)    ;
   DEFINE v_f_nacimiento         CHAR(10)     ;
   DEFINE v_cve_credito          CHAR(1)      ;
   DEFINE v_cve_retiro           CHAR(1)      ;
   DEFINE v_rfc_patron           CHAR(13)     ;
   DEFINE v_nss_patron           CHAR(11)     ;
   DEFINE v_nombre_patron        CHAR(40)     ;
   DEFINE v_exp_infonavit        CHAR(9)      ;
   DEFINE v_sdo_ini_imss         DECIMAL(17,6);
   DEFINE v_sdo_ini_infonavit    DECIMAL(17,2);
   DEFINE v_sdo_act_imss         DECIMAL(17,6);
   DEFINE v_sdo_act_infonavit    DECIMAL(17,2);
   DEFINE v_cve_criterio         CHAR(2)      ;
   DEFINE v_cve_mov_realizado    CHAR(2)      ;
   DEFINE v_cve_mov_actual       CHAR(2)      ;
   DEFINE v_f_ult_movimiento     CHAR(10)     ;
   DEFINE v_f_sol_movimiento     CHAR(10)     ;
   DEFINE v_f_cancel_mov         CHAR(10)     ;
   DEFINE v_mto_apart_imss       DECIMAL(17,6);
   DEFINE v_mto_apart_ifv        DECIMAL(17,2);
   DEFINE v_mto_liq_imss         DECIMAL(17,2);
   DEFINE v_mto_liq_ifv          DECIMAL(17,2);
   DEFINE v_mto_incr_ret_imss    DECIMAL(17,2);
   DEFINE v_mto_incr_viv_ifv     DECIMAL(17,2);
   DEFINE v_consec_cuenta        CHAR(11)     ;
   DEFINE v_sdo_ifv_pesos        DECIMAL(17,2);
   DEFINE v_sdo_ifv_aivs         DECIMAL(17,2);
   DEFINE v_ind_consistencia     CHAR(1)      ;

   SET PDQPRIORITY HIGH;
   SET INDEXES FOR afi_decreto DISABLED;
   SET INDEXES FOR cta_decreto DISABLED;
   SET INDEXES FOR cta_bd92_decreto DISABLED;

   LET v_movimiento = 999;
   LET v_hora       = CURRENT;
   LET v_subcuenta  = 48;
   
   SELECT fecha_corte
     INTO v_fecha_corte
     FROM safre_mig:mig_decreto;

   FOREACH SELECT tm.nss               ,
                  tm.rfc               ,
                  tm.nci               ,
                  tm.tip_entidad       ,
                  tm.cve_icefa         ,
                  tm.curp              ,
                  tm.nombre            ,
                  tm.f_nacimiento      ,
                  tm.cve_credito       ,
                  tm.cve_retiro        ,
                  tm.rfc_patron        ,
                  tm.nss_patron        ,
                  tm.nombre_patron     ,
                  tm.exp_infonavit     ,
                  tm.sdo_ini_imss      ,
                  tm.sdo_ini_infonavit ,
                  tm.sdo_act_imss      ,
                  tm.sdo_act_infonavit ,
                  tm.cve_criterio      ,
                  tm.cve_mov_realizado ,
                  tm.cve_mov_actual    ,
                  tm.f_ult_movimiento  ,
                  tm.f_sol_movimiento  ,
                  tm.f_cancel_mov      ,
                  tm.mto_apart_imss    ,
                  tm.mto_apart_ifv     ,
                  tm.mto_liq_imss      ,
                  tm.mto_liq_ifv       ,
                  tm.mto_incr_ret_imss ,
                  tm.mto_incr_viv_ifv  ,
                  tm.consec_cuenta     ,
                  tm.sdo_ifv_pesos     ,
                  tm.sdo_ifv_aivs      ,
                  tm.ind_consistencia  
             INTO v_nss              ,
                  v_rfc              ,
                  v_nci              ,
                  v_tip_entidad      ,
                  v_cve_icefa        , 
                  v_curp             ,
                  v_nombre           ,
                  v_f_nacimiento     ,
                  v_cve_credito      ,
                  v_cve_retiro       ,
                  v_rfc_patron       ,
                  v_nss_patron       ,
                  v_nombre_patron    ,
                  v_exp_infonavit    ,
                  v_sdo_ini_imss     ,
                  v_sdo_ini_infonavit,
                  v_sdo_act_imss     ,
                  v_sdo_act_infonavit,
                  v_cve_criterio     ,
                  v_cve_mov_realizado,
                  v_cve_mov_actual   ,
                  v_f_ult_movimiento ,
                  v_f_sol_movimiento ,
                  v_f_cancel_mov     ,
                  v_mto_apart_imss   ,
                  v_mto_apart_ifv    ,
                  v_mto_liq_imss     ,
                  v_mto_liq_ifv      ,
                  v_mto_incr_ret_imss,
                  v_mto_incr_viv_ifv ,
                  v_consec_cuenta    ,
                  v_sdo_ifv_pesos    ,
                  v_sdo_ifv_aivs     ,
                  v_ind_consistencia 
             FROM safre_mig:tmp_decreto92 tm

      INSERT INTO cta_bd92_decreto VALUES(seq_cta_bd92_decreto.NEXTVAL,
                                          v_nss              ,
                                          v_rfc              ,
                                          v_nci              ,
                                          v_tip_entidad      ,
                                          v_cve_icefa        , 
                                          v_curp             ,
                                          v_nombre           ,
                                          v_f_nacimiento     ,
                                          v_cve_credito      ,
                                          v_cve_retiro       ,
                                          v_rfc_patron       ,
                                          v_nss_patron       ,
                                          v_nombre_patron    ,
                                          v_exp_infonavit    ,
                                          v_sdo_ini_imss     ,
                                          v_sdo_ini_infonavit,
                                          v_sdo_act_imss     ,
                                          v_sdo_act_infonavit,
                                          v_cve_criterio     ,
                                          v_cve_mov_realizado,
                                          v_cve_mov_actual   ,
                                          v_f_ult_movimiento ,
                                          v_f_sol_movimiento ,
                                          v_f_cancel_mov     ,
                                          v_mto_apart_imss   ,
                                          v_mto_apart_ifv    ,
                                          v_mto_liq_imss     ,
                                          v_mto_liq_ifv      ,
                                          v_mto_incr_ret_imss,
                                          v_mto_incr_viv_ifv ,
                                          v_consec_cuenta    ,
                                          v_sdo_ifv_pesos    ,
                                          v_sdo_ifv_aivs     ,
                                          v_ind_consistencia);

      LET v_id_cta_bd92 = seq_cta_bd92_decreto.CURRVAL ;

      INSERT INTO afi_decreto VALUES(seq_afi_decreto.NEXTVAL,
                                     v_consec_cuenta        ,
                                     v_nci                  ,
                                     v_nss                  ,
                                     v_rfc                  ,
                                     v_curp                 ,
                                     v_tip_entidad          ,
                                     v_cve_icefa            ,
                                     v_nombre               ,
                                     v_f_nacimiento         ,
                                     v_cve_credito          ,
                                     v_cve_retiro           ,
                                     v_rfc_patron           ,
                                     v_nss_patron           ,
                                     v_nombre_patron        ,
                                     v_exp_infonavit        ,
                                     v_ind_consistencia 
                                     );

      LET v_id_decreto = seq_afi_decreto.CURRVAL ;

      INSERT INTO cta_decreto VALUES(v_fecha_corte  ,
                                     v_id_decreto   ,
                                     v_subcuenta    ,
                                     11             ,
                                     v_movimiento   ,
                                     p_folio        ,
                                     v_id_cta_bd92     ,
                                     v_sdo_ifv_aivs ,
                                     v_sdo_ifv_pesos,
                                     v_fecha_corte  ,
                                     TODAY          ,
                                     v_hora         ,
                                     "CARGA INICIAL");

   END FOREACH;

   SET INDEXES FOR afi_decreto ENABLED;
   SET INDEXES FOR cta_decreto ENABLED;
   SET INDEXES FOR cta_bd92_decreto ENABLED;
   UPDATE STATISTICS FOR TABLE afi_decreto;
   UPDATE STATISTICS FOR TABLE cta_decreto ;
   UPDATE STATISTICS FOR TABLE cta_bd92_decreto ;

   SET PDQPRIORITY DEFAULT;

END PROCEDURE
;


