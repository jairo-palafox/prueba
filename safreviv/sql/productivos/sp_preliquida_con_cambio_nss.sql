






CREATE PROCEDURE "safreviv".sp_preliquida_con_cambio_nss(p_folio DECIMAL(9,0), p_usuario CHAR(20), v_tpo_archivo SMALLINT)
 
   RETURNING INTEGER, SMALLINT,CHAR(200);
   
   --acl_preliquida
   DEFINE v_preliq_f_liquida          DATE;                       --f_liquida            date                   
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);               --id_derechohabiente   decimal(9,0)           
   DEFINE v_preliq_subcuenta          SMALLINT;                   --subcuenta            smallint               
   DEFINE v_preliq_fondo_inversion    SMALLINT;                   --fondo_inversion      smallint               
   DEFINE v_preliq_movimiento         SMALLINT;                   --movimiento           smallint               
   DEFINE v_preliq_movimiento_c       SMALLINT;                   --movimiento           smallint   --saci2018-67
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);               --folio_liquida        decimal(9,0)           
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);               --id_referencia        decimal(9,0)           
   DEFINE v_preliq_monto_acciones     DECIMAL(22,2);              --monto_acciones       decimal(22,2)          
   DEFINE v_preliq_monto_pesos        DECIMAL(22,2);              --monto_pesos          decimal(22,2)          
   DEFINE v_preliq_f_valor            DATE;                       --f_valor              date                   
   DEFINE v_preliq_f_registro         DATE;                       --f_registro           date                   
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;    --h_registro           datetime hour to second
   DEFINE v_preliq_usuario            CHAR(20);                   --usuario              char(20)               
   
   --TABLA acl_det_cc_nss
   DEFINE v_det_folio               DECIMAL(9)   ;      --folio                decimal(9,0)     
   DEFINE v_det_periodo_pago        CHAR(6)      ;      --periodo_pago         char(6)          
   DEFINE v_det_id_derechohabiente  DECIMAL(9,0) ;      --id_derechohabiente   decimal(9,0)     
   DEFINE v_det_folio_sua           DECIMAL(6,0) ;      --folio_sua            decimal(6,0)     
   DEFINE v_det_id_referencia       DECIMAL(9,0) ;      --id_referencia        decimal(9,0)     
   DEFINE v_det_tpo_registro        CHAR(1)      ;      --tpo_registro         char(1)          
   DEFINE v_det_cve_ent_receptora   CHAR(3)      ;      --cve_ent_receptora    char(3)          
   DEFINE v_det_nrp                 CHAR(11)     ;      --nrp                  char(11)         
   DEFINE v_det_rfc_patron          CHAR(13)     ;      --rfc_patron           char(13)         
   DEFINE v_det_f_pago_patron       DATE         ;      --f_pago_patron        date             
   DEFINE v_det_rfc                 CHAR(13)     ;      --rfc                  char(13)         
   DEFINE v_det_curp                CHAR(18)     ;      --curp                 char(18)         
   DEFINE v_det_num_crd_ifv         CHAR(10)     ;      --num_crd_ifv          char(10)         
   DEFINE v_det_f_ini_desc_crd_ifv  DATE         ;      --f_ini_desc_crd_ifv   date             
   DEFINE v_det_num__mov_periodo    CHAR(2)      ;      --num_mov_periodo      char(2)          
   DEFINE v_det_nombre_trabajador   CHAR(50)     ;      --nom_trabajador       char(50)         
   DEFINE v_det_ult_sdi             DECIMAL(7,2) ;      --ult_sdi              decimal(7,2)     
   DEFINE v_det_tpo_trabajador      CHAR(1)      ;      --tpo_trabajador       char(1)          
   DEFINE v_det_jornada             CHAR(1)      ;      --jornada              char(1)          
   DEFINE v_det_localiza_trabajador CHAR(1)      ;      --localiza_trabajad+   char(1)          
   DEFINE v_det_destino_ap_viv      CHAR(1)      ;      --destino_ap_viv       char(1)          
   DEFINE v_det_dias_cot_bim        SMALLINT     ;      --dias_cot_bim         smallint         
   DEFINE v_det_dias_incap_bim      SMALLINT     ;      --dias_incap_bim       smallint         
   DEFINE v_det_dias_ausent_bim     SMALLINT     ;      --dias_ausent_bim      smallint         
   DEFINE v_det_imp_ap_pat          DECIMAL(12,2);      --imp_ap_pat           decimal(12,2)    
   DEFINE v_det_imp_am_cre          DECIMAL(12,2);      --imp_am_cre           decimal(12,2)    
   DEFINE v_det_imp_ren_viv         DECIMAL(12,2);      --imp_ren_viv          decimal(12,2)    
   DEFINE v_det_marca_sua           CHAR(2)      ;      --marca_sua            char(2)          
   DEFINE v_det_marca_bdnsar        CHAR(1)      ;      --marca_bdnsar         char(1)          
   DEFINE v_det_diag_aclaracion     CHAR(2)      ;      --diag_aclaracion      char(2)          
   DEFINE v_det_f_proceso           DATE         ;      --f_proceso            date             
   DEFINE v_det_nss_dispersion      char(11)     ;      --nss_dispersion       char(11)  +      
   DEFINE v_det_paterno_afore       char(40)     ;      --paterno_afore        char(40)  +      
   DEFINE v_det_materno_afore       char(40)     ;      --materno_afore        char(40)  +      
   DEFINE v_det_nombre_afore        char(40)     ;      --nombre_afore         char(40)  +   
   DEFINE v_det_tpo_aclaracion      char(2)      ;   
   DEFINE v_det_aivs                DECIMAL(18,6);      --aivs                 decimal(18,6)    
   DEFINE v_det_valor_aiv           DECIMAL(18,6);      --valor_aiv            decimal(18,6)    
   DEFINE v_det_int_gen_pgo_ext     DECIMAL(12,2);      --int_gen_pgo_ext      decimal(12,2)    
   DEFINE v_det_aiv_gen_pgo_ext     DECIMAL(18,6);      --aiv_gen_pgo_ext      decimal(18,6)  
   DEFINE v_det_origen_archivo      SMALLINT;           --origen_archivo       smallint
   DEFINE v_det_id_derhab_nuevo     DECIMAL(9,0);       --id_derhab_nuevo     decimal(9,0)
   
   
   --TABLA acl_sum_cc_nss
   DEFINE v_sum_folio                decimal(9)   ;    --folio                decimal(9,0)  
   DEFINE v_sum_tpo_registro         char(2)      ;    --tpo_registro         char(2)       
   DEFINE v_sum_tot_ap               integer      ;    --tot_ap               integer       
   DEFINE v_sum_suma_ap_pat          decimal(18,2);    --suma_ap_pat          decimal(18,2) 
   DEFINE v_sum_suma_am              decimal(18,2);    --suma_am              decimal(18,2) 
   DEFINE v_sum_suma_aivs            decimal(18,6);    --suma_aivs            decimal(18,6) 
   DEFINE v_sum_suma_int_viv_pgo_ext decimal(18,2);    --suma_int_viv_pgo_+   decimal(18,2) 
   DEFINE v_sum_suma_aiv_pgo_ext     decimal(18,6);    --suma_aiv_pgo_ext     decimal(18,6) 
   
   --DEFINE v_tpo_archivo          smallint;
   DEFINE v_tpo_aclaracion         smallint;
   DEFINE v_fn_existe_pgo_cta      smallint;
   DEFINE v_fn_verifica_existencia smallint;
   DEFINE v_fn_existe_pgo_ant      CHAR(01);
   DEFINE v_bandera_nrp            SMALLINT;
   DEFINE v_tpo_patron             CHAR(2);   
   DEFINE v_ind_liquidacion        SMALLINT;   
   
   DEFINE v_sql_error    SMALLINT;
   DEFINE v_isam_error   SMALLINT;
   DEFINE v_msg_error    CHAR(200);
   DEFINE v_si_resultado SMALLINT;
   
   -- variables para la parametrizacion de movimiento de cambio de NSS y NOMBRE
   DEFINE v_mov_nor_cc_nss SMALLINT;
   DEFINE v_mov_nor_cc_nom SMALLINT;
   DEFINE v_mov_nor_act    SMALLINT;
   DEFINE v_mov_int_cc_nss SMALLINT;
   DEFINE v_mov_int_cc_nom SMALLINT;
   DEFINE v_mov_int_act    SMALLINT;

   DEFINE v_mov_nor_cc_nss_riss_vol SMALLINT;
   DEFINE v_mov_nor_cc_nom_riss_vol SMALLINT;
   DEFINE v_mov_int_cc_nss_riss_vol SMALLINT;
   DEFINE v_mov_int_cc_nom_riss_vol SMALLINT;
   DEFINE v_mov_nor_act_riss_vol    SMALLINT;
   DEFINE v_mov_int_act_riss_vol    SMALLINT;

   DEFINE v_mov_nor_cc_nss_gf_riss_viv SMALLINT;
   DEFINE v_mov_int_cc_nss_gf_riss_viv SMALLINT;
   DEFINE v_mov_nor_cc_nom_gf_riss_viv SMALLINT;
   DEFINE v_mov_int_cc_nom_gf_riss_viv SMALLINT;

   DEFINE v_mov_nor_cc_nss_gf_riss_vol SMALLINT;
   DEFINE v_mov_int_cc_nss_gf_riss_vol SMALLINT;
   DEFINE v_mov_nor_cc_nom_gf_riss_vol SMALLINT;
   DEFINE v_mov_int_cc_nom_gf_riss_vol SMALLINT;

   DEFINE v_mov_nor_act_gf_riss_viv SMALLINT;
   DEFINE v_mov_int_act_gf_riss_viv SMALLINT;
   DEFINE v_mov_nor_act_gf_riss_vol SMALLINT;
   DEFINE v_mov_int_act_gf_riss_vol SMALLINT;

   DEFINE v_mov_trm_apo_abo_cc_nss SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_apo_car_cc_nss SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_amo_abo_cc_nss SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_amo_car_cc_nss SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_int_abo_cc_nss SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_int_car_cc_nss SMALLINT;   --saci2018-67

   DEFINE v_mov_trm_apo_abo_cc_nom SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_apo_car_cc_nom SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_amo_abo_cc_nom SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_amo_car_cc_nom SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_int_abo_cc_nom SMALLINT;   --saci2018-67
   DEFINE v_mov_trm_int_car_cc_nom SMALLINT;   --saci2018-67


   DEFINE v_mov_trm_apo_abo_act SMALLINT;      --saci2018-67
   DEFINE v_mov_trm_apo_car_act SMALLINT;      --saci2018-67
   DEFINE v_mov_trm_amo_abo_act SMALLINT;      --saci2018-67
   DEFINE v_mov_trm_amo_car_act SMALLINT;      --saci2018-67
   DEFINE v_mov_trm_int_abo_act SMALLINT;      --saci2018-67 
   DEFINE v_mov_trm_int_car_act SMALLINT;      --saci2018-67
   
   DEFINE v_cont_nrp_riss SMALLINT;
   DEFINE v_origen_liq    SMALLINT;

   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
	    LET v_si_resultado = v_sql_error ;
	    LET v_msg_error = TRIM(v_msg_error)||":"||v_det_id_derechohabiente ;
      RETURN v_si_resultado,v_isam_error,v_msg_error;
   END EXCEPTION;    --WITH RESUME;
                   
   --SET DEBUG FILE TO  '/ds/safreviv_int/BD/preliquidacionCONCambio.trace'; 
   --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;


   --------------- Movimientos sub44 viv NRP Cualquiera --------------------------
   LET v_mov_nor_cc_nss = 61 ;               -- ABONO POR REGISTRO DE PAGOS CC NSS
   LET v_mov_int_cc_nss = 33;                -- INTERES CC NSS

   LET v_mov_nor_cc_nom = 221;               -- ABONO POR REGISTRO DE PAGOS CC NOM
   LET v_mov_int_cc_nom = 73;                -- INTERES CC NOM
   -------------------------------------------------------------------------------
 
   -------------- Movimientos sub44 viv NRP B0799991101  -------------------------
   LET v_mov_nor_cc_nss_gf_riss_viv = 811;	 -- ABONO ACL SUB RISS GF CC NSS
   LET v_mov_int_cc_nss_gf_riss_viv = 233;   --	INTERESES ACL SUB RISS GF CC NSS

   LET v_mov_nor_cc_nom_gf_riss_viv = 831;   --	ABONO ACL SUB RISS GF CC NOM
   LET v_mov_int_cc_nom_gf_riss_viv = 253;	 -- INTERESES ACL SUB RISS GF CC NOM
   ------------------------------------------------------------------------------

   --------------- Movimientos sub55 vol 32 NRPS  --------------------------------
   LET v_mov_nor_cc_nss_riss_vol = 731;      -- ABONO ACL VOL RISS CC NSS
   LET v_mov_int_cc_nss_riss_vol = 153;      -- INTERES ACL VOL RISS CC NSS
   
   LET v_mov_nor_cc_nom_riss_vol = 751;      -- ABONO ACL VOL RISS CC NOM
   LET v_mov_int_cc_nom_riss_vol = 173;      -- INTERES ACL VOL RISS CC NOM
   ------------------------------------------------------------------------------

   -------------- Movimientos sub55 vol NRP B0799994105  ------------------------
   LET v_mov_nor_cc_nss_gf_riss_vol = 801;	 -- ABONO ACL SUB RISS GF CC NSS
   LET v_mov_int_cc_nss_gf_riss_vol = 223;	 -- INTERESES ACL SUB RISS GF CC NSS

   LET v_mov_nor_cc_nom_gf_riss_vol = 821;	 -- ABONO ACL SUB RISS GF CC NOM
   LET v_mov_int_cc_nom_gf_riss_vol = 243;	 -- INTERESES ACL SUB RISS GF CC NOM
   ------------------------------------------------------------------------------

   ------ Movimientos sub4 viv PAGOS anteriores al 31-OCT-2012 pagados en TRM CC NSS -------
   LET v_mov_trm_apo_abo_cc_nss = 1871;     -- ABONO ACL aport TRM CC NSS     --saci2018-67
   LET v_mov_trm_apo_car_cc_nss = 2102;     -- CARGO ACL aport TRM CC NSS     --saci2018-67
   
   LET v_mov_trm_amo_abo_cc_nss = 1881;     -- ABONO ACL amort TRM CC NSS     --saci2018-67 
   LET v_mov_trm_amo_car_cc_nss = 2112;     -- CARGO ACL amort TRM CC NSS     --saci2018-67

   LET v_mov_trm_int_abo_cc_nss = 1921;     -- ABONO ACL INT TRM CC NSS       --saci2018-67
   LET v_mov_trm_int_car_cc_nss = 2152;     -- CARGO ACL INT TRM CC NSS       --saci2018-67
   -----------------------------------------------------------------------------------------

   ------ Movimientos sub4 viv PAGOS anteriores al 31-OCT-2012 pagados en TRM CC NOM -------
   LET v_mov_trm_apo_abo_cc_nom = 1891;     -- ABONO ACL aport TRM CC NOM     --saci2018-67
   LET v_mov_trm_apo_car_cc_nom = 2122;     -- CARGO ACL aport TRM CC NOM     --saci2018-67
   
   LET v_mov_trm_amo_abo_cc_nom = 1901;     -- ABONO ACL amort TMR CC NOM     --saci2018-67 
   LET v_mov_trm_amo_car_cc_nom = 2132;     -- CARGO ACL amort TRM CC NOM     --saci2018-67

   LET v_mov_trm_int_abo_cc_nom = 1931;     -- ABONO ACL INT TRM CC NOM       --saci2018-67
   LET v_mov_trm_int_car_cc_nom = 2162;     -- ABONO ACL INT TRM CC NOM       --saci2018-67

   -----------------------------------------------------------------------------------------

   IF v_tpo_archivo = 6 THEN
       LET v_mov_nor_act             = v_mov_nor_cc_nss;
       LET v_mov_int_act             = v_mov_int_cc_nss;
       LET v_mov_nor_act_gf_riss_viv = v_mov_nor_cc_nss_gf_riss_viv;
       LET v_mov_int_act_gf_riss_viv = v_mov_int_cc_nss_gf_riss_viv;
       
       LET v_mov_nor_act_riss_vol    = v_mov_nor_cc_nss_riss_vol;
       LET v_mov_int_act_riss_vol    = v_mov_int_cc_nss_riss_vol;
       LET v_mov_nor_act_gf_riss_vol = v_mov_nor_cc_nss_gf_riss_vol;
       LET v_mov_int_act_gf_riss_vol = v_mov_int_cc_nss_gf_riss_vol;
       
       LET v_mov_trm_apo_abo_act     = v_mov_trm_apo_abo_cc_nss;    --saci2018-67
       LET v_mov_trm_apo_car_act     = v_mov_trm_apo_car_cc_nss;    --saci2018-67
       LET v_mov_trm_amo_abo_act     = v_mov_trm_amo_abo_cc_nss;    --saci2018-67
       LET v_mov_trm_amo_car_act     = v_mov_trm_amo_car_cc_nss;    --saci2018-67
       LET v_mov_trm_int_abo_act     = v_mov_trm_int_abo_cc_nss;    --saci2018-67
       LET v_mov_trm_int_car_act     = v_mov_trm_int_car_cc_nss;    --saci2018-67

   ELSE
   	-- si p_origen_archivo = 7
       LET v_mov_nor_act             = v_mov_nor_cc_nom;
       LET v_mov_int_act             = v_mov_int_cc_nom;
       LET v_mov_nor_act_gf_riss_viv = v_mov_nor_cc_nom_gf_riss_viv;
       LET v_mov_int_act_gf_riss_viv = v_mov_int_cc_nom_gf_riss_viv;
       
       LET v_mov_nor_act_riss_vol    = v_mov_nor_cc_nom_riss_vol;
       LET v_mov_int_act_riss_vol    = v_mov_int_cc_nom_riss_vol;
       LET v_mov_nor_act_gf_riss_vol = v_mov_nor_cc_nom_gf_riss_vol;
       LET v_mov_int_act_gf_riss_vol = v_mov_int_cc_nom_gf_riss_vol;

       LET v_mov_trm_apo_abo_act     = v_mov_trm_apo_abo_cc_nom;   --saci2018-67
       LET v_mov_trm_apo_car_act     = v_mov_trm_apo_car_cc_nom;   --saci2018-67
       LET v_mov_trm_amo_abo_act     = v_mov_trm_amo_abo_cc_nom;   --saci2018-67
       LET v_mov_trm_amo_car_act     = v_mov_trm_amo_car_cc_nom;   --saci2018-67    
       LET v_mov_trm_int_abo_act     = v_mov_trm_int_abo_cc_nom;   --saci2018-67
       LET v_mov_trm_int_car_act     = v_mov_trm_int_car_cc_nom;   --saci2018-67

   END IF

   --Constantes para todos los derechohabintes que pertenecen al folio
   --LET v_tpo_archivo = 6;
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_folio_liquida   = p_folio;      
   LET v_preliq_usuario         = p_usuario;
   LET v_bandera_nrp = 0;
   LET v_origen_liq = 1;   -- liquidacion ACL
 
 
   FOREACH cur_his_pagos FOR
      SELECT a.id_referencia         ,
             a.cve_ent_receptora     ,
             a.nrp                   ,
             a.periodo_pago          ,
             a.folio_sua             ,
             a.f_pago                ,
             a.id_derechohabiente    ,
             a.localiza_trabajador   ,
             a.tpo_aclaracion        ,
             c.imp_ap_pat            ,
             a.imp_am_cre            ,
             --se modifica el día 7 de agosto de 2012 por instrucción de Gerardo Vega  y por regla de negocio
             0                       ,
             --imp_ren_viv_pgo_ext   ,
             c.aiv_ap_pat            ,
             a.valor_aiv             ,
             c.int_gen_pgo_ext       ,
             c.aiv_gen_pgo_ext       ,
             a.tpo_patron            ,
             a.ind_liquidacion       ,
             a.origen_archivo        ,
             b.id_derhab_nuevo       ,
             a.destino_ap_viv           --Agregado por CABC
      INTO   v_det_id_referencia       , 
             v_det_cve_ent_receptora   ,
             v_det_nrp                 ,
             v_det_periodo_pago        ,
             v_det_folio_sua           ,
             v_det_f_pago_patron       ,
             v_det_id_derechohabiente  ,
             v_det_localiza_trabajador ,
             v_det_tpo_aclaracion      ,
             v_det_imp_ap_pat          ,
             v_det_imp_am_cre          ,
             v_det_imp_ren_viv         ,
             v_det_aivs                ,
             v_det_valor_aiv           ,
             v_det_int_gen_pgo_ext     ,
             v_det_aiv_gen_pgo_ext     ,
             v_tpo_patron              ,
             v_ind_liquidacion         ,
             v_det_origen_archivo      ,
             v_det_id_derhab_nuevo     ,
             v_det_destino_ap_viv       --Agregado por CABC
      FROM   cta_his_pagos a,
             cta_pag_complemento b,
             acl_pag_registrado c
      WHERE  a.folio = p_folio
      AND    b.folio = a.folio
      and    c.folio = a.folio  
      AND    b.id_referencia = a.id_referencia    
      and    c.id_referencia = a.id_referencia
      AND    result_operacion = 1          --Solo se procesan los aceptados, los que econtraron pago anterior
                                           --Se procesan los rechazados a partir del 18-nov-2015 de acuerdo    YA NO APLICA
                                           --a reunión en oficina de Luis Flores, Hamir comento que se acepten 
                                           --que traigan aclaración nula  

      LET v_fn_existe_pgo_ant = NULL;
      LET v_fn_existe_pgo_cta = 0;

--============================= INICIO REGLA DE NOGICIO 25-JUN-2012 GAVP =============================

      IF v_tpo_patron = "99" THEN
         LET v_bandera_nrp = 1;      -- PAGO SOLO INFONAVIT
         LET v_ind_liquidacion = 2;  -- ACL ADELANTADA SOLO INFONAVIT

      ELSE   
         IF (v_det_tpo_aclaracion = "17" OR v_det_tpo_aclaracion = "13") THEN

             LET v_bandera_nrp = 0;       -- PAGO IMSS
             LET v_ind_liquidacion = 3;   -- ACL ADELANTADA FUSION(13) o TRASPASO(17)

         ELSE

            LET v_bandera_nrp = 0;
            LET v_ind_liquidacion = 5;   -- SALIDA ACLARATIO NORMAL LIQUIDADA

         END IF;

      END IF;

      LET v_fn_existe_pgo_cta = fn_existe_pago(v_det_id_derechohabiente, 
                                               v_det_cve_ent_receptora ,
                                               v_det_nrp,
                                               v_det_periodo_pago,
                                               v_det_folio_sua,
                                               v_det_f_pago_patron,
                                               v_det_imp_ap_pat,          --vivienda
                                               v_det_imp_am_cre,          --amortización
                                               v_det_int_gen_pgo_ext,     --interes generado pago extemporaneo                                               
                                               p_folio,                   --folio arch qse esta procesando
                                               v_ind_liquidacion);        --ind_liquidacion
                                               
      IF v_fn_existe_pgo_cta = 1 THEN   -- 1=EL pago ya existe liquidado

         -- Actualiza estado del pago
         EXECUTE PROCEDURE sp_actualiza_estado_pago
                (p_folio,                  --folio
                 v_det_id_referencia,      --id_referencia  
                 v_det_id_derechohabiente, --id_derechohabiente
                 50,                       --estado_pago = PAGADO PREVIO Y SALIDA CONFIRMADA
                 v_det_tpo_aclaracion,     --tpo_aclaracion
                 TODAY                     --f_actualiza
                 );
                                          
{ --inc1389710
         UPDATE cta_his_pagos
         SET    ind_liquidacion  = 4  -- APORTACION CONCILIADA CON PROCESAR DE ADELANTAMIENTOS
         WHERE  folio = p_folio
         AND    id_referencia = v_det_id_referencia ;         
  --inc1389710 }

------------------------------------ --inc1389710

         IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
            
            DELETE FROM cta_his_pagos WHERE folio = p_folio AND id_referencia = v_det_id_referencia;

            DELETE cta_his_pagos                                    --inc1389710
            WHERE  id_derechohabiente = v_det_id_derechohabiente    --inc1389710
            AND    folio_sua          = v_det_folio_sua             --inc1389710
            AND    periodo_pago       = v_det_periodo_pago          --inc1389710
            AND    nrp                = v_det_nrp                   --inc1389710
            AND    imp_ap_pat         = v_det_imp_ap_pat            --inc1389710
            AND    imp_am_cre         = v_det_imp_am_cre;           --inc1389710

            UPDATE cta_especial_acl
            SET    ind_liquidacion  = 4  -- APORTACION CONCILIADA CON PROCESAR DE ADELANTAMIENTOS
            WHERE  folio = p_folio
            AND    id_referencia = v_det_id_referencia;

         ELSE

            UPDATE cta_his_pagos
            SET    ind_liquidacion  = 4  -- APORTACION CONCILIADA CON PROCESAR DE ADELANTAMIENTOS
            WHERE  folio = p_folio
            AND    id_referencia = v_det_id_referencia;

         END IF

------------------------------------ --inc1389710

         --Ya no se preliquida el registro y continua con el siguiente registro
         CONTINUE FOREACH;
      ELSE                               -- 0=pago no existe liquidado

         -- Pregunta si existe pago que se adelanto en LQINFO (his ant)
         LET v_fn_existe_pgo_ant = fn_existe_pgo_his(v_det_id_derechohabiente,
                                                     v_det_cve_ent_receptora,
                                                     p_folio,
                                                     v_det_nrp,
                                                     v_det_periodo_pago,
                                                     v_det_folio_sua,
                                                     v_det_f_pago_patron,
                                                     v_det_imp_ap_pat,
                                                     v_det_imp_am_cre,
                                                     v_det_int_gen_pgo_ext);

         IF v_fn_existe_pgo_ant = "F" THEN  -- Registro en revisión XQ no encontro his anterior a este

            -- Actualiza estado del pago
            EXECUTE PROCEDURE sp_actualiza_estado_pago
                    (p_folio,                  --folio
                     v_det_id_referencia,      --id_referencia
                     v_det_id_derechohabiente, --id_derechohabiente
                     60,                       --estado_pago = ACLARATORIO SIN LIQUIDAR INCONSISTENTE (NO EXISTE EN HISTORICO ANTERIOR)
                     v_det_tpo_aclaracion,     --tpo_aclaracion
                     TODAY                     --f_actualiza
                    );

{ --inc1389710
            -- Actualiza result_operacion = "02" p/generar reporte de estos casos
            UPDATE cta_his_pagos
            SET    result_operacion = 2
            WHERE  folio = p_folio
            AND    id_referencia = v_det_id_referencia ;
  --inc1389710 }

------------------------------------ --inc1389710
         IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
            
            DELETE FROM cta_his_pagos WHERE folio = p_folio AND id_referencia = v_det_id_referencia;

            DELETE cta_his_pagos                                    --inc1389710
            WHERE  id_derechohabiente = v_det_id_derechohabiente    --inc1389710
            AND    folio_sua          = v_det_folio_sua             --inc1389710
            AND    periodo_pago       = v_det_periodo_pago          --inc1389710
            AND    nrp                = v_det_nrp                   --inc1389710
            AND    imp_ap_pat         = v_det_imp_ap_pat            --inc1389710
            AND    imp_am_cre         = v_det_imp_am_cre;           --inc1389710

            UPDATE cta_especial_acl
            SET    result_operacion = 2
            WHERE  folio = p_folio
            AND    id_referencia = v_det_id_referencia;

         ELSE
         	
            -- Actualiza result_operacion = "02" p/generar reporte de estos casos       	
            UPDATE cta_his_pagos
            SET    result_operacion = 2
            WHERE  folio = p_folio
            AND    id_referencia = v_det_id_referencia ;       	

         END IF
------------------------------------ --inc1389710

            --Ya no se preliquida el registro y continua con el siguiente registro
            CONTINUE FOREACH;

         ELSE

            IF v_fn_existe_pgo_cta = 2 THEN

               -- Actualiza estado del pago
               EXECUTE PROCEDURE sp_actualiza_estado_pago
                       (p_folio,                  --folio
                        v_det_id_referencia,      --id_referencia
                        v_det_id_derechohabiente, --id_derechohabiente
                        40,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA NORMAL NO ADELANTADA punto 3.13 de DF Aclaratorio
                        v_det_tpo_aclaracion,     --tpo_aclaracion
                        TODAY                     --f_actualiza
                       );

            ELSE

               -- Actualiza estado del pago
               EXECUTE PROCEDURE sp_actualiza_estado_pago
                       (p_folio,                  --folio
                        v_det_id_referencia,      --id_referencia
                        v_det_id_derechohabiente, --id_derechohabiente
                        30,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA ADELANTADA
                        v_det_tpo_aclaracion,     --tpo_aclaracion
                        TODAY                     --f_actualiza
                       ); 
                       
            END IF;
              
            --CONTINUA CON LA PRELIQUIDACIÓN
         END IF;
      END IF;

--============================= FIN DE REGLA DE NEGOCIO 26-JUN-2012 GAVP =============================

      LET v_preliq_id_referencia = v_det_id_referencia; 
      LET v_preliq_f_valor       = v_det_f_pago_patron;
      LET v_preliq_f_liquida     = TODAY;
      LET v_preliq_f_registro    = TODAY;
      LET v_preliq_h_registro    = CURRENT HOUR TO SECOND;
      
      IF v_tpo_archivo = 6 THEN
		      LET v_preliq_id_derechohabiente = v_det_id_derhab_nuevo;
		  ELSE      	
          LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;  --- se comenta xq Hamir cambio la regla a que se tiene que liquidar con el segundo nss
      END IF

      LET v_cont_nrp_riss = 0;

      IF EXISTS (SELECT nrp
                 FROM   cat_riss_nrp
                 WHERE  nrp = v_det_nrp
                 AND    id_nrp = 0)  THEN

         LET v_cont_nrp_riss = 1;

      END IF   -- NRP ACTIVO

      IF v_cont_nrp_riss = 1 OR v_det_nrp = "B0799994105" THEN      

         --Primer registro
      	 
--############################Agregado por CABC############################################
         IF v_det_destino_ap_viv <> "3" THEN
               LET v_preliq_subcuenta      = 55;   -- viv riss vol
               LET v_preliq_fondo_inversion = 11;
         ELSE
               LET v_preliq_subcuenta = 60;    -- PORTABILIDAD     
               LET v_preliq_fondo_inversion = 10;      	            	
         END IF 
--############################Fin lineas Agregadas por CABC################################
         

         IF v_det_nrp = "B0799994105" THEN
            LET v_preliq_movimiento = v_mov_nor_act_gf_riss_vol;
         ELSE
            LET v_preliq_movimiento = v_mov_nor_act_riss_vol;
         END IF

         IF v_det_destino_ap_viv <> "3" THEN
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_aivs;
         ELSE
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_imp_ap_pat;         	
         END IF   

         IF v_preliq_monto_pesos > 0 THEN

            EXECUTE PROCEDURE sp_inserta_preliquida
               (v_origen_liq                 ,
                v_preliq_f_liquida           ,
                v_preliq_id_derechohabiente  ,
                v_preliq_subcuenta           ,
                v_preliq_fondo_inversion     ,
                v_preliq_movimiento          ,
                v_preliq_folio_liquida       ,
                v_preliq_id_referencia       ,
                v_preliq_monto_acciones      ,
                v_preliq_monto_pesos         ,
                v_preliq_f_valor             ,
                v_preliq_f_registro          ,
                v_preliq_h_registro          ,
                v_det_nrp
               );
         END IF

         IF v_det_nrp = "B0799994105" THEN
            LET v_preliq_movimiento = v_mov_int_act_gf_riss_vol;
         ELSE
            LET v_preliq_movimiento = v_mov_int_act_riss_vol;
         END IF

         IF v_det_destino_ap_viv <> "3" THEN
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;
         ELSE
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_int_gen_pgo_ext;         	
         END IF
         
         IF v_preliq_monto_pesos > 0 THEN

            EXECUTE PROCEDURE sp_inserta_preliquida
               (v_origen_liq                 ,
                v_preliq_f_liquida           ,
                v_preliq_id_derechohabiente  ,
                v_preliq_subcuenta           ,
                v_preliq_fondo_inversion     ,
                v_preliq_movimiento          ,
                v_preliq_folio_liquida       ,
                v_preliq_id_referencia       ,
                v_preliq_monto_acciones      ,
                v_preliq_monto_pesos         ,
                v_preliq_f_valor             ,
                v_preliq_f_registro          ,
                v_preliq_h_registro          ,
                v_det_nrp
               );
         END IF
--=================================================

      ELSE

         --Primer registro

         LET v_preliq_movimiento_c = NULL;     --saci2018-67

--############################Agregado por CABC############################################
         IF v_det_destino_ap_viv <> "3" THEN            
            IF v_bandera_nrp = 1 THEN
               LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
            ELSE
               LET v_preliq_subcuenta = 4;    -- viv97
            END IF
            LET v_preliq_fondo_inversion = 11;
         ELSE
            LET v_preliq_subcuenta = 60;   -- Portabilidad
            LET v_preliq_fondo_inversion = 10;
         END IF
--############################Fin lineas Agregadas por CABC################################

         IF v_det_nrp = "B0799991101" THEN
            LET v_preliq_movimiento = v_mov_nor_act_gf_riss_viv;
         ELSE
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
               LET v_preliq_movimiento   = v_mov_trm_apo_abo_act;                     --saci2018-67
               LET v_preliq_movimiento_c = v_mov_trm_apo_car_act;                     --saci2018-67
            ELSE
               LET v_preliq_movimiento = v_mov_nor_act;       -- ABONO NORMAL
               LET v_preliq_movimiento_c = NULL;              --sace2018-67
            END IF                                            --saci2018-67
         END IF

         IF v_det_destino_ap_viv <> "3" THEN
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_aivs;
         ELSE   
            LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
            LET v_preliq_monto_acciones = v_det_imp_ap_pat;            
         END IF

         IF v_preliq_monto_pesos > 0 THEN
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN   --saci2018-67
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
               
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento_c        ,      --saci2018-67
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones*-1   ,      --saci2018-67
                   v_preliq_monto_pesos*-1      ,      --saci2018-67
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
            ELSE
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );            
            END IF      
         END IF

         --Segundo registro

         LET v_preliq_fondo_inversion = 10;

         IF v_bandera_nrp = 1 THEN
         	  LET v_preliq_subcuenta = 43;   -- amort viv97 solo infonavit
         ELSE
         	  LET v_preliq_subcuenta = 41;   -- amort viv97
         END IF

         LET v_preliq_monto_pesos    = v_det_imp_am_cre;
         LET v_preliq_monto_acciones = v_det_imp_am_cre;
--         LET v_preliq_movimiento     = v_mov_nor_act;

         IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
            LET v_preliq_movimiento   = v_mov_trm_amo_abo_act;                     --saci2018-67
            LET v_preliq_movimiento_c = v_mov_trm_amo_car_act;                     --saci2018-67
         ELSE
            LET v_preliq_movimiento = v_mov_nor_act;       -- ABONO SC NSS
            LET v_preliq_movimiento_c = NULL;              --sace2018-67
         END IF

         IF v_preliq_monto_pesos > 0 THEN
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN   --saci2018-67
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );            

               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento_c        ,    --saci2018-67
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones*-1   ,    --saci2018-67
                   v_preliq_monto_pesos*-1      ,    --saci2018-67
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
            ELSE
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
            END IF
         END IF         	

         --Tercer registro ya que no se liquidan rendimientos

         --Cuarto registro

--############################Agregado por CABC############################################
         IF v_det_destino_ap_viv <> "3" THEN            
              IF v_bandera_nrp = 1 THEN
                 LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
              ELSE
                 LET v_preliq_subcuenta = 4;    -- viv97
              END IF
                 LET v_preliq_fondo_inversion = 11;
         ELSE
               LET v_preliq_subcuenta = 60;   -- Portabilidad
               LET v_preliq_fondo_inversion = 10;
         END IF
--############################Fin lineas Agregadas por CABC################################
         

         IF v_det_nrp = "B0799991101" THEN
            LET v_preliq_movimiento = v_mov_int_act_gf_riss_viv;   
         ELSE
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
               LET v_preliq_movimiento   = v_mov_trm_int_abo_act;                     --saci2018-67
               LET v_preliq_movimiento_c = v_mov_trm_int_car_act;                     --saci2018-67
            ELSE
               LET v_preliq_movimiento = v_mov_int_act;    --INTERESES 
               LET v_preliq_movimiento_c = NULL;           --sace2018-67
            END IF
         END IF

         IF v_det_destino_ap_viv <> "3" THEN
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_aiv_gen_pgo_ext;
         ELSE
            LET v_preliq_monto_pesos    = v_det_int_gen_pgo_ext;
            LET v_preliq_monto_acciones = v_det_int_gen_pgo_ext;         	
         END IF

         IF v_preliq_monto_pesos > 0 THEN
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN   --saci2018-67
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );

               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento_c        ,    --saci2018-67
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones*-1   ,    --saci2918-67
                   v_preliq_monto_pesos*-1      ,    --saci2918-67
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
            ELSE
               EXECUTE PROCEDURE sp_inserta_preliquida
                  (v_origen_liq                 ,
                   v_preliq_f_liquida           ,
                   v_preliq_id_derechohabiente  ,
                   v_preliq_subcuenta           ,
                   v_preliq_fondo_inversion     ,
                   v_preliq_movimiento          ,
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones      ,
                   v_preliq_monto_pesos         ,
                   v_preliq_f_valor             ,
                   v_preliq_f_registro          ,
                   v_preliq_h_registro          ,
                   v_det_nrp
                  );
            END IF
         END IF

      END IF

      IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
         DELETE FROM cta_his_pagos WHERE folio = p_folio AND id_referencia = v_det_id_referencia;

         DELETE cta_his_pagos                                       --inc1389710
         WHERE  id_derechohabiente = v_preliq_id_derechohabiente    --inc1389710
         AND    folio_sua          = v_det_folio_sua                --inc1389710
         AND    periodo_pago       = v_det_periodo_pago             --inc1389710
         AND    nrp                = v_det_nrp                      --inc1389710
         AND    imp_ap_pat         = v_det_imp_ap_pat               --inc1389710
         AND    imp_am_cre         = v_det_imp_am_cre;              --inc1389710
      END IF

   END FOREACH
   
--   IF EXISTS (SELECT unique folio
--              FROM   cta_especial_acl
--              WHERE  folio = p_folio)  THEN
--      
--      DELETE FROM cta_his_pagos WHERE folio = p_folio;
--   END IF
   
   --Se actualiza la tabla que lleva el control de los folios
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio;

   --Se actualiza el estatus del archivo
   UPDATE glo_ctr_archivo
   SET    estado = 2
   WHERE  folio = p_folio;

   LET v_si_resultado = 0 ;
   LET v_isam_error = 0 ;
   LET v_msg_error  = "El proceso de preliquidación de REGISTRO DE PAGOS CON CAMBIO terminó correctamente";

   RETURN v_si_resultado,v_isam_error,v_msg_error;

END PROCEDURE;


