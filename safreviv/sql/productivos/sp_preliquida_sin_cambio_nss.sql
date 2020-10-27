






CREATE PROCEDURE "safreviv".sp_preliquida_sin_cambio_nss(p_folio DECIMAL(9,0), p_usuario CHAR(20))
   RETURNING SMALLINT, INTEGER, VARCHAR(255);
   --acl_preliquida
   DEFINE v_preliq_f_liquida          DATE;                       --f_liquida            date
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);               --id_derechohabiente   decimal(9,0) 
   DEFINE v_preliq_subcuenta          SMALLINT;                   --subcuenta            smallint
   DEFINE v_preliq_fondo_inversion    SMALLINT;                   --fondo_inversion      smallint
   DEFINE v_preliq_movimiento         SMALLINT;                   --movimiento           smallint
   DEFINE v_preliq_movimiento_c       SMALLINT;                   --movimiento           smallint   --saci2018-67   
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);               --folio_liquida        decimal(9,0) 
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);               --id_referencia        decimal(9,0) 
   DEFINE v_preliq_monto_acciones     DECIMAL(12,2);              --monto_acciones       decimal(22,2)
   DEFINE v_preliq_monto_pesos        DECIMAL(12,2);              --monto_pesos          decimal(22,2)
   DEFINE v_preliq_f_valor            DATE;                       --f_valor              date
   DEFINE v_preliq_f_registro         DATE;                       --f_registro           date
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;    --h_registro           datetime hour to second
   DEFINE v_preliq_usuario            CHAR(20);                   --usuario              char(20)
   
   --TABLA acl_det_sc_nss
   DEFINE v_det_folio               DECIMAL(9)   ;
   DEFINE v_det_periodo_pago        CHAR(6)      ;
   DEFINE v_det_id_derechohabiente  DECIMAL(9,0) ;
   DEFINE v_det_folio_sua           DECIMAL(6,0) ;
   DEFINE v_det_id_referencia       DECIMAL(9,0) ;
   DEFINE v_det_tpo_registro        CHAR(1)      ;
   DEFINE v_det_cve_ent_receptora   CHAR(3)      ;
   DEFINE v_det_nrp                 CHAR(11)     ;
   DEFINE v_det_rfc_patron          CHAR(13)     ;
   DEFINE v_det_f_pago_patron       DATE         ;
   DEFINE v_det_rfc                 CHAR(13)     ;
   DEFINE v_det_curp                CHAR(18)     ;
   DEFINE v_det_num_crd_ifv         CHAR(10)     ;
   DEFINE v_det_f_ini_desc_crd_ifv  DATE         ;
   DEFINE v_det_num__mov_periodo    CHAR(2)      ;
   DEFINE v_det_nombre_trabajador   CHAR(50)     ;
   DEFINE v_det_ult_sdi             DECIMAL(7,2) ;
   DEFINE v_det_tpo_trabajador      CHAR(1)      ;
   DEFINE v_det_jornada             CHAR(1)      ;
   DEFINE v_det_localiza_trabajador CHAR(1)      ;
   DEFINE v_det_destino_ap_viv      CHAR(1)      ;
   DEFINE v_det_dias_cot_bim        SMALLINT     ;
   DEFINE v_det_dias_incap_bim      SMALLINT     ;
   DEFINE v_det_dias_ausent_bim     SMALLINT     ;
   DEFINE v_det_imp_ap_pat          DECIMAL(12,2);
   DEFINE v_det_imp_am_cre          DECIMAL(12,2);
   DEFINE v_det_imp_ren_viv         DECIMAL(12,2);
   DEFINE v_det_marca_sua           CHAR(2)      ;
   DEFINE v_det_marca_bdnsar        CHAR(1)      ;
   DEFINE v_det_diag_aclaracion     CHAR(2)      ;
   DEFINE v_det_f_proceso           DATE         ;
   DEFINE v_det_tpo_aclaracion      char(2)      ;
   DEFINE v_det_aivs                DECIMAL(18,6);
   DEFINE v_det_valor_aiv           DECIMAL(18,6);
   DEFINE v_det_int_gen_pgo_ext     DECIMAL(12,2);
   DEFINE v_det_aiv_gen_pgo_ext     DECIMAL(18,6);

   --TABLA acl_sum_sc_nss
   DEFINE v_sum_folio                DECIMAL(9)   ;
   DEFINE v_sum_tpo_registro         CHAR(2)      ;
   DEFINE v_sum_tot_ap               INTEGER      ;
   DEFINE v_sum_suma_ap_pat          DECIMAL(18,2);
   DEFINE v_sum_suma_am              DECIMAL(18,2);
   DEFINE v_sum_suma_aivs            DECIMAL(18,6);
   DEFINE v_sum_suma_int_viv_pgo_ext DECIMAL(18,2);
   DEFINE v_sum_suma_aiv_pgo_ext     DECIMAL(18,6);

   DEFINE v_tpo_archivo            SMALLINT;
   DEFINE v_fn_existe_pgo_cta      SMALLINT;
   DEFINE v_fn_verifica_existencia SMALLINT;
   DEFINE v_fn_existe_pgo_ant      CHAR(01);
	 DEFINE v_bandera_nrp            SMALLINT;
   DEFINE v_tpo_patron             CHAR(2);
   DEFINE v_ind_liquidacion        SMALLINT;

  -- variables para control de excepciones
	 DEFINE v_sql_error    SMALLINT;
   DEFINE v_isam_error   INTEGER;
   DEFINE v_msg_error    VARCHAR(250);
   DEFINE v_si_resultado SMALLINT;

   DEFINE v_mov_nor SMALLINT;
   DEFINE v_mov_int SMALLINT;
   DEFINE v_mov_nor_riss_vol SMALLINT;
   DEFINE v_mov_int_riss_vol SMALLINT;
   DEFINE v_mov_nor_gf_riss_viv SMALLINT;
   DEFINE v_mov_int_gf_riss_viv SMALLINT;
   DEFINE v_mov_nor_gf_riss_vol SMALLINT;
   DEFINE v_mov_int_gf_riss_vol SMALLINT;
   
   DEFINE v_mov_trm_apo_abo_sc_nss SMALLINT;    --saci2018-67
   DEFINE v_mov_trm_apo_car_sc_nss SMALLINT;    --saci2018-67
   DEFINE v_mov_trm_amo_abo_sc_nss SMALLINT;    --saci2018-67
   DEFINE v_mov_trm_amo_car_sc_nss SMALLINT;    --saci2018-67
   DEFINE v_mov_trm_int_abo_sc_nss SMALLINT;    --saci2018-67
   DEFINE v_mov_trm_int_car_sc_nss SMALLINT;    --saci2018-67

   DEFINE v_cont_nrp_riss SMALLINT;
   DEFINE v_origen_liq    SMALLINT;

	 ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
	    LET v_msg_error = TRIM(v_msg_error)||":"||v_det_id_derechohabiente;
      RETURN v_sql_error,v_isam_error,v_msg_error;
   END EXCEPTION;    --WITH RESUME;


   -- Movimientos sub44 viv NRP Cualquiera
   LET v_mov_nor = 51;       -- ABONO SC NSS
   LET v_mov_int = 23;       -- INTERES SC NSS

   -- Movimientos sub44 viv NRP B0799991101
   LET v_mov_nor_gf_riss_viv = 791; -- ABONO ACL SUB RISS GF SC NSS
   LET v_mov_int_gf_riss_viv = 213; -- INTERESES ACL SUB RISS GF SC NSS

   -- Movimientos sub55 vol 32 NRPS
   LET v_mov_nor_riss_vol = 741; -- ABONO ACL VOL RISS SC NSS
   LET v_mov_int_riss_vol = 163; -- INTERESES ACL VOL RISS SC NSS

   -- Movimientos sub55 vol NRP B0799994105
   LET v_mov_nor_gf_riss_vol = 781;  -- ABONO ACL SUB RISS GF SC NSS
   LET v_mov_int_gf_riss_vol = 203;  -- INTERESES ACL SUB RISS GF SC NSS
   
   ------ Movimientos sub4 viv PAGOS anteriores al 31-OCT-2012 pagados en TRM SC NSS -------
   LET v_mov_trm_apo_abo_sc_nss = 1851;     -- ABONO ACL aport TRM SC NSS     --saci2018-67
   LET v_mov_trm_apo_car_sc_nss = 2082;     -- CARGO ACL aport TRM SC NSS     --saci2018-67
   
   LET v_mov_trm_amo_abo_sc_nss = 1861;     -- ABONO ACL amort TMR SC NSS     --saci2018-67 
   LET v_mov_trm_amo_car_sc_nss = 2092;     -- CARGO ACL amort TRM SC NSS     --saci2018-67
   
   LET v_mov_trm_int_abo_sc_nss = 1911;        -- ABONO ACL INT TRM SC NSS     --saci2018-67
   LET v_mov_trm_int_car_sc_nss = 2142;        -- CARGO ACL INT TRM SC NSS     --saci2018-67

   -----------------------------------------------------------------------------------------


  --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquidacionSinCambio.trace';
  --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;

   --Constantes para todos los derechohabintes que pertenecen al folio
   LET v_tpo_archivo = 5;
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_folio_liquida   = p_folio;
   LET v_preliq_usuario         = p_usuario;
   LET v_bandera_nrp = 0;
   LET v_origen_liq = 1;   -- liquidacion ACL

  -- TRACE "Inicia  FOREACH";

   FOREACH cur_his_pagos FOR
      SELECT a.id_referencia           ,
             a.cve_ent_receptora       ,
             a.nrp                     ,
             a.periodo_pago            ,
             a.folio_sua               ,
             a.f_pago                  ,                                    
             a.id_derechohabiente      ,
             a.localiza_trabajador     , 
             a.tpo_aclaracion          ,
             c.imp_ap_pat              , --c
             a.imp_am_cre              ,
             --se modifica el día 7 de agosto de 2012 por instrucción de Gerardo Vega  y por regla de negocio
             0                         ,
             --imp_ren_viv_pgo_ext     ,
             c.aiv_ap_pat              , --c
             a.valor_aiv               ,
             c.int_gen_pgo_ext         , --c
             c.aiv_gen_pgo_ext         , --c
             a.tpo_patron              ,
             a.ind_liquidacion         ,
             a.destino_ap_viv			 --Agregado por CABC
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
             v_ind_liquidacion		     ,
             v_det_destino_ap_viv      --Agregado por CABC
      FROM   cta_his_pagos a,
             acl_pag_registrado c
      WHERE  a.folio = p_folio
      and    c.folio = a.folio
      and    c.id_referencia = a.id_referencia
      AND    a.result_operacion = 1  --Solo se procesan los aceptados, los que econtraron pago anterior
                                          --Se procesan los rechazados a partir del 18-nov-2015 de acuerdo YA NO APLICA
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
                                               v_det_cve_ent_receptora,
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
         AND    id_referencia = v_det_id_referencia;
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
         AND    id_referencia = v_det_id_referencia;
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

      LET v_cont_nrp_riss = 0;
      
      IF EXISTS (SELECT nrp
                 FROM   cat_riss_nrp
                 WHERE  nrp = v_det_nrp
                 AND    id_nrp = 0)  THEN
         
         LET v_cont_nrp_riss = 1;
      
      END IF   -- NRP ACTIVO
      
      IF v_cont_nrp_riss = 1 OR v_det_nrp = "B0799994105"  THEN

         --Primer registro
         LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;
         
--############################Agregado por CABC############################################
         IF v_det_destino_ap_viv <> "3" THEN
               LET v_preliq_subcuenta = 55;   -- viv97 vol  riss
               LET v_preliq_fondo_inversion = 11;
            ELSE
               LET v_preliq_subcuenta = 60;    -- PORTABILIDAD     
               LET v_preliq_fondo_inversion = 10;      	            	
         END IF 
--############################Fin lineas Agregadas por CABC################################
         
         
         IF v_det_nrp = "B0799994105" THEN
            LET v_preliq_movimiento = v_mov_nor_gf_riss_vol ; -- ABONO ACL SUB RISS GF SC NSS
         ELSE
         	  LET v_preliq_movimiento = v_mov_nor_riss_vol;     -- ABONO ACL VOL RISS SC NSS
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

        -- LET v_preliq_fondo_inversion = 11;
         
         IF v_det_nrp = "B0799994105" THEN
            LET v_preliq_movimiento = v_mov_int_gf_riss_vol;  -- INTERESES ACL SUB RISS GF SC NSS
         ELSE
         	  LET v_preliq_movimiento = v_mov_int_riss_vol;     -- INTERESES ACL VOL RISS SC NSS
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
--=============================================
      ELSE

         --Primer registro
         LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;
         
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
            LET v_preliq_subcuenta = 60;      -- Portabilidad
            LET v_preliq_fondo_inversion = 10;
         END IF
--############################Fin lineas Agregadas por CABC################################

         IF v_det_nrp = "B0799991101" THEN
            LET v_preliq_movimiento = v_mov_nor_gf_riss_viv;  -- ABONO ACL SUB RISS GF SC NSS
         ELSE
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
               LET v_preliq_movimiento   = v_mov_trm_apo_abo_sc_nss;                  --saci2018-67
               LET v_preliq_movimiento_c = v_mov_trm_apo_car_sc_nss;                  --saci2018-67
            ELSE
               LET v_preliq_movimiento = v_mov_nor;       -- ABONO SC NSS
               LET v_preliq_movimiento_c = NULL;          --sace2018-67            
            END IF                                        --saci2018-67
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
                   v_preliq_movimiento_c        ,   --saci2018-67
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones*-1   ,   --saci2018-67
                   v_preliq_monto_pesos*-1      ,   --saci2018-67
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
         --LET v_preliq_movimiento     = v_mov_nor;        --saci2018-67
         
         IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
         	  LET v_preliq_movimiento   = v_mov_trm_amo_abo_sc_nss;                   --saci2018-67
         	  LET v_preliq_movimiento_c = v_mov_trm_amo_car_sc_nss;                   --saci2018-67
         ELSE
            LET v_preliq_movimiento = v_mov_nor;       -- ABONO SC NSS
            LET v_preliq_movimiento_c = NULL;          --sace2018-67
         END IF                                        --saci2018-67
         
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
                   v_preliq_movimiento_c        ,     --saci2018-67
                   v_preliq_folio_liquida       ,
                   v_preliq_id_referencia       ,
                   v_preliq_monto_acciones*-1   ,     --saci2018-67
                   v_preliq_monto_pesos*-1      ,     --saci2018-67
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

         --Tercer registro no se liquidan rendimientos

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
            LET v_preliq_subcuenta = 60;      -- Portabilidad
            LET v_preliq_fondo_inversion = 10;
         END IF
--############################Fin lineas Agregadas por CABC################################

         IF v_det_nrp = "B0799991101" THEN
            LET v_preliq_movimiento = v_mov_int_gf_riss_viv;  -- INTERESES ACL SUB RISS GF SC NSS       
         ELSE
            IF v_det_destino_ap_viv = "1" AND v_det_f_pago_patron <= "10/31/2012" THEN  --saci2018-67
               LET v_preliq_movimiento   = v_mov_trm_int_abo_sc_nss;                  --saci2018-67
               LET v_preliq_movimiento_c = v_mov_trm_int_car_sc_nss;                  --saci2018-67
           ELSE
              LET v_preliq_movimiento = v_mov_int;      --INTERES SC NSS
              LET v_preliq_movimiento_c = NULL;         --sace2018-67            
            END IF                                      --saci2018-67
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
--              WHERE  folio = p_folio and id_referencia = v_det_id_referencia)  THEN
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
    LET v_msg_error  = "El proceso de preliquidación de REGISTRO DE PAGOS SIN CAMBIO terminó correctamente" ;

   RETURN v_si_resultado,v_isam_error,v_msg_error;
END PROCEDURE;


