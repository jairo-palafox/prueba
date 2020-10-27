






CREATE PROCEDURE "safreviv".sp_preliquida_carga_inicial_aclaraciones(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT, INTEGER, VARCHAR(255)

   --acl_preliquida
   DEFINE v_preliq_f_liquida          DATE;                       --f_liquida            date
   DEFINE v_preliq_id_derechohabiente DECIMAL(9,0);               --id_derechohabiente   decimal(9,0)
   DEFINE v_preliq_subcuenta          SMALLINT;                   --subcuenta            smallint
   DEFINE v_preliq_fondo_inversion    SMALLINT;                   --fondo_inversion      smallint
   DEFINE v_preliq_movimiento         SMALLINT;                   --movimiento           smallint
   DEFINE v_preliq_folio_liquida      DECIMAL(9,0);               --folio_liquida        decimal(9,0)
   DEFINE v_preliq_id_referencia      DECIMAL(9,0);               --id_referencia        decimal(9,0)
   DEFINE v_preliq_monto_acciones     DECIMAL(22,2);              --monto_acciones       decimal(22,2)
   DEFINE v_preliq_monto_pesos        DECIMAL(22,2);              --monto_pesos          decimal(22,2)
   DEFINE v_preliq_f_valor            DATE;                       --f_valor              date
   DEFINE v_preliq_f_registro         DATE;                       --f_registro           date
   DEFINE v_preliq_h_registro         DATETIME HOUR TO SECOND;    --h_registro           datetime hour to second
   DEFINE v_preliq_usuario            CHAR(20);                   --usuario              char(20)

   --TABLA acl_det_aclaracion
   DEFINE v_det_folio                decimal(9,0) ;
   DEFINE v_det_periodo_pago         char(6)      ;
   DEFINE v_det_id_derechohabiente   decimal(9,0) ;
   DEFINE v_det_folio_sua            decimal(6,0) ;
   DEFINE v_det_id_referencia        decimal(9,0) ;
   DEFINE v_det_tpo_registro         char(1)      ;
   DEFINE v_det_cve_ent_receptora    char(3)      ;
   DEFINE v_det_f_pago_patron        date         ;
   DEFINE v_det_nrp                  char(11)     ;
   DEFINE v_det_num_crd_ifv          char(10)     ;
   DEFINE v_det_imp_ap_pat           decimal(12,2);
   DEFINE v_det_imp_am_cre           decimal(12,2);
   DEFINE v_det_ind_crd_ifv          char(1)      ;
   DEFINE v_det_tpo_aclaracion       char(2)      ;
   DEFINE v_det_aivs                 decimal(18,6);
   DEFINE v_det_valor_aiv            decimal(18,6);
   DEFINE v_tpo_patron               CHAR(02);
   DEFINE v_ind_liquidacion          SMALLINT;
   DEFINE v_int_gen_pgo_ext          DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext          DECIMAL(18,6);      

   --TABLA acl_sum_aclaracion
   DEFINE v_sum_folio                decimal(9,0) ;  --folio                decimal(9,0)
   DEFINE v_sum_tpo_registro         char(2)      ;  --tpo_registro         char(1)
   DEFINE v_sum_tot_ap               integer      ;  --tot_ap               integer
   DEFINE v_sum_suma_ap_pat          decimal(18,2);  --suma_ap_pat          decimal(18,2)
   DEFINE v_sum_suma_am              decimal(18,2);  --suma_am              decimal(18,2)
   DEFINE v_sum_suma_aivs            decimal(18,6);  --suma_aivs            decimal(18,6)
   
   DEFINE v_tpo_afiliacion          SMALLINT;
   DEFINE v_tpo_aclaracion          SMALLINT;
   DEFINE v_tpo_archivo             smallint;
   DEFINE v_fn_existe_pgo_cta       SMALLINT;
   DEFINE v_bandera_nrp             SMALLINT;

   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

   -- estados de pago
   DEFINE v_PAGO_NORMAL_LIQUIDADO                     SMALLINT; -- PAGO NORMAL LIQUIDADO                                                                                                                         
   DEFINE v_ACLARATORIO_SIN_LIQUIDAR                  SMALLINT; -- ACLARATORIO SIN LIQUIDAR                                                                                                                      
   DEFINE v_SALIDA_ACL_LIQUIDADA_ADELANTADA           SMALLINT; -- SALIDA ACLARATORIO LIQUIDADA ADELANTADA                                                                                                       
   DEFINE v_SALIDA_ACL_LIQUIDADA_NORMAL_NO_ADELANTADA SMALLINT; -- SALIDA ACLARATORIO LIQUIDADA NORMAL NO ADELANTADA (con cambio nss y nombre, sin cambio nss) punto 3.13 de DF Aclaratorio                      
   DEFINE v_PAGADO_PREVIO_SALIDA_CONFIRMADA           SMALLINT; -- PAGADO PREVIO Y SALIDA CONFIRMADA                                                                                                             
   DEFINE v_ACLARATORIO_SIN_LIQ_INCONSISTENTE         SMALLINT; -- ACLARATORIO SIN LIQUIDAR INCONSISTENTE (con cambio nss y nombre, sin cambio nss) punt 3.17 de DF Aclaratorio po no existir historico anterior 
   DEFINE v_ACLARATORIO_SIN_CAMBIO_ESTADO             SMALLINT; -- ACLARATORIO SIN CAMBIO DE ESTADO (enaclara) El No del punto 4.8                                                                               

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   --SET DEBUG FILE TO 'sp_preliquida_carga_inicial.trace';
   --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;

   --Constantes para todos los derechohabintes que pertenecen al folio
   LET v_tpo_archivo            = 4;
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_folio_liquida   = p_folio;
   LET v_preliq_usuario         = p_usuario;
   LET v_bandera_nrp            = 0;                      
   LET v_ind_liquidacion        = 0;
   LET v_tpo_afiliacion         = 0;
   LET v_tpo_aclaracion         = 0;

   -- constantes de estados de pago
   LET v_PAGO_NORMAL_LIQUIDADO                     = 10;
   LET v_ACLARATORIO_SIN_LIQUIDAR                  = 20;
   LET v_SALIDA_ACL_LIQUIDADA_ADELANTADA           = 30;
   LET v_SALIDA_ACL_LIQUIDADA_NORMAL_NO_ADELANTADA = 40;
   LET v_PAGADO_PREVIO_SALIDA_CONFIRMADA           = 50;
   LET v_ACLARATORIO_SIN_LIQ_INCONSISTENTE         = 60;
   LET v_ACLARATORIO_SIN_CAMBIO_ESTADO             = 70;


   --Borra la tabla: acl_enaclara_preliquida
   DROP TABLE  if exists  acl_enaclara_preliquida ;   
   
   --TRACE 'crea la tabla :acl_enaclara_preliquida';
		CREATE TABLE acl_enaclara_preliquida
		  (
		    f_liquida                  DATE NOT NULL ,
		    id_derechohabiente         DECIMAL(9,0) NOT NULL ,
		    subcuenta                  SMALLINT NOT NULL ,
		    fondo_inversion            SMALLINT NOT NULL ,
		    movimiento                 SMALLINT NOT NULL ,
		    folio_liquida              DECIMAL(9,0) NOT NULL ,
		    id_referencia              DECIMAL(9,0) NOT NULL ,
		    monto_acciones             DECIMAL(22,2),
		    monto_pesos                DECIMAL(22,2),
		    f_valor                    DATE,
		    f_registro                 DATE,
		    h_registro                 DATETIME HOUR TO SECOND,
		    origen                     CHAR(20)
		  );
		
		revoke all on acl_enaclara_preliquida from "public" as "safreviv";
		create unique index xpkacl_enaclara_preliquida on acl_enaclara_preliquida 
		   (f_liquida,id_derechohabiente,subcuenta,fondo_inversion,
		    movimiento,folio_liquida,id_referencia) using btree ;   


   
   --TRACE "Inicia  FOREACH";
   
   FOREACH cur_his_pagos FOR
           SELECT id_referencia         , --v_det_id_referencia
                  cve_ent_receptora     , --v_det_cve_ent_receptora
                  nrp                   , --v_det_nrp
                  periodo_pago          , --v_det_periodo_pago
                  folio_sua             , --v_det_folio_sua
                  f_pago                , --v_det_f_pago_patron          --v_det_f_pago_patron
                  id_derechohabiente    , --v_det_id_derechohabiente
                  tpo_aclaracion        , --v_det_tpo_aclaracion
                  imp_ap_pat            , --v_det_imp_ap_pat
                  imp_am_cre            , --v_det_imp_am_cre
                  aiv_ap_pat            , --v_det_aivs
                  valor_aiv             , --v_det_valor_aiv
                  num_crd_ifv           , --v_det_num_crd_ifv
                  tpo_patron            , --tipo de patro
                  int_gen_pgo_ext       , --intereses del pago extemporaneo
                  aiv_gen_pgo_ext         --aivs del pago extemporaneo    

           INTO   v_det_id_referencia      ,   --id_referencia
                  v_det_cve_ent_receptora  ,   --cve_ent_receptora
                  v_det_nrp                ,   --nrp
                  v_det_periodo_pago       ,   --periodo_pago
                  v_det_folio_sua          ,   --folio_sua
                  v_det_f_pago_patron      ,   --f_pago
                  v_det_id_derechohabiente ,   --id_derechohabiente
                  v_det_tpo_aclaracion     ,   --tpo_aclaracion
                  v_det_imp_ap_pat         ,   --imp_ap_pat
                  v_det_imp_am_cre         ,   --imp_am_cre
                  v_det_aivs               ,   --aiv_ap_pat
                  v_det_valor_aiv          ,   --valor_aiv
                  v_det_num_crd_ifv        ,   --num_crd_ifv
                  v_tpo_patron             ,
                  v_int_gen_pgo_ext        ,
                  v_aiv_gen_pgo_ext              
           FROM   cta_his_pagos
           WHERE  folio = p_folio
           AND    result_operacion = 1

--============================= INICIO REGLA DE NOGICIO 25-JUN-2012 GAVP =============================
  
      IF v_tpo_patron = "99" THEN
         LET v_bandera_nrp = 1;       -- PAGO SOLO INFONAVIT
         LET v_ind_liquidacion = 2;   -- ACL ADELANTADA SOLO INFONAVIT
         LET v_tpo_afiliacion  = 2;   -- SOLO INFONAVIT

      ELSE
         IF (v_det_tpo_aclaracion = "17" OR v_det_tpo_aclaracion = "13" ) THEN
         
             LET v_bandera_nrp = 0;       -- PAGO IMSS
             LET v_ind_liquidacion = 3;   -- ACL ADELANTADA FUSION(13) o TRASPASO(17)
             LET v_tpo_afiliacion  = 1;   -- AFORE
         ELSE

            -- Actualiza estado del pago
            EXECUTE PROCEDURE sp_actualiza_estado_pago
                   (p_folio,                  --folio
                    v_det_id_referencia,      --id_referencia  
                    v_det_id_derechohabiente, --id_derechohabiente
                    v_ACLARATORIO_SIN_LIQUIDAR, 
                    v_tpo_aclaracion,         --tpo_aclaracion
                    TODAY                     --f_actualiza
                    );

            --TRACE "Proceso 6: pag_excep_aclaracion";
            --Si se cumple con esta confición no se preliquida y se genera una excepción
            INSERT INTO pag_excep_aclaracion
            VALUES (p_folio,
                    v_det_id_referencia,
                    v_det_id_derechohabiente,
                    v_tpo_archivo);
                    
            CONTINUE FOREACH;         
         
         END IF;
      END IF;




-----------      LET v_fn_existe_pgo_cta = fn_existe_pago(v_det_id_derechohabiente,
-----------                                               v_det_nrp,
-----------                                               v_det_periodo_pago,
-----------                                               v_det_folio_sua,
-----------                                               v_det_f_pago_patron,
-----------                                               v_det_imp_ap_pat,     --monto_pesos
-----------                                               v_det_imp_am_cre,          --amortización
-----------                                               v_int_gen_pgo_ext,     --interes generado pago extemporaneo
-----------                                               p_folio,              --folio arch qse esta procesando
-----------                                               v_ind_liquidacion);   --ind_liquidacion  
-----------
-----------      IF v_fn_existe_pgo_cta = 1 THEN   -- 1=EL pago ya existe liquidado
-----------
-----------            -- Actualiza estado del pago
-----------            EXECUTE PROCEDURE sp_actualiza_estado_pago
-----------                   (p_folio,                  --folio
-----------                    v_det_id_referencia,      --id_referencia  
-----------                    v_det_id_derechohabiente, --id_derechohabiente
-----------                    v_PAGADO_PREVIO_SALIDA_CONFIRMADA,                       --estado_pago = PAGADO PREVIO Y SALIDA CONFIRMADA
-----------                    v_tpo_aclaracion,         --tpo_aclaracion
-----------                    TODAY                     --f_actualiza
-----------                    );
-----------
-----------                                               
-----------         UPDATE cta_his_pagos
-----------         SET    result_operacion = 2
-----------         WHERE  CURRENT OF cur_his_pagos;
-----------
-----------         --Ya no se preliquida el registro y continua con el siguiente registro
-----------         CONTINUE FOREACH;
-----------      ELSE                               -- 0=pago no existe liquidado
-----------            -- Actualiza estado del pago
-----------            EXECUTE PROCEDURE sp_actualiza_estado_pago
-----------                   (p_folio,                  --folio
-----------                    v_det_id_referencia,      --id_referencia  
-----------                    v_det_id_derechohabiente, --id_derechohabiente
-----------                    v_SALIDA_ACL_LIQUIDADA_ADELANTADA,
-----------                    v_tpo_aclaracion,         --tpo_aclaracion
-----------                    TODAY                     --f_actualiza
-----------                    );
-----------                                               
-----------         --CONTINUA CON LA PRELIQUIDACIÓN
-----------      END IF;      
 
      --TRACE "Paso 7: Preliquidacion";
      --SI PASO LAS VALIDACIONES ANTERIORES ***  INICIA LA PRELIQUIDACIÓN  ****

      LET v_preliq_id_referencia = v_det_id_referencia;
      LET v_preliq_f_valor       = v_det_f_pago_patron;
      LET v_preliq_f_liquida     = TODAY;
      LET v_preliq_f_registro    = TODAY;
      LET v_preliq_h_registro    = CURRENT HOUR TO SECOND;

      --Primer registro
      LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 44; --vivienda 97 solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 4;  --vivienda 97
      END IF
      LET v_preliq_movimiento     = 41;
      LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
      LET v_preliq_monto_acciones = v_det_aivs;

      IF ( v_preliq_monto_pesos > 0 ) THEN
         INSERT INTO acl_preliquida (
            f_liquida         , 
            id_derechohabiente, 
            subcuenta         , 
            fondo_inversion   , 
            movimiento        ,
            folio_liquida     , 
            id_referencia     , 
            monto_acciones    , 
            monto_pesos       , 
            f_valor           ,
            f_registro        , 
            h_registro        , 
            origen
            )
         VALUES (
            v_preliq_f_liquida         ,
            v_preliq_id_derechohabiente,
            v_preliq_subcuenta         ,
            v_preliq_fondo_inversion   ,
            v_preliq_movimiento        ,
            v_preliq_folio_liquida     ,
            v_preliq_id_referencia     ,
            v_preliq_monto_acciones    ,
            v_preliq_monto_pesos       ,
            v_preliq_f_valor           ,
            v_preliq_f_registro        ,
            v_preliq_h_registro        ,
            v_det_nrp
            );
      END IF

      --Segundo registro
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 43;  --amortizacion viv97 solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 41;  --amortizacion viv97
      	-- HCRG 16-04-2012 para subcuenta 41 y movimiento 41, siefore = 10
      	LET v_preliq_fondo_inversion = 10;
      END IF
      
      LET v_preliq_movimiento     = 41;
      LET v_preliq_monto_pesos    = v_det_imp_am_cre;
      LET v_preliq_monto_acciones = v_det_imp_am_cre;

      IF ( v_preliq_monto_pesos > 0 ) THEN
         INSERT INTO acl_preliquida (
            f_liquida           ,
            id_derechohabiente  ,
            subcuenta           ,
            fondo_inversion     ,
            movimiento          ,
            folio_liquida       ,
            id_referencia       ,
            monto_acciones      ,
            monto_pesos         ,
            f_valor             ,
            f_registro          ,
            h_registro          ,
            origen
            )
         VALUES (
            v_preliq_f_liquida          ,
            v_preliq_id_derechohabiente ,
            v_preliq_subcuenta          ,
            v_preliq_fondo_inversion    ,
            v_preliq_movimiento         ,
            v_preliq_folio_liquida      ,
            v_preliq_id_referencia      ,
            v_preliq_monto_acciones     ,
            v_preliq_monto_pesos        ,
            v_preliq_f_valor            ,
            v_preliq_f_registro         ,
            v_preliq_h_registro         ,
            v_det_nrp
            );
      END IF
      -- HCRG 16-04-2012 si subcuenta fué 41 y movimiento 41, siefore regresa a 11
      LET v_preliq_fondo_inversion = 11;
      
      -- =======================================================
      -- 6 sep 2012. Se agrega preliquidacion de intereses de pago extemporaneo
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 4;    -- viv97
      END IF
      
      -- se asignan los datos al movimiento y montos
      LET v_preliq_movimiento     = 63;  
      LET v_preliq_monto_pesos    = v_int_gen_pgo_ext; -- intereses del pago extemporaneo
      LET v_preliq_monto_acciones = v_aiv_gen_pgo_ext; -- aivs del pago extemporaneo

      IF ( v_preliq_monto_pesos > 0 ) THEN
         INSERT INTO acl_preliquida (
            f_liquida           ,
            id_derechohabiente  ,
            subcuenta           ,
            fondo_inversion     ,
            movimiento          ,
            folio_liquida       ,
            id_referencia       ,
            monto_acciones      ,
            monto_pesos         ,
            f_valor             ,
            f_registro          ,
            h_registro          ,
            origen
            )
         VALUES (
            v_preliq_f_liquida          ,
            v_preliq_id_derechohabiente ,
            v_preliq_subcuenta          ,
            v_preliq_fondo_inversion    ,
            v_preliq_movimiento         ,
            v_preliq_folio_liquida      ,
            v_preliq_id_referencia      ,
            v_preliq_monto_acciones     ,
            v_preliq_monto_pesos        ,
            v_preliq_f_valor            ,
            v_preliq_f_registro         ,
            v_preliq_h_registro         ,
            v_det_nrp
            );
      END IF

   END FOREACH

   --Se actualiza la tabla que lleva el control de los folios
   UPDATE glo_folio
   SET    status = 1
   WHERE  folio = p_folio;
   
   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de preliquidación de CI de Aclaraciones finalizó correctamente.";

   RETURN v_si_resultado, isam_err, err_txt;
END PROCEDURE
;


