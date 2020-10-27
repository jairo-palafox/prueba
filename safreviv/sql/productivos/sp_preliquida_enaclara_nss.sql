






CREATE PROCEDURE "safreviv".sp_preliquida_enaclara_nss(p_folio DECIMAL(9,0), p_usuario CHAR(20))
RETURNING SMALLINT, SMALLINT, VARCHAR(255), DECIMAL(9,0)
   --acl_enaclara_preliquida
   DEFINE v_preliq_f_liquida               DATE;                       --f_liquida            date                   
   DEFINE v_preliq_id_derechohabiente      DECIMAL(9,0);               --id_derechohabiente   decimal(9,0)           
   DEFINE v_preliq_subcuenta               SMALLINT;                   --subcuenta            smallint               
   DEFINE v_preliq_fondo_inversion         SMALLINT;                   --fondo_inversion      smallint               
   DEFINE v_preliq_movimiento              SMALLINT;                   --movimiento           smallint               
   DEFINE v_preliq_folio_liquida           DECIMAL(9,0);               --folio_liquida        decimal(9,0)           
   DEFINE v_preliq_id_referencia           DECIMAL(9,0);               --id_referencia        decimal(9,0)           
   DEFINE v_preliq_monto_acciones          DECIMAL(18,2);              --monto_acciones       decimal(22,2)          
   DEFINE v_preliq_monto_pesos             DECIMAL(18,2);              --monto_pesos          decimal(22,2)          
   DEFINE v_preliq_f_valor                 DATE;                       --f_valor              date                   
   DEFINE v_preliq_f_registro              DATE;                       --f_registro           date                   
   DEFINE v_preliq_h_registro              DATETIME HOUR TO SECOND;    --h_registro           datetime hour to second
   DEFINE v_preliq_usuario                 CHAR(20);                   --usuario              char(20)               
   
   --TABLA acl_det_aclaracion
   DEFINE v_det_folio                      DECIMAL(9,0) ;
   DEFINE v_det_periodo_pago               CHAR(6)      ;
   DEFINE v_det_id_derechohabiente         DECIMAL(9,0) ;
   DEFINE v_det_folio_sua                  DECIMAL(6,0) ;
   DEFINE v_det_id_referencia              DECIMAL(9,0) ;
   DEFINE v_det_tpo_registro               CHAR(1)      ;
   DEFINE v_det_cve_ent_receptora          CHAR(3)      ;
   DEFINE v_det_f_pago_patron              DATE         ;
   DEFINE v_det_nrp                        CHAR(11)     ;
   DEFINE v_det_rfc                        CHAR(13)     ;
   DEFINE v_det_curp                       CHAR(18)     ;
   DEFINE v_det_num_crd_ifv                CHAR(10)     ;
   DEFINE v_det_f_ini_desc_cre_ifv         DATE         ;
   DEFINE v_det_nom_trabajador             CHAR(50)     ;
   DEFINE v_det_ult_sdi                    DECIMAL(7,2) ;
   DEFINE v_det_dias_cot_bim               SMALLINT     ;
   DEFINE v_det_dias_incap_bim             SMALLINT     ;
   DEFINE v_det_dias_ausent_bim            SMALLINT     ;
   DEFINE v_det_imp_ap_pat                 DECIMAL(12,2);
   DEFINE v_det_imp_am_cre                 DECIMAL(12,2);
   DEFINE v_det_ind_crd_ifv                CHAR(1)      ;
   DEFINE v_det_tpo_aclaracion             CHAR(2)      ;
   DEFINE v_det_aivs                       DECIMAL(18,6);
   DEFINE v_det_valor_aiv                  DECIMAL(18,6);
   DEFINE v_tpo_patron                     CHAR(02);
   DEFINE v_ind_liquidacion                SMALLINT;
   --TABLA acl_sum_aclaracion
   DEFINE v_sum_folio                      DECIMAL(9)   ;  --folio                decimal(9,0) 
   DEFINE v_sum_tpo_registro               CHAR(2)      ;  --tpo_registro         char(1)      
   DEFINE v_sum_tot_ap                     INTEGER      ;  --tot_ap               integer      
   DEFINE v_sum_suma_ap_pat                DECIMAL(18,2);  --suma_ap_pat          decimal(18,2)
   DEFINE v_sum_suma_am                    DECIMAL(18,2);  --suma_am              decimal(18,2)
   DEFINE v_sum_suma_aivs                  DECIMAL(18,6);  --suma_aivs            decimal(18,6)
   
   DEFINE v_tpo_archivo                    SMALLINT;
   DEFINE v_tpo_aclaracion                 SMALLINT;
   DEFINE v_int_gen_pgo_ext                DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext                DECIMAL(12,2);
   
   DEFINE v_fn_existe_pgo_cta              SMALLINT;
   DEFINE v_bandera_nrp                    SMALLINT;
   DEFINE v_s_bandera_preliquida           SMALLINT;
   
       -- Control de Excepciones
   DEFINE sql_err                          SMALLINT;
   DEFINE isam_err                         SMALLINT;
   DEFINE err_txt                          VARCHAR(255);
   DEFINE v_si_resultado                   SMALLINT;   
   
   
     --manejo de excepciones
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
      RETURN v_si_resultado, isam_err, err_txt, v_det_id_derechohabiente;
   END EXCEPTION
                 
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_preliquida_enaclara_nss.trace';
   --TRACE 'Inicia Preliquidacion con Folio:' || p_folio ||" - Fecha:" || TODAY;
   
   --Constantes para todos los derechohabintes que pertenecen al folio
   LET v_tpo_archivo = 8;
   LET v_preliq_fondo_inversion = 11;
   LET v_preliq_folio_liquida   = p_folio;
   LET v_preliq_usuario         = p_usuario;
   LET v_bandera_nrp = 0;
   LET v_ind_liquidacion = 0;      
   LET v_s_bandera_preliquida = 0;
   LET v_det_id_derechohabiente = NULL;
   
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
      SELECT id_referencia         ,      --v_det_id_referencia                                        
                  cve_ent_receptora     ,      --v_det_cve_ent_receptora                                    
                  nrp                   ,      --v_det_nrp                                                  
                  periodo_pago          ,      --v_det_periodo_pago                                         
                  folio_sua             ,      --v_det_folio_sua                                            
                  f_pago                ,      --v_det_f_pago_patron          --v_det_f_pago_patron         
                  id_derechohabiente    ,      --v_det_id_derechohabiente                                   
                  tpo_aclaracion        ,      --v_det_tpo_aclaracion                                       
                  imp_ap_pat            ,      --v_det_imp_ap_pat                                           
                  imp_am_cre            ,      --v_det_imp_am_cre                                           
                  aiv_ap_pat            ,      --v_det_aivs                                                 
                  valor_aiv             ,      --v_det_valor_aiv                                            
                  num_crd_ifv           ,      --v_det_num_crd_ifv
                  tpo_patron
           INTO   v_det_id_referencia      , --cp.id_referencia      
                  v_det_cve_ent_receptora  , --cp.cve_ent_receptora  
                  v_det_nrp                , --cp.nrp                
                  v_det_periodo_pago       , --cp.periodo_pago       
                  v_det_folio_sua          , --cp.folio_sua          
                  v_det_f_pago_patron      , --cp.f_pago             
                  v_det_id_derechohabiente , --cp.id_derechohabiente 
                  v_det_tpo_aclaracion     , --cp.tpo_aclaracion     
                  v_det_imp_ap_pat         , --cp.imp_ap_pat         
                  v_det_imp_am_cre         , --cp.imp_am_cre         
                  v_det_aivs               , --cp.aiv_ap_pat         
                  v_det_valor_aiv          , --cp.valor_aiv          
                  v_det_num_crd_ifv        , --cp.num_crd_ifv        
                  v_tpo_patron
           FROM   cta_his_pagos
           WHERE  folio = p_folio
         

      --TRACE "Proceso 0:";
 
      LET v_tpo_aclaracion = NULL;

      -- Regla obtener tpo aclaracion de acuerdo a mail de earanda del 12-dic-2011 14:07  GAVP 30-06-2012
      --Se obtiene el tipo de aclaracion de la tabla concentradora
      
      SELECT tpo_aclaracion,
             int_gen_pgo_ext,
             aiv_gen_pgo_ext
      INTO   v_tpo_aclaracion,
             v_int_gen_pgo_ext,
             v_aiv_gen_pgo_ext             
      FROM   cta_his_pagos
      WHERE  id_derechohabiente = v_det_id_derechohabiente
      AND    folio_sua          = v_det_folio_sua
      AND    periodo_pago       = v_det_periodo_pago
      AND    f_pago             = v_det_f_pago_patron
      AND    nrp                = v_det_nrp
      AND    ind_liquidacion = 1      
      AND    cve_ent_receptora  = v_det_cve_ent_receptora
      AND    imp_ap_pat         = v_det_imp_ap_pat 
      AND    imp_am_cre         = v_det_imp_am_cre
      AND    origen_archivo     in (1,4);
      
      --Preguntar a enrique si pueden llegar nuevas aclaraciones 01-jul-2012 13:03
      IF v_tpo_aclaracion IS NULL AND (v_det_tpo_aclaracion IS NULL OR v_det_tpo_aclaracion="") THEN
         --TRACE "Proceso 1: pag_excep_aclaracion";
         --Si no encontro el registro se marca un excepción 
         INSERT INTO pag_excep_aclaracion
         VALUES (p_folio
                ,v_det_id_referencia
                ,v_det_id_derechohabiente
                ,v_tpo_archivo);
         --Se pasa al siguiente registro ya que no se le realiza la preliquidación
         
         UPDATE cta_his_pagos
         SET    result_operacion = 2
         WHERE  CURRENT OF cur_his_pagos;
         
         CONTINUE FOREACH;
      END IF;

      IF v_det_tpo_aclaracion IS NULL OR v_det_tpo_aclaracion="" THEN
         LET v_det_tpo_aclaracion = v_tpo_aclaracion;
      END IF;
      
      IF v_int_gen_pgo_ext IS NULL THEN
         LET v_int_gen_pgo_ext = 0;
         LET v_aiv_gen_pgo_ext = 0;
      END IF;
      
      --=============================================

      IF (v_det_tpo_aclaracion = "17" OR v_det_tpo_aclaracion = "13") THEN

          LET v_bandera_nrp = 0;       -- PAGO IMSS
          LET v_ind_liquidacion = 3;   -- ACL ADELANTADA FUSION(13) o TRASPASO(17)

      ELSE

         -- Compara ACL nueva con ACL historica y si es igual 
         -- Generar estado_pago=90 ACLARATORIO CON CAMBIO DE ESTADO SIN LIQUIDAR
       
         IF v_det_tpo_aclaracion = v_tpo_aclaracion THEN  

            -- Actualiza estado del pago
            EXECUTE PROCEDURE sp_actualiza_estado_pago
                   (p_folio,                  --folio
                    v_det_id_referencia,      --id_referencia  
                    v_det_id_derechohabiente, --id_derechohabiente
                    70,                       --estado_pago = ACLARATORIO SIN CAMBIO DE ESTADO SIN LIQUDIAR (enaclara) El No del punto 4.8                    
                    v_det_tpo_aclaracion,     --tpo_aclaracion
                    TODAY                     --f_actualiza
                    );         

         ELSE
      
            -- Actualiza estado del pago
            EXECUTE PROCEDURE sp_actualiza_estado_pago
                   (p_folio,                  --folio
                    v_det_id_referencia,      --id_referencia  
                    v_det_id_derechohabiente, --id_derechohabiente
                    90,                       --estado_pago = ACLARATORIO CON CAMBIO DE ESTADO SIN LIQUIDAR
                    v_det_tpo_aclaracion,     --tpo_aclaracion
                    TODAY                     --f_actualiza
                    );

         END IF;

         UPDATE cta_his_pagos
         SET    ind_liquidacion  = 1,  -- ACLARATORIO SIN CAMBIO DE ESTADO (enaclara) El No del punto 4.8
                result_operacion = 2
         WHERE CURRENT OF cur_his_pagos;

         CONTINUE FOREACH;
         
      END IF;

      LET v_fn_existe_pgo_cta = fn_existe_pago_enaclara(v_det_id_derechohabiente,
                                               v_det_cve_ent_receptora,
                                               v_det_nrp,
                                               v_det_periodo_pago,
                                               v_det_folio_sua,
                                               v_det_f_pago_patron,
                                               v_det_imp_ap_pat,          --vivienda
                                               v_det_imp_am_cre,          --amortización
                                               --v_det_int_gen_pgo_ext,     --interes generado pago extemporaneo                                               
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

         UPDATE cta_his_pagos
         SET    ind_liquidacion  = 4  -- APORTACION CONCILIADA CON PROCESAR DE ADELANTAMIENTOS
         WHERE CURRENT OF cur_his_pagos;

         --Ya no se preliquida el registro y continua con el siguiente registro
         CONTINUE FOREACH;
         
      ELSE                               -- 0=pago no existe liquidado
      
         -- Actualiza estado del pago
         EXECUTE PROCEDURE sp_actualiza_estado_pago
                 (p_folio,                  --folio
                  v_det_id_referencia,      --id_referencia
                  v_det_id_derechohabiente, --id_derechohabiente
                  30,                       --estado_pago = SALIDA ACLARATORIO LIQUIDADA ADELANTADA
                  v_det_tpo_aclaracion,     --tpo_aclaracion
                  TODAY                     --f_actualiza
                 );
                     
         -- definición de Hamir del 17-sep-2012 en oficiona de EFP
         -- debe actualiar los intereses con los encontrados en el historico                    
         UPDATE cta_his_pagos
         SET    int_gen_pgo_ext = v_int_gen_pgo_ext,
                aiv_gen_pgo_ext = v_aiv_gen_pgo_ext
         WHERE CURRENT OF cur_his_pagos;
                                      
         --CONTINUA CON LA PRELIQUIDACIÓN
      END IF;
--=============================================      
      
      --TRACE "Paso 7: Preliquidacion";
      --SI PASO LAS VALIDACIONES ANTERIORES ***  INICIA LA PRELIQUIDACIÓN  ****
      
      LET v_preliq_id_referencia   = v_det_id_referencia; 
      LET v_preliq_f_valor         = v_det_f_pago_patron;
      LET v_preliq_f_liquida      = TODAY;
      LET v_preliq_f_registro     = TODAY;
      LET v_preliq_h_registro     = CURRENT HOUR TO SECOND;
      
      --Primer registro
      LET v_preliq_id_derechohabiente = v_det_id_derechohabiente;
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 44;      -- viv97 solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 4;       -- viv97
      END IF
      LET v_preliq_movimiento     = 41;
      LET v_preliq_monto_pesos    = v_det_imp_ap_pat;
      LET v_preliq_monto_acciones = v_det_aivs;
      
      IF v_preliq_monto_pesos > 0 THEN
         --Se inicializa la bandera significa que preliquido un registro
         LET v_s_bandera_preliquida = 1 ;
      	
      	--TRACE "v_preliq_monto_pesos"||v_preliq_movimiento ;
      	 
            INSERT INTO acl_enaclara_preliquida
                     ( 
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
               VALUES(
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
      
      --Segundo registro
      -- HCRG 16-04-2012 para subcuenta 41 y movimiento 41, siefore = 10
      LET v_preliq_fondo_inversion = 10; 
          
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 43;     -- amort viv97  solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 41;     -- amort viv97
      END IF
      
      LET v_preliq_movimiento     = 41;
      LET v_preliq_monto_pesos    = v_det_imp_am_cre;
      LET v_preliq_monto_acciones = v_det_imp_am_cre;
      
      IF v_preliq_monto_pesos > 0 THEN
         --Se inicializa la bandera significa que preliquido un registro              
         LET v_s_bandera_preliquida = 1 ;	
      	 --TRACE "v_preliq_monto_pesos 2"||v_preliq_movimiento ;
         INSERT INTO acl_enaclara_preliquida
                     ( 
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
               VALUES(
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
      
      -- HCRG 16-04-2012 si subcuenta fué 41 y movimiento 41, siefore regresa a 11
      LET v_preliq_fondo_inversion = 11;
      
      --Cuarto registro
      IF v_bandera_nrp = 1 THEN
      	LET v_preliq_subcuenta = 44;   -- viv97 solo infonavit
      ELSE
      	LET v_preliq_subcuenta = 4;    -- viv97
      END IF
      
      LET v_preliq_movimiento     = 63;
      LET v_preliq_monto_pesos    = v_int_gen_pgo_ext;
      LET v_preliq_monto_acciones = v_aiv_gen_pgo_ext;
      
      
     -- TRACE 'INSERT 4: ' || v_preliq_monto_pesos;
     IF v_preliq_monto_pesos > 0 THEN
         INSERT INTO acl_enaclara_preliquida
                      ( 
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
               VALUES(
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
            
   END FOREACH   
                 


   IF (v_s_bandera_preliquida = 1 ) THEN                   
      --Se actualiza la tabla que lleva el control de los folios
      UPDATE glo_folio
      SET    status = 1
      WHERE  folio = p_folio;

      --Se actualiza el estatus del archivo
      UPDATE glo_ctr_archivo
      SET    estado = 2
      WHERE  folio = p_folio;
      
      LET v_si_resultado = 0 ;
      LET isam_err = 0 ;
      LET err_txt  = "El proceso de preliquidación de REGISTRO DE PAGOS ENACLARA terminó correctamente";
      
   ELSE 
      LET v_si_resultado = 1 ;
      LET isam_err = 1 ;
      LET err_txt  = "No se preliquida ya que no existen tipo de aclaración 13 o 17" ;
   END IF    

   RETURN v_si_resultado, isam_err, err_txt, v_det_id_derechohabiente;
   
END PROCEDURE;


