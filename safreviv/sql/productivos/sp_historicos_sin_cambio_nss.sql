






CREATE PROCEDURE "safreviv".sp_historicos_sin_cambio_nss(p_folio         DECIMAL(9,0),
                                              p_pid           DECIMAL(9,0),
                                              p_proceso_cod   SMALLINT     
                                              )

RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(11)

   --TABLA tmp_det_sc_nss
   DEFINE v_tmp_det_sc_nss_tpo_registro        CHAR(1);       --tpo_registro         char(1)     
   DEFINE v_tmp_det_sc_nss_cve_ent_receptora   CHAR(3);       --cve_ent_receptora    char(3)     
   DEFINE v_tmp_det_sc_nss_nrp                 CHAR(11);      --nrp                  char(11)    
   DEFINE v_tmp_det_sc_nss_rfc_patron          CHAR(13);      --rfc_patron           char(13)    
   DEFINE v_tmp_det_sc_nss_periodo_pago        CHAR(6);       --periodo_pago         char(6)     
   DEFINE v_tmp_det_sc_nss_f_pago_patron       DATE;          --f_pago_patron        date        
   DEFINE v_tmp_det_sc_nss_folio_sua           DECIMAL(6);    --folio_sua            decimal(6)  
   DEFINE v_tmp_det_sc_nss_nss                 CHAR(11);      --nss                  char(11)    
   DEFINE v_tmp_det_sc_nss_rfc                 CHAR(13);      --rfc                  char(13)    
   DEFINE v_tmp_det_sc_nss_curp                CHAR(18);      --curp                 char(18)    
   DEFINE v_tmp_det_sc_nss_num_crd_ifv         CHAR(10);      --num_crd_ifv          char(10)    
   DEFINE v_tmp_det_sc_nss_f_ini_desc_crd_ifv  DATE;          --f_ini_desc_crd_ifv   date        
   DEFINE v_tmp_det_sc_nss_num_mov_periodo     DECIMAL(2);    --num_mov_periodo      decimal(2)  
   DEFINE v_tmp_det_sc_nss_nombre_trabajador   CHAR(50);      --nombre_trabajador    char(50)    
   DEFINE v_tmp_det_sc_nss_ult_sdi             DECIMAL(9);    --ult_sdi              decimal(7)  
   DEFINE v_tmp_det_sc_nss_tpo_trabajador      CHAR(1);       --tpo_trabajador       char(1)     
   DEFINE v_tmp_det_sc_nss_jornada             CHAR(1);       --jornada              char(1)     
   DEFINE v_tmp_det_sc_nss_localiza_trabajador CHAR(1);       --localiza_trabajad+   char(1)     
   DEFINE v_tmp_det_sc_nss_destino_ap_viv      CHAR(1);       --destino_ap_viv       char(1)     
   DEFINE v_tmp_det_sc_nss_dias_cot_bim        DECIMAL(2);    --dias_cot_bim         decimal(2)  
   DEFINE v_tmp_det_sc_nss_dias_incap_bim      DECIMAL(2);    --dias_incap_bim       decimal(2)  
   DEFINE v_tmp_det_sc_nss_dias_ausent_bim     DECIMAL(2);    --dias_ausent_bim      decimal(2)  
   DEFINE v_tmp_det_sc_nss_imp_ap_pat          DECIMAL(16,2); --imp_ap_pat           decimal(12) 
   DEFINE v_tmp_det_sc_nss_imp_am_cre          DECIMAL(16,2); --imp_am_cre           decimal(12) 
   DEFINE v_tmp_det_sc_nss_imp_ren_viv         DECIMAL(16,2); --imp_ren_viv          decimal(12) 
   DEFINE v_tmp_det_sc_nss_marca_sua           CHAR(2);       --marca_sua            char(2)     
   DEFINE v_tmp_det_sc_nss_marca_bdnsar        CHAR(1);       --marca_bdnsar         char(1)     
   DEFINE v_tmp_det_sc_nss_diag_aclaracion     CHAR(2);       --diag_aclaracion      char(2)     
   DEFINE v_tmp_det_sc_nss_f_proceso           DATE;          --f_proceso            date        
   DEFINE v_tmp_det_sc_nss_aivs                DECIMAL(18,2); --aivs                 decimal(18) 
   DEFINE v_tmp_det_sc_nss_valor_aiv           DECIMAL(18,2); --valor_aiv            decimal(18) 
   DEFINE v_tmp_det_sc_nss_int_gen_pgo_ext     DECIMAL(18,2); --int_gen_pgo_ext      decimal(12) 
   DEFINE v_tmp_det_sc_nss_aiv_gen_pgo_ext     DECIMAL(18,2); --aiv_gen_pgo_ext      decimal(18) 

   --TABLA acl_det_sc_nss
   DEFINE v_acl_det_sc_nss_imp_ap_pat          DECIMAL(12,2);
   DEFINE v_acl_det_sc_nss_ult_sdi             DECIMAL(7,2);
   DEFINE v_acl_det_sc_nss_imp_am_cre          DECIMAL(12,2);
   DEFINE v_acl_det_sc_nss_imp_ren_viv         DECIMAL(12,2);
   DEFINE v_acl_det_sc_nss_aivs                DECIMAL(18,6);
   DEFINE v_acl_det_sc_nss_valor_aiv           DECIMAL(18,6);
   DEFINE v_acl_det_sc_nss_int_gen_pgo_ext     DECIMAL(12,2);   --modificado: DECIMAL(18,6)
   DEFINE v_acl_det_sc_nss_aiv_gen_pgo_ext     DECIMAL(18,6);
   
   --TABLA tmp_sum_sc_nss
   DEFINE v_tmp_sum_sc_nss_tpo_registro          CHAR(1);       --tpo_registro         char(1)    
   DEFINE v_tmp_sum_sc_nss_tot_ap                DECIMAL(9);    --tot_ap               decimal(9) 
   DEFINE v_tmp_sum_sc_nss_suma_ap_pat           DECIMAL(20);   --suma_ap_pat          decimal(18)
   DEFINE v_tmp_sum_sc_nss_suma_am               DECIMAL(20);   --suma_am              decimal(18)
   DEFINE v_tmp_sum_sc_nss_suma_aivs             DECIMAL(20);   --suma_aivs            decimal(18)
   DEFINE v_tmp_sum_sc_nss_suma_int_viv_pgo_ext  DECIMAL(20);   --suma_int_viv_pgo_+   decimal(18)
   DEFINE v_tmp_sum_sc_nss_suma_aiv_pgo_ext      DECIMAL(20);   --suma_aiv_pgo_ext     decimal(18)
   
   --TABLA acl_sum_sc_nss
   DEFINE v_acl_sum_sc_nss_suma_ap_pat           DECIMAL(18,2);
   DEFINE v_acl_sum_sc_nss_suma_am               DECIMAL(18,2);
   DEFINE v_acl_sum_sc_nss_suma_aivs             DECIMAL(18,6);  --modificado: DECIMAL(18,2)
   DEFINE v_acl_sum_sc_nss_suma_int_viv_pgo_ext  DECIMAL(18,2);
   DEFINE v_acl_sum_sc_nss_suma_aiv_pgo_ext      DECIMAL(18,6);  --modificado: DECIMAL(18,2)
   DEFINE v_ide_derechohabiente                  DECIMAL(9,0);
	 
	 DEFINE v_tipo_trabajador  CHAR(1);
	 
	 -- variables para registro de duplicados en pagos encontrados
	 DEFINE v_id_referencia       DECIMAL(9,0);
	 DEFINE v_folio_duplicado     DECIMAL(9,0);
	 DEFINE v_registro_usado      CHAR(1); -- indica que registro duplicado se uso para integrar
	 DEFINE v_contador_duplicados SMALLINT; -- para contar cuantos registros de pago en aclara se encuentran duplicados
	 DEFINE v_origen_archivo      SMALLINT;
	 DEFINE v_ind_cta             SMALLINT; -- 1 pago que se toma para aclaracion. 0 - pago duplicado
	 	       
   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;
   
   -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         DECIMAL(9,0); -- registros detalle
   DEFINE v_num_registros_detalle_sum     DECIMAL(9,0); -- registros de detalle indicados en sumario
   
   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;
   DEFINE v_err_suma_intereses_vivienda_no_coincide     SMALLINT;
   DEFINE v_err_suma_AIVS_pago_extemporaneo_no_coincide SMALLINT;
   
   --TABLA acl_det_sc_nss
   DEFINE acl_det_sc_imp_ap_pat           DECIMAL(12,2);
   DEFINE acl_det_sc_imp_am_cre           DECIMAL(12,2);
   DEFINE acl_det_sc_aivs                 DECIMAL(18,6);
   DEFINE acl_det_sc_int_gen_pgo_ext      DECIMAL(12,2);
   DEFINE acl_det_sc_aiv_gen_pgo_ext      DECIMAL(18,6);

   --TABLA acl_sum_sc_nss   
   DEFINE acl_sum_sc_suma_ap_pat          DECIMAL(18,2);
   DEFINE acl_sum_sc_suma_am              DECIMAL(18,2);
   DEFINE acl_sum_sc_suma_aivs            DECIMAL(18,6);
   DEFINE acl_sum_sc_suma_int_viv_pgo_ext DECIMAL(18,2);
   DEFINE acl_sum_sc_suma_aiv_pgo_ext     DECIMAL(18,6);
   
   DEFINE v_cta_his_pagos_tpo_patron CHAR(2);
   
   DEFINE v_sql_error INTEGER;
   DEFINE v_isam_error SMALLINT;
   DEFINE v_msg_error CHAR(200);
   
   DEFINE v_ind_liquidacion SMALLINT;
   DEFINE v_tpo_patron      CHAR(02);    

   DEFINE v_error_en_sumario SMALLINT; -- booleana para saber si hubo error en sumario
   DEFINE v_result_operacion SMALLINT;  -- se asigna valor original que es 1 = "aceptado"
   DEFINE v_tpo_aclaracion   CHAR(02);

   DEFINE v_folio_reg           DECIMAL(9,0);
   DEFINE v_id_referencia_reg   DECIMAL(9,0);
   DEFINE v_imp_ap_pat          DECIMAL(12,2);
   DEFINE v_imp_am_cre          DECIMAL(12,2);
   DEFINE v_imp_ren_viv_pgo_ext DECIMAL(12,2);
   DEFINE v_aiv_ap_pat          DECIMAL(18,6);
   DEFINE v_int_gen_pgo_ext     DECIMAL(12,2);
   DEFINE v_aiv_gen_pgo_ext     DECIMAL(18,6);
   
   DEFINE v_result_opera  SMALLINT;

   --g-
   DEFINE v_cont SMALLINT;
   DEFINE v_cont_referencia DECIMAL(9,0);

   DEFINE v_recha SMALLINT;
   
	 -- Captura el error sql
   ON EXCEPTION SET v_sql_error,v_isam_error,v_msg_error
      
      RETURN v_sql_error,v_isam_error,v_msg_error, v_tmp_det_sc_nss_nss;
   END EXCEPTION WITH RESUME;
   
   
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/sp_historicos_sin_cambio_nss.trace';
   --TRACE 'Inicia el store procedure de registro historicos de SIN CAMBIO EN NSS';

   LET v_sql_error = 0;
   LET v_isam_error = 0;
   LET v_msg_error = NULL;
   LET v_tmp_det_sc_nss_nss = NULL;
   
   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   LET v_err_suma_AIVS_pago_extemporaneo_no_coincide = 5;
   LET v_err_suma_intereses_vivienda_no_coincide     = 6;

   LET v_tpo_patron = "";
   
   LET v_result_operacion =  1;  -- "aceptado"

   --g- ALTER SEQUENCE seq_cta_his_pagos RESTART 1;
   
    -- se obtienen las sumas de montos de la tabla del sumario
   SELECT 
      SUM(suma_ap_pat         ) / 100    , -- aportaciones patronales
      SUM(suma_am             ) / 100    , -- amortizacion
      SUM(suma_aivs           ) / 1000000, -- aplicacion de intereses de vivienda
      SUM(suma_int_viv_pgo_ext) / 100    , -- intereses de vivienda
      SUM(suma_aiv_pgo_ext    ) / 1000000 -- aplicacion de intereses de vivienda pagos extemporaneos
   INTO 
      acl_sum_sc_suma_ap_pat          ,
      acl_sum_sc_suma_am              ,
      acl_sum_sc_suma_aivs            ,
      acl_sum_sc_suma_int_viv_pgo_ext ,
      acl_sum_sc_suma_aiv_pgo_ext     
   FROM safre_tmp:tmp_sum_sc_nss;

   --TRACE 'sumario'||acl_sum_sc_suma_ap_pat;
   -- se obtiene la suma de los monto de la tabla de detalle
   
   SELECT 
      SUM(imp_ap_pat     ) / 100    ,
      SUM(imp_am_cre     ) / 100    ,
      SUM(aivs           ) / 1000000,
      SUM(int_gen_pgo_ext) / 100    ,
      SUM(aiv_gen_pgo_ext) / 1000000
   INTO 
      acl_det_sc_imp_ap_pat      , -- aportaciones patronales                                
      acl_det_sc_imp_am_cre      , -- amortizacion                                           
      acl_det_sc_aivs            , -- aplicacion de intereses de vivienda                    
      acl_det_sc_int_gen_pgo_ext , -- intereses de vivienda                                  
      acl_det_sc_aiv_gen_pgo_ext   -- aplicacion de intereses de vivienda pagos extemporaneos
   FROM safre_tmp:tmp_det_sc_nss;
   
   
   --TRACE 'detalle '||acl_det_sc_imp_ap_pat;
   -- se verifica que el numero de registros en la tabla de detalle sea el mismo
   -- que el encontrado en el sumario
   SELECT COUNT(*)
   INTO v_num_registros_detalle
   FROM safre_tmp:tmp_det_sc_nss;
      
   SELECT tot_ap
   INTO v_num_registros_detalle_sum
   FROM safre_tmp:tmp_sum_sc_nss;
   
   IF v_num_registros_detalle_sum IS NULL THEN
   	  LET v_num_registros_detalle_sum = 0;
   END IF

   -- se asume que no hay error en sumario
   LET v_error_en_sumario = 0;

   IF ( v_num_registros_detalle <> v_num_registros_detalle_sum ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_numero_regs_detalle_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El número de registros de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
      
   END IF
   
   -- ========================================================================
   -- se verifican las cifras de montos totales recibidos
   -- ========================================================================

 
   -- aportaciones patronales
   IF ( acl_sum_sc_suma_ap_pat <> acl_det_sc_imp_ap_pat ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aportaciones patronales de detalle cargado no corresponde con el reportado en archivo";
   
      LET v_error_en_sumario = 1;
         
   END IF

   -- amortizacion del credito
   IF ( acl_sum_sc_suma_am <> acl_det_sc_imp_am_cre ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo";

      LET v_error_en_sumario = 1;
              
   END IF

   -- aplicacion de intereses de vivienda
   IF ( acl_sum_sc_suma_aivs <> acl_det_sc_aivs ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";
      
      LET v_error_en_sumario = 1;
      
   END IF

   -- intereses de vivienda pago extemporaneo
   IF ( acl_sum_sc_suma_int_viv_pgo_ext <> acl_det_sc_int_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_intereses_vivienda_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";
      
      LET v_error_en_sumario = 1;
      
   END IF

   -- aplicacion de intereses de vivienda pago extemporaneo
   IF ( acl_sum_sc_suma_aiv_pgo_ext <> acl_det_sc_aiv_gen_pgo_ext ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_pago_extemporaneo_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de intereses de vivienda de pago extemporáneo de detalle cargado no corresponde con el reportado en archivo";
      
      LET v_error_en_sumario = 1;
      
   END IF

   -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
   IF v_error_en_sumario = 1 THEN
      -- se actualiza el folio en turno como erroneo
      UPDATE glo_folio
      SET    status = -1
      WHERE  folio = p_folio;
      
      -- se deja el archivo en estatus de error
      UPDATE glo_ctr_archivo
      SET    folio = P_folio,
             estado = 2 -- integrado
      WHERE  proceso_cod = p_proceso_cod
      AND    opera_cod   = 1 -- archivo cargado
      AND    estado      = 1; -- etapa de carga
      
      -- se devuelve el error
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_sc_nss_nss;
   END IF

   -- Se asigna el folio al archivo y se indica que ha sido integrado
    
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   --==========   EJECUTA DETALLE   ==================================
   
   EXECUTE PROCEDURE sp_registros_sin_cuenta_acl(P_folio)
   INTO v_si_resultado, isam_err, err_txt, v_tmp_det_sc_nss_nss;

   IF v_si_resultado <> 0 THEN
      RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_sc_nss_nss;
   END IF;
      
   --TRACE 'inicia el foreach de la tabla tmp_det_sc_nss';
 
   --g-
   LET v_cont_referencia = 0;   
   
   FOREACH SELECT
   	       afi.id_derechohabiente  ,
           tmp.tpo_registro        ,
           tmp.cve_ent_receptora   ,
           tmp.nrp                 ,
           tmp.rfc_patron          ,
           tmp.periodo_pago        ,
           tmp.f_pago_patron       ,
           tmp.folio_sua           ,
           tmp.nss                 ,
           tmp.rfc                 ,
           tmp.curp                ,
           tmp.num_crd_ifv         ,
           tmp.f_ini_desc_crd_ifv  ,
           tmp.num_mov_periodo     ,
           tmp.nombre_trabajador   ,
           tmp.ult_sdi/100         ,
           tmp.tpo_trabajador      ,
           tmp.jornada             ,
           tmp.localiza_trabajador ,
           tmp.destino_ap_viv      ,
           tmp.dias_cot_bim        ,
           tmp.dias_incap_bim      ,
           tmp.dias_ausent_bim     ,
           tmp.imp_ap_pat/100      ,
           tmp.imp_am_cre/100      ,
           tmp.imp_ren_viv/100     ,
           tmp.marca_sua           ,
           tmp.marca_bdnsar        ,
           tmp.diag_aclaracion     ,
           tmp.f_proceso           ,
           tmp.aivs/1000000        ,
           tmp.valor_aiv/1000000   ,
           tmp.int_gen_pgo_ext/100 ,
           tmp.aiv_gen_pgo_ext/1000000     
      INTO v_ide_derechohabiente               ,
           v_tmp_det_sc_nss_tpo_registro       ,
           v_tmp_det_sc_nss_cve_ent_receptora  ,
           v_tmp_det_sc_nss_nrp                ,
           v_tmp_det_sc_nss_rfc_patron         ,
           v_tmp_det_sc_nss_periodo_pago       ,
           v_tmp_det_sc_nss_f_pago_patron      ,
           v_tmp_det_sc_nss_folio_sua          ,
           v_tmp_det_sc_nss_nss                ,
           v_tmp_det_sc_nss_rfc                ,
           v_tmp_det_sc_nss_curp               ,
           v_tmp_det_sc_nss_num_crd_ifv        ,
           v_tmp_det_sc_nss_f_ini_desc_crd_ifv ,
           v_tmp_det_sc_nss_num_mov_periodo    ,
           v_tmp_det_sc_nss_nombre_trabajador  ,
           v_tmp_det_sc_nss_ult_sdi            ,
           v_tmp_det_sc_nss_tpo_trabajador     ,
           v_tmp_det_sc_nss_jornada            ,
           v_tmp_det_sc_nss_localiza_trabajador,
           v_tmp_det_sc_nss_destino_ap_viv     ,
           v_tmp_det_sc_nss_dias_cot_bim       ,
           v_tmp_det_sc_nss_dias_incap_bim     ,
           v_tmp_det_sc_nss_dias_ausent_bim    ,
           v_tmp_det_sc_nss_imp_ap_pat         ,
           v_tmp_det_sc_nss_imp_am_cre         ,
           v_tmp_det_sc_nss_imp_ren_viv        ,
           v_tmp_det_sc_nss_marca_sua          ,
           v_tmp_det_sc_nss_marca_bdnsar       ,
           v_tmp_det_sc_nss_diag_aclaracion    ,
           v_tmp_det_sc_nss_f_proceso          ,
           v_tmp_det_sc_nss_aivs               ,
           v_tmp_det_sc_nss_valor_aiv          ,
           v_tmp_det_sc_nss_int_gen_pgo_ext    ,
           v_tmp_det_sc_nss_aiv_gen_pgo_ext
      FROM safre_tmp:tmp_det_sc_nss tmp,
           afi_derechohabiente afi
      WHERE tmp.nss = afi.nss                  
      
      --g-
      LET v_cont_referencia = v_cont_referencia + 1;
        
      --Actualiza la precisión de los importes correspondientes
      LET v_acl_det_sc_nss_ult_sdi         = v_tmp_det_sc_nss_ult_sdi        ;
      LET v_acl_det_sc_nss_imp_ap_pat      = v_tmp_det_sc_nss_imp_ap_pat     ;
      LET v_acl_det_sc_nss_imp_am_cre      = v_tmp_det_sc_nss_imp_am_cre     ;
      LET v_acl_det_sc_nss_imp_ren_viv     = v_tmp_det_sc_nss_imp_ren_viv    ;
      LET v_acl_det_sc_nss_aivs            = v_tmp_det_sc_nss_aivs           ;
      LET v_acl_det_sc_nss_valor_aiv       = v_tmp_det_sc_nss_valor_aiv      ;
      LET v_acl_det_sc_nss_int_gen_pgo_ext = v_tmp_det_sc_nss_int_gen_pgo_ext;
      LET v_acl_det_sc_nss_aiv_gen_pgo_ext = v_tmp_det_sc_nss_aiv_gen_pgo_ext;
      LET v_cta_his_pagos_tpo_patron       = v_tmp_det_sc_nss_nrp;

      --Inicializacion de variables     
      LET v_tpo_patron = v_tmp_det_sc_nss_nrp;   
      
      
      -- asignación para cuando trae valores en nulo 12-feb-2012      
      IF v_acl_det_sc_nss_int_gen_pgo_ext IS NULL THEN
         LET v_acl_det_sc_nss_int_gen_pgo_ext = 0;
      END IF
      
      IF v_acl_det_sc_nss_aiv_gen_pgo_ext IS NULL THEN
         LET v_acl_det_sc_nss_aiv_gen_pgo_ext = 0;
      END IF

      LET v_tpo_aclaracion =" ";


--    --g- SE QUITA VALIDACIÓN DE DUPLICADOS DE ACUERDO A REUNIÓN DEL 18-NOV-2015
--    --g- EN LA OFICIAN DE LUIS FLORES
      { --g-
      -- nva funcionalidad para que no ingrese --g-
      -- duplicados al reprocesar rechazados 5-nov-2015
      LET v_cont = 0;
      SELECT count(*)
      INTO   v_cont
      FROM   cta_his_pagos
      WHERE  id_derechohabiente = v_ide_derechohabiente
      AND    folio_sua          = v_tmp_det_sc_nss_folio_sua
      AND    periodo_pago       = v_tmp_det_sc_nss_periodo_pago
      AND    f_pago             = v_tmp_det_sc_nss_f_pago_patron
      AND    nrp                = v_tmp_det_sc_nss_nrp
      AND    cve_ent_receptora  = v_tmp_det_sc_nss_cve_ent_receptora
      AND    imp_ap_pat         = v_tmp_det_sc_nss_imp_ap_pat
      AND    imp_am_cre         = v_tmp_det_sc_nss_imp_am_cre;

      IF v_cont > 1 THEN   -- encontro duplicado y se genera excepcion
         INSERT INTO acl_rech_duplicados VALUES (p_folio,v_cont_referencia,p_origen_archivo);
      ELSE
            
      } --g-   
      
      IF v_ide_derechohabiente <> 52112212 THEN --xvi-141      
      
         --======================================================================
         -- se coloca este query por orden de Hamir ya que dice que en estos 
         -- archivos no viene la aclaración y hay que obteneral de historico
         -- 08Nov2012. Esta consulta se agrupa en un foreach para encontrar posibles duplicados
         -- se inicia el contador de duplicados
      
         LET v_contador_duplicados = 0;
         
         FOREACH
            SELECT tpo_aclaracion,
                   folio,
                   id_referencia,
                   origen_archivo,
                   folio,
                   id_referencia,
                   imp_ap_pat,
                   imp_am_cre,
                   imp_ren_viv_pgo_ext,
                   aiv_ap_pat,
                   int_gen_pgo_ext,
                   aiv_gen_pgo_ext
            INTO   v_tpo_aclaracion,
                   v_folio_duplicado,
                   v_id_referencia,
                   v_origen_archivo,
                   v_folio_reg,
                   v_id_referencia_reg,
                   v_imp_ap_pat,
                   v_imp_am_cre,
                   v_imp_ren_viv_pgo_ext,
                   v_aiv_ap_pat,
                   v_int_gen_pgo_ext,
                   v_aiv_gen_pgo_ext                 
            FROM   cta_his_pagos
            WHERE  id_derechohabiente = v_ide_derechohabiente
            AND    folio_sua          = v_tmp_det_sc_nss_folio_sua
            AND    periodo_pago       = v_tmp_det_sc_nss_periodo_pago
            AND    nrp                = v_tmp_det_sc_nss_nrp
            AND    imp_ap_pat         = v_tmp_det_sc_nss_imp_ap_pat 
            AND    imp_am_cre         = v_tmp_det_sc_nss_imp_am_cre
            AND    origen_archivo     in (1,4) -- buscar el tipo de aclaracion en los folios de LQINFO

--          SE QUITA VALIDACIÓN DEL CAMPO f_pago DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--          el campo iba después de periodo_pago
--                     AND    f_pago             = v_tmp_det_sc_nss_f_pago_patron
--          SE QUITA VALIDACIÓN DEL CAMPO cve_ent_receptora DE ACUERDO A REQUEIMIENTO SACI2018-49  15-JUN-2018.
--          el campo iba después de nrp
--                     AND    cve_ent_receptora  = v_tmp_det_sc_nss_cve_ent_receptora
-----------------------------------------------------------------------------------------------------------------------------------------------------
--            AND    int_gen_pgo_ext    = v_tmp_det_sc_nss_int_gen_pgo_ext     -- se cambia validación de pesos por aivs por solicitud de Hamir 5-feb-2013
--        SE QUITA VALIDACIÓN DEL CAMPO aiv_gen_pgo_ext DE ACUERDO
--        A REQUERIMIENTO JIRA PRODINF-241  22-ABR-2014
--            AND    aiv_gen_pgo_ext    = v_tmp_det_sc_nss_aiv_gen_pgo_ext       -- se cambia validación de pesos por aivs por solicitud de Hamir 5-feb-2013
         
            
            -- se cuenta un registro
            LET v_contador_duplicados = v_contador_duplicados + 1;
            
            -- se toma como el registro correcto el primero que se encuentre
            IF ( v_contador_duplicados = 1 ) THEN 
               LET v_ind_cta = 1; -- se marca el registro como el tomado para aclaracion
            ELSE
               LET v_ind_cta = 0; -- registro duplicado
            END IF
            
            -- se inserta el registro duplicado
            EXECUTE PROCEDURE sp_inserta_acl_his_duplicados(v_folio_duplicado                   ,
                                                            v_origen_archivo                    ,
                                                            v_ide_derechohabiente               ,
                                                            v_id_referencia                     ,
                                                            v_tmp_det_sc_nss_folio_sua          ,
                                                            v_tmp_det_sc_nss_periodo_pago       ,
                                                            v_tmp_det_sc_nss_f_pago_patron      ,
                                                            v_tmp_det_sc_nss_nrp                ,
                                                            v_tmp_det_sc_nss_cve_ent_receptora  ,
                                                            v_tmp_det_sc_nss_imp_ap_pat         ,
                                                            v_tmp_det_sc_nss_imp_am_cre         ,
                                                            v_tmp_det_sc_nss_int_gen_pgo_ext    ,
                                                            v_ind_cta                           ,
                                                            p_folio                             
                                                            );
         END FOREACH;
         
         -- si no hay duplicados, se borra la tabla de duplicados
         IF ( v_contador_duplicados = 1 ) THEN
            DELETE FROM acl_his_duplicados
            WHERE  id_derechohabiente = v_ide_derechohabiente
            AND    folio_sua          = v_tmp_det_sc_nss_folio_sua
            AND    periodo_pago       = v_tmp_det_sc_nss_periodo_pago
            AND    f_pago             = v_tmp_det_sc_nss_f_pago_patron
            AND    nrp                = v_tmp_det_sc_nss_nrp
            AND    cve_ent_receptora  = v_tmp_det_sc_nss_cve_ent_receptora
            AND    imp_ap_pat         = v_tmp_det_sc_nss_imp_ap_pat 
            AND    imp_am_cre         = v_tmp_det_sc_nss_imp_am_cre
--            AND    int_gen_pgo_ext    = v_tmp_det_sc_nss_int_gen_pgo_ext
            AND    folio_integracion  = p_folio;
         END IF
      
     END IF    --xvi-141


      IF v_ide_derechohabiente = 52112212 THEN    --xvi-141 nss en blanco
         INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,1,TODAY);  --xvi-141 código 1 Diferente nss en LQ o CI
      	 LET v_result_operacion = 2;
         LET v_tpo_aclaracion   = " ";
      ELSE
         IF v_tpo_aclaracion = " " OR v_tpo_aclaracion IS NULL THEN
            LET v_result_operacion = 2;       -- se rechaza x inconsistencia x no tener reg historico anterior
            LET v_tpo_aclaracion   = " ";
  	        EXECUTE PROCEDURE sp_reg_rechazo_acl(p_folio,v_cont_referencia,v_ide_derechohabiente,v_tmp_det_sc_nss_folio_sua,v_tmp_det_sc_nss_periodo_pago,v_tmp_det_sc_nss_f_pago_patron,v_tmp_det_sc_nss_nrp,v_tmp_det_sc_nss_cve_ent_receptora,v_tmp_det_sc_nss_imp_ap_pat,v_tmp_det_sc_nss_imp_am_cre) INTO v_recha; --xvi-141
  	        IF v_recha = 1 THEN
               INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,100,TODAY);  --xvi-141 código 100 No existe registro anterior
  	        END IF
         ELSE
            LET v_result_operacion = 1;
         END IF
      END IF

{
--==================      
      IF v_tpo_aclaracion = " " OR v_tpo_aclaracion IS NULL THEN    --xvi-141

         LET v_result_operacion = 2;  -- se rechaza por inconsistencia por no tener reg historico anterior
         LET v_tpo_aclaracion   = " ";

         IF v_ide_derechohabiente = 52112212 THEN   --xvi-141 nss en blanco
            INSERT INTO acl_pag_rechazo VALUES (p_folio,v_cont_referencia,1,TODAY);  --xvi-141 código 1 Diferente nss en LQ o CI            
         ELSE
  	        EXECUTE PROCEDURE sp_reg_rechazo_acl(p_folio,v_cont_referencia,v_ide_derechohabiente,v_tmp_det_sc_nss_folio_sua,v_tmp_det_sc_nss_periodo_pago,v_tmp_det_sc_nss_f_pago_patron,v_tmp_det_sc_nss_nrp,v_tmp_det_sc_nss_cve_ent_receptora,v_tmp_det_sc_nss_imp_ap_pat,v_tmp_det_sc_nss_imp_am_cre); --xvi-141
  	        
         END IF       --xvi-141
        
      ELSE
         LET v_result_operacion = 1;      
      END IF
--================
}
          
----------------======================================================================

      -- Regla Negocio de Adelantamientos--

      IF v_tpo_patron = "99" THEN

         LET v_tipo_trabajador = "S";
         LET v_ind_liquidacion = 2;  --ACL adelantada liquidada SI

      ELSE

         LET v_tipo_trabajador = "I";

         IF (v_tpo_aclaracion = "13" OR
             v_tpo_aclaracion = "17") THEN

            LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

         ELSE
            
            IF v_tpo_aclaracion = " " THEN

               LET v_ind_liquidacion = 1;  --significa que no existe tpo_aclaracion

            ELSE

               LET v_ind_liquidacion = 5; --ACL normal liquidada

            END IF
         END IF
      END IF

------------------------------======================================================================

--      IF v_result_operacion = 2 AND v_ind_liquidacion = 1 THEN  --saci2018-67
	
      IF v_result_operacion = 2 THEN  --saci2018-67
      	
      	 IF v_tmp_det_sc_nss_destino_ap_viv = "1" AND v_tmp_det_sc_nss_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
      	 	
            INSERT INTO cta_especial_acl (
               folio              ,
               origen_archivo     ,
               id_referencia      ,
               cve_ent_receptora  ,
               nrp                ,
               periodo_pago       ,
               folio_sua          ,
               f_pago             ,
               id_derechohabiente ,
               localiza_trabajador,
               tpo_aclaracion     ,
               imp_ap_pat         ,
               imp_am_cre         ,
               imp_ren_viv_pgo_ext,
               aiv_ap_pat         ,
               valor_aiv          ,
               int_gen_pgo_ext    ,
               aiv_gen_pgo_ext    ,
               result_operacion   ,
               ind_liquidacion    ,
               num_crd_ifv        ,
               f_proceso          ,
               tpo_patron         ,
               folio_referencia   ,
               destino_ap_viv         
               )          
            VALUES (
               p_folio,
               5,
               v_cont_referencia,
               v_tmp_det_sc_nss_cve_ent_receptora   ,
               v_tmp_det_sc_nss_nrp                 ,
               v_tmp_det_sc_nss_periodo_pago        ,
               v_tmp_det_sc_nss_folio_sua           ,
               v_tmp_det_sc_nss_f_pago_patron       ,
               v_ide_derechohabiente                ,
               v_tmp_det_sc_nss_localiza_trabajador ,
               v_tpo_aclaracion                     ,
               v_acl_det_sc_nss_imp_ap_pat          ,
               v_acl_det_sc_nss_imp_am_cre          ,
               v_acl_det_sc_nss_imp_ren_viv         ,
               v_acl_det_sc_nss_aivs                ,
               v_acl_det_sc_nss_valor_aiv           ,
               v_acl_det_sc_nss_int_gen_pgo_ext     ,
               v_acl_det_sc_nss_aiv_gen_pgo_ext     ,
               2,       --result_operacion
               v_ind_liquidacion                    ,
               v_tmp_det_sc_nss_num_crd_ifv         ,
               TODAY                                ,
               v_tpo_patron                         ,
               0                                    ,
               v_tmp_det_sc_nss_destino_ap_viv         
               );  --saci2018-67-02
      	 	
               DELETE cta_his_pagos                                         --inc1389710
               WHERE  id_derechohabiente = v_ide_derechohabiente            --inc1389710
               AND    folio_sua          = v_tmp_det_sc_nss_folio_sua       --inc1389710
               AND    periodo_pago       = v_tmp_det_sc_nss_periodo_pago    --inc1389710
               AND    nrp                = v_tmp_det_sc_nss_nrp             --inc1389710
               AND    imp_ap_pat         = v_acl_det_sc_nss_imp_ap_pat      --inc1389710
               AND    imp_am_cre         = v_acl_det_sc_nss_imp_am_cre;     --inc1389710

      	 ELSE      	                                    --saci2018-67-02
            INSERT INTO cta_rechazos_acl (
               folio              ,
               origen_archivo     ,
               id_referencia      ,
               cve_ent_receptora  ,
               nrp                ,
               periodo_pago       ,
               folio_sua          ,
               f_pago             ,
               id_derechohabiente ,
               localiza_trabajador,
               tpo_aclaracion     ,
               imp_ap_pat         ,
               imp_am_cre         ,
               imp_ren_viv_pgo_ext,
               aiv_ap_pat         ,
               valor_aiv          ,
               int_gen_pgo_ext    ,
               aiv_gen_pgo_ext    ,
               result_operacion   ,
               ind_liquidacion    ,
               num_crd_ifv        ,
               f_proceso          ,
               tpo_patron         ,
               folio_referencia   ,
               destino_ap_viv         
               )          
            VALUES (
               p_folio,
               5,
               v_cont_referencia,
               v_tmp_det_sc_nss_cve_ent_receptora   ,
               v_tmp_det_sc_nss_nrp                 ,
               v_tmp_det_sc_nss_periodo_pago        ,
               v_tmp_det_sc_nss_folio_sua           ,
               v_tmp_det_sc_nss_f_pago_patron       ,
               v_ide_derechohabiente                ,
               v_tmp_det_sc_nss_localiza_trabajador ,
               v_tpo_aclaracion                     ,
               v_acl_det_sc_nss_imp_ap_pat          ,
               v_acl_det_sc_nss_imp_am_cre          ,
               v_acl_det_sc_nss_imp_ren_viv         ,
               v_acl_det_sc_nss_aivs                ,
               v_acl_det_sc_nss_valor_aiv           ,
               v_acl_det_sc_nss_int_gen_pgo_ext     ,
               v_acl_det_sc_nss_aiv_gen_pgo_ext     ,
               v_result_operacion                   ,
               v_ind_liquidacion                    ,
               v_tmp_det_sc_nss_num_crd_ifv         ,
               TODAY                                ,
               v_tpo_patron                         ,
               0                                    ,
               v_tmp_det_sc_nss_destino_ap_viv         
               );  --saci2018-67
         
         END IF                                --saci2018-67-02
      ELSE     --   A C E P T A D O S  --

      	 IF v_tmp_det_sc_nss_destino_ap_viv = "1" AND v_tmp_det_sc_nss_f_pago_patron <= "10/31/2012" THEN     --saci2018-67-02
      	 	
            INSERT INTO cta_especial_acl (
               folio              ,
               origen_archivo     ,
               id_referencia      ,
               cve_ent_receptora  ,
               nrp                ,
               periodo_pago       ,
               folio_sua          ,
               f_pago             ,
               id_derechohabiente ,
               localiza_trabajador,
               tpo_aclaracion     ,
               imp_ap_pat         ,
               imp_am_cre         ,
               imp_ren_viv_pgo_ext,
               aiv_ap_pat         ,
               valor_aiv          ,
               int_gen_pgo_ext    ,
               aiv_gen_pgo_ext    ,
               result_operacion   ,
               ind_liquidacion    ,
               num_crd_ifv        ,
               f_proceso          ,
               tpo_patron         ,
               folio_referencia   ,
               destino_ap_viv         
               )          
            VALUES (
               p_folio,
               5,
               v_cont_referencia,
               v_tmp_det_sc_nss_cve_ent_receptora   ,
               v_tmp_det_sc_nss_nrp                 ,
               v_tmp_det_sc_nss_periodo_pago        ,
               v_tmp_det_sc_nss_folio_sua           ,
               v_tmp_det_sc_nss_f_pago_patron       ,
               v_ide_derechohabiente                ,
               v_tmp_det_sc_nss_localiza_trabajador ,
               v_tpo_aclaracion                     ,
               v_acl_det_sc_nss_imp_ap_pat          ,
               v_acl_det_sc_nss_imp_am_cre          ,
               v_acl_det_sc_nss_imp_ren_viv         ,
               v_acl_det_sc_nss_aivs                ,
               v_acl_det_sc_nss_valor_aiv           ,
               v_acl_det_sc_nss_int_gen_pgo_ext     ,
               v_acl_det_sc_nss_aiv_gen_pgo_ext     ,
               1,       --result_operacion
               v_ind_liquidacion                    ,
               v_tmp_det_sc_nss_num_crd_ifv         ,
               TODAY                                ,
               v_tpo_patron                         ,
               0                                    ,
               v_tmp_det_sc_nss_destino_ap_viv         
               );  --saci2018-67-02

            INSERT INTO cta_his_pagos
                     (folio                  ,
                      origen_archivo         ,
                      id_referencia          ,
                      cve_ent_receptora      ,
                      nrp                    ,
                      periodo_pago           ,
                      folio_sua              ,
                      f_pago                 ,
                      id_derechohabiente     ,
                      localiza_trabajador    ,
                      tpo_aclaracion         ,
                      imp_ap_pat             ,
                      imp_am_cre             ,
                      imp_ren_viv_pgo_ext    ,
                      aiv_ap_pat             ,
                      valor_aiv              ,
                      int_gen_pgo_ext        ,
                      aiv_gen_pgo_ext        ,
                      result_operacion       ,
                      ind_liquidacion        ,
                      num_crd_ifv            ,
                      f_proceso              ,
                      tpo_patron             ,
                      folio_referencia       ,
                      destino_ap_viv         
                      )
              VALUES (
                      p_folio,
                      5,
                      --g- seq_cta_his_pagos.NEXTVAL  ,
                      v_cont_referencia,
                      v_tmp_det_sc_nss_cve_ent_receptora   ,
                      v_tmp_det_sc_nss_nrp                 ,
                      v_tmp_det_sc_nss_periodo_pago        ,
                      v_tmp_det_sc_nss_folio_sua           ,
                      v_tmp_det_sc_nss_f_pago_patron       ,
                      v_ide_derechohabiente                ,
                      v_tmp_det_sc_nss_localiza_trabajador ,
                      v_tpo_aclaracion                     ,
                      v_acl_det_sc_nss_imp_ap_pat          ,
                      v_acl_det_sc_nss_imp_am_cre          ,
                      v_acl_det_sc_nss_imp_ren_viv         ,
                      v_acl_det_sc_nss_aivs                ,
                      v_acl_det_sc_nss_valor_aiv           ,
                      v_acl_det_sc_nss_int_gen_pgo_ext     ,
                      v_acl_det_sc_nss_aiv_gen_pgo_ext     ,
                      v_result_operacion                   ,
                      v_ind_liquidacion                    ,
                      v_tmp_det_sc_nss_num_crd_ifv         ,
                      TODAY                                ,
                      v_tpo_patron                         ,
                      0                                    ,
                      v_tmp_det_sc_nss_destino_ap_viv
                      );

{ Quitar de aquí y colocar en sp_preliquida_sin_cambio_nss
               DELETE cta_his_pagos                                         --inc1389710
               WHERE  id_derechohabiente = v_ide_derechohabiente            --inc1389710
               AND    folio_sua          = v_tmp_det_sc_nss_folio_sua       --inc1389710
               AND    periodo_pago       = v_tmp_det_sc_nss_periodo_pago    --inc1389710
               AND    nrp                = v_tmp_det_sc_nss_nrp             --inc1389710
               AND    imp_ap_pat         = v_acl_det_sc_nss_imp_ap_pat      --inc1389710
               AND    imp_am_cre         = v_acl_det_sc_nss_imp_am_cre      --inc1389710
               AND    folio              < p_folio;                         --inc1389710
}
                      
         ELSE                                          --saci2018-67-02

            INSERT INTO cta_his_pagos
                     (folio                  ,
                      origen_archivo         ,
                      id_referencia          ,
                      cve_ent_receptora      ,
                      nrp                    ,
                      periodo_pago           ,
                      folio_sua              ,
                      f_pago                 ,
                      id_derechohabiente     ,
                      localiza_trabajador    ,
                      tpo_aclaracion         ,
                      imp_ap_pat             ,
                      imp_am_cre             ,
                      imp_ren_viv_pgo_ext    ,
                      aiv_ap_pat             ,
                      valor_aiv              ,
                      int_gen_pgo_ext        ,
                      aiv_gen_pgo_ext        ,
                      result_operacion       ,
                      ind_liquidacion        ,
                      num_crd_ifv            ,
                      f_proceso              ,
                      tpo_patron             ,
                      folio_referencia       ,
                      destino_ap_viv         
                      )
              VALUES (
                      p_folio,
                      5,
                      --g- seq_cta_his_pagos.NEXTVAL  ,
                      v_cont_referencia,
                      v_tmp_det_sc_nss_cve_ent_receptora   ,
                      v_tmp_det_sc_nss_nrp                 ,
                      v_tmp_det_sc_nss_periodo_pago        ,
                      v_tmp_det_sc_nss_folio_sua           ,
                      v_tmp_det_sc_nss_f_pago_patron       ,
                      v_ide_derechohabiente                ,
                      v_tmp_det_sc_nss_localiza_trabajador ,
                      v_tpo_aclaracion                     ,
                      v_acl_det_sc_nss_imp_ap_pat          ,
                      v_acl_det_sc_nss_imp_am_cre          ,
                      v_acl_det_sc_nss_imp_ren_viv         ,
                      v_acl_det_sc_nss_aivs                ,
                      v_acl_det_sc_nss_valor_aiv           ,
                      v_acl_det_sc_nss_int_gen_pgo_ext     ,
                      v_acl_det_sc_nss_aiv_gen_pgo_ext     ,
                      v_result_operacion                   ,
                      v_ind_liquidacion                    ,
                      v_tmp_det_sc_nss_num_crd_ifv         ,
                      TODAY                                ,
                      v_tpo_patron                         ,
                      0                                    ,
                      v_tmp_det_sc_nss_destino_ap_viv
                      );
         END IF                                         --saci2018-67-02
      END IF --saci2018-67

      INSERT INTO cta_pag_complemento
               (folio,
                origen_archivo,
                id_referencia,
                id_derechohabiente,
                rfc_patron,
                rfc,
                curp,
                num_mov_periodo,
                f_ini_desc_crd_ifv,
                ult_sdi,
                tpo_trabajador,
                jornada,
                destino_ap_viv,
                dias_cot_bim,
                dias_incap_bim,
                dias_ausent_bim,
                marca_sua,
                marca_bdnsar)
         VALUES(p_folio,
                5,
                --g- seq_cta_his_pagos.CURRVAL ,
                v_cont_referencia,
                v_ide_derechohabiente               ,
                v_tmp_det_sc_nss_rfc_patron         ,
                v_tmp_det_sc_nss_rfc                ,
                v_tmp_det_sc_nss_curp               ,
                v_tmp_det_sc_nss_num_mov_periodo    ,
                v_tmp_det_sc_nss_f_ini_desc_crd_ifv ,
                v_acl_det_sc_nss_ult_sdi            ,
                v_tmp_det_sc_nss_tpo_trabajador     ,
                v_tmp_det_sc_nss_jornada            ,
                v_tmp_det_sc_nss_destino_ap_viv     ,
                v_tmp_det_sc_nss_dias_cot_bim       ,
                v_tmp_det_sc_nss_dias_incap_bim     ,
                v_tmp_det_sc_nss_dias_ausent_bim    ,
                v_tmp_det_sc_nss_marca_sua          ,
                v_tmp_det_sc_nss_marca_bdnsar       );
             
      IF v_result_operacion = 1 THEN
            INSERT INTO acl_pag_registrado VALUES (
               p_folio,
               --g- seq_cta_his_pagos.CURRVAL,
               v_cont_referencia,
               v_imp_ap_pat,         
               v_imp_am_cre,         
               v_imp_ren_viv_pgo_ext,
               v_aiv_ap_pat,         
               v_int_gen_pgo_ext,    
               v_aiv_gen_pgo_ext,
               v_folio_reg,          
               v_id_referencia_reg,
               5
               );              
      END IF     
     
   END FOREACH
   
   --TRACE 'inicia el foreach de la tabla tmp_sum_sc_nss';
   FOREACH SELECT 
           tpo_registro         ,
           tot_ap               ,
           suma_ap_pat          ,
           suma_am              ,
           suma_aivs            ,
           suma_int_viv_pgo_ext ,
           suma_aiv_pgo_ext     
      INTO v_tmp_sum_sc_nss_tpo_registro        ,
           v_tmp_sum_sc_nss_tot_ap              ,
           v_tmp_sum_sc_nss_suma_ap_pat         ,
           v_tmp_sum_sc_nss_suma_am             ,
           v_tmp_sum_sc_nss_suma_aivs           ,
           v_tmp_sum_sc_nss_suma_int_viv_pgo_ext,
           v_tmp_sum_sc_nss_suma_aiv_pgo_ext    
      FROM safre_tmp:tmp_sum_sc_nss
      
      --TRACE li_contador;   
      
      --Actualizar la precision para los campos correspondientes
      LET v_acl_sum_sc_nss_suma_ap_pat         = v_tmp_sum_sc_nss_suma_ap_pat         /100;
      LET v_acl_sum_sc_nss_suma_am             = v_tmp_sum_sc_nss_suma_am             /100;
      LET v_acl_sum_sc_nss_suma_aivs           = v_tmp_sum_sc_nss_suma_aivs           /1000000;
      LET v_acl_sum_sc_nss_suma_int_viv_pgo_ext= v_tmp_sum_sc_nss_suma_int_viv_pgo_ext/100;
      LET v_acl_sum_sc_nss_suma_aiv_pgo_ext    = v_tmp_sum_sc_nss_suma_aiv_pgo_ext    /1000000;
      
      INSERT INTO acl_sum_sc_nss (
               folio                  ,
               tpo_registro           ,
               tot_ap                 ,
               suma_ap_pat            ,
               suma_am                ,
               suma_aivs              ,
               suma_int_viv_pgo_ext   ,
               suma_aiv_pgo_ext       
              )
       VALUES (
               p_folio ,
               v_tmp_sum_sc_nss_tpo_registro          ,
               v_tmp_sum_sc_nss_tot_ap                ,
               v_acl_sum_sc_nss_suma_ap_pat           ,
               v_acl_sum_sc_nss_suma_am               ,
               v_acl_sum_sc_nss_suma_aivs             ,
               v_acl_sum_sc_nss_suma_int_viv_pgo_ext  ,
               v_acl_sum_sc_nss_suma_aiv_pgo_ext
              );
   END FOREACH;
   
   --TRACE 'Finaliza el store procedure de registro historicos de SIN CAMBIO EN NSS';
   --UPDATE STATISTICS FOR TABLE acl_det_sc_nss    ;
   UPDATE STATISTICS FOR TABLE cta_his_pagos;
   UPDATE STATISTICS FOR TABLE cta_pag_complemento;
   UPDATE STATISTICS FOR TABLE cta_especial_acl;
   UPDATE STATISTICS FOR TABLE cta_rechazos_acl;   	
   UPDATE STATISTICS FOR TABLE acl_sum_sc_nss;


   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, v_tmp_det_sc_nss_nss;

END PROCEDURE;


