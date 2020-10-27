






CREATE PROCEDURE "safreviv".sp_historicos_enaclara(p_folio DECIMAL(9,0),
                                        p_pid DECIMAL(9,0),
                                        p_proceso_cod SMALLINT )
RETURNING SMALLINT, INTEGER, VARCHAR(255), CHAR(11)
   --TABLA tmp_det_aclaracion
   DEFINE tmp_det_acl_tpo_registro         char(1);         
   DEFINE tmp_det_acl_cve_ent_receptora    char(3);         
   DEFINE tmp_det_acl_f_pago_patron        date;            
   DEFINE tmp_det_acl_nrp                  char(11);        
   DEFINE tmp_det_acl_periodo_pago         char(6);         
   DEFINE tmp_det_acl_folio_sua            char(6);         
   DEFINE tmp_det_acl_nss                  char(11);        
   DEFINE tmp_det_acl_rfc                  char(13);        
   DEFINE tmp_det_acl_curp                 char(18);        
   DEFINE tmp_det_acl_num_crd_ifv          char(10);        
   DEFINE tmp_det_acl_f_ini_desc_cre_ifv   date;            
   DEFINE tmp_det_acl_nombre_trabajador    char(50);        
   DEFINE tmp_det_acl_ult_sdi              decimal(15);     
   DEFINE tmp_det_acl_dias_cot_bim         decimal(4);      
   DEFINE tmp_det_acl_dias_incap_bim       decimal(4);      
   DEFINE tmp_det_acl_dias_ausent_bim      decimal(4);      
   DEFINE tmp_det_acl_imp_ap_pat           decimal(12,2);     
   DEFINE tmp_det_acl_imp_am_cre           decimal(12,2);     
   DEFINE tmp_det_acl_ind_crd_ifv          char(1);         
   DEFINE tmp_det_acl_tpo_aclaracion       char(2);         
   DEFINE tmp_det_acl_aivs                 decimal(18);     
   DEFINE tmp_det_acl_valor_aiv            decimal(18);     
   
   --TABLA acl_det_aclaracion
   DEFINE acl_det_acl_ult_sdi              DECIMAL(7,2);
   DEFINE acl_det_acl_imp_ap_pat           DECIMAL(12,2);
   DEFINE acl_det_acl_imp_am_cre           DECIMAL(12,2);
   DEFINE acl_det_acl_ind_crd_ifv          CHAR(1);
   DEFINE acl_det_acl_aivs                 DECIMAL(18,6);
   DEFINE acl_det_acl_valor_aiv            DECIMAL(18,6);
   
   --TABLA tmp_sum_aclaracion
   DEFINE tmp_sum_acl_tpo_registro         CHAR(1);              
   DEFINE tmp_sum_acl_tot_ap               DECIMAL(9);     
   DEFINE tmp_sum_acl_suma_ap_pat          DECIMAL(18,2);    
   DEFINE tmp_sum_acl_suma_am              DECIMAL(18,2);    
   DEFINE tmp_sum_acl_suma_aivs            DECIMAL(18,2);    
                                                           
   --TABLA acl_sum_aclaracion
   DEFINE acl_sum_acl_folio                DECIMAL(9,0);
   DEFINE acl_sum_acl_tpo_registro         CHAR(2);
   DEFINE acl_sum_acl_tot_ap               INTEGER;
   DEFINE acl_sum_acl_suma_ap_pat          DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_am              DECIMAL(18,2);
   DEFINE acl_sum_acl_suma_aivs            DECIMAL(18,6);
   
   DEFINE ld_ide_derechohabiente  DECIMAL(9);
   DEFINE contador   INTEGER;
   DEFINE v_tipo_trabajador   CHAR(1);
   
   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;
   
   -- verificacion de registros recibidos
   DEFINE v_num_registros_detalle         INTEGER; -- registros detalle
   DEFINE v_num_registros_detalle_sum     INTEGER; -- registros de detalle indicados en sumario
   
   -- validacion de datos cargados
   DEFINE v_err_numero_regs_detalle_no_coincide         SMALLINT; -- error por inconsistencia de numero de registros de detalle
   DEFINE v_err_suma_aportacion_patronal_no_coincide    SMALLINT;
   DEFINE v_err_suma_amortizacion_no_coincide           SMALLINT;
   DEFINE v_err_suma_AIVS_no_coincide                   SMALLINT;

   DEFINE v_ind_liquidacion SMALLINT;
   DEFINE v_tpo_patron      CHAR(02);
                                                                          
   DEFINE tmp_det_acl_nrp_short            CHAR(2);
   DEFINE v_result_operacion                            SMALLINT; -- se asigna el valor original que es 1 = "aceptado"
   DEFINE v_error_en_sumario                            SMALLINT; -- booleana para saber si hubo error en sumario   
  
   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss;
   END EXCEPTION

   -- se asignan los codigos de error
   LET v_err_numero_regs_detalle_no_coincide         = 1; -- error por inconsistencia de numero de registros de detalle
   LET v_err_suma_aportacion_patronal_no_coincide    = 2;
   LET v_err_suma_amortizacion_no_coincide           = 3;
   LET v_err_suma_AIVS_no_coincide                   = 4;
   LET v_tpo_patron = "";   
   LET tmp_det_acl_nrp_short = "";

     
   -- se asigna el valor de result_operacion
   LET v_result_operacion = 1;
   LET tmp_det_acl_nss = NULL;
     
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/historicoenaclara.trace';
   --TRACE 'Inicia el store procedure de registro historicos de EN ACLARACION';
   -- se inicia el contador   
   LET contador = 1;
   
   LET v_num_registros_detalle = 0;
   LET v_num_registros_detalle_sum = 0;
   
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio        = p_folio,
          estado       = 2 -- integrado
   WHERE  proceso_cod  = p_proceso_cod
   AND    folio        = p_folio;
       
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;
   
   -- verificando cifras de sumario
   SELECT  SUM(suma_ap_pat)/100,
           SUM(suma_am    )/100,        
           SUM(suma_aivs  )/1000000
   INTO
      acl_sum_acl_suma_ap_pat,
      acl_sum_acl_suma_am    ,
      acl_sum_acl_suma_aivs  
      FROM tmp_sum_aclaracion;

   -- se obtiene la suma de los monto de la tabla de detalle
   SELECT 
      SUM(imp_ap_pat) / 100    ,
      SUM(imp_am_cre) / 100    ,
      SUM(aivs      ) / 1000000
   INTO 
      acl_det_acl_imp_ap_pat ,
      acl_det_acl_imp_am_cre ,
      acl_det_acl_aivs       
   FROM tmp_det_aclaracion;

   -- se verifica que el numero de registros en la tabla de detalle sea el mismo
   -- que el encontrado en el sumario
   SELECT COUNT(*)
   INTO v_num_registros_detalle
   FROM tmp_det_aclaracion;
      
   SELECT tot_ap
   INTO v_num_registros_detalle_sum
   FROM tmp_sum_aclaracion;

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
   IF ( acl_sum_acl_suma_ap_pat <> acl_det_acl_imp_ap_pat ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_aportacion_patronal_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aportaciones patronales de detalle cargado no corresponde con el reportado en archivo.";
      
      LET v_error_en_sumario = 1;
   END IF

   -- amortizacion del credito
   IF ( acl_sum_acl_suma_am <> acl_det_acl_imp_am_cre ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_amortizacion_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de amortización del crédito de detalle cargado no corresponde con el reportado en archivo: " || acl_det_acl_imp_am_cre || " - " || acl_sum_acl_suma_am ;
      
      LET v_error_en_sumario = 1;
   END IF

   -- aplicacion de intereses de vivienda
   IF ( acl_sum_acl_suma_aivs <> acl_det_acl_aivs ) THEN
      -- se rechaza el archivo
      LET v_si_resultado = v_err_suma_AIVS_no_coincide;
      LET isam_err       = 0;
      LET err_txt        = "El monto de aplicación de intereses de vivienda de detalle cargado no corresponde con el reportado en archivo";
      
      LET v_error_en_sumario = 1;
   END IF


   -- si hubo error en sumario, entonces se cancela el folio y se devuelve el error
     IF ( v_error_en_sumario = 1 ) THEN
        -- se actualiza el folio en turno como erroneo
        UPDATE glo_folio
        SET    status = -1
        WHERE  folio = p_folio;
        
        -- se deja el archivo en estatus de error
        UPDATE glo_ctr_archivo
           SET folio = P_folio,
               estado = 2 -- integrado
         WHERE proceso_cod    = p_proceso_cod
           AND opera_cod      = 1 -- archivo cargado
           AND estado         = 1; -- etapa de carga
        
        -- se devuelve el error
        RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss;
     END IF
   

   UPDATE tmp_det_aclaracion
   SET    num_crd_ifv = 0
   WHERE  num_crd_ifv LIKE '%/%';

   
   -- EJECUTA DETALLE --
   -- se abren las cuentas de los NSS nuevos
   EXECUTE PROCEDURE sp_registros_sin_cuenta_acl_enaclara(P_folio)
   INTO v_si_resultado, isam_err, err_txt, tmp_det_acl_nss; 

   -- si ocurrio algun erro en la apertura de cuentas se finaliza el proceso
   IF ( v_si_resultado <> 0 ) THEN
      RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss; 
   END IF;

     
   --TRACE 'inicia el foreach de la tabla tmp_det_sc_nss';
   FOREACH SELECT
           afi.id_derechohabiente   ,
   	       tmp.tpo_registro         ,
           tmp.cve_ent_receptora    ,
           tmp.f_pago_patron        ,
           tmp.nrp                  ,
           tmp.periodo_pago         ,
           tmp.folio_sua            ,
           tmp.nss                  ,
           tmp.rfc                  ,
           tmp.curp                 ,
           tmp.num_crd_ifv          ,
           tmp.f_ini_desc_cre_ifv   ,
           tmp.nom_trabajador       ,
           tmp.ult_sdi/100          ,
           tmp.dias_cot_bim         ,
           tmp.dias_incap_bim       ,
           tmp.dias_ausent_bim      ,
           tmp.imp_ap_pat/100       ,
           tmp.imp_am_cre/100       ,
           tmp.ind_crd_ifv          ,
           tmp.tpo_aclaracion       ,
           tmp.aivs/1000000         ,
           tmp.valor_aiv/1000000            
      INTO ld_ide_derechohabiente,
           tmp_det_acl_tpo_registro,
           tmp_det_acl_cve_ent_receptora,
           tmp_det_acl_f_pago_patron,
           tmp_det_acl_nrp,
           tmp_det_acl_periodo_pago,
           tmp_det_acl_folio_sua,
           tmp_det_acl_nss,
           tmp_det_acl_rfc,
           tmp_det_acl_curp,
           tmp_det_acl_num_crd_ifv,
           tmp_det_acl_f_ini_desc_cre_ifv,
           tmp_det_acl_nombre_trabajador,
           tmp_det_acl_ult_sdi,
           tmp_det_acl_dias_cot_bim,
           tmp_det_acl_dias_incap_bim,
           tmp_det_acl_dias_ausent_bim,
           tmp_det_acl_imp_ap_pat,
           tmp_det_acl_imp_am_cre,
           tmp_det_acl_ind_crd_ifv,
           tmp_det_acl_tpo_aclaracion,
           tmp_det_acl_aivs,
           tmp_det_acl_valor_aiv
      FROM tmp_det_aclaracion tmp,
           afi_derechohabiente afi
      WHERE tmp.nss = afi.nss

      --se asigna el tipo de patron con las posiciones 1,2 del nrp
      --INSERT INTO tmp_nss_cuenta VALUES ( tmp_det_acl_nss );

      LET acl_det_acl_ult_sdi         = tmp_det_acl_ult_sdi;
      LET acl_det_acl_imp_ap_pat      = tmp_det_acl_imp_ap_pat;
      LET acl_det_acl_imp_am_cre      = tmp_det_acl_imp_am_cre;
      LET acl_det_acl_ind_crd_ifv     = tmp_det_acl_ind_crd_ifv;    
      LET acl_det_acl_aivs            = tmp_det_acl_aivs;
      LET acl_det_acl_valor_aiv       = tmp_det_acl_valor_aiv;
      
      -- asignación para cuando trae valores en nulo 12-feb-2012      
      IF acl_det_acl_aivs IS NULL THEN
         LET acl_det_acl_aivs = 0;
      END IF

      IF acl_det_acl_valor_aiv IS NULL THEN
         LET acl_det_acl_valor_aiv = 0;
      END IF


      --Inicializacion de variables     
      LET v_tpo_patron = tmp_det_acl_nrp;

--======================================================================
      -- Regla Negocio de Adelantamientos--

      LET v_tipo_trabajador = "I";

      IF (tmp_det_acl_tpo_aclaracion = "13" OR
          tmp_det_acl_tpo_aclaracion = "17") THEN

         LET v_ind_liquidacion = 3; --ACL adelantada liquidada IMSS

      ELSE

         LET v_ind_liquidacion = 1; --ACL normal SIN liquidar

      END IF

      INSERT INTO cta_his_pagos(folio              ,              
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
                                destino_ap_viv)
                     VALUES (p_folio                          ,                      
                             8                                ,  -- origen_archivo=8 
                             seq_cta_his_pagos.NEXTVAL        , 
                             tmp_det_acl_cve_ent_receptora    ,
                             tmp_det_acl_nrp                  ,
                             tmp_det_acl_periodo_pago         ,
                             tmp_det_acl_folio_sua            ,
                             tmp_det_acl_f_pago_patron        ,
                             ld_ide_derechohabiente           ,
                             NULL                             ,
                             tmp_det_acl_tpo_aclaracion       ,
                             acl_det_acl_imp_ap_pat           ,
                             acl_det_acl_imp_am_cre           ,
                             NULL                             ,
                             acl_det_acl_aivs                 ,
                             acl_det_acl_valor_aiv            ,
                             0                                ,
                             0                                ,
                             v_result_operacion               , -- result_operacion
                             v_ind_liquidacion                ,
                             tmp_det_acl_num_crd_ifv          ,
                             TODAY                            ,
                             v_tpo_patron                     ,
                             0                                ,
                             NULL);
                             
  -- Inserta a tabla cta_pag_complemento para control de complementos
      INSERT INTO cta_pag_complemento
         (folio                     
         ,origen_archivo            
         ,id_referencia             
         ,id_derechohabiente        
         ,rfc_patron                
         ,rfc                       
         ,curp                      
         ,num_mov_periodo           
         ,f_ini_desc_crd_ifv        
         ,ult_sdi                   
         ,tpo_trabajador            
         ,jornada                   
         ,destino_ap_viv            
         ,dias_cot_bim              
         ,dias_incap_bim            
         ,dias_ausent_bim           
         ,marca_sua                 
         ,marca_bdnsar)
      VALUES(p_folio
            ,8                                   -- origen_archivo=8
            ,seq_cta_his_pagos.CURRVAL
            ,ld_ide_derechohabiente
            ,NULL
            ,tmp_det_acl_rfc
            ,tmp_det_acl_curp
            ,0
            ,tmp_det_acl_f_ini_desc_cre_ifv
            ,acl_det_acl_ult_sdi
            ,NULL
            ,NULL 
            ,NULL 
            ,tmp_det_acl_dias_cot_bim
            ,tmp_det_acl_dias_incap_bim
            ,tmp_det_acl_dias_ausent_bim
            ,NULL
            ,NULL);
      LET contador = contador + 1;
   END FOREACH

   --CREATE INDEX nss_cuenta ON tmp_nss_cuenta(nss);

   --UPDATE STATISTICS FOR TABLE tmp_nss_cuenta;

--========================================================
--========================================================
   FOREACH SELECT
  	       tpo_registro  ,
           tot_ap        ,
           suma_ap_pat/100   ,
           suma_am/100       ,
           suma_aivs/1000000     
      INTO tmp_sum_acl_tpo_registro  ,
           tmp_sum_acl_tot_ap        ,
           tmp_sum_acl_suma_ap_pat   ,
           tmp_sum_acl_suma_am       ,
           tmp_sum_acl_suma_aivs
      FROM tmp_sum_aclaracion
      
      --TRACE li_contador;   
      
      --Actualizar la precision para los campos correspondientes
      
      LET acl_sum_acl_suma_ap_pat = tmp_sum_acl_suma_ap_pat;
      LET acl_sum_acl_suma_am     = tmp_sum_acl_suma_am;
      LET acl_sum_acl_suma_aivs   = tmp_sum_acl_suma_aivs;

      INSERT INTO acl_sum_aclaracion (
         folio          ,
         tpo_registro   ,
         tot_ap         ,
         suma_ap_pat    ,
         suma_am        ,
         suma_aivs      
        ) 
      VALUES (
         p_folio                     ,
         tmp_sum_acl_tpo_registro    ,
         tmp_sum_acl_tot_ap          ,
         acl_sum_acl_suma_ap_pat     ,
         acl_sum_acl_suma_am         ,
         acl_sum_acl_suma_aivs       
        );
   END FOREACH

   --UPDATE STATISTICS FOR TABLE acl_det_aclaracion  ;
   UPDATE STATISTICS FOR TABLE cta_his_pagos       ;
   UPDATE STATISTICS FOR TABLE acl_sum_aclaracion  ;
   UPDATE STATISTICS FOR TABLE cta_pag_complemento ;

   --TRACE 'Finaliza el store procedure de registro historicos de EN ACLARACION';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de integración finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, tmp_det_acl_nss;
END PROCEDURE;


