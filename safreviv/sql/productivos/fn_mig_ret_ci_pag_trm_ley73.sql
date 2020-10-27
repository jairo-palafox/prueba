






CREATE FUNCTION "safreviv".fn_mig_ret_ci_pag_trm_ley73(p_usuario_cod CHAR(20),
                                  p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40),
                                  p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255), VARCHAR(11)
   
   -- campos de la tabla de migracion
   -- tmp_ret_det_pagos_marcas(
   DEFINE tmp_ret_det_pagmarca_nss               CHAR(11);
   DEFINE tmp_ret_det_pagmarca_cod_estatus       CHAR(4) ;
   DEFINE tmp_ret_det_pagmarca_cod_estatus_jfca  CHAR(4) ;
   DEFINE tmp_ret_det_pagmarca_tpo_proceso       CHAR(4) ;
   DEFINE tmp_ret_det_pagmarca_f_marca           CHAR(10);
   DEFINE tmp_ret_det_pagmarca_f_pago            CHAR(10);
   DEFINE tmp_ret_det_pagmarca_imp_pagado        CHAR(10);
   DEFINE tmp_ret_det_pagmarca_doc_pago_fico     CHAR(15);
   DEFINE tmp_ret_det_pagmarca_f_pago_fico       CHAR(10);
   DEFINE tmp_ret_det_pagmarca_ref_pago_fico     CHAR(7) ;
   DEFINE tmp_ret_det_pagmarca_id_beneficiario   CHAR(1) ;
   DEFINE tmp_ret_det_pagmarca_nombres           CHAR(40);
   DEFINE tmp_ret_det_pagmarca_ap_paterno        CHAR(40);
   DEFINE tmp_ret_det_pagmarca_ap_materno        CHAR(40);
   DEFINE tmp_ret_det_pagmarca_clabe             CHAR(18);
   DEFINE tmp_ret_det_pagmarca_cve_banco         CHAR(5) ;
   DEFINE tmp_ret_det_pagmarca_ent_federativa    CHAR(2) ;
   DEFINE tmp_ret_det_pagmarca_f_autorizacion    CHAR(10);
   DEFINE tmp_ret_det_pagmarca_cve_afore         CHAR(4) ;
   DEFINE tmp_ret_det_pagmarca_num_caso_adai     CHAR(10);
   DEFINE tmp_ret_det_pagmarca_num_laudo         CHAR(9) ;
   DEFINE tmp_ret_det_pagmarca_num_junta_con_arb CHAR(2) ;
   DEFINE tmp_ret_det_pagmarca_res_operacion     CHAR(2) ;
   DEFINE tmp_ret_det_pagmarca_motivo_rch1       CHAR(3) ;
   DEFINE tmp_ret_det_pagmarca_motivo_rch2       CHAR(3) ;

   -- tabla destino ret_pago_trm
   DEFINE ret_pago_trm_id_solicitud          decimal(9,0) ;
   DEFINE ret_pago_trm_id_derechohabiente    decimal(9,0) ;
   DEFINE ret_pago_trm_folio                 decimal(9,0) ;
   DEFINE ret_pago_trm_estatus_trm           smallint     ;
   DEFINE ret_pago_trm_estatus_jfca          smallint     ;
   DEFINE ret_pago_trm_tpo_proceso           smallint     ;
   DEFINE ret_pago_trm_f_marca               date         ;
   DEFINE ret_pago_trm_f_pago                date         ;
   DEFINE ret_pago_trm_importe_pagado        decimal(10,2);
   DEFINE ret_pago_trm_doc_pago_fico         char(10)     ;
   DEFINE ret_pago_trm_doc_por_pagar_fico    char(15)     ;
   DEFINE ret_pago_trm_f_pago_fico           date         ;
   DEFINE ret_pago_trm_ef_cta_por_pagar_fico smallint     ;
   DEFINE ret_pago_trm_referencia_pago_fico  char(7)      ;
   DEFINE ret_pago_trm_tpo_beneficiario      smallint     ;
   DEFINE ret_pago_trm_nombre_beneficiario   char(40)     ;
   DEFINE ret_pago_trm_paterno_beneficiario  char(40)     ;
   DEFINE ret_pago_trm_materno_beneficiario  char(40)     ;
   DEFINE ret_pago_trm_clabe                 char(18)     ;
   DEFINE ret_pago_trm_cve_banco             INTEGER      ;
   DEFINE ret_pago_trm_entidad_federativa    smallint     ;
   DEFINE ret_pago_trm_f_autorizacion        date         ;
   DEFINE ret_pago_trm_cve_afore             smallint     ;
   DEFINE ret_pago_trm_caso_adai             integer      ;
   DEFINE ret_pago_trm_num_laudo             char(9)      ;
   DEFINE ret_pago_trm_num_junta             char(2)      ;
   DEFINE ret_pago_trm_resultado_operacion   smallint     ;
   DEFINE ret_pago_trm_cod_rechazo_1         smallint     ;
   DEFINE ret_pago_trm_cod_rechazo_2         smallint     ;
   
   --AUXILIARES PARA CONVERSION DE VALORES
   DEFINE v_aux_fecha DATE;


   --PARA REGRESAR EL ESATODO DE LAS SENTENCIAS SQL 
   DEFINE v_estatus_proceso                         SMALLINT     ;  
   DEFINE v_id_datamart                             DECIMAL (9,0);
   DEFINE v_cont_matriz_derecho                     SMALLINT     ;  
   DEFINE v_mensajeno_insert                        VARCHAR(255) ;
   DEFINE v_mensaje                                 VARCHAR(255) ;   
   DEFINE v_cont_registros                          INTEGER      ;
   DEFINE v_fecha                                   VARCHAR(10)  ;

   -- Control de Excepciones
   DEFINE sql_err                         INTEGER;
   DEFINE isam_err                        INTEGER;
   DEFINE err_txt                         VARCHAR(255);
   DEFINE v_c_msj                         VARCHAR(255);
   DEFINE v_si_resultado                  SMALLINT;

 -- para marcar las cuentas
 DEFINE v_i_estado_marca                INTEGER;
 DEFINE v_marca_trm_pagado_laudo_amparo INTEGER; -- 590 tramite judicial pagado laudo/amparo
 DEFINE v_marca_en_tramite_judicial     INTEGER; -- 590 en tramite juducial

   -- se configura el regreso del codigo de error
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;
    
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_pagmarca_nss;
   END EXCEPTION
   
   -- se asume que no hay error
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "Proceso finalizado correctamente";
   
   LET tmp_ret_det_pagmarca_nss = "INICIO";
   
   -- se inician las variables para marca
   LET v_marca_trm_pagado_laudo_amparo  = 590; -- marca para retiro tramite judicial
   LET v_marca_en_tramite_judicial      = 591; -- marca para retiro tramite judicial

   LET v_i_estado_marca = 0;

   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE safre_mig:glo_ctr_archivo
   SET folio = P_folio, estado = 2 -- integrado
   WHERE proceso_cod    = p_proceso_cod
     AND opera_cod      = 1 -- archivo cargado
     AND estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE safre_mig:bat_ctr_operacion 
   SET folio         = P_folio
   WHERE proceso_cod = p_proceso_cod 
     AND opera_cod   = 2
     AND pid         = p_pid;

   --- se consultan los datos de la tabla temporal
   FOREACH
    SELECT      
       nss               ,
       cod_estatus       ,
       cod_estatus_jfca  ,
       tpo_proceso       ,
       f_marca           ,
       f_pago            ,
       imp_pagado        ,
       doc_pago_fico     ,
       f_pago_fico       ,
       ref_pago_fico     ,
       id_beneficiario   ,
       nombres           ,
       ap_paterno        ,
       ap_materno        ,
       clabe             ,
       cve_banco         ,
       ent_federativa    ,
       f_autorizacion    ,
       cve_afore         ,
       num_caso_adai     ,
       num_laudo         ,
       num_junta_con_arb ,
       res_operacion     ,
       motivo_rch1       ,
       motivo_rch2       
    INTO 
       tmp_ret_det_pagmarca_nss               ,
       tmp_ret_det_pagmarca_cod_estatus       ,
       tmp_ret_det_pagmarca_cod_estatus_jfca  ,
       tmp_ret_det_pagmarca_tpo_proceso       ,
       tmp_ret_det_pagmarca_f_marca           ,
       tmp_ret_det_pagmarca_f_pago            ,
       tmp_ret_det_pagmarca_imp_pagado        ,
       tmp_ret_det_pagmarca_doc_pago_fico     ,
       tmp_ret_det_pagmarca_f_pago_fico       ,
       tmp_ret_det_pagmarca_ref_pago_fico     ,
       tmp_ret_det_pagmarca_id_beneficiario   ,
       tmp_ret_det_pagmarca_nombres           ,
       tmp_ret_det_pagmarca_ap_paterno        ,
       tmp_ret_det_pagmarca_ap_materno        ,
       tmp_ret_det_pagmarca_clabe             ,
       tmp_ret_det_pagmarca_cve_banco         ,
       tmp_ret_det_pagmarca_ent_federativa    ,
       tmp_ret_det_pagmarca_f_autorizacion    ,
       tmp_ret_det_pagmarca_cve_afore         ,
       tmp_ret_det_pagmarca_num_caso_adai     ,
       tmp_ret_det_pagmarca_num_laudo         ,
       tmp_ret_det_pagmarca_num_junta_con_arb ,
       tmp_ret_det_pagmarca_res_operacion     ,
       tmp_ret_det_pagmarca_motivo_rch1       ,
       tmp_ret_det_pagmarca_motivo_rch2       
    FROM
       safre_mig:tmp_ret_det_pagos_marcas

       -- se asignan los rechazos
       LET ret_pago_trm_cod_rechazo_1         = tmp_ret_det_pagmarca_motivo_rch1; -- smallint     
       LET ret_pago_trm_cod_rechazo_2         = tmp_ret_det_pagmarca_motivo_rch2; -- smallint     

       -- se busca al derechohabiente
       SELECT id_derechohabiente 
         INTO ret_pago_trm_id_derechohabiente
         FROM afi_derechohabiente
        WHERE nss = tmp_ret_det_pagmarca_nss;    
       
       -- si no se encuentra el NSS, se rechaza
       IF ( ret_pago_trm_id_derechohabiente IS NULL ) THEN
          -- se indica rechazo por no existir derechohabiente
          LET ret_pago_trm_id_derechohabiente    = 0;
          LET ret_pago_trm_cod_rechazo_1         = 1; -- NSS no existe
       END IF
       
       -- se asignan los datos al registro destino
       -- recuera secuencia
       SELECT seq_ret_solicitud.NEXTVAL
         INTO ret_pago_trm_id_solicitud
         FROM systables
        WHERE tabid = 1;

       LET ret_pago_trm_estatus_trm           = tmp_ret_det_pagmarca_cod_estatus; -- smallint     
       LET ret_pago_trm_estatus_jfca          = tmp_ret_det_pagmarca_cod_estatus_jfca; -- smallint     
       LET ret_pago_trm_tpo_proceso           = tmp_ret_det_pagmarca_tpo_proceso; -- smallint -- validar por grupo trabajador
       -- recupera fecha con formato mmddaaaa
       EXECUTE FUNCTION fn_transforma_fecha_ret_ley73(tmp_ret_det_pagmarca_f_marca) 
                   INTO ret_pago_trm_f_marca;
       --LET ret_pago_trm_f_marca               = ; -- date
       -- recupera fecha con formato mmddaaaa
       EXECUTE FUNCTION fn_transforma_fecha_ret_ley73(tmp_ret_det_pagmarca_f_pago) 
                   INTO ret_pago_trm_f_pago;
       --LET ret_pago_trm_f_pago                = ; -- date       
       LET ret_pago_trm_importe_pagado        = tmp_ret_det_pagmarca_imp_pagado; -- decimal(10,2)
       LET ret_pago_trm_doc_pago_fico         = tmp_ret_det_pagmarca_doc_pago_fico; -- char(10)     
       LET ret_pago_trm_doc_por_pagar_fico    = NULL; -- char(15)     

       EXECUTE FUNCTION fn_transforma_fecha_ret_ley73(tmp_ret_det_pagmarca_f_pago_fico) 
                   INTO ret_pago_trm_f_pago_fico;
       --LET ret_pago_trm_f_pago_fico           = tmp_ret_det_pagmarca_f_pago_fico; -- date         
       LET ret_pago_trm_ef_cta_por_pagar_fico = NULL; -- smallint     
       LET ret_pago_trm_referencia_pago_fico  = tmp_ret_det_pagmarca_ref_pago_fico; -- char(7)      
       LET ret_pago_trm_tpo_beneficiario      = tmp_ret_det_pagmarca_id_beneficiario; -- smallint     
       LET ret_pago_trm_nombre_beneficiario   = tmp_ret_det_pagmarca_nombres; -- char(40)     
       LET ret_pago_trm_paterno_beneficiario  = tmp_ret_det_pagmarca_ap_paterno; -- char(40)     
       LET ret_pago_trm_materno_beneficiario  = tmp_ret_det_pagmarca_ap_materno; -- char(40)     

       LET ret_pago_trm_clabe                 = tmp_ret_det_pagmarca_clabe; -- char(18)     

       
       --
       LET ret_pago_trm_cve_banco             = tmp_ret_det_pagmarca_cve_banco; -- smallint     
       --LET ret_pago_trm_cve_banco             = 1;
       --
       
       
       LET ret_pago_trm_entidad_federativa    = tmp_ret_det_pagmarca_ent_federativa; -- smallint     
       -- recupera fecha con formato mmddaaaa
       EXECUTE FUNCTION fn_transforma_fecha_ret_ley73(tmp_ret_det_pagmarca_f_autorizacion) 
                   INTO ret_pago_trm_f_autorizacion;
       --LET ret_pago_trm_f_autorizacion        = ; -- date
       LET ret_pago_trm_cve_afore             = tmp_ret_det_pagmarca_cve_afore; -- smallint     
       LET ret_pago_trm_caso_adai             = tmp_ret_det_pagmarca_num_caso_adai; -- integer      
       LET ret_pago_trm_num_laudo             = tmp_ret_det_pagmarca_num_laudo; -- char(9)      
       LET ret_pago_trm_num_junta             = tmp_ret_det_pagmarca_num_junta_con_arb; -- char(2)      
       LET ret_pago_trm_resultado_operacion   = tmp_ret_det_pagmarca_res_operacion; -- smallint     
   
       -- se asigna el folio
       LET ret_pago_trm_folio = p_folio;

       -- proceso de marcado
       --1.	Marcar todos los registros con la marca "Tramite Judicial en Proceso" cuyo "Código de Estatus JFCA" sea igual a:
       --  a.	0001 En proceso JFCA
       --  b.	0002 En proceso TJ (Trámite Judicial)
       --  c.	0003 En proceso JFCA y TJ
       --
       --2.	Si el registro "Id 3" "Código de estatus" trae un código: 
       --  a.	0016 Pagado por laudo anteriormente
       --  b.	0026 Pagado por amparo anteriormente
       --  c.	0036 Ya existe una disposición de recursos

       -- se intenta marcar por ret_pago_trm_estatus_jfca sea 0001, 0002, 0003 
       -- o cuando estatus_trm sea 0016,0026 y 0036
       -- [son tipo de dato SMALLINT]
       IF ( (ret_pago_trm_estatus_jfca = 1) OR
            (ret_pago_trm_estatus_jfca = 2) OR
            (ret_pago_trm_estatus_jfca = 3) ) THEN
          
          -- si el estatos de trm es 16, 26 o 36 se le pone marca de tramite judicial pagada por laudo o amparo
          IF ( (ret_pago_trm_estatus_trm = 16) OR
               (ret_pago_trm_estatus_trm = 26) OR
               (ret_pago_trm_estatus_trm = 36) ) THEN
          
             LET v_i_estado_marca = 0;
             
             EXECUTE FUNCTION fn_marca_cuenta(
                     ret_pago_trm_id_derechohabiente
                    ,v_marca_trm_pagado_laudo_amparo -- marca de retiro tramite judicial pagado amparo/laudo
                    ,ret_pago_trm_id_solicitud -- id de solicitud
                    ,ret_pago_trm_folio
                    ,0 -- estado marca
                    ,0 -- codigo de rechazo
                    ,0 -- marca de la causa
                    ,NULL -- fecha de la causa
                    ,p_usuario_cod
                    ,p_proceso_cod)
                INTO v_i_estado_marca;
                
          ELSE
             -- se le pone marca de tramite judicial en proceso
             LET v_i_estado_marca = 0;
             
             EXECUTE FUNCTION fn_marca_cuenta(
                     ret_pago_trm_id_derechohabiente
                    ,v_marca_en_tramite_judicial -- marca de retiro en tramite judicial
                    ,ret_pago_trm_id_solicitud -- id de solicitud
                    ,ret_pago_trm_folio
                    ,0 -- estado marca
                    ,0 -- codigo de rechazo
                    ,0 -- marca de la causa
                    ,NULL -- fecha de la causa
                    ,p_usuario_cod
                    ,p_proceso_cod)
                INTO v_i_estado_marca;
                
          END IF
 
          -- si no se pudo marcar, se rechaza el registro
          IF ( v_i_estado_marca > 0 ) THEN
             LET ret_pago_trm_resultado_operacion = "02"; -- rechazada
             LET ret_pago_trm_cod_rechazo_1       = 130; -- no se pudo marcar la cuenta
             LET ret_pago_trm_cod_rechazo_2       = v_i_estado_marca; -- no se pudo marcar la cuenta
          END IF

       END IF

       
       -- comentado por cambio 22 nov 2012
       -- se intenta marcar solo en estatus_trm sea 0016,0026 y 0036
       --IF ( ret_pago_trm_id_derechohabiente <> 0 ) THEN
       --   IF ( (ret_pago_trm_estatus_trm = "0016") OR
       --        (ret_pago_trm_estatus_trm = "0026") OR
       --        (ret_pago_trm_estatus_trm = "0036") ) THEN
       --   
       --      LET v_i_estado_marca = 0;
       --      
       --      EXECUTE FUNCTION fn_marca_cuenta(
       --              ret_pago_trm_id_derechohabiente
       --             ,v_marca_trm -- marca de retiro tramite judicial
       --             ,ret_pago_trm_id_solicitud -- id de solicitud
       --             ,ret_pago_trm_folio
       --             ,0 -- estado marca
       --             ,0 -- codigo de rechazo
       --             ,0 -- marca de la causa
       --             ,NULL -- fecha de la causa
       --             ,p_usuario_cod
       --             ,p_proceso_cod)
       --         INTO v_i_estado_marca;
       --         
       --      -- si no se pudo marcar, se rechaza el registro
       --      IF ( v_i_estado_marca > 0 ) THEN
       --         LET ret_pago_trm_resultado_operacion = "02"; -- rechazada
       --         LET ret_pago_trm_cod_rechazo_1       = 130; -- no se pudo marcar la cuenta
       --         LET ret_pago_trm_cod_rechazo_2       = v_i_estado_marca; -- no se pudo marcar la cuenta
       --      END IF
       --   END IF
       --END IF
       
       
       
      -- se insertan los registros en ret_datamart 
      INSERT INTO safre_viv:ret_pago_trm (
         id_solicitud          ,
         id_derechohabiente    ,
         folio                 ,
         estatus_trm           ,
         estatus_jfca          ,
         tpo_proceso           ,
         f_marca               ,
         f_pago                ,
         importe_pagado        ,
         doc_pago_fico         ,
         doc_por_pagar_fico    ,
         f_pago_fico           ,
         ef_cta_por_pagar_fico ,
         referencia_pago_fico  ,
         tpo_beneficiario      ,
         nombre_beneficiario   ,
         paterno_beneficiario  ,
         materno_beneficiario  ,
         clabe                 ,
         cve_banco             ,
         entidad_federativa    ,
         f_autorizacion        ,
         cve_afore             ,
         caso_adai             ,
         num_laudo             ,
         num_junta             ,
         resultado_operacion   ,
         cod_rechazo_1         ,
         cod_rechazo_2         
         )
      VALUES ( 
         ret_pago_trm_id_solicitud          ,
         ret_pago_trm_id_derechohabiente    ,
         ret_pago_trm_folio                 ,
         ret_pago_trm_estatus_trm           ,
         ret_pago_trm_estatus_jfca          ,
         ret_pago_trm_tpo_proceso           ,
         ret_pago_trm_f_marca               ,
         ret_pago_trm_f_pago                ,
         ret_pago_trm_importe_pagado        ,
         ret_pago_trm_doc_pago_fico         ,
         ret_pago_trm_doc_por_pagar_fico    ,
         ret_pago_trm_f_pago_fico           ,
         ret_pago_trm_ef_cta_por_pagar_fico ,
         ret_pago_trm_referencia_pago_fico  ,
         ret_pago_trm_tpo_beneficiario      ,
         ret_pago_trm_nombre_beneficiario   ,
         ret_pago_trm_paterno_beneficiario  ,
         ret_pago_trm_materno_beneficiario  ,
         ret_pago_trm_clabe                 ,
         ret_pago_trm_cve_banco             ,
         ret_pago_trm_entidad_federativa    ,
         ret_pago_trm_f_autorizacion        ,
         ret_pago_trm_cve_afore             ,
         ret_pago_trm_caso_adai             ,
         ret_pago_trm_num_laudo             ,
         ret_pago_trm_num_junta             ,
         ret_pago_trm_resultado_operacion   ,
         ret_pago_trm_cod_rechazo_1         ,
         ret_pago_trm_cod_rechazo_2         
         );
   END FOREACH;
   
   --TRACE 'termino integracion de carga inicial de Tramite Judicial';

   -- el proceso termino correctamente
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET err_txt        = "El proceso de carga inicial de Retiros Pagos TRM (LEY 73) finalizó correctamente.";
   
   RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_pagmarca_nss;
END FUNCTION;


