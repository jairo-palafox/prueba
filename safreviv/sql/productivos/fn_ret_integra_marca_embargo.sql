






CREATE FUNCTION "safreviv".fn_ret_integra_marca_embargo(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)

-- detalle de la tabla temporal
--tmp_ret_marca_embargo

DEFINE tmp_ret_marca_embargo_nss                     CHAR    (11);
DEFINE tmp_ret_marca_embargo_estatus_ssv             CHAR    (13);
DEFINE tmp_ret_marca_embargo_desc_ssv                CHAR    (40);
DEFINE tmp_ret_marca_embargo_estatus_jfca            CHAR    (14);
DEFINE tmp_ret_marca_embargo_desc_jfca               CHAR    (40);
DEFINE tmp_ret_marca_embargo_tpo_proceso             CHAR    (15);
DEFINE tmp_ret_marca_embargo_desc_tpo_proceso        CHAR    (40);
DEFINE tmp_ret_marca_embargo_nombre_afore            CHAR    (40);
DEFINE tmp_ret_marca_embargo_ap_pat_afore            CHAR    (40);
DEFINE tmp_ret_marca_embargo_ap_mat_afore            CHAR    (40);
DEFINE tmp_ret_marca_embargo_benefi_tit              CHAR    (26);
DEFINE tmp_ret_marca_embargo_nombre_benefi           CHAR    (40);
DEFINE tmp_ret_marca_embargo_ap_pat_benefi           CHAR    (40);
DEFINE tmp_ret_marca_embargo_ap_mat_benefi           CHAR    (40);
DEFINE tmp_ret_marca_embargo_curp                    CHAR    (20);
DEFINE tmp_ret_marca_embargo_rfc                     CHAR    (15);
DEFINE tmp_ret_marca_embargo_ent_fed                 CHAR    (18);
DEFINE tmp_ret_marca_embargo_f_ini_tramite           CHAR    (26);
DEFINE tmp_ret_marca_embargo_f_aut_pago              CHAR    (26);
DEFINE tmp_ret_marca_embargo_num_doc_cta_fico        CHAR    (25);
DEFINE tmp_ret_marca_embargo_eje_fisc_cta_fico       CHAR    (25);
DEFINE tmp_ret_marca_embargo_no_docto_pago_fico      CHAR    (26);
DEFINE tmp_ret_marca_embargo_f_pago_fico             CHAR    (15);
DEFINE tmp_ret_marca_embargo_imp_pago_fico           CHAR    (20);
DEFINE tmp_ret_marca_embargo_ref_pago_fico           CHAR    (23);
DEFINE tmp_ret_marca_embargo_caso_adai               CHAR    (19);
DEFINE tmp_ret_marca_embargo_num_laudo               CHAR    (15);
DEFINE tmp_ret_marca_embargo_num_junta_esp           CHAR    (24);
DEFINE tmp_ret_marca_embargo_imp_pago_ant            CHAR    (26);
DEFINE tmp_ret_marca_embargo_f_pago_ant              CHAR    (26);
DEFINE tmp_ret_marca_embargo_cve_banco               CHAR    (14);
DEFINE tmp_ret_marca_embargo_cta_bancaria            CHAR    (18);
DEFINE tmp_ret_marca_embargo_imp_transf_ssv          CHAR    (30);
DEFINE tmp_ret_marca_embargo_f_transf_ssv            CHAR    (28);
DEFINE tmp_ret_marca_embargo_ssv_dif                 CHAR    (29);
DEFINE tmp_ret_marca_embargo_f_marca_tj              CHAR    (21);
DEFINE tmp_ret_marca_embargo_ssv_error               CHAR    (30);
DEFINE tmp_ret_marca_embargo_ssv_cve_afore           CHAR    (15);
DEFINE tmp_ret_marca_embargo_estatus_marca_ant       CHAR    (23);
DEFINE tmp_ret_marca_embargo_aivs_ssv_97             CHAR    (24);
DEFINE tmp_ret_marca_embargo_imp_ssv_92              CHAR    (23);
DEFINE tmp_ret_marca_embargo_aivs_ssv_92             CHAR    (24);
DEFINE tmp_ret_marca_embargo_env_tesofe              CHAR    (29);
DEFINE tmp_ret_marca_embargo_pagado_tesofe           CHAR    (25);
DEFINE tmp_ret_marca_embargo_rec_env_tesofe          CHAR    (24);
DEFINE tmp_ret_marca_embargo_rec_pag_tesofe          CHAR    (24);

-- tablas destino
DEFINE ret_marca_embargo_id_derechohabiente      DECIMAL (9,0);
DEFINE ret_marca_embargo_folio                   DECIMAL (9,0);
DEFINE ret_marca_embargo_nss                     CHAR    (11);
DEFINE ret_marca_embargo_estatus_ssv             CHAR    (13);
DEFINE ret_marca_embargo_desc_ssv                CHAR    (40);
DEFINE ret_marca_embargo_estatus_jfca            CHAR    (14);
DEFINE ret_marca_embargo_desc_jfca               CHAR    (40);
DEFINE ret_marca_embargo_tpo_proceso             CHAR    (15);
DEFINE ret_marca_embargo_desc_tpo_proceso        CHAR    (40);
DEFINE ret_marca_embargo_nombre_afore            CHAR    (40);
DEFINE ret_marca_embargo_ap_pat_afore            CHAR    (40);
DEFINE ret_marca_embargo_ap_mat_afore            CHAR    (40);
DEFINE ret_marca_embargo_benefi_tit              CHAR    (26);
DEFINE ret_marca_embargo_nombre_benefi           CHAR    (40);
DEFINE ret_marca_embargo_ap_pat_benefi           CHAR    (40);
DEFINE ret_marca_embargo_ap_mat_benefi           CHAR    (40);
DEFINE ret_marca_embargo_curp                    CHAR    (20);
DEFINE ret_marca_embargo_rfc                     CHAR    (15);
DEFINE ret_marca_embargo_ent_fed                 CHAR    (18);
DEFINE ret_marca_embargo_f_ini_tramite           CHAR    (26);
DEFINE ret_marca_embargo_f_aut_pago              CHAR    (26);
DEFINE ret_marca_embargo_num_doc_cta_fico        CHAR    (25);
DEFINE ret_marca_embargo_eje_fisc_cta_fico       CHAR    (25);
DEFINE ret_marca_embargo_no_docto_pago_fico      CHAR    (26);
DEFINE ret_marca_embargo_f_pago_fico             CHAR    (15);
DEFINE ret_marca_embargo_imp_pago_fico           CHAR    (20);
DEFINE ret_marca_embargo_ref_pago_fico           CHAR    (23);
DEFINE ret_marca_embargo_caso_adai               CHAR    (19);
DEFINE ret_marca_embargo_num_laudo               CHAR    (15);
DEFINE ret_marca_embargo_num_junta_esp           CHAR    (24);
DEFINE ret_marca_embargo_imp_pago_ant            CHAR    (26);
DEFINE ret_marca_embargo_f_pago_ant              CHAR    (26);
DEFINE ret_marca_embargo_cve_banco               CHAR    (14);
DEFINE ret_marca_embargo_cta_bancaria            CHAR    (18);
DEFINE ret_marca_embargo_imp_transf_ssv          CHAR    (30);
DEFINE ret_marca_embargo_f_transf_ssv            CHAR    (28);
DEFINE ret_marca_embargo_ssv_dif                 CHAR    (29);
DEFINE ret_marca_embargo_f_marca_tj              CHAR    (21);
DEFINE ret_marca_embargo_ssv_error               CHAR    (30);
DEFINE ret_marca_embargo_ssv_cve_afore           CHAR    (15);
DEFINE ret_marca_embargo_estatus_marca_ant       CHAR    (23);
DEFINE ret_marca_embargo_aivs_ssv_97             CHAR    (24);
DEFINE ret_marca_embargo_imp_ssv_92              CHAR    (23);
DEFINE ret_marca_embargo_aivs_ssv_92             CHAR    (24);
DEFINE ret_marca_embargo_env_tesofe              CHAR    (29);
DEFINE ret_marca_embargo_pagado_tesofe           CHAR    (25);
DEFINE ret_marca_embargo_rec_env_tesofe          CHAR    (24);
DEFINE ret_marca_embargo_rec_pag_tesofe          CHAR    (24);
DEFINE ret_marca_embargo_estado_solicitud        SMALLINT    ;
DEFINE ret_marca_embargo_cod_rechazo             SMALLINT    ;

-- registros marcados
DEFINE v_marcados                                SMALLINT;
-- registros con error en marca
DEFINE v_con_error_marca                         SMALLINT;
-- registros procesados
DEFINE v_procesados                              SMALLINT;
-- marca correspondiente para 0027
DEFINE v_marca_593                               SMALLINT;
-- marca correspondiente para 0028
DEFINE v_marca_594                               SMALLINT;
-- marca paso
DEFINE v_marca                                   SMALLINT;
-- estado solicitud
DEFINE v_estado_solicitud                        SMALLINT;
-- codigo de rechazo
DEFINE v_cod_rechazo                             SMALLINT;
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
-- cantidad de registro para el mismo nss
DEFINE v_recurrencias                            SMALLINT; 
-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE v_resultado                               SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_marca_embargo.txt";

   -- se inician los contadores de registros insertados y rechazados

   LET v_marcados             = 0;
   LET v_con_error_marca      = 0;
   LET v_procesados           = 0;
   LET v_marca_593            = 593;
   LET v_marca_594            = 594;
   LET v_estado_solicitud     = 0;
   LET v_cod_rechazo          = 0;
   LET v_marca                = 0;

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';
  

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

   -- integracion de detalle
   FOREACH
   SELECT
        nss                     ,
        estatus_ssv             ,
        desc_ssv                ,
        estatus_jfca            ,
        desc_jfca               ,
        tpo_proceso             ,
        desc_tpo_proceso        ,
        nombre_afore            ,
        ap_pat_afore            ,
        ap_mat_afore            ,
        benefi_tit              ,
        nombre_benefi           ,
        ap_pat_benefi           ,
        ap_mat_benefi           ,
        curp                    ,
        rfc                     ,
        ent_fed                 ,
        f_ini_tramite           ,
        f_aut_pago              ,
        num_doc_cta_fico        ,
        eje_fisc_cta_fico       ,
        no_docto_pago_fico      ,
        f_pago_fico             ,
        imp_pago_fico           ,
        ref_pago_fico           ,
        caso_adai               ,
        num_laudo               ,
        num_junta_esp           ,
        imp_pago_ant            ,
        f_pago_ant              ,
        cve_banco               ,
        cta_bancaria            ,
        imp_transf_ssv          ,
        f_transf_ssv            ,
        ssv_dif                 ,
        f_marca_tj              ,
        ssv_error               ,
        ssv_cve_afore           ,
        estatus_marca_ant       ,
        aivs_ssv_97             ,
        imp_ssv_92              ,
        aivs_ssv_92             ,
        env_tesofe              ,
        pagado_tesofe           ,
        rec_env_tesofe          ,
        rec_pag_tesofe          
   INTO
        tmp_ret_marca_embargo_nss                     ,
        tmp_ret_marca_embargo_estatus_ssv             ,
        tmp_ret_marca_embargo_desc_ssv                ,
        tmp_ret_marca_embargo_estatus_jfca            ,
        tmp_ret_marca_embargo_desc_jfca               ,
        tmp_ret_marca_embargo_tpo_proceso             ,
        tmp_ret_marca_embargo_desc_tpo_proceso        ,
        tmp_ret_marca_embargo_nombre_afore            ,
        tmp_ret_marca_embargo_ap_pat_afore            ,
        tmp_ret_marca_embargo_ap_mat_afore            ,
        tmp_ret_marca_embargo_benefi_tit              ,
        tmp_ret_marca_embargo_nombre_benefi           ,
        tmp_ret_marca_embargo_ap_pat_benefi           ,
        tmp_ret_marca_embargo_ap_mat_benefi           ,
        tmp_ret_marca_embargo_curp                    ,
        tmp_ret_marca_embargo_rfc                     ,
        tmp_ret_marca_embargo_ent_fed                 ,
        tmp_ret_marca_embargo_f_ini_tramite           ,
        tmp_ret_marca_embargo_f_aut_pago              ,
        tmp_ret_marca_embargo_num_doc_cta_fico        ,
        tmp_ret_marca_embargo_eje_fisc_cta_fico       ,
        tmp_ret_marca_embargo_no_docto_pago_fico      ,
        tmp_ret_marca_embargo_f_pago_fico             ,
        tmp_ret_marca_embargo_imp_pago_fico           ,
        tmp_ret_marca_embargo_ref_pago_fico           ,
        tmp_ret_marca_embargo_caso_adai               ,
        tmp_ret_marca_embargo_num_laudo               ,
        tmp_ret_marca_embargo_num_junta_esp           ,
        tmp_ret_marca_embargo_imp_pago_ant            ,
        tmp_ret_marca_embargo_f_pago_ant              ,
        tmp_ret_marca_embargo_cve_banco               ,
        tmp_ret_marca_embargo_cta_bancaria            ,
        tmp_ret_marca_embargo_imp_transf_ssv          ,
        tmp_ret_marca_embargo_f_transf_ssv            ,
        tmp_ret_marca_embargo_ssv_dif                 ,
        tmp_ret_marca_embargo_f_marca_tj              ,
        tmp_ret_marca_embargo_ssv_error               ,
        tmp_ret_marca_embargo_ssv_cve_afore           ,
        tmp_ret_marca_embargo_estatus_marca_ant       ,
        tmp_ret_marca_embargo_aivs_ssv_97             ,
        tmp_ret_marca_embargo_imp_ssv_92              ,
        tmp_ret_marca_embargo_aivs_ssv_92             ,
        tmp_ret_marca_embargo_env_tesofe              ,
        tmp_ret_marca_embargo_pagado_tesofe           ,
        tmp_ret_marca_embargo_rec_env_tesofe          ,
        tmp_ret_marca_embargo_rec_pag_tesofe          
   FROM
        safre_tmp:tmp_ret_marca_embargo
   
      -- se busca el derechohabiente
      LET v_recurrencias = 0;
      SELECT COUNT(*)
        INTO v_recurrencias
        FROM afi_derechohabiente
       WHERE nss = tmp_ret_marca_embargo_nss;

      IF v_recurrencias > 1 THEN
          LET ret_marca_embargo_id_derechohabiente = 0;
          LET v_estado_solicitud                   = 100;
          LET v_cod_rechazo                        = 52;   -- Se encontro mas de un registro para el NSS
      ELSE 
          IF v_recurrencias = 0 THEN
              LET ret_marca_embargo_id_derechohabiente = 0;
              LET v_estado_solicitud                   = 100;
              LET v_cod_rechazo                        = 7;    -- No existe el NSS
          ELSE 
              SELECT id_derechohabiente
                INTO ret_marca_embargo_id_derechohabiente
                FROM afi_derechohabiente
               WHERE nss = tmp_ret_marca_embargo_nss;

              LET v_marca = 0;
              IF tmp_ret_marca_embargo_estatus_ssv = "0027" THEN
                  LET v_marca = v_marca_593;
              END IF
              IF tmp_ret_marca_embargo_estatus_ssv = "0028" THEN
                  LET v_marca = v_marca_594;
              END IF
              IF v_marca <> 0 THEN
                  CALL fn_marca_cuenta(ret_marca_embargo_id_derechohabiente, v_marca, 0, p_folio,0,0,0,NULL,"SAFREVIV",2605) RETURNING v_resultado;      --- Proceso de Marca de Embargo
                  IF v_resultado <> 0 THEN
                      LET v_estado_solicitud = 100;
                      LET v_cod_rechazo      = v_resultado;
                  ELSE 
                      LET v_estado_solicitud = 0;
                      LET v_cod_rechazo      = 0;       -- no hubo problemas con la marca
                  END IF

              ELSE 
                  LET v_estado_solicitud = 100;
                  LET v_cod_rechazo      = 333;         -- Problemas de datos
              END IF
          END IF
      END IF  

      -- se transfieren los datos a registro
      LET ret_marca_embargo_folio              = p_folio                                       ;
      LET ret_marca_embargo_nss                = tmp_ret_marca_embargo_nss                     ;
      LET ret_marca_embargo_estatus_ssv        = tmp_ret_marca_embargo_estatus_ssv             ;
      LET ret_marca_embargo_desc_ssv           = tmp_ret_marca_embargo_desc_ssv                ;
      LET ret_marca_embargo_estatus_jfca       = tmp_ret_marca_embargo_estatus_jfca            ;
      LET ret_marca_embargo_desc_jfca          = tmp_ret_marca_embargo_desc_jfca               ;
      LET ret_marca_embargo_tpo_proceso        = tmp_ret_marca_embargo_tpo_proceso             ;
      LET ret_marca_embargo_desc_tpo_proceso   = tmp_ret_marca_embargo_desc_tpo_proceso        ;
      LET ret_marca_embargo_nombre_afore       = tmp_ret_marca_embargo_nombre_afore            ;
      LET ret_marca_embargo_ap_pat_afore       = tmp_ret_marca_embargo_ap_pat_afore            ;
      LET ret_marca_embargo_ap_mat_afore       = tmp_ret_marca_embargo_ap_mat_afore            ;
      LET ret_marca_embargo_benefi_tit         = tmp_ret_marca_embargo_benefi_tit              ;
      LET ret_marca_embargo_nombre_benefi      = tmp_ret_marca_embargo_nombre_benefi           ;
      LET ret_marca_embargo_ap_pat_benefi      = tmp_ret_marca_embargo_ap_pat_benefi           ;
      LET ret_marca_embargo_ap_mat_benefi      = tmp_ret_marca_embargo_ap_mat_benefi           ;
      LET ret_marca_embargo_curp               = tmp_ret_marca_embargo_curp                    ;
      LET ret_marca_embargo_rfc                = tmp_ret_marca_embargo_rfc                     ;
      LET ret_marca_embargo_ent_fed            = tmp_ret_marca_embargo_ent_fed                 ;
      LET ret_marca_embargo_f_ini_tramite      = tmp_ret_marca_embargo_f_ini_tramite           ;
      LET ret_marca_embargo_f_aut_pago         = tmp_ret_marca_embargo_f_aut_pago              ;
      LET ret_marca_embargo_num_doc_cta_fico   = tmp_ret_marca_embargo_num_doc_cta_fico        ;
      LET ret_marca_embargo_eje_fisc_cta_fico  = tmp_ret_marca_embargo_eje_fisc_cta_fico       ;
      LET ret_marca_embargo_no_docto_pago_fico = tmp_ret_marca_embargo_no_docto_pago_fico      ;
      LET ret_marca_embargo_f_pago_fico        = tmp_ret_marca_embargo_f_pago_fico             ;
      LET ret_marca_embargo_imp_pago_fico      = tmp_ret_marca_embargo_imp_pago_fico           ;
      LET ret_marca_embargo_ref_pago_fico      = tmp_ret_marca_embargo_ref_pago_fico           ;
      LET ret_marca_embargo_caso_adai          = tmp_ret_marca_embargo_caso_adai               ;
      LET ret_marca_embargo_num_laudo          = tmp_ret_marca_embargo_num_laudo               ;
      LET ret_marca_embargo_num_junta_esp      = tmp_ret_marca_embargo_num_junta_esp           ;
      LET ret_marca_embargo_imp_pago_ant       = tmp_ret_marca_embargo_imp_pago_ant            ;
      LET ret_marca_embargo_f_pago_ant         = tmp_ret_marca_embargo_f_pago_ant              ;
      LET ret_marca_embargo_cve_banco          = tmp_ret_marca_embargo_cve_banco               ;
      LET ret_marca_embargo_cta_bancaria       = tmp_ret_marca_embargo_cta_bancaria            ;
      LET ret_marca_embargo_imp_transf_ssv     = tmp_ret_marca_embargo_imp_transf_ssv          ;
      LET ret_marca_embargo_f_transf_ssv       = tmp_ret_marca_embargo_f_transf_ssv            ;
      LET ret_marca_embargo_ssv_dif            = tmp_ret_marca_embargo_ssv_dif                 ;
      LET ret_marca_embargo_f_marca_tj         = tmp_ret_marca_embargo_f_marca_tj              ;
      LET ret_marca_embargo_ssv_error          = tmp_ret_marca_embargo_ssv_error               ;
      LET ret_marca_embargo_ssv_cve_afore      = tmp_ret_marca_embargo_ssv_cve_afore           ;
      LET ret_marca_embargo_estatus_marca_ant  = tmp_ret_marca_embargo_estatus_marca_ant       ;
      LET ret_marca_embargo_aivs_ssv_97        = tmp_ret_marca_embargo_aivs_ssv_97             ;
      LET ret_marca_embargo_imp_ssv_92         = tmp_ret_marca_embargo_imp_ssv_92              ;
      LET ret_marca_embargo_aivs_ssv_92        = tmp_ret_marca_embargo_aivs_ssv_92             ;
      LET ret_marca_embargo_env_tesofe         = tmp_ret_marca_embargo_env_tesofe              ;
      LET ret_marca_embargo_pagado_tesofe      = tmp_ret_marca_embargo_pagado_tesofe           ;
      LET ret_marca_embargo_rec_env_tesofe     = tmp_ret_marca_embargo_rec_env_tesofe          ;
      LET ret_marca_embargo_rec_pag_tesofe     = tmp_ret_marca_embargo_rec_pag_tesofe          ;
      LET ret_marca_embargo_estado_solicitud   = v_estado_solicitud                            ;
      LET ret_marca_embargo_cod_rechazo        = v_cod_rechazo                                 ;
     
      -- se inserta en tabla destino
      INSERT INTO ret_marca_embargo (
                    id_derechohabiente      ,
                    folio                   ,
                    nss                     ,
                    estatus_ssv             ,
                    desc_ssv                ,
                    estatus_jfca            ,
                    desc_jfca               ,
                    tpo_proceso             ,
                    desc_tpo_proceso        ,
                    nombre_afore            ,
                    ap_pat_afore            ,
                    ap_mat_afore            ,
                    benefi_tit              ,
                    nombre_benefi           ,
                    ap_pat_benefi           ,
                    ap_mat_benefi           ,
                    curp                    ,
                    rfc                     ,
                    ent_fed                 ,
                    f_ini_tramite           ,
                    f_aut_pago              ,
                    num_doc_cta_fico        ,
                    eje_fisc_cta_fico       ,
                    no_docto_pago_fico      ,
                    f_pago_fico             ,
                    imp_pago_fico           ,
                    ref_pago_fico           ,
                    caso_adai               ,
                    num_laudo               ,
                    num_junta_esp           ,
                    imp_pago_ant            ,
                    f_pago_ant              ,
                    cve_banco               ,
                    cta_bancaria            ,
                    imp_transf_ssv          ,
                    f_transf_ssv            ,
                    ssv_dif                 ,
                    f_marca_tj              ,
                    ssv_error               ,
                    ssv_cve_afore           ,
                    estatus_marca_ant       ,
                    aivs_ssv_97             ,
                    imp_ssv_92              ,
                    aivs_ssv_92             ,
                    env_tesofe              ,
                    pagado_tesofe           ,
                    rec_env_tesofe          ,
                    rec_pag_tesofe          ,
                    estado_solicitud        ,
                    cod_rechazo
      ) VALUES (
                  ret_marca_embargo_id_derechohabiente ,
                  ret_marca_embargo_folio              ,
                  ret_marca_embargo_nss                ,
                  ret_marca_embargo_estatus_ssv        ,
                  ret_marca_embargo_desc_ssv           ,
                  ret_marca_embargo_estatus_jfca       ,
                  ret_marca_embargo_desc_jfca          ,
                  ret_marca_embargo_tpo_proceso        ,
                  ret_marca_embargo_desc_tpo_proceso   ,
                  ret_marca_embargo_nombre_afore       ,
                  ret_marca_embargo_ap_pat_afore       ,
                  ret_marca_embargo_ap_mat_afore       ,
                  ret_marca_embargo_benefi_tit         ,
                  ret_marca_embargo_nombre_benefi      ,
                  ret_marca_embargo_ap_pat_benefi      ,
                  ret_marca_embargo_ap_mat_benefi      ,
                  ret_marca_embargo_curp               ,
                  ret_marca_embargo_rfc                ,
                  ret_marca_embargo_ent_fed            ,
                  ret_marca_embargo_f_ini_tramite      ,
                  ret_marca_embargo_f_aut_pago         ,
                  ret_marca_embargo_num_doc_cta_fico   ,
                  ret_marca_embargo_eje_fisc_cta_fico  ,
                  ret_marca_embargo_no_docto_pago_fico ,
                  ret_marca_embargo_f_pago_fico        ,
                  ret_marca_embargo_imp_pago_fico      ,
                  ret_marca_embargo_ref_pago_fico      ,
                  ret_marca_embargo_caso_adai          ,
                  ret_marca_embargo_num_laudo          ,
                  ret_marca_embargo_num_junta_esp      ,
                  ret_marca_embargo_imp_pago_ant       ,
                  ret_marca_embargo_f_pago_ant         ,
                  ret_marca_embargo_cve_banco          ,
                  ret_marca_embargo_cta_bancaria       ,
                  ret_marca_embargo_imp_transf_ssv     ,
                  ret_marca_embargo_f_transf_ssv       ,
                  ret_marca_embargo_ssv_dif            ,
                  ret_marca_embargo_f_marca_tj         ,
                  ret_marca_embargo_ssv_error          ,
                  ret_marca_embargo_ssv_cve_afore      ,
                  ret_marca_embargo_estatus_marca_ant  ,
                  ret_marca_embargo_aivs_ssv_97        ,
                  ret_marca_embargo_imp_ssv_92         ,
                  ret_marca_embargo_aivs_ssv_92        ,
                  ret_marca_embargo_env_tesofe         ,
                  ret_marca_embargo_pagado_tesofe      ,
                  ret_marca_embargo_rec_env_tesofe     ,
                  ret_marca_embargo_rec_pag_tesofe     ,
                  ret_marca_embargo_estado_solicitud   ,
                  ret_marca_embargo_cod_rechazo        
      );
   
   END FOREACH;
   
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_marca_embargo;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


