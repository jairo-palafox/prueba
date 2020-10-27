






CREATE FUNCTION "safreviv".fn_ret_tipon_reverso_inconsistencia(p_usuario_cod CHAR(20), 
                                                    p_folio       DECIMAL(10,0),
                                                    p_pid         DECIMAL(9,0)
                                                    ) 
  RETURNING INTEGER

-- tablas origen

-- encabezado de la tabla historica/integrada de retiros tipo N
   DEFINE ret_cza_tipoN_folio                   DECIMAL(9,0)                ;
   DEFINE ret_cza_tipoN_nombre_archivo          CHAR(20)                    ;
   DEFINE ret_cza_tipoN_f_operacion_procesar     DATE                        ;
   DEFINE ret_cza_tipoN_f_carga                 DATE                        ;
   DEFINE ret_cza_tipoN_hora_carga              DATETIME HOUR TO SECOND     ;
   DEFINE ret_cza_tipoN_total_registros         INTEGER                     ;
   DEFINE ret_cza_tipoN_total_ret92             DECIMAL(17,2)               ;
   DEFINE ret_cza_tipoN_total_viv92             DECIMAL(18,6)               ;

-- detalle de la tabla historica/integrada de retiros por tipo N
   DEFINE ret_tipoN_id_solicitud                       DECIMAL(9,0)         ; 
   DEFINE ret_tipoN_id_derechohabiente                 DECIMAL(9,0)         ; 
   DEFINE ret_tipoN_diag_registro                      CHAR(3)              ; 
   DEFINE ret_tipoN_folio                              DECIMAL(9,0)         ; 
   DEFINE ret_tipoN_id_ret_matriz_derecho              SMALLINT             ; 
   DEFINE ret_tipoN_nss_icefa                          CHAR(11)             ; 
   DEFINE ret_tipoN_rfc_icefa                          CHAR(13)             ; 
   DEFINE ret_tipoN_num_ctr_interno                    CHAR(30)             ; 
   DEFINE ret_tipoN_cve_icefa                          CHAR(3)              ; 
   DEFINE ret_tipoN_nombre_icefa                       CHAR(120)            ; 
   DEFINE ret_tipoN_rfc                                CHAR(13)             ; 
   DEFINE ret_tipoN_nombre                             CHAR(120)            ; 
   DEFINE ret_tipoN_cve_doc_probatorio                 CHAR(1)              ; 
   DEFINE ret_tipoN_num_referencia                     CHAR(18)             ; 
   DEFINE ret_tipoN_origen_retiro                      CHAR(1)              ; 
   --DEFINE ret_tipoN_tpo_seguro                         CHAR(2)              ; 
   --DEFINE ret_tipoN_tpo_pension                        CHAR(2)              ; 
   --DEFINE ret_tipoN_tpo_prestacion                     CHAR(2)              ; 
   --DEFINE ret_tipoN_regimen                            SMALLINT             ; 
   DEFINE ret_tipoN_f_inicio_pension                   DATE                 ; 
   DEFINE ret_tipoN_f_resolucion                       DATE                 ; 
   DEFINE ret_tipoN_porcentaje_valuacion               DECIMAL(5,2)         ; 
   DEFINE ret_tipoN_actuario                           CHAR(7)              ; 
   DEFINE ret_tipoN_num_plan_privado                   CHAR(8)              ; 
   DEFINE ret_tipoN_importe_sar92                      DECIMAL(15,2)        ; 
   DEFINE ret_tipoN_aivs_viv92                         DECIMAL(15,6)        ; 
   DEFINE ret_tipoN_cve_afore                          SMALLINT             ; 
   DEFINE ret_tipoN_cod_rechazo                        SMALLINT             ; 
   DEFINE ret_tipoN_estado_solicitud                   SMALLINT             ; 

-- =============================================================================
   -- rechazo de encabezado
   -- ret_cza_transferencia_rch
   DEFINE ret_cza_tipoN_rch_folio                    DECIMAL(9,0)           ;
   DEFINE ret_cza_tipoN_rch_nombre_archivo           CHAR(20)               ;
   DEFINE ret_cza_tipoN_rch_f_operacion_procesar      DATE                   ;
   DEFINE ret_cza_tipoN_rch_f_carga                  DATE                   ;
   DEFINE ret_cza_tipoN_rch_hora_carga               DATETIME HOUR TO SECOND;
   DEFINE ret_cza_tipoN_rch_total_registros          INTEGER                ;
   DEFINE ret_cza_tipoN_rch_total_ret92              DECIMAL(17,2)          ;
   DEFINE ret_cza_tipoN_rch_total_viv92              DECIMAL(18,6)          ;
   DEFINE ret_cza_tipoN_rch_usuario                  CHAR(20)               ;
   DEFINE ret_cza_tipoN_rch_resultado_operacion      SMALLINT               ;
   DEFINE ret_cza_tipoN_rch_cod_rechazo_1            SMALLINT               ;
   DEFINE ret_cza_tipoN_rch_cod_rechazo_2            SMALLINT               ;
   DEFINE ret_cza_tipoN_rch_cod_rechazo_3            SMALLINT               ;

-- =============================================================================
   -- rechazo de detalle
   -- ret_transferencia_rch
   DEFINE ret_tipN_rch_id_solicitud                    DECIMAL(9,0)          ;
   DEFINE ret_tipN_rch_id_derechohabiente              DECIMAL(9,0)          ;
   DEFINE ret_tipN_rch_diag_registro                   CHAR(3)               ;
   DEFINE ret_tipN_rch_folio                           DECIMAL(9,0)          ;
   DEFINE ret_tipN_rch_id_ret_matriz_dercho            SMALLINT              ;
   DEFINE ret_tipN_rch_nss_icefa                       CHAR(11)              ;
   DEFINE ret_tipN_rch_rfc_icefa                       CHAR(13)              ;
   DEFINE ret_tipN_rch_num_ctr_interno                 CHAR(30)              ;
   DEFINE ret_tipN_rch_cve_icefa                       CHAR(3)               ;
   DEFINE ret_tipN_rch_nombre_icefa                    CHAR(120)             ;
   DEFINE ret_tipN_rch_rfc                             CHAR(13)              ;
   DEFINE ret_tipN_rch_nombre                          CHAR(120)             ;
   DEFINE ret_tipN_rch_cve_doc_probatorio              CHAR(1)               ;
   DEFINE ret_tipN_rch_num_referencia                  CHAR(18)              ;
   DEFINE ret_tipN_rch_origen_retiro                   CHAR(1)               ;
   DEFINE ret_tipN_rch_tpo_seguro                      CHAR(2)               ;
   DEFINE ret_tipN_rch_tpo_pension                     CHAR(2)               ;
   DEFINE ret_tipN_rch_tpo_prestacion                  CHAR(2)               ;
   DEFINE ret_tipN_rch_regimen                         SMALLINT              ;
   DEFINE ret_tipN_rch_f_inicio_pension                DATE                  ;
   DEFINE ret_tipN_rch_f_resolucion                    DATE                  ;
   DEFINE ret_tipN_rch_porcentaje_valuacion            DECIMAL(5,2)          ;
   DEFINE ret_tipN_rch_actuario                        CHAR(7)               ;
   DEFINE ret_tipN_rch_num_plan_privado                CHAR(8)               ;
   DEFINE ret_tipN_rch_importe_sar92                   DECIMAL(15,2)         ;
   DEFINE ret_tipN_rch_aivs_viv92                      DECIMAL(15,6)         ;
   DEFINE ret_tipN_rch_cve_afore                       SMALLINT              ;
   DEFINE ret_tipN_rch_estado_solicitud                SMALLINT              ;
   DEFINE ret_tipN_rch_resultado_operacion             SMALLINT              ;
   DEFINE ret_tipN_rch_cod_rechazo_1                   SMALLINT              ;
   DEFINE ret_tipN_rch_cod_rechazo_2                   SMALLINT              ;
   DEFINE ret_tipN_rch_cod_rechazo_3                   SMALLINT              ;
                                                                             
                                                                             
   DEFINE ret_dispos_rch_tipo_retiro                   CHAR(1)               ;
   DEFINE ret_dispos_rch_regimen                       CHAR(2)               ;
   DEFINE ret_dispos_rch_tpo_seguro                    CHAR(2)               ;
   DEFINE ret_dispos_rch_tpo_pension                   CHAR(2)               ;
   DEFINE ret_dispos_rch_tpo_prestacion                CHAR(2)               ;

   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                 DECIMAL(9,0);
   DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                        DECIMAL(14)  ;
   DEFINE v_pesos_aiv97                        decimal(14,6);
   DEFINE v_pesos_aiv92                        decimal(14,6);
   
   -- para rechazos
   DEFINE v_b_rechazo_encabezado               SMALLINT;
   DEFINE v_b_rechazo_detalle                  SMALLINT;
   DEFINE v_validar_3_primeros_campos          VARCHAR(6); -- se concatenan los 3 primeros campos para validar
   DEFINE v_afore_cod                          SMALLINT; -- clave de afore
   -- id matriz derecho
   DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS
 
 --
 DEFINE v_sumario_importe_total                 DECIMAL(22,2);
 DEFINE v_sumario_total_registros               DECIMAL(2,0) ;
 DEFINE v_total_registros                       DECIMAL(2,0) ;
 DEFINE v_numero_registros                      DECIMAL(2,0) ;
 DEFINE v_saldo_cuenta                          DECIMAL(14,6);
 
 DEFINE v_motivo_rechazo_1                      SMALLINT;
 DEFINE v_motivo_rechazo_2                      SMALLINT;
 DEFINE v_motivo_rechazo_3                      SMALLINT;
 -- arreglo de codigos de rechazo
 DEFINE v_codigos_rechazo                       CHAR(30); -- los codigos van de tres en tres
 DEFINE v_indice_codigos_rechazo                SMALLINT; 
 
 -- conteo de rechazos e inserciones
 DEFINE v_reg_cza_insertados                    SMALLINT; -- total de registros de encabezado insertados
 DEFINE v_reg_cza_rechazados                    SMALLINT; -- total de registros de encabezado rechazados
 DEFINE v_reg_det_insertados                    SMALLINT; -- total de registros de detalle insertados
 DEFINE v_reg_det_rechazados                    SMALLINT; -- total de registros de detalle rechazados
 
 -- codigos de error en encabezado
 DEFINE v_error_cza_reg_totales_no_coinciden      DECIMAL(10,5);
 DEFINE v_error_cza_tpo_registro_invalido         SMALLINT;
 DEFINE v_error_cza_id_servicio_invalido          SMALLINT;
 DEFINE v_error_cza_sin_precio_fondo              SMALLINT;
 DEFINE v_error_cza_sin_fecha_procesar            SMALLINT;
 DEFINE v_error_cza_sin_fecha_valuacion           SMALLINT;
 -- codigos de error en detalle
 DEFINE v_error_det_nss_no_encontrado             SMALLINT;
 DEFINE v_error_det_tpo_registro_invalido         SMALLINT;
 DEFINE v_error_det_id_servicio_invalido          SMALLINT;
 DEFINE v_error_det_id_operacion_invalido         SMALLINT;
 DEFINE v_error_det_matriz_derecho_no_encontrado  SMALLINT;
 DEFINE v_error_det_sec_pension_invalido          SMALLINT;
 DEFINE v_error_det_fec_solicitud_invalido        SMALLINT;
 DEFINE v_error_det_afore_invalido                SMALLINT;
 
 -- estatus del proceso
 DEFINE v_estatus_proceso                         SMALLINT;   
 --marca de la cuenta
 DEFINE v_marca_entra                             SMALLINT;   

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_tipoN_inconsistencia.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden      = 1000;
   LET v_error_cza_tpo_registro_invalido         = 2;
   LET v_error_cza_id_servicio_invalido          = 3;
   LET v_error_cza_sin_precio_fondo              = 4;
   LET v_error_cza_sin_fecha_procesar            = 5;
   LET v_error_cza_sin_fecha_valuacion           = 6;
 
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_det_id_servicio_invalido          = 3;
   LET v_error_det_id_operacion_invalido         = 4;
   LET v_error_det_matriz_derecho_no_encontrado  = 5;
   LET v_error_det_sec_pension_invalido          = 6;
   LET v_error_det_fec_solicitud_invalido        = 7;
   LET v_error_det_afore_invalido                = 8;  
   
   --marca de tipo N 
   LET  v_marca_entra = 804 ;

   -- se copia el encabezado al rechazo
   FOREACH
   SELECT
      folio                             ,
      nombre_archivo                    ,
      f_operacion_procesar              ,
      f_carga                           ,
      hora_carga                        ,
      total_registros                   ,
      total_ret92                       ,
      total_viv92                       
   INTO    
      ret_cza_tipoN_folio               ,
      ret_cza_tipoN_nombre_archivo      ,
      ret_cza_tipoN_f_operacion_procesar ,
      ret_cza_tipoN_f_carga             ,
      ret_cza_tipoN_hora_carga          ,
      ret_cza_tipoN_total_registros     ,
      ret_cza_tipoN_total_ret92         ,
      ret_cza_tipoN_total_viv92
   FROM
      ret_cza_tipo_n
   WHERE
      folio = p_folio
      
        -- se llenan los datos de rechazo del encabezado
      LET ret_cza_tipoN_rch_folio                  = ret_cza_tipoN_folio               ;
      LET ret_cza_tipoN_rch_nombre_archivo         = ret_cza_tipoN_nombre_archivo      ;
      LET ret_cza_tipoN_rch_f_operacion_procesar    = ret_cza_tipoN_f_operacion_procesar ;
      LET ret_cza_tipoN_rch_f_carga                = ret_cza_tipoN_f_carga             ;
      LET ret_cza_tipoN_rch_hora_carga             = ret_cza_tipoN_hora_carga          ;
      LET ret_cza_tipoN_rch_total_registros        = ret_cza_tipoN_total_registros     ;
      LET ret_cza_tipoN_rch_total_ret92            = ret_cza_tipoN_total_ret92         ;
      LET ret_cza_tipoN_rch_total_viv92            = ret_cza_tipoN_total_viv92         ;
      LET ret_cza_tipoN_rch_usuario                = p_usuario_cod                     ;
      LET ret_cza_tipoN_rch_resultado_operacion    = 2                                 ;
      LET ret_cza_tipoN_rch_cod_rechazo_1          = 101                               ;
      LET ret_cza_tipoN_rch_cod_rechazo_2          = 0                                 ;
      LET ret_cza_tipoN_rch_cod_rechazo_3          = 0                                 ;

      -- se inserta el rechazo
      INSERT INTO ret_cza_tipo_n_rch (
        folio                                            ,
        nombre_archivo                                   ,
        f_operacion_procesar                              ,
        f_carga                                          ,
        hora_carga                                       ,
        total_registros                                  ,
        total_ret92                                      ,
        total_viv92                                      ,
        usuario                                          ,
        resultado_operacion                              ,
        cod_rechazo_1                                    ,
        cod_rechazo_2                                    ,
        cod_rechazo_3                                    
      )
      VALUES 
      (
        ret_cza_tipoN_rch_folio                          ,
        ret_cza_tipoN_rch_nombre_archivo                 ,
        ret_cza_tipoN_rch_f_operacion_procesar            ,
        ret_cza_tipoN_rch_f_carga                        ,
        ret_cza_tipoN_rch_hora_carga                     ,
        ret_cza_tipoN_rch_total_registros                ,
        ret_cza_tipoN_rch_total_ret92                    ,
        ret_cza_tipoN_rch_total_viv92                    ,
        ret_cza_tipoN_rch_usuario                        ,
        ret_cza_tipoN_rch_resultado_operacion            ,
        ret_cza_tipoN_rch_cod_rechazo_1                  ,
        ret_cza_tipoN_rch_cod_rechazo_2                  ,
        ret_cza_tipoN_rch_cod_rechazo_3                  
      );
      
   END FOREACH;


   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      id_solicitud                ,
      id_derechohabiente          ,
      diag_registro               ,
      folio                       ,
      id_ret_matriz_derecho       ,
      nss_icefa                   ,
      rfc_icefa                   ,
      num_ctr_interno             ,
      cve_icefa                   ,
      nombre_icefa                ,
      rfc                         ,
      nombre                      ,
      cve_doc_probatorio          ,
      num_referencia              ,
      origen_retiro               ,
      f_inicio_pension            ,
      f_resolucion                ,
      porcentaje_valuacion        ,
      actuario                    ,
      num_plan_privado            ,
      importe_sar92               ,
      aivs_viv92                  ,
      cve_afore                   ,
      cod_rechazo                 ,
      estado_solicitud            

   INTO
      ret_tipoN_id_solicitud            ,
      ret_tipoN_id_derechohabiente      ,
      ret_tipoN_diag_registro           ,
      ret_tipoN_folio                   ,
      ret_tipoN_id_ret_matriz_derecho   ,
      ret_tipoN_nss_icefa               ,
      ret_tipoN_rfc_icefa               ,
      ret_tipoN_num_ctr_interno         ,
      ret_tipoN_cve_icefa               ,
      ret_tipoN_nombre_icefa            ,
      ret_tipoN_rfc                     ,
      ret_tipoN_nombre                  ,
      ret_tipoN_cve_doc_probatorio      ,
      ret_tipoN_num_referencia          ,
      ret_tipoN_origen_retiro           ,  
      ret_tipoN_f_inicio_pension        ,
      ret_tipoN_f_resolucion            ,
      ret_tipoN_porcentaje_valuacion    ,
      ret_tipoN_actuario                ,
      ret_tipoN_num_plan_privado        ,
      ret_tipoN_importe_sar92           ,
      ret_tipoN_aivs_viv92              ,
      ret_tipoN_cve_afore               ,
      ret_tipoN_cod_rechazo             ,
      ret_tipoN_estado_solicitud         
   FROM                                  
      ret_tipo_n
   WHERE
      folio = p_folio
      
  
         -- se obtienen los datos del retiro
         SELECT
            tpo_retiro     ,
            regimen        ,
            tpo_seguro     ,
            tpo_pension    ,
            tpo_prestacion 
         INTO
            ret_dispos_rch_tipo_retiro   ,
            ret_dispos_rch_regimen       ,
            ret_dispos_rch_tpo_seguro    ,
            ret_dispos_rch_tpo_pension   ,
            ret_dispos_rch_tpo_prestacion
         FROM ret_matriz_derecho
         WHERE
               id_ret_matriz_derecho = ret_tipoN_id_ret_matriz_derecho;

         -- se asignan los datos al registro de rechazo de detalle 
         LET ret_tipN_rch_id_solicitud            = ret_tipoN_id_solicitud            ;
         LET ret_tipN_rch_id_derechohabiente      = ret_tipoN_id_derechohabiente      ;
         LET ret_tipN_rch_diag_registro           = ret_tipoN_diag_registro           ;
         LET ret_tipN_rch_folio                   = ret_tipoN_folio                   ;
         LET ret_tipN_rch_id_ret_matriz_dercho    = ret_tipoN_id_ret_matriz_derecho   ;
         LET ret_tipN_rch_nss_icefa               = ret_tipoN_nss_icefa               ;
         LET ret_tipN_rch_rfc_icefa               = ret_tipoN_rfc_icefa               ;
         LET ret_tipN_rch_num_ctr_interno         = ret_tipoN_num_ctr_interno         ;
         LET ret_tipN_rch_cve_icefa               = ret_tipoN_cve_icefa               ;
         LET ret_tipN_rch_nombre_icefa            = ret_tipoN_nombre_icefa            ;
         LET ret_tipN_rch_rfc                     = ret_tipoN_rfc                     ;
         LET ret_tipN_rch_nombre                  = ret_tipoN_nombre                  ;
         LET ret_tipN_rch_cve_doc_probatorio      = ret_tipoN_cve_doc_probatorio      ;
         LET ret_tipN_rch_num_referencia          = ret_tipoN_num_referencia          ;
         LET ret_tipN_rch_origen_retiro           = ret_tipoN_origen_retiro           ;
         LET ret_tipN_rch_tpo_seguro              = NULL         ;
         LET ret_tipN_rch_tpo_pension             = NULL         ;
         LET ret_tipN_rch_tpo_prestacion          = NULL         ;
         LET ret_tipN_rch_regimen                 = NULL         ;
         LET ret_tipN_rch_f_inicio_pension        = ret_tipoN_f_inicio_pension        ;
         LET ret_tipN_rch_f_resolucion            = ret_tipoN_f_resolucion            ;
         LET ret_tipN_rch_porcentaje_valuacion    = ret_tipoN_porcentaje_valuacion    ;
         LET ret_tipN_rch_actuario                = ret_tipoN_actuario                ;
         LET ret_tipN_rch_num_plan_privado        = ret_tipoN_num_plan_privado        ;
         LET ret_tipN_rch_importe_sar92           = ret_tipoN_importe_sar92           ;
         LET ret_tipN_rch_aivs_viv92              = ret_tipoN_aivs_viv92              ;
         LET ret_tipN_rch_cve_afore               = ret_tipoN_cve_afore               ;
         LET ret_tipN_rch_estado_solicitud        = ret_tipoN_estado_solicitud        ;
         LET ret_tipN_rch_resultado_operacion     = 2                                 ;
         LET ret_tipN_rch_cod_rechazo_1           = 101                               ;
         LET ret_tipN_rch_cod_rechazo_2           = 0                                 ;
         LET ret_tipN_rch_cod_rechazo_3           = 0                                 ;


         --se invoca SP que reversa la marca de la cuenta consultada
         EXECUTE PROCEDURE sp_reversa_marca ( ret_tipN_rch_id_derechohabiente,
                                              v_marca_entra  ,
                                              ret_tipN_rch_id_solicitud ,
                                              ret_tipN_rch_folio
                                             );

         -- se inserta el registro de rechazo
         INSERT INTO ret_tipo_n_rch(
             id_solicitud                       ,
             id_derechohabiente                 ,
             diag_registro                      ,
             folio                              ,
             id_ret_matriz_derecho              , 
             nss_icefa                          ,
             rfc_icefa                          ,
             num_ctr_interno                    ,
             cve_icefa                          ,
             nombre_icefa                       ,
             rfc                                ,
             nombre                             ,
             cve_doc_probatorio                 ,
             num_referencia                     ,
             origen_retiro                      ,
             tpo_seguro                         ,
             tpo_pension                        ,
             tpo_prestacion                     ,
             regimen                            ,
             f_inicio_pension                   ,
             f_resolucion                       ,
             porcentaje_valuacion               ,
             actuario                           ,
             num_plan_privado                   ,
             importe_sar92                      ,
             aivs_viv92                         ,
             cve_afore                          ,
             estado_solicitud                   ,
             resultado_operacion                ,
             cod_rechazo_1                      ,
             cod_rechazo_2                      ,
             cod_rechazo_3          
         )
         VALUES 
         (
             ret_tipN_rch_id_solicitud          ,
             ret_tipN_rch_id_derechohabiente    ,
             ret_tipN_rch_diag_registro         ,
             ret_tipN_rch_folio                 ,
             ret_tipN_rch_id_ret_matriz_dercho  ,
             ret_tipN_rch_nss_icefa             ,
             ret_tipN_rch_rfc_icefa             ,
             ret_tipN_rch_num_ctr_interno       ,
             ret_tipN_rch_cve_icefa             ,
             ret_tipN_rch_nombre_icefa          ,
             ret_tipN_rch_rfc                   ,
             ret_tipN_rch_nombre                ,
             ret_tipN_rch_cve_doc_probatorio    ,
             ret_tipN_rch_num_referencia        ,
             ret_tipN_rch_origen_retiro         ,
             ret_tipN_rch_tpo_seguro            ,
             ret_tipN_rch_tpo_pension           ,
             ret_tipN_rch_tpo_prestacion        ,
             ret_tipN_rch_regimen               ,
             ret_tipN_rch_f_inicio_pension      ,
             ret_tipN_rch_f_resolucion          ,
             ret_tipN_rch_porcentaje_valuacion  ,
             ret_tipN_rch_actuario              ,
             ret_tipN_rch_num_plan_privado      ,
             ret_tipN_rch_importe_sar92         ,
             ret_tipN_rch_aivs_viv92            ,
             ret_tipN_rch_cve_afore             ,
             ret_tipN_rch_estado_solicitud      ,
             ret_tipN_rch_resultado_operacion   ,
             ret_tipN_rch_cod_rechazo_1         ,
             ret_tipN_rch_cod_rechazo_2         ,
             ret_tipN_rch_cod_rechazo_3         
         );                           
         
   END FOREACH;

   -- se borran los registros originales
   DELETE FROM ret_tipo_n
   WHERE       folio = p_folio; 

   -- se borra el registro original
   DELETE FROM ret_cza_tipo_n
   WHERE       folio = p_folio;
   
    
   UPDATE STATISTICS FOR TABLE ret_tipo_n_rch ;
   UPDATE STATISTICS FOR TABLE ret_tipo_n ; 

   -- se borra el registro del archivo cargado para que permita cargarlo de nuevo
   DELETE FROM glo_ctr_archivo
    WHERE folio = p_folio;

   LET v_estatus_proceso = 0; -- rechazado
 
 
   RETURN v_estatus_proceso;
END FUNCTION;


