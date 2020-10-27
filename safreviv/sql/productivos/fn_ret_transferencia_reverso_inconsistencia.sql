






CREATE FUNCTION "safreviv".fn_ret_transferencia_reverso_inconsistencia(p_usuario_cod CHAR(20), p_folio DECIMAL(10,0),
                                                          p_pid DECIMAL(9,0)) 
  RETURNING INTEGER

-- tablas origen

-- encabezado de la tabla historica/integrada de retiros por transferencia
   DEFINE ret_cza_trans_folio                 DECIMAL(9,0)           ;
   DEFINE ret_cza_trans_nombre_archivo        CHAR(20)               ;
   DEFINE ret_cza_trans_f_operacion_procesar  DATE                   ;
   DEFINE ret_cza_trans_f_carga               DATE                   ;
   DEFINE ret_cza_trans_h_carga               DATETIME HOUR TO SECOND;
   DEFINE ret_cza_trans_f_valor_transferencia DATE                   ;
   DEFINE ret_cza_trans_precio_fondo          DECIMAL(14,6)          ;
	 DEFINE ret_cza_trans_total_registros       INTEGER                ;
	 DEFINE ret_cza_trans_total_importe         DECIMAL(18,6)          ;
   DEFINE ret_cza_trans_usuario               CHAR(20)               ;

-- detalle de la tabla historica/integrada de retiros por transferencia
   DEFINE ret_transf_id_solicitud          DECIMAL(9,0) ;
   DEFINE ret_transf_id_derechohabiente    DECIMAL(9,0) ;
   DEFINE ret_transf_id_ret_matriz_derecho SMALLINT     ;
   DEFINE ret_transf_sec_pension           SMALLINT     ;
   DEFINE ret_transf_diag_registro         CHAR(3)      ;
   DEFINE ret_transf_folio                 DECIMAL(9,0) ;
   DEFINE ret_transf_estado_solicitud      SMALLINT     ;
   DEFINE ret_transf_curp                  CHAR(18)     ;
   DEFINE ret_transf_nombre_datamart       CHAR(50)     ;
   DEFINE ret_transf_nombre_afore          CHAR(40)     ;
   DEFINE ret_transf_paterno_afore         CHAR(40)     ;
   DEFINE ret_transf_materno_afore         CHAR(40)     ;
   DEFINE ret_transf_tpo_movimiento        CHAR(3)      ;
   DEFINE ret_transf_f_inicio_pension      DATE         ;
   DEFINE ret_transf_f_resolucion          CHAR(18)     ;
   DEFINE ret_transf_porcentaje_valuacion  DECIMAL(5,2) ;
   DEFINE ret_transf_semanas_cotizadas     INTEGER      ;
   DEFINE ret_transf_f_carga_datamart      DATE         ;
   DEFINE ret_transf_estado_sub_viv        SMALLINT     ;
   DEFINE ret_transf_aivs_viv97            DECIMAL(14,6);
   DEFINE ret_transf_cve_afore             SMALLINT     ;
   DEFINE ret_transf_cod_rechazo           SMALLINT     ;

-- =============================================================================
   -- rechazo de encabezado
   -- ret_cza_transferencia_rch
   DEFINE ret_cza_trans_rch_folio                 decimal(9,0)           ;
   DEFINE ret_cza_trans_rch_nombre_archivo        char(20)               ;
   DEFINE ret_cza_trans_rch_f_operacion_procesar  date                   ;
   DEFINE ret_cza_trans_rch_f_carga               date                   ;
   DEFINE ret_cza_trans_rch_h_carga               datetime hour to minute;
   DEFINE ret_cza_trans_rch_f_valor_transferencia date                   ;
   DEFINE ret_cza_trans_rch_precio_fondo          decimal(14,6)          ;
   DEFINE ret_cza_trans_rch_total_registros       integer                ;
   DEFINE ret_cza_trans_rch_total_importe         decimal(18,6)          ;
   DEFINE ret_cza_trans_rch_usuario               char(20)               ;
   DEFINE ret_cza_trans_rch_cod_rechazo_1         smallint               ;
   DEFINE ret_cza_trans_rch_cod_rechazo_2         smallint               ;
   DEFINE ret_cza_trans_rch_cod_rechazo_3         smallint               ;

   
-- =============================================================================
   -- rechazo de detalle
   -- ret_transferencia_rch
   DEFINE ret_transf_rch_id_derechohabiente    decimal(9,0) ;
   DEFINE ret_transf_rch_folio                 decimal(9,0) ;
   DEFINE ret_transf_rch_id_ret_matriz_derecho smallint     ;
   DEFINE ret_transf_rch_sec_pension           smallint     ;
   DEFINE ret_transf_rch_diag_registro         char(3)      ;
   DEFINE ret_transf_rch_estado_solicitud      smallint     ;
   DEFINE ret_transf_rch_curp                  char(18)     ;
   DEFINE ret_transf_rch_nombre_datamart       char(50)     ;
   DEFINE ret_transf_rch_nombre_afore          char(40)     ;
   DEFINE ret_transf_rch_paterno_afore         char(40)     ;
   DEFINE ret_transf_rch_materno_afore         char(40)     ;
   DEFINE ret_transf_rch_tpo_movimiento        char(3)      ;
   DEFINE ret_transf_rch_f_inicio_pension      date         ;
   DEFINE ret_transf_rch_f_resolucion          date         ;
   DEFINE ret_transf_rch_porcentaje_valuacion  decimal(5,2) ;
   DEFINE ret_transf_rch_semanas_cotizadas     integer      ;
   DEFINE ret_transf_rch_f_carga_datamart      date         ;
   DEFINE ret_transf_rch_estado_sub_viv        smallint     ;
   DEFINE ret_transf_rch_aivs_viv97            decimal(14,6);
   DEFINE ret_transf_rch_cve_afore             smallint     ;
   DEFINE ret_transf_rch_cod_rechazo           smallint     ;
   DEFINE ret_transf_rch_cod_rechazo_1         smallint     ;
   DEFINE ret_transf_rch_cod_rechazo_2         smallint     ;
   DEFINE ret_transf_rch_cod_rechazo_3         smallint     ;



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
 

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_disposicion_inconsistencia.txt";

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
   
   LET v_marca_entra   =  806;

   -- se copia el encabezado al rechazo
   FOREACH
   SELECT
      folio                 ,
      nombre_archivo        ,
      f_operacion_procesar  ,
      f_carga               ,
      h_carga            ,
      f_valor_transferencia ,
      precio_fondo          ,
	    total_registros       ,
	    total_importe         ,
      usuario               
   INTO
      ret_cza_trans_folio                 ,
      ret_cza_trans_nombre_archivo        ,
      ret_cza_trans_f_operacion_procesar  ,
      ret_cza_trans_f_carga               ,
      ret_cza_trans_h_carga               ,
      ret_cza_trans_f_valor_transferencia ,
      ret_cza_trans_precio_fondo          ,
	    ret_cza_trans_total_registros       ,
	    ret_cza_trans_total_importe         ,
      ret_cza_trans_usuario               
   FROM
      ret_cza_transferencia
   WHERE
      folio = p_folio
      
      -- se llenan los datos de rechazo del encabezado
      LET ret_cza_trans_rch_folio                 = ret_cza_trans_folio;
      LET ret_cza_trans_rch_nombre_archivo        = ret_cza_trans_nombre_archivo;
      LET ret_cza_trans_rch_f_operacion_procesar  = ret_cza_trans_f_operacion_procesar;
      LET ret_cza_trans_rch_f_carga               = ret_cza_trans_f_carga;
      LET ret_cza_trans_rch_h_carga               = ret_cza_trans_h_carga;
      LET ret_cza_trans_rch_f_valor_transferencia = ret_cza_trans_f_valor_transferencia;
      LET ret_cza_trans_rch_precio_fondo          = ret_cza_trans_precio_fondo;
      LET ret_cza_trans_rch_total_registros       = ret_cza_trans_total_registros;
      LET ret_cza_trans_rch_total_importe         = ret_cza_trans_total_importe;
      LET ret_cza_trans_rch_usuario               = ret_cza_trans_usuario;
      LET ret_cza_trans_rch_cod_rechazo_1         = 101;
      LET ret_cza_trans_rch_cod_rechazo_2         = 0;
      LET ret_cza_trans_rch_cod_rechazo_3         = 0;
      
      -- se inserta el rechazo
      INSERT INTO ret_cza_transferencia_rch (
         folio                 ,
         nombre_archivo        ,
         f_operacion_procesar  ,
         f_carga               ,
         h_carga               ,
         f_valor_transferencia ,
         precio_fondo          ,
         total_registros       ,
         total_importe         ,
         usuario               ,
         cod_rechazo_1         ,
         cod_rechazo_2         ,
         cod_rechazo_3         
      )
      VALUES (
         ret_cza_trans_rch_folio                 ,
         ret_cza_trans_rch_nombre_archivo        ,
         ret_cza_trans_rch_f_operacion_procesar  ,
         ret_cza_trans_rch_f_carga               ,
         ret_cza_trans_rch_h_carga               ,
         ret_cza_trans_rch_f_valor_transferencia ,
         ret_cza_trans_rch_precio_fondo          ,
         ret_cza_trans_rch_total_registros       ,
         ret_cza_trans_rch_total_importe         ,
         ret_cza_trans_rch_usuario               ,
         ret_cza_trans_rch_cod_rechazo_1         ,
         ret_cza_trans_rch_cod_rechazo_2         ,
         ret_cza_trans_rch_cod_rechazo_3         
      );
      
   END FOREACH;

   -- se borra el registro original
   DELETE FROM ret_cza_transferencia
   WHERE       folio = p_folio;

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      id_solicitud          ,
      id_derechohabiente    ,
      id_ret_matriz_derecho ,
      sec_pension           ,
      diag_registro         ,
      folio                 ,
      estado_solicitud      ,
      curp                  ,
      nombre_datamart       ,
      nombre_afore          ,
      paterno_afore         ,
      materno_afore         ,
      tpo_movimiento        ,
      f_inicio_pension      ,
      f_resolucion          ,
      porcentaje_valuacion  ,
      semanas_cotizadas     ,
      f_carga_datamart      ,
      estado_sub_viv        ,
      aivs_viv97            ,
      cve_afore             ,
      cod_rechazo           
   INTO
      ret_transf_id_solicitud          ,
      ret_transf_id_derechohabiente    ,
      ret_transf_id_ret_matriz_derecho ,
      ret_transf_sec_pension           ,
      ret_transf_diag_registro         ,
      ret_transf_folio                 ,
      ret_transf_estado_solicitud      ,
      ret_transf_curp                  ,
      ret_transf_nombre_datamart       ,
      ret_transf_nombre_afore          ,
      ret_transf_paterno_afore         ,
      ret_transf_materno_afore         ,
      ret_transf_tpo_movimiento        ,
      ret_transf_f_inicio_pension      ,
      ret_transf_f_resolucion          ,
      ret_transf_porcentaje_valuacion  ,
      ret_transf_semanas_cotizadas     ,
      ret_transf_f_carga_datamart      ,
      ret_transf_estado_sub_viv        ,
      ret_transf_aivs_viv97            ,
      ret_transf_cve_afore             ,
      ret_transf_cod_rechazo           
   FROM
      ret_transferencia
   WHERE
      folio = p_folio
      
{  
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
               id_ret_matriz_derecho = ret_disposicion_id_ret_matriz_derecho;
}
         -- se asignan los datos al registro de rechazo de detalle
         LET ret_transf_rch_id_derechohabiente    = ret_transf_id_derechohabiente;
         LET ret_transf_rch_folio                 = ret_transf_folio;
         LET ret_transf_rch_id_ret_matriz_derecho = ret_transf_id_ret_matriz_derecho;
         LET ret_transf_rch_sec_pension           = ret_transf_sec_pension;
         LET ret_transf_rch_diag_registro         = ret_transf_diag_registro;
         LET ret_transf_rch_estado_solicitud      = 101; -- rechazo por inconsistencia de montos
         LET ret_transf_rch_curp                  = ret_transf_curp;
         LET ret_transf_rch_nombre_datamart       = ret_transf_nombre_datamart;
         LET ret_transf_rch_nombre_afore          = ret_transf_nombre_afore;
         LET ret_transf_rch_paterno_afore         = ret_transf_paterno_afore;
         LET ret_transf_rch_materno_afore         = ret_transf_materno_afore;
         LET ret_transf_rch_tpo_movimiento        = ret_transf_tpo_movimiento;
         LET ret_transf_rch_f_inicio_pension      = ret_transf_f_inicio_pension;
         LET ret_transf_rch_f_resolucion          = ret_transf_f_resolucion;
         LET ret_transf_rch_porcentaje_valuacion  = ret_transf_porcentaje_valuacion;
         LET ret_transf_rch_semanas_cotizadas     = ret_transf_semanas_cotizadas;
         LET ret_transf_rch_f_carga_datamart      = ret_transf_f_carga_datamart;
         LET ret_transf_rch_estado_sub_viv        = ret_transf_estado_sub_viv;
         LET ret_transf_rch_aivs_viv97            = ret_transf_aivs_viv97;
         LET ret_transf_rch_cve_afore             = ret_transf_cve_afore;
         LET ret_transf_rch_cod_rechazo           = ret_transf_cod_rechazo;
         LET ret_transf_rch_cod_rechazo_1         = 101;
         LET ret_transf_rch_cod_rechazo_2         = 0;
         LET ret_transf_rch_cod_rechazo_3         = 0;

         


         --se invoca SP que reversa la marca de la cuenta consultada
         EXECUTE PROCEDURE sp_reversa_marca ( ret_transf_id_derechohabiente,
                                              v_marca_entra  ,
                                              ret_transf_id_solicitud ,
                                              ret_transf_rch_folio
                                             );
                    
                    
         -- se inserta el registro de rechazo
         INSERT INTO ret_transferencia_rch(
            id_derechohabiente    ,
            folio                 ,
            id_ret_matriz_derecho ,
            sec_pension           ,
            diag_registro         ,
            estado_solicitud      ,
            curp                  ,
            nombre_datamart       ,
            nombre_afore          ,
            paterno_afore         ,
            materno_afore         ,
            tpo_movimiento        ,
            f_inicio_pension      ,
            f_resolucion          ,
            porcentaje_valuacion  ,
            semanas_cotizadas     ,
            f_carga_datamart      ,
            estado_sub_viv        ,
            aivs_viv97            ,
            cve_afore             ,
            cod_rechazo           ,
            cod_rechazo_1         ,
            cod_rechazo_2         ,
            cod_rechazo_3         
         )
         VALUES (
            ret_transf_rch_id_derechohabiente    ,
            ret_transf_rch_folio                 ,
            ret_transf_rch_id_ret_matriz_derecho ,
            ret_transf_rch_sec_pension           ,
            ret_transf_rch_diag_registro         ,
            ret_transf_rch_estado_solicitud      ,
            ret_transf_rch_curp                  ,
            ret_transf_rch_nombre_datamart       ,
            ret_transf_rch_nombre_afore          ,
            ret_transf_rch_paterno_afore         ,
            ret_transf_rch_materno_afore         ,
            ret_transf_rch_tpo_movimiento        ,
            ret_transf_rch_f_inicio_pension      ,
            ret_transf_rch_f_resolucion          ,
            ret_transf_rch_porcentaje_valuacion  ,
            ret_transf_rch_semanas_cotizadas     ,
            ret_transf_rch_f_carga_datamart      ,
            ret_transf_rch_estado_sub_viv        ,
            ret_transf_rch_aivs_viv97            ,
            ret_transf_rch_cve_afore             ,
            ret_transf_rch_cod_rechazo           ,
            ret_transf_rch_cod_rechazo_1         ,
            ret_transf_rch_cod_rechazo_2         ,
            ret_transf_rch_cod_rechazo_3         
         );
   END FOREACH;

   -- se borran los registros originales
   DELETE FROM ret_transferencia
   WHERE       folio = p_folio; 
 

   -- se borra el registro del archivo cargado para que permita cargarlo de nuevo
   DELETE FROM glo_ctr_archivo
    WHERE folio = p_folio;
   
   LET v_estatus_proceso = 0; -- rechazado
 
 
   RETURN v_estatus_proceso;
END FUNCTION;


