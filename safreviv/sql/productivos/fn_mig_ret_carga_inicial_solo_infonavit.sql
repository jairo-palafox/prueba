






CREATE FUNCTION "safreviv".fn_mig_ret_carga_inicial_solo_infonavit(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                                        p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                        p_proceso_cod SMALLINT)
RETURNING INTEGER, INTEGER, VARCHAR(250)


   -- campos de la tabla de encabezado de retiros por solo_infonavit de recursos (sin filler)

   --EFINE tmp_ret_cza_tpo_registro           CHAR(2)    ;
   --EFINE tmp_ret_cza_id_servicio            CHAR(2)    ;
   --EFINE tmp_ret_cza_tpo_entidad_origen     CHAR(2)    ;
   --EFINE tmp_ret_cza_cve_entidad_origen     CHAR(3)    ;
   --EFINE tmp_ret_cza_tpo_entidad_destino    CHAR(2)    ;
   --EFINE tmp_ret_cza_cve_entidad_destino    CHAR(3)    ;
   --EFINE tmp_ret_cza_f_operacion            DATE       ;
   --EFINE tmp_ret_cza_f_valor_transferencia  DATE       ;
   --EFINE tmp_ret_cza_val_participacion      DECIMAL(14);
   --EFINE tmp_ret_cza_resultado_operacion    CHAR(2)    ;
   --EFINE tmp_ret_cza_motivo_rech_1          CHAR(3)    ;
   --EFINE tmp_ret_cza_motivo_rech_2          CHAR(3)    ;
   --EFINE tmp_ret_cza_motivo_rech_3          CHAR(3)    ;
 
   -- campos de la tabla de detalle de retiros por solo_infonavit de recursos (sin filler)
   
   DEFINE tmp_ret_det_consecutivo                   DECIMAL(4,0)   ;
   DEFINE tmp_ret_det_nss                           CHAR(11)       ;
   DEFINE tmp_ret_det_causal_retiro                 DECIMAL(2,0)   ;
   DEFINE tmp_ret_det_importe_viv97                 DECIMAL(10,0)  ;
   DEFINE tmp_ret_det_sar97_dap                     CHAR(25)       ;
   DEFINE tmp_ret_det_beneficiario                  CHAR(60)       ;
   DEFINE tmp_ret_det_f_valuacion                   DATE           ;
   DEFINE tmp_ret_det_resultado_operacion           CHAR(2)        ;
   DEFINE tmp_ret_det_motivo_rech1                  CHAR(3)        ;
   DEFINE tmp_ret_det_motivo_rech2                  CHAR(3)        ;


-- tablas destino

-- encabezado de la tabla historica/integrada de retiros por solo_infonavit de recursos
   --DEFINE ret_cza_solo_infonavit_folio                 DECIMAL(9,0)           ;
   --DEFINE ret_cza_solo_infonavit_nombre_archivo        CHAR(20)               ;
   --DEFINE ret_cza_solo_infonavit_f_operacion_procesar  DATE                   ;
   --DEFINE ret_cza_solo_infonavit_f_carga               DATE                   ; -- esperando cambio de nombre antes: f_carga_infonavit
   --DEFINE ret_cza_solo_infonavit_h_carga               DATETIME HOUR TO SECOND;
   --DEFINE ret_cza_solo_infonavit_f_valor_transferencia DATE                   ;
   --DEFINE ret_cza_solo_infonavit_precio_fondo          DECIMAL(14,6)          ;
   -- DEFINE ret_cza_solo_infonavit_total_registros       INTEGER                ; -- nuevo
   -- DEFINE ret_cza_solo_infonavit_total_importe         DECIMAL(18,6)          ; -- nuevo
   --DEFINE ret_cza_solo_infonavit_usuario               CHAR(20)               ;

-- detalle de la tabla historica/integrada de retiros por solo_infonavit de recursos
-- ret_solo_infonavit
   
   DEFINE ret_solo_infonavit_id_solicitud         DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_id_derechohabiente   DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_f_solicitud          DATE                    ;
   DEFINE ret_solo_infonavit_estado_solicitud     SMALLINT                ;
   DEFINE ret_solo_infonavit_folio                DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_aivs_viv97           DECIMAL(18,6)           ;
   DEFINE ret_solo_infonavit_importe_viv97        DECIMAL(20,2)           ;
   DEFINE ret_solo_infonavit_clabe                CHAR(18)                ;
   DEFINE ret_solo_infonavit_banco                SMALLINT                ;
   DEFINE ret_solo_infonavit_entidad_federativa   SMALLINT                ;
   DEFINE ret_solo_infonavit_causal_retiro        SMALLINT                ;
   DEFINE ret_solo_infonavit_f_valuacion          DATE                    ;
   DEFINE ret_solo_infonavit_f_captura            DATE                    ;
   DEFINE ret_solo_infonavit_h_captura            DATETIME HOUR TO SECOND ;
   DEFINE ret_solo_infonavit_usuario              CHAR(20)                ;
   DEFINE ret_solo_infonavit_cod_rechazo          SMALLINT                ;

-- =============================================================================
   -- rechazo de encabezado
   -- ret_cza_solo_infonavit_rch
   --DEFINE ret_cza_solo_infonavit_rch_folio                 DECIMAL(9,0)           ;
   --DEFINE ret_cza_solo_infonavit_rch_nombre_archivo        CHAR(20)               ;
   --DEFINE ret_cza_solo_infonavit_rch_f_operacion_procesar  DATE                   ;
   --DEFINE ret_cza_solo_infonavit_rch_f_carga               DATE                   ;
   --DEFINE ret_cza_solo_infonavit_rch_h_carga               DATETIME HOUR TO SECOND;
   --DEFINE ret_cza_solo_infonavit_rch_f_valor_transferencia DATE                   ;
   --DEFINE ret_cza_solo_infonavit_rch_precio_fondo          DECIMAL(14,6)          ;
   --DEFINE ret_cza_solo_infonavit_rch_total_registros       INTEGER                ;
   --DEFINE ret_cza_solo_infonavit_rch_total_importe         DECIMAL(18,6)          ;
   --DEFINE ret_cza_solo_infonavit_rch_usuario               CHAR(20)               ;
   --DEFINE ret_cza_solo_infonavit_rch_resultado_operacion   SMALLINT               ;
   --DEFINE ret_cza_solo_infonavit_rch_cod_rechazo_1         SMALLINT               ;
   --DEFINE ret_cza_solo_infonavit_rch_cod_rechazo_2         SMALLINT               ;
   --DEFINE ret_cza_solo_infonavit_rch_cod_rechazo_3         SMALLINT               ;     
-- =============================================================================
   -- rechazo de detalle
   -- ret_solo_infonavit_rch
   DEFINE ret_solo_infonavit_rch_id_solicitud         DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_rch_id_derechohabiente   DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_rch_f_solicitud          DATE                    ;
   DEFINE ret_solo_infonavit_rch_estado_solicitud     SMALLINT                ;
   DEFINE ret_solo_infonavit_rch_folio                DECIMAL(9,0)            ;
   DEFINE ret_solo_infonavit_rch_aivs_viv97           DECIMAL(18,6)           ;
   DEFINE ret_solo_infonavit_rch_importe_viv97        DECIMAL(20,2)           ;
   DEFINE ret_solo_infonavit_rch_clabe                CHAR(18)                ;
   DEFINE ret_solo_infonavit_rch_banco                SMALLINT                ;
   DEFINE ret_solo_infonavit_rch_entidad_federativa   SMALLINT                ;
   DEFINE ret_solo_infonavit_rch_causal_retiro        SMALLINT                ;
   DEFINE ret_solo_infonavit_rch_f_valuacion          DATE                    ;
   DEFINE ret_solo_infonavit_rch_f_captura            DATE                    ;
   DEFINE ret_solo_infonavit_rch_h_captura            DATETIME HOUR TO SECOND ;
   DEFINE ret_solo_infonavit_rch_usuario              CHAR(20)                ;
   DEFINE ret_solo_infonavit_rch_cod_rechazo          SMALLINT                ;
   
   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                 DECIMAL(9,0);
   DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                        DECIMAL(14,6);
   DEFINE v_pesos_aiv97                        DECIMAL(14,6);
   DEFINE v_pesos_aiv92                        DECIMAL(14,6);
   
   -- para rechazos
   DEFINE v_b_rechazo_encabezado               SMALLINT;
   DEFINE v_b_rechazo_detalle                  SMALLINT;
   DEFINE v_validar_3_primeros_campos          VARCHAR(6); -- se concatenan los 3 primeros campos para validar
   DEFINE v_afore_cod                          SMALLINT; -- clave de afore
   -- id matriz derecho
   DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS
 
 DEFINE v_sumario_importe_total                 DECIMAL(22,6);
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
 DEFINE v_error_det_lote_invalido                 SMALLINT; -- el lote quedo invalido porque hubo rechazos
 
 -- estatus del proceso
 DEFINE v_estatus_proceso                         SMALLINT;
 
 -- para marcar las cuentas
 DEFINE v_i_estado_marca                          INTEGER;
 DEFINE v_marca_solo_infonavit                       INTEGER; -- 805 de acuerdo a catalogo

 -- Control de Excepciones
 DEFINE v_si_resultado                            SMALLINT;
 DEFINE sql_err                                   INTEGER;
 DEFINE isam_err                                  INTEGER;
 DEFINE err_txt                                   VARCHAR(250);
 DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_carga_inicial_solo_infonavit.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

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
   LET v_error_det_lote_invalido                 = 9;


   -- se inician las variables para marca
   LET v_marca_solo_infonavit = 1513; -- marca para solo_infonavit de recursos
   LET v_i_estado_marca    = 0;
   
   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   --trace "create tmp_codigos_rechazo";
   DROP TABLE IF EXISTS tmp_codigos_rechazo ;
   CREATE TEMP TABLE tmp_codigos_rechazo (
                                          id_codigo       SMALLINT,
                                          codigo_rechazo SMALLINT
                                          );
   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
   
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;

--trace "create dantes tmp_ret_det_solo_inf";

   -- se obtienen los datos del detalle
   FOREACH
     SELECT
        consecutivo         ,
        nss                 ,
        causal_retiro       ,
        importe_viv97       ,
        sar97_dap           ,
        beneficiario        ,
        f_valuacion         ,
        resultado_operacion ,
        motivo_rech1        ,
        motivo_rech2        
     
     INTO
        tmp_ret_det_consecutivo         ,
        tmp_ret_det_nss                 ,
        tmp_ret_det_causal_retiro       ,
        tmp_ret_det_importe_viv97       ,
        tmp_ret_det_sar97_dap           ,
        tmp_ret_det_beneficiario        ,
        tmp_ret_det_f_valuacion         ,
        tmp_ret_det_resultado_operacion ,
        tmp_ret_det_motivo_rech1        ,
        tmp_ret_det_motivo_rech2         
        
     FROM
        safre_mig:tmp_ret_det_solo_inf
        
        --trace "create despes  tmp_ret_det_solo_inf";
        -- se asume que no hay rechazos en el detalle del archivo
        LET v_b_rechazo_detalle    = 0;
     
        -- se obtiene el id_derechohabiente
        SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM
          safre_viv:afi_derechohabiente
        WHERE
           nss = tmp_ret_det_nss;    
      
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
      
      --TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- REGISTROS ACEPTADOS    
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      
      --TRACE("Asignando datos a registro de detalle solo_infonavit");     
      
      LET ret_solo_infonavit_id_solicitud         = v_id_solicitud                      ;
      LET ret_solo_infonavit_id_derechohabiente   = v_id_derechohabiente                ;
      LET ret_solo_infonavit_f_solicitud          = TODAY                               ;
      LET ret_solo_infonavit_estado_solicitud     = 10                                  ; -- CAPTURADA [integrada](ret_estado_solicitud)          
      LET ret_solo_infonavit_folio                = p_folio                             ;
      LET ret_solo_infonavit_aivs_viv97           = 1                                   ;
      LET ret_solo_infonavit_importe_viv97        = tmp_ret_det_importe_viv97 / 100     ;
      LET ret_solo_infonavit_clabe                = 123                                 ;
      LET ret_solo_infonavit_banco                = 0                                   ;
      LET ret_solo_infonavit_entidad_federativa   = 0                                   ;
      LET ret_solo_infonavit_causal_retiro        = tmp_ret_det_causal_retiro           ;
      LET ret_solo_infonavit_f_valuacion          = tmp_ret_det_f_valuacion             ;
      LET ret_solo_infonavit_f_captura            = TODAY                               ;
      LET ret_solo_infonavit_h_captura            = CURRENT HOUR TO SECOND              ;
      LET ret_solo_infonavit_usuario              = USER                                ;
      LET ret_solo_infonavit_cod_rechazo          = 0                                   ;

      
      --TRACE("multiplicando AIVs por valor del fondo");
      LET v_pesos_aiv97 = ret_solo_infonavit_aivs_viv97 * 1; -- falta el valor del fondo
      
      -- se inserta en la tabla historia de detalle de retiro por solo_infonavit de recursos
      INSERT INTO safre_viv:ret_solo_infonavit(
                  id_solicitud        ,
                  id_derechohabiente  ,
                  f_solicitud         ,
                  estado_solicitud    ,
                  folio               ,
                  aivs_viv97          ,
                  importe_viv97       ,
                  clabe               ,
                  banco               ,
                  entidad_federativa  ,
                  causal_retiro       ,
                  f_valuacion         ,
                  f_captura           ,
                  h_captura           ,
                  usuario             ,
                  cod_rechazo         
             )
      VALUES (
                 safre_viv:seq_ret_solicitud.NEXTVAL             ,
                 ret_solo_infonavit_id_derechohabiente  ,
                 ret_solo_infonavit_f_solicitud         ,
                 ret_solo_infonavit_estado_solicitud    ,
                 ret_solo_infonavit_folio               ,
                 ret_solo_infonavit_aivs_viv97          ,
                 ret_solo_infonavit_importe_viv97       ,
                 ret_solo_infonavit_clabe               ,
                 ret_solo_infonavit_banco               ,
                 ret_solo_infonavit_entidad_federativa  ,
                 ret_solo_infonavit_causal_retiro       ,
                 ret_solo_infonavit_f_valuacion         ,
                 ret_solo_infonavit_f_captura           ,
                 ret_solo_infonavit_h_captura           ,
                 ret_solo_infonavit_usuario             ,
                 ret_solo_infonavit_cod_rechazo         
            );
      
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

      -- se marca la cuenta

   END FOREACH;
 
   UPDATE STATISTICS FOR TABLE safre_viv:ret_solo_infonavit;

 
    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE safre_mig:glo_ctr_archivo
       SET 
        folio = p_folio,
       estado = 2 -- integrado
     WHERE proceso_cod    = p_proceso_cod
       AND opera_cod      = 1 -- archivo cargado
       AND estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE safre_mig:bat_ctr_operacion 
      SET folio       = p_folio
    WHERE proceso_cod = p_proceso_cod 
      AND opera_cod   = 2
      AND pid         = p_pid;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


