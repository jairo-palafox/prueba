






CREATE FUNCTION "safreviv".fn_mig_ret_carga_inicial_disposicion(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                          p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                          p_proceso_cod SMALLINT, p_fecha_valor DATE)
RETURNING INTEGER, INTEGER, VARCHAR(250)


   -- campos de la tabla de encabezado de retiros por disposicion de recursos (sin filler)

   DEFINE tmp_ret_cza_tpo_registro           CHAR(2)    ;
   DEFINE tmp_ret_cza_id_servicio            CHAR(2)    ;
   DEFINE tmp_ret_cza_tpo_entidad_origen     CHAR(2)    ;
   DEFINE tmp_ret_cza_cve_entidad_origen     CHAR(3)    ;
   DEFINE tmp_ret_cza_tpo_entidad_destino    CHAR(2)    ;
   DEFINE tmp_ret_cza_cve_entidad_destino    CHAR(3)    ;
   DEFINE tmp_ret_cza_f_operacion            DATE       ;
   DEFINE tmp_ret_cza_f_valor_transferencia  DATE       ;
   DEFINE tmp_ret_cza_val_participacion      DECIMAL(14);
   DEFINE tmp_ret_cza_resultado_operacion    CHAR(2)    ;
   DEFINE tmp_ret_cza_motivo_rech_1          CHAR(3)    ;
   DEFINE tmp_ret_cza_motivo_rech_2          CHAR(3)    ;
   DEFINE tmp_ret_cza_motivo_rech_3          CHAR(3)    ;
 
   -- campos de la tabla de detalle de retiros por disposicion de recursos (sin filler)
   DEFINE tmp_ret_det_tpo_registro          CHAR(2)      ;
   DEFINE tmp_ret_det_id_servicio           CHAR(2)      ;
   DEFINE tmp_ret_det_id_operacion          CHAR(2)      ;
   DEFINE tmp_ret_det_nss                   CHAR(11)     ;
   DEFINE tmp_ret_det_curp                  CHAR(18)     ;
   DEFINE tmp_ret_det_nombre_afore          CHAR(40)     ;
   DEFINE tmp_ret_det_paterno_afore         CHAR(40)     ;
   DEFINE tmp_ret_det_materno_afore         CHAR(40)     ;
   DEFINE tmp_ret_det_sec_pension           CHAR(2)      ;
   DEFINE tmp_ret_det_tpo_retiro            CHAR(1)      ;
   DEFINE tmp_ret_det_regimen               CHAR(2)      ;
   DEFINE tmp_ret_det_tpo_seguro            CHAR(2)      ;
   DEFINE tmp_ret_det_tpo_pension           CHAR(2)      ;
   DEFINE tmp_ret_det_tpo_prestacion        CHAR(2)      ;
   DEFINE tmp_ret_det_f_inicio_pension      DATE         ;
   DEFINE tmp_ret_det_f_emision_resol       DATE         ;
   DEFINE tmp_ret_det_porc_valuacion        DECIMAL(5)   ;
   DEFINE tmp_ret_det_sem_cotizadas         DECIMAL(4)   ;
   DEFINE tmp_ret_det_f_solicitud_trab      DATE         ;
   DEFINE tmp_ret_det_cve_doc_probatorio    CHAR(1)      ;
   DEFINE tmp_ret_det_f_nacimiento          DATE         ;
   DEFINE tmp_ret_det_aseguradora           CHAR(3)      ;
   DEFINE tmp_ret_det_actuario_autor        CHAR(7)      ;
   DEFINE tmp_ret_det_num_reg_ppp           CHAR(8)      ;
   DEFINE tmp_ret_det_periodo_pago          DECIMAL(6)   ;
   DEFINE tmp_ret_det_acciones_ret97        DECIMAL(14)  ;
   DEFINE tmp_ret_det_acciones_cv           DECIMAL(14)  ;
   DEFINE tmp_ret_det_acciones_cuotsol      DECIMAL(14)  ;
   DEFINE tmp_ret_det_acciones_ret92        DECIMAL(14)  ;
   DEFINE tmp_ret_det_aiv97                 DECIMAL(14)  ;
   DEFINE tmp_ret_det_aiv92                 DECIMAL(14)  ;
   DEFINE tmp_ret_det_consec_trab           DECIMAL(11)  ;
   DEFINE tmp_ret_det_fondo_subcta_viv72    DECIMAL(14)  ;
   DEFINE tmp_ret_det_diagnostico_reg       CHAR(3)      ;
   DEFINE tmp_ret_det_estatus_subcta        CHAR(1)      ;
   DEFINE tmp_ret_det_result_operacion      CHAR(2)      ;
   DEFINE tmp_ret_det_cve_afore             DECIMAL(3)   ;
   DEFINE tmp_ret_det_motivo_rech1          CHAR(3)      ;
   DEFINE tmp_ret_det_motivo_rech2          CHAR(3)      ;
   DEFINE tmp_ret_det_f_carga_infonavit     DATE         ;
   DEFINE tmp_ret_det_f_transferencia       DATE         ;
   DEFINE tmp_ret_det_valor_participacion   DECIMAL(14,0);
   DEFINE tmp_ret_det_monto_pesos           DECIMAL(10,0);


-- tablas destino

-- encabezado de la tabla historica/integrada de retiros por disposicion de recursos
   DEFINE ret_cza_disposicion_folio                 DECIMAL(9,0)           ;
   DEFINE ret_cza_disposicion_nombre_archivo        CHAR(20)               ;
   DEFINE ret_cza_disposicion_f_operacion_procesar  DATE                   ;
   DEFINE ret_cza_disposicion_f_carga               DATE                   ; -- esperando cambio de nombre antes: f_carga_infonavit
   DEFINE ret_cza_disposicion_h_carga               DATETIME HOUR TO SECOND;
   DEFINE ret_cza_disposicion_f_valor_transferencia DATE                   ;
   DEFINE ret_cza_disposicion_precio_fondo          DECIMAL(14,6)          ;
	 DEFINE ret_cza_disposicion_total_registros       INTEGER                ; -- nuevo
	 DEFINE ret_cza_disposicion_total_importe         DECIMAL(18,6)          ; -- nuevo
   DEFINE ret_cza_disposicion_usuario               CHAR(20)               ;

-- detalle de la tabla historica/integrada de retiros por disposicion de recursos
-- ret_disposicion
   DEFINE ret_disposicion_id_solicitud           DECIMAL(9,0) ;
   DEFINE ret_disposicion_id_derechohabiente     DECIMAL(9,0) ;
   DEFINE ret_disposicion_f_solicitud            DATE         ;
   DEFINE ret_disposicion_id_ret_matriz_derecho  SMALLINT     ;
   DEFINE ret_disposicion_sec_pension            CHAR(2)      ;
   DEFINE ret_disposicion_diag_registro          CHAR(3)      ;
   DEFINE ret_disposicion_folio                  DECIMAL(9,0) ;
   DEFINE ret_disposicion_curp                   CHAR(18)     ;
   DEFINE ret_disposicion_nombre_afore           CHAR(40)     ;
   DEFINE ret_disposicion_paterno_afore          CHAR(40)     ;
   DEFINE ret_disposicion_materno_afore          CHAR(40)     ;
   DEFINE ret_disposicion_f_inicio_pension       DATE         ;
   DEFINE ret_disposicion_f_resolucion           DATE         ;
   DEFINE ret_disposicion_porcentaje_valuacion   DECIMAL(5,2) ;
   DEFINE ret_disposicion_semanas_cotizadas      INTEGER      ;
   DEFINE ret_disposicion_cve_doc_probatorio     SMALLINT     ;
   DEFINE ret_disposicion_f_nacimiento           DATE         ;
   DEFINE ret_disposicion_aseguradora            CHAR(3)      ;
   DEFINE ret_disposicion_actuario               CHAR(7)      ;
   DEFINE ret_disposicion_num_plan_privado       CHAR(8)      ;
   DEFINE ret_disposicion_periodo_primer_pago    INTEGER      ;
   DEFINE ret_disposicion_aivs_ret97             DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_cv                DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_cs                DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_sar92             DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_viv97             DECIMAL(14,6);
   DEFINE ret_disposicion_aivs_viv92             DECIMAL(14,6);
   DEFINE ret_disposicion_consec_trabajador      DECIMAL(11,0);
   DEFINE ret_disposicion_importe_viv72          DECIMAL(14,2);
   DEFINE ret_disposicion_estado_sub_viv         SMALLINT     ;
   DEFINE ret_disposicion_cve_afore              SMALLINT     ;
   DEFINE ret_disposicion_cod_rechazo            SMALLINT     ;
   DEFINE ret_disposicion_estado_solicitud       SMALLINT     ;

-- =============================================================================
   -- rechazo de encabezado
   -- ret_cza_disposicion_rch
   DEFINE ret_cza_disposicion_rch_folio                 DECIMAL(9,0)           ;
   DEFINE ret_cza_disposicion_rch_nombre_archivo        CHAR(20)               ;
   DEFINE ret_cza_disposicion_rch_f_operacion_procesar  DATE                   ;
   DEFINE ret_cza_disposicion_rch_f_carga               DATE                   ;
   DEFINE ret_cza_disposicion_rch_h_carga               DATETIME HOUR TO SECOND;
   DEFINE ret_cza_disposicion_rch_f_valor_transferencia DATE                   ;
   DEFINE ret_cza_disposicion_rch_precio_fondo          DECIMAL(14,6)          ;
   DEFINE ret_cza_disposicion_rch_total_registros       INTEGER                ;
   DEFINE ret_cza_disposicion_rch_total_importe         DECIMAL(18,6)          ;
   DEFINE ret_cza_disposicion_rch_usuario               CHAR(20)               ;
   DEFINE ret_cza_disposicion_rch_resultado_operacion   SMALLINT               ;
   DEFINE ret_cza_disposicion_rch_cod_rechazo_1         SMALLINT               ;
   DEFINE ret_cza_disposicion_rch_cod_rechazo_2         SMALLINT               ;
   DEFINE ret_cza_disposicion_rch_cod_rechazo_3         SMALLINT               ;
-- =============================================================================
   -- rechazo de detalle
   -- ret_disposicion_rch
   DEFINE ret_dispos_rch_id_solicitud         decimal(9,0) ;
   DEFINE ret_dispos_rch_id_derechohabiente   decimal(9,0) ;
   DEFINE ret_dispos_rch_f_solicitud          date         ;
   DEFINE ret_dispos_rch_folio                decimal(9,0) ;
   DEFINE ret_dispos_rch_nss                  char(11)     ;
   DEFINE ret_dispos_rch_curp                 char(18)     ;
   DEFINE ret_dispos_rch_nombre_afore         char(40)     ;
   DEFINE ret_dispos_rch_paterno_afore        char(40)     ;
   DEFINE ret_dispos_rch_materno_afore        char(40)     ;
   DEFINE ret_dispos_rch_sec_pension          char(2)      ;
   DEFINE ret_dispos_rch_tipo_retiro          char(1)      ;
   DEFINE ret_dispos_rch_regimen              char(2)      ;
   DEFINE ret_dispos_rch_tpo_seguro           char(2)      ;
   DEFINE ret_dispos_rch_tpo_pension          char(2)      ;
   DEFINE ret_dispos_rch_tpo_prestacion       char(2)      ;
   DEFINE ret_dispos_rch_f_inicio_pension     date         ;
   DEFINE ret_dispos_rch_f_resolucion         date         ;
   DEFINE ret_dispos_rch_porcentaje_valuacion decimal(5,2) ;
   DEFINE ret_dispos_rch_semanas_cotizadas    integer      ;
   DEFINE ret_dispos_rch_cve_doc_probatorio   smallint     ;
   DEFINE ret_dispos_rch_f_nacimiento         date         ;
   DEFINE ret_dispos_rch_aseguradora          char(3)      ;
   DEFINE ret_dispos_rch_actuario             char(7)      ;
   DEFINE ret_dispos_rch_num_plan_privado     char(8)      ;
   DEFINE ret_dispos_rch_periodo_primer_pago  integer      ;
   DEFINE ret_dispos_rch_aivs_ret_97          decimal(14,6);
   DEFINE ret_dispos_rch_aivs_cv              decimal(14,6);
   DEFINE ret_dispos_rch_aivs_cs              decimal(14,6);
   DEFINE ret_dispos_rch_aivs_sar92           decimal(14,6);
   DEFINE ret_dispos_rch_aivs_viv97           decimal(14,6);
   DEFINE ret_dispos_rch_aivs_viv92           decimal(14,6);
   DEFINE ret_dispos_rch_consec_trabajador    decimal(11,0);
   DEFINE ret_dispos_rch_importe_viv72        decimal(14,2);
   DEFINE ret_dispos_rch_diag_registro        char(3)      ;
   DEFINE ret_dispos_rch_estado_sub_viv       smallint     ;
   DEFINE ret_dispos_rch_cve_afore            smallint     ;
   DEFINE ret_dispos_rch_estado_solicitud     smallint     ;
   DEFINE ret_dispos_rch_resultado_operacion  smallint     ;
   DEFINE ret_dispos_rch_cod_rechazo_1        smallint     ;
   DEFINE ret_dispos_rch_cod_rechazo_2        smallint     ;
   DEFINE ret_dispos_rch_cod_rechazo_3        smallint     ;

   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                 DECIMAL(9,0);
   DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                        DECIMAL(14,6);
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
 DEFINE v_reg_cza_insertados                    INTEGER; -- total de registros de encabezado insertados
 DEFINE v_reg_cza_rechazados                    INTEGER; -- total de registros de encabezado rechazados
 DEFINE v_reg_det_insertados                    INTEGER; -- total de registros de detalle insertados
 DEFINE v_reg_det_rechazados                    INTEGER; -- total de registros de detalle rechazados
 
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
 
 -- para transformar fechas
 DEFINE v_fecha                                   VARCHAR(10);
 
 -- estatus del proceso
 DEFINE v_estatus_proceso                         SMALLINT;
 
 -- para marcar las cuentas
 DEFINE v_i_estado_marca                          INTEGER;
 DEFINE v_marca_disposicion                       INTEGER; -- 805 de acuerdo a catalogo

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


   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_carga_inicial_disposicion.txt";

    -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE safre_mig:glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2
   WHERE  nombre_archivo = p_nombre_archivo
   AND    proceso_cod = p_proceso_cod
   AND    opera_cod   = 1;
   
   -- Agregar folio a operacion de integracion
   UPDATE safre_mig:bat_ctr_operacion 
      SET folio       = P_folio
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 2
      AND pid         = p_pid;


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
   LET v_marca_disposicion = 805; -- marca para disposicion de recursos
   LET v_i_estado_marca    = 0;
   
   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
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


   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      tpo_registro        ,
      id_servicio         ,
      id_operacion        ,
      nss                 ,
      curp                ,
      nombre_afore        ,
      paterno_afore       ,
      materno_afore       ,
      sec_pension         ,
      tpo_retiro          ,
      regimen             ,
      tpo_seguro          ,
      tpo_pension         ,
      tpo_prestacion      ,
      f_inicio_pension    ,
      f_emision_resol     ,
      porc_valuacion      ,
      sem_cotizadas       ,
      f_solicitud_trab    ,
      cve_doc_probatorio  ,
      f_nacimiento        ,
      aseguradora         ,
      actuario_autor      ,
      num_reg_ppp         ,
      periodo_pago        ,
      acciones_ret97      ,
      acciones_cv         ,
      acciones_cuotsol    ,
      acciones_ret92      ,
      aiv97               ,
      aiv92               ,
      consec_trab         ,
      fondo_subcta_viv72  ,
      diagnostico_reg     ,
      estatus_subcta      ,
      result_operacion    ,
      cve_afore           ,
      motivo_rech1        ,
      motivo_rech2        ,
      f_carga_infonavit   ,
      f_transferencia     ,
      valor_participacion ,
      monto_pesos         

   INTO
      tmp_ret_det_tpo_registro        ,
      tmp_ret_det_id_servicio         ,
      tmp_ret_det_id_operacion        ,
      tmp_ret_det_nss                 ,
      tmp_ret_det_curp                ,
      tmp_ret_det_nombre_afore        ,
      tmp_ret_det_paterno_afore       ,
      tmp_ret_det_materno_afore       ,
      tmp_ret_det_sec_pension         ,
      tmp_ret_det_tpo_retiro          ,
      tmp_ret_det_regimen             ,
      tmp_ret_det_tpo_seguro          ,
      tmp_ret_det_tpo_pension         ,
      tmp_ret_det_tpo_prestacion      ,
      tmp_ret_det_f_inicio_pension    ,
      tmp_ret_det_f_emision_resol     ,
      tmp_ret_det_porc_valuacion      ,
      tmp_ret_det_sem_cotizadas       ,
      tmp_ret_det_f_solicitud_trab    ,
      tmp_ret_det_cve_doc_probatorio  ,
      tmp_ret_det_f_nacimiento        ,
      tmp_ret_det_aseguradora         ,
      tmp_ret_det_actuario_autor      ,
      tmp_ret_det_num_reg_ppp         ,
      tmp_ret_det_periodo_pago        ,
      tmp_ret_det_acciones_ret97      ,
      tmp_ret_det_acciones_cv         ,
      tmp_ret_det_acciones_cuotsol    ,
      tmp_ret_det_acciones_ret92      ,
      tmp_ret_det_aiv97               ,
      tmp_ret_det_aiv92               ,
      tmp_ret_det_consec_trab         ,
      tmp_ret_det_fondo_subcta_viv72  ,
      tmp_ret_det_diagnostico_reg     ,
      tmp_ret_det_estatus_subcta      ,
      tmp_ret_det_result_operacion    ,
      tmp_ret_det_cve_afore           ,
      tmp_ret_det_motivo_rech1        ,
      tmp_ret_det_motivo_rech2        ,
      tmp_ret_det_f_carga_infonavit   ,
      tmp_ret_det_f_transferencia     ,
      tmp_ret_det_valor_participacion ,
      tmp_ret_det_monto_pesos        
   FROM
      safre_mig:tmp_ret_det_disposicion
   WHERE f_transferencia = p_fecha_valor -- para la fecha valor en turno
      
      -- se obtiene el valor de la participacion O precio del fondo
      LET ret_cza_disposicion_precio_fondo = tmp_ret_det_valor_participacion / 1000000;
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle    = 0;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_ret_det_nss;    
      
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
      
      ----TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      ----TRACE "verificando si hay derechohabiente";

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


--TRACE "verificando los primeros tres campos";

      LET v_validar_3_primeros_campos = tmp_ret_det_tpo_registro || tmp_ret_det_id_servicio || tmp_ret_det_id_operacion;
      
      -- si la concatenacion no es igual a la esperada, entonces algun campo es incorrecto
      IF ( v_validar_3_primeros_campos <> "030408" ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
                  
                  
         -- 1	Tipo de Registro	X	02	00	001	-	002	03 Detalle Transacciones
         IF ( tmp_ret_det_tpo_registro <> "03" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 2	Identificador de Servicio	X	02	00	003	-	004	04 Retiros
         IF ( tmp_ret_det_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 3	Identificador de Operación	X	02	00	005	-	006	08 Disposición Procesar-Infonavit
         IF ( tmp_ret_det_id_operacion <> "08" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_operacion_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
      END IF
       
      -- se obtiene el id_matriz_derecho para la combinacion del registro
      SELECT id_ret_matriz_derecho
      INTO   v_id_ret_matriz_derecho
      FROM   ret_matriz_derecho
      WHERE  tpo_retiro           = tmp_ret_det_tpo_retiro
      AND    regimen              = tmp_ret_det_regimen
      AND    tpo_seguro           = tmp_ret_det_tpo_seguro
      AND    tpo_pension          = tmp_ret_det_tpo_pension
      AND    tpo_prestacion       = tmp_ret_det_tpo_prestacion;
         
      -- si no se encontro, se rechaza
      IF ( v_id_ret_matriz_derecho IS NULL ) THEN
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_matriz_derecho_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

      END IF
      
      -- la secuencia de pension debe existir y ser numerica
      -- 22 Agosto 2012. Solo se revisa para el tipo de retiro E
      IF ( tmp_ret_det_tpo_retiro = "E" ) THEN
         IF ( tmp_ret_det_sec_pension IS NULL OR (tmp_ret_det_sec_pension < "00" OR tmp_ret_det_sec_pension > "99")) THEN
            -- se rechaza
            LET v_b_rechazo_detalle    = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_sec_pension_invalido);
         
            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         
         END IF
      END IF

      -- la clave de afore tiene que estar en catalogo
      SELECT afore_cod
      INTO   v_afore_cod
      FROM   cat_afore
      WHERE  afore_cod = tmp_ret_det_cve_afore;
         
      IF ( v_afore_cod IS NULL ) THEN
         -- se rechaza
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_afore_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

      END IF

    
      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN
      
         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;

      
         -- se leen los tres primeros errores
         FOREACH
         SELECT FIRST 3
            id_codigo,
            codigo_rechazo
         INTO 
            v_indice_codigos_rechazo, v_codigos_rechazo
         FROM
            tmp_codigos_rechazo
         ORDER BY
            id_codigo
            
            IF ( v_indice_codigos_rechazo = 1 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_1 = v_codigos_rechazo;
            END IF

            IF ( v_indice_codigos_rechazo = 2 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_2 = v_codigos_rechazo;
            END IF

            IF ( v_indice_codigos_rechazo = 3 ) THEN
               -- se asignan los primeros 3 codigos de rechazo
               LET v_motivo_rechazo_3 = v_codigos_rechazo;
               EXIT FOREACH;
            END IF 
         END FOREACH;
         
         --TRACE "asignando datos al registro rechazado";
         
         -- se asignan los datos al registro de rechazo de detalle
         LET ret_dispos_rch_id_solicitud          = v_id_solicitud; -- decimal(9,0) ;
         LET ret_dispos_rch_id_derechohabiente    = v_id_derechohabiente; -- decimal(9,0) ;
         
         --LET v_fecha                              = tmp_ret_det_f_solicitud_trab[5,6] || "/" || tmp_ret_det_f_solicitud_trab[7,8] || "/" || tmp_ret_det_f_solicitud_trab[1,4];
         --LET ret_dispos_rch_f_solicitud           = DATE(v_fecha); -- date         ;
         LET ret_dispos_rch_f_solicitud           = tmp_ret_det_f_solicitud_trab;
         
         LET ret_dispos_rch_folio                 = p_folio; -- decimal(9,0) ;
         LET ret_dispos_rch_nss                   = tmp_ret_det_nss; -- char(11)     ;
         LET ret_dispos_rch_curp                  = tmp_ret_det_curp; -- char(18)     ;
         LET ret_dispos_rch_nombre_afore          = tmp_ret_det_nombre_afore; -- char(40)     ;
         LET ret_dispos_rch_paterno_afore         = tmp_ret_det_paterno_afore; -- char(40)     ;
         LET ret_dispos_rch_materno_afore         = tmp_ret_det_materno_afore; -- char(40)     ;
         LET ret_dispos_rch_sec_pension           = tmp_ret_det_sec_pension; -- char(2)      ;
         LET ret_dispos_rch_tipo_retiro           = tmp_ret_det_tpo_retiro; -- char(1)      ;
         LET ret_dispos_rch_regimen               = tmp_ret_det_regimen; -- char(2)      ;
         LET ret_dispos_rch_tpo_seguro            = tmp_ret_det_tpo_seguro; -- char(2)      ;
         LET ret_dispos_rch_tpo_pension           = tmp_ret_det_tpo_pension; -- char(2)      ;
         LET ret_dispos_rch_tpo_prestacion        = tmp_ret_det_tpo_prestacion; -- char(2)      ;
         
         --LET v_fecha                              = tmp_ret_det_f_inicio_pension[5,6] || "/" || tmp_ret_det_f_inicio_pension[7,8] || "/" || tmp_ret_det_f_inicio_pension[1,4];
         --LET ret_dispos_rch_f_inicio_pension      = DATE(v_fecha); -- date         ;
         LET ret_dispos_rch_f_inicio_pension      = tmp_ret_det_f_inicio_pension;
         
         --LET v_fecha                              = tmp_ret_det_f_emision_resol[5,6] || "/" || tmp_ret_det_f_emision_resol[7,8] || "/" || tmp_ret_det_f_emision_resol[1,4];
         --LET ret_dispos_rch_f_resolucion          = DATE(v_fecha); -- date         ;
         LET ret_dispos_rch_f_resolucion          = tmp_ret_det_f_emision_resol;
         
         LET ret_dispos_rch_porcentaje_valuacion  = tmp_ret_det_porc_valuacion / 100; -- decimal(5,2) ;
         LET ret_dispos_rch_semanas_cotizadas     = tmp_ret_det_sem_cotizadas; -- integer      ;
         LET ret_dispos_rch_cve_doc_probatorio    = tmp_ret_det_cve_doc_probatorio; -- smallint     ;
         
         --LET v_fecha                              = tmp_ret_det_f_nacimiento[5,6] || "/" || tmp_ret_det_f_nacimiento[7,8] || "/" || tmp_ret_det_f_nacimiento[1,4];
         --LET ret_dispos_rch_f_nacimiento          = DATE(v_fecha); -- date         ;
         LET ret_dispos_rch_f_nacimiento          = tmp_ret_det_f_nacimiento;
         
         LET ret_dispos_rch_aseguradora           = tmp_ret_det_aseguradora; -- char(3)      ;
         LET ret_dispos_rch_actuario              = tmp_ret_det_actuario_autor; -- char(7)      ;
         LET ret_dispos_rch_num_plan_privado      = tmp_ret_det_num_reg_ppp; -- char(8)      ;
         LET ret_dispos_rch_periodo_primer_pago   = tmp_ret_det_periodo_pago; -- integer      ;
         LET ret_dispos_rch_aivs_ret_97           = tmp_ret_det_acciones_ret97 / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_aivs_cv               = tmp_ret_det_acciones_cv / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_aivs_cs               = tmp_ret_det_acciones_cuotsol / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_aivs_sar92            = tmp_ret_det_acciones_ret92 / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_aivs_viv97            = tmp_ret_det_aiv97 / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_aivs_viv92            = tmp_ret_det_aiv92 / 1000000; -- decimal(14,6);
         LET ret_dispos_rch_consec_trabajador     = tmp_ret_det_consec_trab; -- decimal(11,0);
         LET ret_dispos_rch_importe_viv72         = tmp_ret_det_fondo_subcta_viv72 / 100; -- decimal(14,2);
         LET ret_dispos_rch_diag_registro         = tmp_ret_det_diagnostico_reg; -- char(3)      ;
         LET ret_dispos_rch_estado_sub_viv        = tmp_ret_det_estatus_subcta; -- smallint     ;
         LET ret_dispos_rch_cve_afore             = tmp_ret_det_cve_afore; -- smallint     ;
         LET ret_dispos_rch_estado_solicitud      = 100; -- RECHAZADA (ret_estado_solicitud)
         LET ret_dispos_rch_resultado_operacion   = tmp_ret_det_result_operacion; -- smallint     ;
         LET ret_dispos_rch_cod_rechazo_1         = v_motivo_rechazo_1; -- smallint     ;
         LET ret_dispos_rch_cod_rechazo_2         = v_motivo_rechazo_2; -- smallint     ;
         LET ret_dispos_rch_cod_rechazo_3         = v_motivo_rechazo_3; -- smallint     ;
                    
         -- se inserta el registro de rechazo
         INSERT INTO ret_disposicion_rch(
            id_solicitud         ,     
            id_derechohabiente   ,     
            f_solicitud          ,     
            folio                ,     
            nss                  ,     
            curp                 ,     
            nombre_afore         ,     
            paterno_afore        ,     
            materno_afore        ,     
            sec_pension          ,     
            tipo_retiro          ,     
            regimen              ,     
            tpo_seguro           ,     
            tpo_pension          ,     
            tpo_prestacion       ,     
            f_inicio_pension     ,     
            f_resolucion         ,     
            porcentaje_valuacion ,     
            semanas_cotizadas    ,     
            cve_doc_probatorio   ,     
            f_nacimiento         ,     
            aseguradora          ,     
            actuario             ,     
            num_plan_privado     ,     
            periodo_primer_pago  ,        
            aivs_viv97           ,     
            aivs_viv92           ,     
            consec_trabajador    ,     
            importe_viv72        ,     
            diag_registro        ,  
            estado_sub_viv       ,  
            cve_afore            ,  
            estado_solicitud     ,  
            resultado_operacion  ,
            cod_rechazo_1        ,
            cod_rechazo_2        ,
            cod_rechazo_3        
         )
         VALUES (
            seq_ret_solicitud.NEXTVAL           , 
            ret_dispos_rch_id_derechohabiente   , 
            ret_dispos_rch_f_solicitud          , 
            ret_dispos_rch_folio                , 
            ret_dispos_rch_nss                  , 
            ret_dispos_rch_curp                 , 
            ret_dispos_rch_nombre_afore         , 
            ret_dispos_rch_paterno_afore        , 
            ret_dispos_rch_materno_afore        , 
            ret_dispos_rch_sec_pension          , 
            ret_dispos_rch_tipo_retiro          , 
            ret_dispos_rch_regimen              , 
            ret_dispos_rch_tpo_seguro           , 
            ret_dispos_rch_tpo_pension          , 
            ret_dispos_rch_tpo_prestacion       , 
            ret_dispos_rch_f_inicio_pension     , 
            ret_dispos_rch_f_resolucion         , 
            ret_dispos_rch_porcentaje_valuacion , 
            ret_dispos_rch_semanas_cotizadas    , 
            ret_dispos_rch_cve_doc_probatorio   , 
            ret_dispos_rch_f_nacimiento         , 
            ret_dispos_rch_aseguradora          , 
            ret_dispos_rch_actuario             , 
            ret_dispos_rch_num_plan_privado     , 
            ret_dispos_rch_periodo_primer_pago  , 
            ret_dispos_rch_aivs_viv97           , 
            ret_dispos_rch_aivs_viv92           , 
            ret_dispos_rch_consec_trabajador    , 
            ret_dispos_rch_importe_viv72        , 
            ret_dispos_rch_diag_registro        , 
            ret_dispos_rch_estado_sub_viv       , 
            ret_dispos_rch_cve_afore            , 
            ret_dispos_rch_estado_solicitud     , 
            ret_dispos_rch_resultado_operacion  ,
            ret_dispos_rch_cod_rechazo_1        ,
            ret_dispos_rch_cod_rechazo_2        ,
            ret_dispos_rch_cod_rechazo_3        
         );

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- si fue rechazado no se inserta en el historico
         CONTINUE FOREACH;
      END IF

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- REGISTROS ACEPTADOS    
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      
      --TRACE("Asignando datos a registro de detalle disposicion");     
      LET ret_disposicion_id_solicitud          = v_id_solicitud;
      LET ret_disposicion_id_derechohabiente    = v_id_derechohabiente;
      
      --LET v_fecha                               = tmp_ret_det_f_solicitud_trab[5,6] || "/" || tmp_ret_det_f_solicitud_trab[7,8] || "/" || tmp_ret_det_f_solicitud_trab[1,4];
      --LET ret_disposicion_f_solicitud           = v_fecha;
      LET ret_disposicion_f_solicitud           = tmp_ret_det_f_solicitud_trab;
      
      LET ret_disposicion_id_ret_matriz_derecho = v_id_ret_matriz_derecho;
      LET ret_disposicion_sec_pension           = tmp_ret_det_sec_pension;
      LET ret_disposicion_diag_registro         = tmp_ret_det_diagnostico_reg;
      LET ret_disposicion_folio                 = p_folio;
      LET ret_disposicion_curp                  = tmp_ret_det_curp;
      LET ret_disposicion_nombre_afore          = tmp_ret_det_nombre_afore;
      LET ret_disposicion_paterno_afore         = tmp_ret_det_paterno_afore;
      LET ret_disposicion_materno_afore         = tmp_ret_det_materno_afore;
      
      --LET v_fecha                               = tmp_ret_det_f_inicio_pension[5,6] || "/" || tmp_ret_det_f_inicio_pension[7,8] || "/" || tmp_ret_det_f_inicio_pension[1,4];
      --LET ret_disposicion_f_inicio_pension      = DATE(v_fecha);
      LET ret_disposicion_f_inicio_pension      = tmp_ret_det_f_inicio_pension;
      
      --LET v_fecha                               = tmp_ret_det_f_emision_resol[5,6] || "/" || tmp_ret_det_f_emision_resol[7,8] || "/" || tmp_ret_det_f_emision_resol[1,4];
      --LET ret_disposicion_f_resolucion          = DATE(v_fecha);
      LET ret_disposicion_f_resolucion          = tmp_ret_det_f_emision_resol;
      
      LET ret_disposicion_porcentaje_valuacion  = tmp_ret_det_porc_valuacion / 100;
      LET ret_disposicion_semanas_cotizadas     = tmp_ret_det_sem_cotizadas;
      LET ret_disposicion_cve_doc_probatorio    = tmp_ret_det_cve_doc_probatorio;
      
      --LET v_fecha                               = tmp_ret_det_f_nacimiento[5,6] || "/" || tmp_ret_det_f_nacimiento[7,8] || "/" || tmp_ret_det_f_nacimiento[1,4];
      --LET ret_disposicion_f_nacimiento          = DATE(v_fecha);
      LET ret_disposicion_f_nacimiento          = tmp_ret_det_f_nacimiento;
      
      LET ret_disposicion_aseguradora           = tmp_ret_det_aseguradora;
      LET ret_disposicion_actuario              = tmp_ret_det_actuario_autor;
      LET ret_disposicion_num_plan_privado      = tmp_ret_det_num_reg_ppp;
      LET ret_disposicion_periodo_primer_pago   = tmp_ret_det_periodo_pago;
      LET ret_disposicion_aivs_ret97            = tmp_ret_det_acciones_ret97 / 1000000;
      LET ret_disposicion_aivs_cv               = tmp_ret_det_acciones_cv / 1000000;
      LET ret_disposicion_aivs_cs               = tmp_ret_det_acciones_cuotsol / 1000000;
      LET ret_disposicion_aivs_sar92            = tmp_ret_det_acciones_ret92 / 1000000;
      LET ret_disposicion_consec_trabajador     = tmp_ret_det_consec_trab;
      LET ret_disposicion_importe_viv72         = tmp_ret_det_fondo_subcta_viv72 / 100;
      LET ret_disposicion_estado_sub_viv        = tmp_ret_det_estatus_subcta;
      LET ret_disposicion_cve_afore             = tmp_ret_det_cve_afore;
      LET ret_disposicion_cod_rechazo           = 0; -- NO ESTA RECHAZADA
      LET ret_disposicion_estado_solicitud      = 30; -- RECIBIDA [integrada](ret_estado_solicitud)

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- calculo de las AIVs 97 y 92 a pesos
      ----TRACE("Asignando AIVs a pesos");
      LET ret_disposicion_aivs_viv97            = (tmp_ret_det_aiv97 / 1000000);
      LET ret_disposicion_aivs_viv92            = (tmp_ret_det_aiv92 / 1000000);

      
      ----TRACE("multiplicando AIVs por valor del fondo");
      LET v_pesos_aiv97 = ret_disposicion_aivs_viv97 * ret_cza_disposicion_precio_fondo;
      LET v_pesos_aiv92 = ret_disposicion_aivs_viv92 * ret_cza_disposicion_precio_fondo;

--      ret_cza_disposicion_precio_fondo ,
--      tmp_ret_det_monto_pesos        

      -- incremento del importe total
      --TRACE("acumulando importe total");
      --TRACE(v_sumario_importe_total);
      LET v_sumario_importe_total = v_sumario_importe_total + v_pesos_aiv97 + v_pesos_aiv92 + ret_disposicion_importe_viv72;

      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
      INSERT INTO ret_disposicion (
         id_solicitud          ,  
         id_derechohabiente    ,   
         f_solicitud           ,   
         id_ret_matriz_derecho ,   
         sec_pension           ,   
         diag_registro         ,   
         folio                 ,   
         curp                  ,   
         nombre_afore          ,   
         paterno_afore         ,   
         materno_afore         ,   
         f_inicio_pension      ,   
         f_resolucion          ,   
         porcentaje_valuacion  ,   
         semanas_cotizadas     ,   
         cve_doc_probatorio    ,   
         f_nacimiento          ,   
         aseguradora           ,   
         actuario              ,   
         num_plan_privado      ,   
         periodo_primer_pago   ,   
         aivs_viv97            ,   
         aivs_viv92            ,   
         consec_trabajador     ,   
         importe_viv72         ,   
         estado_sub_viv        ,   
         cve_afore             ,   
         cod_rechazo           ,   
         estado_solicitud          
      )
      VALUES (
         seq_ret_solicitud.NEXTVAL             ,
         ret_disposicion_id_derechohabiente    ,
         ret_disposicion_f_solicitud           ,
         ret_disposicion_id_ret_matriz_derecho ,
         ret_disposicion_sec_pension           ,
         ret_disposicion_diag_registro         ,
         ret_disposicion_folio                 ,
         ret_disposicion_curp                  ,
         ret_disposicion_nombre_afore          ,
         ret_disposicion_paterno_afore         ,
         ret_disposicion_materno_afore         ,
         ret_disposicion_f_inicio_pension      ,
         ret_disposicion_f_resolucion          ,
         ret_disposicion_porcentaje_valuacion  ,
         ret_disposicion_semanas_cotizadas     ,
         ret_disposicion_cve_doc_probatorio    ,
         ret_disposicion_f_nacimiento          ,
         ret_disposicion_aseguradora           ,
         ret_disposicion_actuario              ,
         ret_disposicion_num_plan_privado      ,
         ret_disposicion_periodo_primer_pago   ,
         ret_disposicion_aivs_viv97            ,
         ret_disposicion_aivs_viv92            ,
         ret_disposicion_consec_trabajador     ,
         ret_disposicion_importe_viv72         ,
         ret_disposicion_estado_sub_viv        ,
         ret_disposicion_cve_afore             ,
         ret_disposicion_cod_rechazo           ,
         ret_disposicion_estado_solicitud       
      );
      
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

      -- se marca la cuenta

   END FOREACH;
   
   -- ===========================================================   
   -- se genera el registro de ENCABEZADO
   LET ret_cza_disposicion_folio                 = p_folio; 
   LET ret_cza_disposicion_nombre_archivo        = p_nombre_archivo;
   LET ret_cza_disposicion_f_operacion_procesar  = TODAY; 
   LET ret_cza_disposicion_f_carga               = TODAY; 
   LET ret_cza_disposicion_h_carga               = CURRENT HOUR TO SECOND; 
   LET ret_cza_disposicion_f_valor_transferencia = p_fecha_valor; 
   --LET ret_cza_disposicion_precio_fondo          = 0.0; -- se asigno al leer el detalle
	 LET ret_cza_disposicion_total_registros       = 0; 
	 LET ret_cza_disposicion_total_importe         = v_sumario_importe_total; 
   LET ret_cza_disposicion_usuario               = p_usuario_cod; 

   SELECT COUNT(*)
   INTO ret_cza_disposicion_total_registros
   FROM safre_viv:ret_disposicion
   WHERE folio = p_folio;
   
   -- se inserta el registro de encabezado
   INSERT INTO safre_viv:ret_cza_disposicion (
      folio                 ,
      nombre_archivo        ,
      f_operacion_procesar  ,
      f_carga               ,
      h_carga               ,
      f_valor_transferencia ,
      precio_fondo          ,
	    total_registros       ,
	    total_importe         ,
      usuario               
   )
   VALUES (
      ret_cza_disposicion_folio                 ,
      ret_cza_disposicion_nombre_archivo        ,
      ret_cza_disposicion_f_operacion_procesar  ,
      ret_cza_disposicion_f_carga               ,
      ret_cza_disposicion_h_carga               ,
      ret_cza_disposicion_f_valor_transferencia ,
      ret_cza_disposicion_precio_fondo          ,
	    ret_cza_disposicion_total_registros       ,
	    ret_cza_disposicion_total_importe         ,
      ret_cza_disposicion_usuario               
   );

 
   -- si hubo rechazos en la integracion, de detalle o en el encabezado, el lote se rechaza completo
   IF ( v_reg_det_rechazados > 0 ) THEN
      LET v_estatus_proceso = 100; -- rechazado
           
      -- se borran todos los registros de la tabla de detalle y se transfieren a la tabla de rechazos
      -- porque un rechazo implica el rechazo del lote completo
      
      FOREACH
      SELECT 
         id_solicitud          ,                              
         id_derechohabiente    ,  
         f_solicitud           ,  
         id_ret_matriz_derecho ,  
         sec_pension           ,  
         diag_registro         ,  
         folio                 ,  
         curp                  ,  
         nombre_afore          ,  
         paterno_afore         ,  
         materno_afore         ,  
         f_inicio_pension      ,  
         f_resolucion          ,  
         porcentaje_valuacion  ,  
         semanas_cotizadas     ,  
         cve_doc_probatorio    ,  
         f_nacimiento          ,  
         aseguradora           ,  
         actuario              ,  
         num_plan_privado      ,  
         periodo_primer_pago   ,  
         aivs_viv97            ,  
         aivs_viv92            ,  
         consec_trabajador     ,  
         importe_viv72         ,  
         estado_sub_viv        ,  
         cve_afore             ,  
         cod_rechazo           ,  
         estado_solicitud         
      INTO
         v_id_solicitud                        ,                            
         ret_disposicion_id_derechohabiente    ,
         ret_disposicion_f_solicitud           ,
         ret_disposicion_id_ret_matriz_derecho ,
         ret_disposicion_sec_pension           ,
         ret_disposicion_diag_registro         ,
         ret_disposicion_folio                 ,
         ret_disposicion_curp                  ,
         ret_disposicion_nombre_afore          ,
         ret_disposicion_paterno_afore         ,
         ret_disposicion_materno_afore         ,
         ret_disposicion_f_inicio_pension      ,
         ret_disposicion_f_resolucion          ,
         ret_disposicion_porcentaje_valuacion  ,
         ret_disposicion_semanas_cotizadas     ,
         ret_disposicion_cve_doc_probatorio    ,
         ret_disposicion_f_nacimiento          ,
         ret_disposicion_aseguradora           ,
         ret_disposicion_actuario              ,
         ret_disposicion_num_plan_privado      ,
         ret_disposicion_periodo_primer_pago   ,
         ret_disposicion_aivs_viv97            ,
         ret_disposicion_aivs_viv92            ,
         ret_disposicion_consec_trabajador     ,
         ret_disposicion_importe_viv72         ,
         ret_disposicion_estado_sub_viv        ,
         ret_disposicion_cve_afore             ,
         ret_disposicion_cod_rechazo           ,
         ret_disposicion_estado_solicitud       
      FROM ret_disposicion
      WHERE
         folio = p_folio
   
         -- se obtiene el nss
         SELECT nss
         INTO   ret_dispos_rch_nss
         FROM   afi_derechohabiente
         WHERE  id_derechohabiente = ret_disposicion_id_derechohabiente;
            
         SELECT
            tpo_retiro      ,
            regimen         ,
            tpo_seguro      ,
            tpo_pension     ,
            tpo_prestacion  
         INTO
            ret_dispos_rch_tipo_retiro    ,
            ret_dispos_rch_regimen        ,
            ret_dispos_rch_tpo_seguro     ,
            ret_dispos_rch_tpo_pension    ,
            ret_dispos_rch_tpo_prestacion 
         FROM
            ret_matriz_derecho
         WHERE
            id_ret_matriz_derecho = ret_disposicion_id_ret_matriz_derecho;
         
         -- se asignan los datos al registro de rechazo
         LET ret_dispos_rch_id_solicitud          = v_id_solicitud; 
         LET ret_dispos_rch_id_derechohabiente    = ret_disposicion_id_derechohabiente;
         LET ret_dispos_rch_f_solicitud           = ret_disposicion_f_solicitud;
         LET ret_dispos_rch_folio                 = p_folio;
         LET ret_dispos_rch_curp                  = ret_disposicion_curp;
         LET ret_dispos_rch_nombre_afore          = ret_disposicion_nombre_afore;
         LET ret_dispos_rch_paterno_afore         = ret_disposicion_paterno_afore;
         LET ret_dispos_rch_materno_afore         = ret_disposicion_materno_afore;
         LET ret_dispos_rch_sec_pension           = ret_disposicion_sec_pension;
   
         LET ret_dispos_rch_f_inicio_pension      = ret_disposicion_f_inicio_pension;
         LET ret_dispos_rch_f_resolucion          = ret_disposicion_f_resolucion;
         LET ret_dispos_rch_porcentaje_valuacion  = ret_disposicion_porcentaje_valuacion;
         LET ret_dispos_rch_semanas_cotizadas     = ret_disposicion_semanas_cotizadas;
         LET ret_dispos_rch_cve_doc_probatorio    = ret_disposicion_cve_doc_probatorio;
         LET ret_dispos_rch_f_nacimiento          = ret_disposicion_f_nacimiento;
         LET ret_dispos_rch_aseguradora           = ret_disposicion_aseguradora;
         LET ret_dispos_rch_actuario              = ret_disposicion_actuario;
         LET ret_dispos_rch_num_plan_privado      = ret_disposicion_num_plan_privado;
         LET ret_dispos_rch_periodo_primer_pago   = ret_disposicion_periodo_primer_pago;
         LET ret_dispos_rch_aivs_ret_97           = ret_disposicion_aivs_ret97;
         LET ret_dispos_rch_aivs_cv               = ret_disposicion_aivs_cv;
         LET ret_dispos_rch_aivs_cs               = ret_disposicion_aivs_cs;
         LET ret_dispos_rch_aivs_sar92            = ret_disposicion_aivs_sar92;
         LET ret_dispos_rch_aivs_viv97            = ret_disposicion_aivs_viv97;
         LET ret_dispos_rch_aivs_viv92            = ret_disposicion_aivs_viv92;
         LET ret_dispos_rch_consec_trabajador     = ret_disposicion_consec_trabajador;
         LET ret_dispos_rch_importe_viv72         = ret_disposicion_importe_viv72;
         LET ret_dispos_rch_diag_registro         = ret_disposicion_diag_registro;
         LET ret_dispos_rch_estado_sub_viv        = ret_disposicion_estado_sub_viv;
         LET ret_dispos_rch_cve_afore             = ret_disposicion_cve_afore;
         LET ret_dispos_rch_estado_solicitud      = 100; -- RECHAZADA (ret_estado_solicitud)
         LET ret_dispos_rch_resultado_operacion   = 0; -- que va aqui?
         LET ret_dispos_rch_cod_rechazo_1         = 0;
         LET ret_dispos_rch_cod_rechazo_2         = 0;
         LET ret_dispos_rch_cod_rechazo_3         = 0;
   
   
         -- se inserta el registro de rechazo
         INSERT INTO ret_disposicion_rch(
            id_solicitud         ,     
            id_derechohabiente   ,     
            f_solicitud          ,     
            folio                ,     
            nss                  ,     
            curp                 ,     
            nombre_afore         ,     
            paterno_afore        ,     
            materno_afore        ,     
            sec_pension          ,     
            tipo_retiro          ,     
            regimen              ,     
            tpo_seguro           ,     
            tpo_pension          ,     
            tpo_prestacion       ,     
            f_inicio_pension     ,     
            f_resolucion         ,     
            porcentaje_valuacion ,     
            semanas_cotizadas    ,     
            cve_doc_probatorio   ,     
            f_nacimiento         ,     
            aseguradora          ,     
            actuario             ,     
            num_plan_privado     ,     
            periodo_primer_pago  ,     
            aivs_viv97           ,     
            aivs_viv92           ,     
            consec_trabajador    ,     
            importe_viv72        ,     
            diag_registro        ,     
            estado_sub_viv       ,     
            cve_afore            ,     
            estado_solicitud     ,     
            resultado_operacion  ,
            cod_rechazo_1        ,
            cod_rechazo_2        ,
            cod_rechazo_3        
         )
         VALUES (
            seq_ret_solicitud.NEXTVAL           , 
            ret_dispos_rch_id_derechohabiente   , 
            ret_dispos_rch_f_solicitud          , 
            ret_dispos_rch_folio                , 
            ret_dispos_rch_nss                  , 
            ret_dispos_rch_curp                 , 
            ret_dispos_rch_nombre_afore         , 
            ret_dispos_rch_paterno_afore        , 
            ret_dispos_rch_materno_afore        , 
            ret_dispos_rch_sec_pension          , 
            ret_dispos_rch_tipo_retiro          , 
            ret_dispos_rch_regimen              , 
            ret_dispos_rch_tpo_seguro           , 
            ret_dispos_rch_tpo_pension          , 
            ret_dispos_rch_tpo_prestacion       , 
            ret_dispos_rch_f_inicio_pension     , 
            ret_dispos_rch_f_resolucion         , 
            ret_dispos_rch_porcentaje_valuacion , 
            ret_dispos_rch_semanas_cotizadas    , 
            ret_dispos_rch_cve_doc_probatorio   , 
            ret_dispos_rch_f_nacimiento         , 
            ret_dispos_rch_aseguradora          , 
            ret_dispos_rch_actuario             , 
            ret_dispos_rch_num_plan_privado     , 
            ret_dispos_rch_periodo_primer_pago  , 
            ret_dispos_rch_aivs_viv97           , 
            ret_dispos_rch_aivs_viv92           , 
            ret_dispos_rch_consec_trabajador    , 
            ret_dispos_rch_importe_viv72        , 
            ret_dispos_rch_diag_registro        , 
            ret_dispos_rch_estado_sub_viv       , 
            ret_dispos_rch_cve_afore            , 
            ret_dispos_rch_estado_solicitud     , 
            ret_dispos_rch_resultado_operacion  ,
            ret_dispos_rch_cod_rechazo_1        ,
            ret_dispos_rch_cod_rechazo_2        ,
            ret_dispos_rch_cod_rechazo_3        
         );

      END FOREACH ;
           
      -- se rechaza el encabezado
      INSERT INTO ret_cza_disposicion_rch (
         folio                 ,
         f_operacion_procesar  ,
         nombre_archivo        ,
         f_carga               ,
         hora_carga            ,
         f_valor_transferencia ,
         precio_fondo          ,
         total_registros       ,
         total_importe         ,
         usuario               ,
         resultado_operacion   ,
         cod_rechazo_1         ,
         cod_rechazo_2         ,
         cod_rechazo_3         
      )
      VALUES (
         ret_cza_disposicion_folio                 ,
         ret_cza_disposicion_f_operacion_procesar  ,
         ret_cza_disposicion_nombre_archivo        ,
         ret_cza_disposicion_f_carga               ,
         ret_cza_disposicion_h_carga               ,
         ret_cza_disposicion_f_valor_transferencia ,
         ret_cza_disposicion_precio_fondo          ,
         ret_cza_disposicion_total_registros       ,
         ret_cza_disposicion_total_importe         ,
         ret_cza_disposicion_usuario               ,
         100                                       ,
         0                                         ,
         0                                         ,
         0         
      );

      -- se borran los registros de la tabla de detalle
      DELETE FROM ret_disposicion
      WHERE  folio = p_folio;

      DELETE FROM ret_cza_disposicion
      WHERE  folio = p_folio;

   END IF

   -- se elimina la tabla temporal
   DROP TABLE tmp_codigos_rechazo;

   -- se actualizan las estadisticas 
   UPDATE STATISTICS FOR TABLE ret_disposicion;
   UPDATE STATISTICS FOR TABLE ret_cza_disposicion;
   UPDATE STATISTICS FOR TABLE ret_disposicion_rch;
   UPDATE STATISTICS FOR TABLE ret_cza_disposicion_rch;
 
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


