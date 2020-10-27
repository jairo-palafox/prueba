






CREATE FUNCTION "safreviv".fn_ret_integra_transferencia( p_usuario_cod    CHAR(20),
                                              p_folio          DECIMAL(10,0),
                                              p_nombre_archivo VARCHAR(40,0),
                                              p_pid            DECIMAL(9,0),
                                              p_proceso_cod         SMALLINT  
                                            )
  RETURNING INTEGER, INTEGER, VARCHAR(250)

   -- campos de la tabla de encabezado de retiros por transferencia (sin filler)
   -- tmp_ret_cza_transferencia
   DEFINE tmp_ret_cza_trans_tpo_registro            CHAR(2);
   DEFINE tmp_ret_cza_trans_id_servicio             CHAR(2);
   DEFINE tmp_ret_cza_trans_tpo_entidad_origen      CHAR(2);
   DEFINE tmp_ret_cza_trans_cve_entidad_origen      CHAR(3);
   DEFINE tmp_ret_cza_trans_tpo_entidad_destino     CHAR(2);
   DEFINE tmp_ret_cza_trans_cve_entidad_destino     CHAR(3);
   DEFINE tmp_ret_cza_trans_f_operacion             DATE;
   DEFINE tmp_ret_cza_trans_f_valor_transferencia   DATE;
   DEFINE tmp_ret_cza_trans_val_aplicacion_aivs     DECIMAL(14);
   DEFINE tmp_ret_cza_trans_resultado_operacion     CHAR(2);
   DEFINE tmp_ret_cza_trans_motivo_rech_1           CHAR(3);
   DEFINE tmp_ret_cza_trans_motivo_rech_2           CHAR(3);
   DEFINE tmp_ret_cza_trans_motivo_rech_3           CHAR(3);

   -- campos de la tabla de detalle de retiros por transferencia (sin filler)
   DEFINE tmp_det_transf_tpo_registro               CHAR(2);
   DEFINE tmp_det_transf_id_servicio                CHAR(2);
   DEFINE tmp_det_transf_id_operacion               CHAR(2);
   DEFINE tmp_det_transf_nss                        CHAR(11);
   DEFINE tmp_det_transf_curp                       CHAR(18);
   DEFINE tmp_det_transf_nombre_trab_datamart       CHAR(50);
   DEFINE tmp_det_transf_nombre_afore               CHAR(40);
   DEFINE tmp_det_transf_paterno_afore              CHAR(40);
   DEFINE tmp_det_transf_materno_afore              CHAR(40);
   DEFINE tmp_det_transf_sec_pension                CHAR(2);
   DEFINE tmp_det_transf_tpo_movimiento             CHAR(3);
   DEFINE tmp_det_transf_regimen                    CHAR(2);
   DEFINE tmp_det_transf_tpo_retiro                 CHAR(1);
   DEFINE tmp_det_transf_tpo_seguro                 CHAR(2);
   DEFINE tmp_det_transf_tpo_pension                CHAR(2);
   DEFINE tmp_det_transf_tpo_prestacion             CHAR(2);
   DEFINE tmp_det_transf_f_inicio_pension           DATE;
   DEFINE tmp_det_transf_f_emision_resol            DATE;
   DEFINE tmp_det_transf_porc_valuacion             DECIMAL(5);
   DEFINE tmp_det_transf_sem_cotizadas              DECIMAL(4);
   DEFINE tmp_det_transf_f_carga_datamart           DECIMAL(8);
   DEFINE tmp_det_transf_diagnostico_reg            CHAR(3);
   DEFINE tmp_det_transf_estatus_subcta             CHAR(1);
   DEFINE tmp_det_transf_periodo_pago               DECIMAL(6);
   DEFINE tmp_det_transf_acciones_ret97             DECIMAL(14);
   DEFINE tmp_det_transf_acciones_cv                DECIMAL(14);
   DEFINE tmp_det_transf_acciones_cuotsol           DECIMAL(14);
   DEFINE tmp_det_transf_aiv97                      DECIMAL(14);
   DEFINE tmp_det_transf_result_operacion           CHAR(2);
   DEFINE tmp_det_transf_cve_afore                  DECIMAL(3);
   DEFINE tmp_det_transf_motivo_rech1               CHAR(3);
   DEFINE tmp_det_transf_motivo_rech2               CHAR(3);
      
   -- =================================================================================
   -- tablas destino
   -- encabezado de la tabla historica/integrada de retiros por transferencia
   -- ret_cza_transferencia
   DEFINE ret_cza_trans_folio                       DECIMAL(9,0);
   DEFINE ret_cza_trans_nombre_archivo              CHAR(20);
   DEFINE ret_cza_trans_f_operacion_procesar        DATE;
   DEFINE ret_cza_trans_f_carga                     DATE;
   DEFINE ret_cza_trans_h_carga                     DATETIME HOUR TO MINUTE;
   DEFINE ret_cza_trans_f_valor_transferencia       DATE;
   DEFINE ret_cza_trans_precio_fondo                DECIMAL(14,6);
   DEFINE ret_cza_trans_total_registros             INTEGER;
   DEFINE ret_cza_trans_total_importe               DECIMAL(18,6);
   DEFINE ret_cza_trans_usuario                     CHAR(20);

   -- detalle de la tabla historica/integrada de retiros por transferencia
   -- ret_transferencia
   DEFINE ret_transf_id_solicitud                   DECIMAL(9,0);
   DEFINE ret_transf_id_derechohabiente             DECIMAL(9,0);
   DEFINE ret_transf_id_ret_matriz_derecho          SMALLINT;
   DEFINE ret_transf_sec_pension                    SMALLINT;
   DEFINE ret_transf_diag_registro                  CHAR(3);
   DEFINE ret_transf_folio                          DECIMAL(9,0);
   DEFINE ret_transf_estado_solicitud               SMALLINT;
   DEFINE ret_transf_curp                           CHAR(18);
   DEFINE ret_transf_nombre_datamart                CHAR(50);
   DEFINE ret_transf_nombre_afore                   CHAR(40);
   DEFINE ret_transf_paterno_afore                  CHAR(40);
   DEFINE ret_transf_materno_afore                  CHAR(40);
   DEFINE ret_transf_tpo_movimiento                 CHAR(3);
   DEFINE ret_transf_f_inicio_pension               DATE;
   DEFINE ret_transf_f_resolucion                   CHAR(18);
   DEFINE ret_transf_porcentaje_valuacion           DECIMAL(5,2);
   DEFINE ret_transf_semanas_cotizadas              INTEGER;
   DEFINE ret_transf_f_carga_datamart               DATE;
   DEFINE ret_transf_estado_sub_viv                 SMALLINT;
   DEFINE ret_transf_aivs_viv97                     DECIMAL(14,6);
   DEFINE ret_transf_cve_afore                      SMALLINT;
   DEFINE ret_transf_cod_rechazo                    SMALLINT;

   -- =============================================================================
   -- rechazo de encabezado
   -- ret_cza_transferencia_rch
   DEFINE ret_cza_trans_rch_folio                   DECIMAL(9,0);
   DEFINE ret_cza_trans_rch_nombre_archivo          CHAR(20);
   DEFINE ret_cza_trans_rch_f_operacion_procesar    DATE;
   DEFINE ret_cza_trans_rch_f_carga                 DATE;
   DEFINE ret_cza_trans_rch_h_carga                 DATETIME HOUR TO MINUTE;
   DEFINE ret_cza_trans_rch_f_valor_transferencia   DATE;
   DEFINE ret_cza_trans_rch_precio_fondo            DECIMAL(14,6);
   DEFINE ret_cza_trans_rch_total_registros         INTEGER;
   DEFINE ret_cza_trans_rch_total_importe           DECIMAL(18,6);
   DEFINE ret_cza_trans_rch_usuario                 CHAR(20);
   DEFINE ret_cza_trans_rch_cod_rechazo_1           SMALLINT;
   DEFINE ret_cza_trans_rch_cod_rechazo_2           SMALLINT;
   DEFINE ret_cza_trans_rch_cod_rechazo_3           SMALLINT;

      
   -- =============================================================================
   -- rechazo de detalle
   -- ret_transferencia_rch
   DEFINE ret_transf_rch_id_derechohabiente         DECIMAL(9,0);
   DEFINE ret_transf_rch_nss                        CHAR(11); -- campo agregado
   DEFINE ret_transf_rch_folio                      DECIMAL(9,0);
   DEFINE ret_transf_rch_regimen                    CHAR(2); -- agregar este
   DEFINE ret_transf_rch_tpo_retiro                 CHAR(1); -- agregar este
   DEFINE ret_transf_rch_tpo_seguro                 CHAR(2); -- agregar este
   DEFINE ret_transf_rch_tpo_pension                CHAR(2); -- agregar este
   DEFINE ret_transf_rch_tpo_prestacion             CHAR(2); -- agregar este
   DEFINE ret_transf_rch_sec_pension                SMALLINT;
   DEFINE ret_transf_rch_diag_registro              CHAR(3);
   DEFINE ret_transf_rch_estado_solicitud           SMALLINT;
   DEFINE ret_transf_rch_curp                       CHAR(18);
   DEFINE ret_transf_rch_nombre_datamart            CHAR(50);
   DEFINE ret_transf_rch_nombre_afore               CHAR(40);
   DEFINE ret_transf_rch_paterno_afore              CHAR(40);
   DEFINE ret_transf_rch_materno_afore              CHAR(40);
   DEFINE ret_transf_rch_tpo_movimiento             CHAR(3);
   DEFINE ret_transf_rch_f_inicio_pension           DATE;
   DEFINE ret_transf_rch_f_resolucion               DATE;
   DEFINE ret_transf_rch_porcentaje_valuacion       DECIMAL(5,2);
   DEFINE ret_transf_rch_semanas_cotizadas          INTEGER;
   DEFINE ret_transf_rch_f_carga_datamart           DATE;
   DEFINE ret_transf_rch_estado_sub_viv             SMALLINT;
   DEFINE ret_transf_rch_aivs_viv97                 DECIMAL(14,6);
   DEFINE ret_transf_rch_cve_afore                  SMALLINT;
   DEFINE ret_transf_rch_cod_rechazo                SMALLINT;
   DEFINE ret_transf_rch_cod_rechazo_1              SMALLINT;
   DEFINE ret_transf_rch_cod_rechazo_2              SMALLINT;
   DEFINE ret_transf_rch_cod_rechazo_3              SMALLINT;

   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                      DECIMAL(9,0);
   DEFINE v_id_solicitud                            DECIMAL(9,0);
   -- =============================================================================
   -- para calcular las AIVs a pesos
   DEFINE v_valor_fondo                             DECIMAL(14);
   DEFINE v_pesos_aiv97                             DECIMAL(14,6);
   DEFINE v_pesos_aiv92                             DECIMAL(14,6);

   -- para rechazos
   DEFINE v_b_rechazo_encabezado                    SMALLINT;
   DEFINE v_b_rechazo_detalle                       SMALLINT;
   DEFINE v_validar_3_primeros_campos               VARCHAR(6); -- se concatenan los 3 primeros campos para validar
   DEFINE v_afore_cod                               SMALLINT; -- clave de afore
   -- id matriz derecho
   DEFINE v_id_ret_matriz_derecho                   SMALLINT; -- id de la matriz de derecho de retiros
   DEFINE v_grupo                                   SMALLINT; -- grupo para revisar que cuentas se pagan por retiro
   DEFINE v_paga_viv92                              SMALLINT; -- booleana para saber si se paga viv92
   DEFINE v_paga_viv97                              SMALLINT; -- booleana para saber si se paga viv97
   DEFINE v_paga_viv72                              SMALLINT; -- booleana para saber si se paga viv72
    
   DEFINE v_sumario_importe_total                   DECIMAL(22,6);   
   DEFINE v_sumario_total_registros                 DECIMAL(6,0);
   DEFINE v_total_registros                         DECIMAL(6,0);
   DEFINE v_numero_registros                        DECIMAL(6,0);
   DEFINE v_saldo_cuenta                            DECIMAL(14,6);

   DEFINE v_motivo_rechazo_1                        SMALLINT;
   DEFINE v_motivo_rechazo_2                        SMALLINT;
   DEFINE v_motivo_rechazo_3                        SMALLINT;
   -- arreglo de codigos de rechazo
   DEFINE v_codigos_rechazo                         CHAR(30); -- los codigos van de tres en tres
   DEFINE v_indice_codigos_rechazo                  SMALLINT; 

   -- conteo de rechazos e inserciones
   DEFINE v_reg_cza_insertados                      SMALLINT; -- total de registros de encabezado insertados
   DEFINE v_reg_cza_rechazados                      SMALLINT; -- total de registros de encabezado rechazados
   DEFINE v_reg_det_insertados                      DECIMAL(6,0); -- total de registros de detalle insertados
   DEFINE v_reg_det_rechazados                      DECIMAL(6,0); -- total de registros de detalle rechazados

   -- codigos de error en encabezado
   DEFINE v_error_cza_reg_totales_no_coinciden      DECIMAL(10,5);
   DEFINE v_error_cza_tpo_registro_invalido         SMALLINT;
   DEFINE v_error_cza_id_servicio_invalido          SMALLINT;
   DEFINE v_error_cza_sin_precio_fondo              SMALLINT;
   DEFINE v_error_cza_sin_fecha_procesar            SMALLINT;
   DEFINE v_error_cza_sin_fecha_valuacion           SMALLINT;
   DEFINE v_error_cza_precio_fondo_invalido         SMALLINT; -- cuando el precio del fondo no coincide con el valor del fondo segun la fecha dada
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
   DEFINE v_error_det_marca_unificacion             SMALLINT; -- el lote quedo invalido porque hubo rechazos
   DEFINE v_error_det_marca_credito                 SMALLINT; -- el lote quedo invalido porque hubo rechazos
   DEFINE v_error_det_marca_separacion              SMALLINT; -- el lote quedo invalido porque hubo rechazos
   DEFINE v_error_det_apaterno_afore_invalido       SMALLINT;
   DEFINE v_error_det_amaterno_afore_invalido       SMALLINT;
   DEFINE v_error_det_nombre_afore_invalido         SMALLINT;
   DEFINE v_error_det_saldo_en_garantia             SMALLINT;
   DEFINE v_error_det_marca_no_convive              SMALLINT;
   DEFINE v_error_det_en_devolucion_de_subcuenta    SMALLINT;
   DEFINE v_error_det_subcuenta_no_corresoponde     SMALLINT; -- subcuenta no corresponde a matriz de derechos
   DEFINE v_error_det_registro_duplicado_archivo    SMALLINT; -- el registro esta dos veces
   DEFINE v_monto_viv97_invalido                    SMALLINT;
   DEFINE v_max_aivs_sobregiro                      DECIMAL(18,6);  --cantidad maxima de aivs para sobregiro

   DEFINE v_nombre_af                               CHAR(40);
   DEFINE v_ap_paterno_af                           CHAR(40);
   DEFINE v_ap_materno_af                           CHAR(40);

   -- para validar marca de credito
   DEFINE v_tpo_originacion                         SMALLINT;
   DEFINE v_marca_infonavit                         SMALLINT;
   DEFINE v_saldo_97_aivs                           DECIMAL(18,6); -- saldo del derechohabiente en viv92
   DEFINE v_saldo_97_pesos                          DECIMAL(18,6); -- saldo del derechohabiente en viv97
   DEFINE v_saldo_vol_aivs                          DECIMAL(18,6); -- saldo del derechohabiente en voluntarias
   DEFINE v_saldo_vol_pesos                         DECIMAL(18,6); -- saldo del derechohabiente en voluntarias
   DEFINE v_resultado_consulta                      SMALLINT;

   -- estatus del proceso
   DEFINE v_estatus_proceso                         SMALLINT;

   -- para marcar las cuentas
   DEFINE v_i_estado_marca                          INTEGER;
   DEFINE v_marca_transferencia                     INTEGER; -- 806 transferencia de recurso

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

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finalizó exitosamente.';

 
   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/debug_ret_transferencia.txt");

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados = 0; -- total de registros de detalle rechazados

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden = 1000;
   LET v_error_cza_tpo_registro_invalido = 2;
   LET v_error_cza_id_servicio_invalido = 3;
   LET v_error_cza_sin_precio_fondo = 4;
   LET v_error_cza_sin_fecha_procesar = 5;
   LET v_error_cza_sin_fecha_valuacion = 6;
   LET v_error_cza_precio_fondo_invalido = 7;
 
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado = 49; -- PROCESAR
   LET v_error_det_tpo_registro_invalido = 2;
   LET v_error_det_id_servicio_invalido = 3;
   LET v_error_det_id_operacion_invalido = 4;
   LET v_error_det_matriz_derecho_no_encontrado = 5;
   LET v_error_det_sec_pension_invalido = 6;
   LET v_error_det_fec_solicitud_invalido = 7;
   LET v_error_det_afore_invalido = 8;
   LET v_error_det_lote_invalido = 9;
   LET v_error_det_marca_unificacion = 597; -- NSS esta en unificacion

   LET v_error_det_saldo_en_garantia = 961; -- NSS tiene un saldo en garantia
   LET v_error_det_marca_no_convive = 599; -- NSS esta en otro proceso operativo
   LET v_error_det_en_devolucion_de_subcuenta = 598; -- NSS esta en un proceso de retiro
   LET v_error_det_subcuenta_no_corresoponde = 600; -- la subcuenta no corresponde al tipo de retiro
   
   LET v_error_det_marca_credito = 100; -- NSS tiene un credito vigente  
   LET v_error_det_marca_separacion = 201; -- NSS esta en separacion
   LET v_error_det_apaterno_afore_invalido = 758; -- apellido paterno afore no coincide con el de base de datos
   LET v_error_det_amaterno_afore_invalido = 759; -- apellido materno afore no coincide con el de base de datos
   LET v_error_det_nombre_afore_invalido = 760; -- nombre afore no coincide con el de base de datos
   LET v_error_det_registro_duplicado_archivo = 768; -- el registro esta duplicado en archivo
   LET v_monto_viv97_invalido = 767; -- el saldo es insuficiente

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio          = p_folio,
          estado         = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1
   AND    folio          IS NULL; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  pid         = p_pid;

   SELECT diferencia_maxima
   INTO   v_max_aivs_sobregiro
   FROM   ret_sobregiro_max_permitido
   WHERE  proceso_cod = p_proceso_cod;

   
   -- se inician las variables para marca
   LET v_marca_transferencia = 806; -- marca para transferencia de recurso
   LET v_i_estado_marca = 0;

   --- se cuentan los registros de la tabla temporal de detalle
   SELECT COUNT(*)
   INTO   v_numero_registros
   FROM   safre_tmp:tmp_ret_det_transferencia;

   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO   v_total_registros
   FROM   safre_tmp:tmp_ret_det_transferencia;

   SELECT total_registros
   INTO   v_sumario_total_registros
   FROM   safre_tmp:tmp_ret_sum_transferencia;
      
   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN
      -- se rechaza el lote y no integra
      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET v_c_msj = "No coinciden numero de registros cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado                    = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo                  = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
                     id_codigo       SMALLINT,
                     codigo_rechazo  SMALLINT
   );
      
   -- se obtienen los datos del encabezado
   FOREACH
    SELECT 
     tpo_registro,
     id_servicio,
     tpo_entidad_origen,
     cve_entidad_origen,
     tpo_entidad_destino,
     cve_entidad_destino,
     f_operacion,
     f_valor_transferencia,
     val_aplicacion_aivs,
     resultado_operacion,
     motivo_rech_1,
     motivo_rech_2,
     motivo_rech_3         
    INTO
     tmp_ret_cza_trans_tpo_registro,
     tmp_ret_cza_trans_id_servicio,
     tmp_ret_cza_trans_tpo_entidad_origen,
     tmp_ret_cza_trans_cve_entidad_origen,
     tmp_ret_cza_trans_tpo_entidad_destino,
     tmp_ret_cza_trans_cve_entidad_destino,
     tmp_ret_cza_trans_f_operacion,
     tmp_ret_cza_trans_f_valor_transferencia,
     tmp_ret_cza_trans_val_aplicacion_aivs,
     tmp_ret_cza_trans_resultado_operacion,
     tmp_ret_cza_trans_motivo_rech_1,
     tmp_ret_cza_trans_motivo_rech_2,
     tmp_ret_cza_trans_motivo_rech_3         
    FROM     safre_tmp:tmp_ret_cza_transferencia
 
       -- se asume que no hay error
      LET v_b_rechazo_encabezado                 = 0;
 
       -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
 
      LET ret_cza_trans_folio = p_folio; -- DECIMAL(9,0)           ;
      LET ret_cza_trans_nombre_archivo = p_nombre_archivo; -- CHAR(20)               ;
      LET ret_cza_trans_f_operacion_procesar = tmp_ret_cza_trans_f_operacion; -- DATE                   ;
      LET ret_cza_trans_f_carga = TODAY; -- DATE                   ;
      LET ret_cza_trans_h_carga = CURRENT HOUR TO SECOND; -- DATETIME HOUR TO MINUTE;
      LET ret_cza_trans_f_valor_transferencia = tmp_ret_cza_trans_f_valor_transferencia; -- DATE
      LET ret_cza_trans_precio_fondo = tmp_ret_cza_trans_val_aplicacion_aivs / 1000000; -- DECIMAL(14,6)          ;
      LET ret_cza_trans_total_registros = v_numero_registros;
      LET ret_cza_trans_total_importe = 0; -- se sumara al final
      LET ret_cza_trans_usuario = p_usuario_cod; -- CHAR(20)               ;
 
      -- se establece el valor del fondo
      LET v_valor_fondo = ret_cza_trans_precio_fondo;

      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;
      
      -- =========================================================
      -- validando tipo de registro
      IF ( tmp_ret_cza_trans_tpo_registro <> "01" OR tmp_ret_cza_trans_id_servicio <> "04" ) THEN
         -- se activa la bandera de rechazo de encabezado
         LET v_b_rechazo_encabezado = 1;
      
         -- 1	Tipo de Registro	X	02	00	001	-	002	"01" Encabezado de lote
         IF ( tmp_ret_cza_trans_tpo_registro <> "01" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
         IF ( tmp_ret_cza_trans_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      END IF
      
      -- =========================================================
      -- se verifica si el encabezado contiene el precio del fondo
      IF ( ret_cza_trans_precio_fondo <= 0 ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_precio_fondo);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      
      
      -- =========================================================
      -- se verifica que el valor del fondo segun la fecha de archivo
      -- corresponda a lo encontrado en base de datos
      SELECT precio_fondo
      INTO   ret_cza_trans_rch_precio_fondo
      FROM   glo_valor_fondo
      WHERE  fondo       = 11
      AND    f_valuacion = tmp_ret_cza_trans_f_valor_transferencia;
      
      -- si no son iguales, entonces supone un error
      IF ( ret_cza_trans_rch_precio_fondo <> ret_cza_trans_precio_fondo ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_precio_fondo_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      
      
      -- =========================================================
      -- se verifica si el encabezado contiene fecha procesar
      IF ( tmp_ret_cza_trans_f_operacion IS NULL ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_procesar);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      

      -- =========================================================
      -- se verifica si el encabezado contiene fecha de valuacion
      IF ( tmp_ret_cza_trans_f_valor_transferencia IS NULL ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_valuacion);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      

      -- si hubo rechazo de encabezado no se inserta en historico
      IF ( v_b_rechazo_encabezado = 1 ) THEN

         -- se llenan los datos generales de rechazo del encabezado
         LET ret_cza_trans_rch_folio = p_folio; -- DECIMAL(9,0)                    ;
         LET ret_cza_trans_rch_nombre_archivo = p_nombre_archivo; -- CHAR(20)                        ;
         LET ret_cza_trans_rch_f_operacion_procesar = tmp_ret_cza_trans_f_operacion; -- DATE                            ;
         LET ret_cza_trans_rch_f_carga = TODAY; -- DATE                            ;
         LET ret_cza_trans_rch_h_carga = CURRENT HOUR TO SECOND; -- DATETIME FRACTION TO FRACTION(3);
         LET ret_cza_trans_rch_f_valor_transferencia = tmp_ret_cza_trans_f_valor_transferencia; -- DATE                            ;
         LET ret_cza_trans_rch_precio_fondo = ret_cza_trans_precio_fondo; -- DECIMAL(14,6)                   ;
         LET ret_cza_trans_rch_total_registros = 0; -- INTEGER                         ;
         LET ret_cza_trans_rch_total_importe = 0; -- DECIMAL(18,6)                   ;
         LET ret_cza_trans_rch_usuario = p_usuario_cod; -- CHAR(20)                        ;

         -- se asignan los primeros tres errores
         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;

         -- se leen los tres primeros errores
         FOREACH
            SELECT   FIRST 3
                     id_codigo,
                     codigo_rechazo
            INTO     v_indice_codigos_rechazo, v_codigos_rechazo
            FROM     tmp_codigos_rechazo
            ORDER BY id_codigo
            
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

         LET ret_cza_trans_rch_cod_rechazo_1  = v_motivo_rechazo_1;
         LET ret_cza_trans_rch_cod_rechazo_2  = v_motivo_rechazo_2;
         LET ret_cza_trans_rch_cod_rechazo_3  = v_motivo_rechazo_3;

         -- se inserta el rechazo
         INSERT INTO ret_cza_transferencia_rch (
           folio,
           nombre_archivo,
           f_operacion_procesar,
           f_carga,
           h_carga,
           f_valor_transferencia,
           precio_fondo,
           total_registros,
           total_importe,
           usuario,
           cod_rechazo_1,
           cod_rechazo_2,
           cod_rechazo_3         
         )
         VALUES (
           ret_cza_trans_rch_folio,
           ret_cza_trans_rch_nombre_archivo,
           ret_cza_trans_rch_f_operacion_procesar,
           ret_cza_trans_rch_f_carga,
           ret_cza_trans_rch_h_carga,
           ret_cza_trans_rch_f_valor_transferencia,
           ret_cza_trans_rch_precio_fondo,
           ret_cza_trans_rch_total_registros,
           ret_cza_trans_rch_total_importe,
           ret_cza_trans_rch_usuario,
           ret_cza_trans_rch_cod_rechazo_1,
           ret_cza_trans_rch_cod_rechazo_2,
           ret_cza_trans_rch_cod_rechazo_3         
         );

         -- se cuenta un encabezado rechazado
         LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;
         
         -- el registro fue rechazado y no se inserta en el historico
         CONTINUE FOREACH;

      END IF

      
      -- se inserta en la tabla historica del encabezado de retiros por disposicion
      INSERT INTO ret_cza_transferencia (
        folio,
        nombre_archivo,
        f_operacion_procesar,
        f_carga,
        h_carga,
        f_valor_transferencia,
        precio_fondo,
        total_registros,
        total_importe,
        usuario               
      )
      VALUES (
        ret_cza_trans_folio,
        ret_cza_trans_nombre_archivo,
        ret_cza_trans_f_operacion_procesar,
        ret_cza_trans_f_carga,
        ret_cza_trans_h_carga,
        ret_cza_trans_f_valor_transferencia,
        ret_cza_trans_precio_fondo,
        ret_cza_trans_total_registros,
        ret_cza_trans_total_importe,
        ret_cza_trans_usuario               
      );
 
   END FOREACH;

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_transferencia
   LET v_id_solicitud = 0;
   
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;

   IF v_max_aivs_sobregiro IS NULL THEN
       LET v_max_aivs_sobregiro = 0;
   END IF   
      
   -- se obtienen los datos del detalle
   FOREACH
     SELECT
       tpo_registro,
       id_servicio,
       id_operacion,
       nss,
       curp,
       nombre_trab_datamart,
       nombre_afore,
       paterno_afore,
       materno_afore,
       sec_pension,
       tpo_movimiento,
       regimen,
       tpo_retiro,
       tpo_seguro,
       tpo_pension,
       tpo_prestacion,
       f_inicio_pension,
       f_emision_resol,
       porc_valuacion,
       sem_cotizadas,
       f_carga_datamart,
       diagnostico_reg,
       estatus_subcta,
       periodo_pago,
       acciones_ret97,
       acciones_cv,
       acciones_cuotsol,
       nvl(aiv97,0),
       result_operacion,
       cve_afore,
       motivo_rech1,
       motivo_rech2         
     INTO
       tmp_det_transf_tpo_registro,
       tmp_det_transf_id_servicio,
       tmp_det_transf_id_operacion,
       tmp_det_transf_nss,
       tmp_det_transf_curp,
       tmp_det_transf_nombre_trab_datamart,
       tmp_det_transf_nombre_afore,
       tmp_det_transf_paterno_afore,
       tmp_det_transf_materno_afore,
       tmp_det_transf_sec_pension,
       tmp_det_transf_tpo_movimiento,
       tmp_det_transf_regimen,
       tmp_det_transf_tpo_retiro,
       tmp_det_transf_tpo_seguro,
       tmp_det_transf_tpo_pension,
       tmp_det_transf_tpo_prestacion,
       tmp_det_transf_f_inicio_pension,
       tmp_det_transf_f_emision_resol,
       tmp_det_transf_porc_valuacion,
       tmp_det_transf_sem_cotizadas,
       tmp_det_transf_f_carga_datamart,
       tmp_det_transf_diagnostico_reg,
       tmp_det_transf_estatus_subcta,
       tmp_det_transf_periodo_pago,
       tmp_det_transf_acciones_ret97,
       tmp_det_transf_acciones_cv,
       tmp_det_transf_acciones_cuotsol,
       tmp_det_transf_aiv97,
       tmp_det_transf_result_operacion,
       tmp_det_transf_cve_afore,
       tmp_det_transf_motivo_rech1,
       tmp_det_transf_motivo_rech2         
      FROM safre_tmp:tmp_ret_det_transferencia
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente,
             nombre_af,
             ap_paterno_af,
             ap_materno_af
      INTO   v_id_derechohabiente,
             v_nombre_af,
             v_ap_paterno_af,
             v_ap_materno_af
      FROM   afi_derechohabiente
      WHERE  nss = tmp_det_transf_nss;    
      
      -- el id_transferencia se obtiene de la secuencia de retiros
      LET v_id_solicitud = 0;
      
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         LET v_id_derechohabiente = 0; -- no se encontro el derechohabiente
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      LET v_validar_3_primeros_campos = tmp_det_transf_tpo_registro || tmp_det_transf_id_servicio || tmp_det_transf_id_operacion;
      
      -- si la concatenacion no es igual a la esperada, entonces algun campo es incorrecto
      IF ( v_validar_3_primeros_campos <> "030404" ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
                  
         -- 1	Tipo de Registro	X	02	00	001	-	002	03 Detalle Transacciones
         IF ( tmp_det_transf_tpo_registro <> "03" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
         
         -- 3	Identificador de Operación	X	02	00	005	-	006	04 Transferencia Procesar-Infonavit
         IF ( tmp_det_transf_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
         
         -- 3	Identificador de Operación	X	02	00	005	-	006	08 Disposición Procesar-Infonavit
         IF ( tmp_det_transf_id_operacion <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_operacion_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
      END IF
/*       
      -- se valida que el nombre del derechohabiente, desglosado en apellidos y nombre
      IF ( tmp_det_transf_nombre_afore  <> v_nombre_af  ) THEN
         LET v_b_rechazo_detalle = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nombre_afore_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      IF ( tmp_det_transf_paterno_afore  <> v_ap_paterno_af  ) THEN
         LET v_b_rechazo_detalle = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_apaterno_afore_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      IF ( tmp_det_transf_materno_afore  <> v_ap_materno_af  ) THEN
         LET v_b_rechazo_detalle = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_amaterno_afore_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
*/       
      -- se obtiene el id_matriz_derecho para la combinacion del registro
      SELECT id_ret_matriz_derecho, grupo
      INTO   v_id_ret_matriz_derecho, v_grupo
      FROM   ret_matriz_derecho
      WHERE  tpo_retiro      = tmp_det_transf_tpo_retiro
      AND    regimen         = tmp_det_transf_regimen
      AND    tpo_seguro      = tmp_det_transf_tpo_seguro
      AND    tpo_pension     = tmp_det_transf_tpo_pension
      AND    tpo_prestacion  = tmp_det_transf_tpo_prestacion;
         
      -- si no se encontro, se rechaza
      IF ( v_id_ret_matriz_derecho IS NULL ) THEN
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_matriz_derecho_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
      
      -- la secuencia de pension debe existir y ser numerica
      IF ( tmp_det_transf_sec_pension IS NULL OR (tmp_det_transf_sec_pension < "00" OR tmp_det_transf_sec_pension > "99")) THEN
         -- se rechaza
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_sec_pension_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- la clave de afore tiene que estar en catalogo
      SELECT afore_cod
      INTO   v_afore_cod
      FROM   cat_afore
      WHERE  afore_cod = tmp_det_transf_cve_afore;
         
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
            SELECT   FIRST 3
                     id_codigo,
                     codigo_rechazo
            INTO     v_indice_codigos_rechazo, v_codigos_rechazo
            FROM     tmp_codigos_rechazo
            ORDER BY id_codigo
            
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
               
         -- ==============================================================
         LET ret_transf_rch_id_derechohabiente    = v_id_derechohabiente; -- decimal(9,0) ;
         LET ret_transf_rch_folio                 = p_folio; -- decimal(9,0) ;
         LET ret_transf_rch_nss                   = tmp_det_transf_nss;
         LET ret_transf_rch_regimen               = tmp_det_transf_regimen;
         LET ret_transf_rch_tpo_retiro            = tmp_det_transf_tpo_retiro;
         LET ret_transf_rch_tpo_seguro            = tmp_det_transf_tpo_seguro;
         LET ret_transf_rch_tpo_pension           = tmp_det_transf_tpo_pension;
         LET ret_transf_rch_tpo_prestacion        = tmp_det_transf_tpo_prestacion;
         LET ret_transf_rch_sec_pension           = tmp_det_transf_sec_pension; -- smallint     ;
         LET ret_transf_rch_diag_registro         = tmp_det_transf_diagnostico_reg; -- char(3)      ;
         LET ret_transf_rch_estado_solicitud      = 100; -- rechazada
         LET ret_transf_rch_curp                  = tmp_det_transf_curp; -- char(18)     ;
         LET ret_transf_rch_nombre_datamart       = tmp_det_transf_nombre_trab_datamart; -- char(50)     ;
         LET ret_transf_rch_nombre_afore          = tmp_det_transf_nombre_afore; -- char(40)     ;
         LET ret_transf_rch_paterno_afore         = tmp_det_transf_paterno_afore; -- char(40)     ;
         LET ret_transf_rch_materno_afore         = tmp_det_transf_materno_afore; -- char(40)     ;
         LET ret_transf_rch_tpo_movimiento        = tmp_det_transf_tpo_movimiento; -- char(3)      ;
         LET ret_transf_rch_f_inicio_pension      = tmp_det_transf_f_inicio_pension; -- date         ;
         LET ret_transf_rch_f_resolucion          = tmp_det_transf_f_emision_resol; -- date         ;
         LET ret_transf_rch_porcentaje_valuacion  = tmp_det_transf_porc_valuacion / 100; -- decimal(5,2) ;
         LET ret_transf_rch_semanas_cotizadas     = tmp_det_transf_sem_cotizadas; -- integer      ;
         LET ret_transf_rch_f_carga_datamart      = tmp_det_transf_f_carga_datamart; -- date         ;
         LET ret_transf_rch_estado_sub_viv        = tmp_det_transf_estatus_subcta; -- smallint     ;
         LET ret_transf_rch_aivs_viv97            = tmp_det_transf_aiv97 / 1000000; -- decimal(14,6);
         LET ret_transf_rch_cve_afore             = tmp_det_transf_cve_afore; -- smallint     ;
         LET ret_transf_rch_cod_rechazo           = 0; -- cual es?
         LET ret_transf_rch_cod_rechazo_1         = v_motivo_rechazo_1; -- smallint     ;
         LET ret_transf_rch_cod_rechazo_2         = v_motivo_rechazo_2; -- smallint     ;
         LET ret_transf_rch_cod_rechazo_3         = v_motivo_rechazo_3; -- smallint     ;
                    
         -- se inserta el registro de rechazo
         INSERT INTO ret_transferencia_rch(
           id_derechohabiente,
           folio,
           nss,
           regimen,
           tpo_retiro,
           tpo_seguro,
           tpo_pension,
           tpo_prestacion,
           sec_pension,
           diag_registro,
           estado_solicitud,
           curp,
           nombre_datamart,
           nombre_afore,
           paterno_afore,
           materno_afore,
           tpo_movimiento,
           f_inicio_pension,
           f_resolucion,
           porcentaje_valuacion,
           semanas_cotizadas,
           f_carga_datamart,
           estado_sub_viv,
           aivs_viv97,
           cve_afore,
           cod_rechazo,
           cod_rechazo_1,
           cod_rechazo_2,
           cod_rechazo_3        
         )
         VALUES (
           ret_transf_rch_id_derechohabiente,
           ret_transf_rch_folio,
           ret_transf_rch_nss,
           ret_transf_rch_regimen,
           ret_transf_rch_tpo_retiro,
           ret_transf_rch_tpo_seguro,
           ret_transf_rch_tpo_pension,
           ret_transf_rch_tpo_prestacion,
           ret_transf_rch_sec_pension,
           ret_transf_rch_diag_registro,
           ret_transf_rch_estado_solicitud,
           ret_transf_rch_curp,
           ret_transf_rch_nombre_datamart,
           ret_transf_rch_nombre_afore,
           ret_transf_rch_paterno_afore,
           ret_transf_rch_materno_afore,
           ret_transf_rch_tpo_movimiento,
           ret_transf_rch_f_inicio_pension,
           ret_transf_rch_f_resolucion,
           ret_transf_rch_porcentaje_valuacion,
           ret_transf_rch_semanas_cotizadas,
           ret_transf_rch_f_carga_datamart,
           ret_transf_rch_estado_sub_viv,
           ret_transf_rch_aivs_viv97,
           ret_transf_rch_cve_afore,
           ret_transf_rch_cod_rechazo,
           ret_transf_rch_cod_rechazo_1,
           ret_transf_rch_cod_rechazo_2,
           ret_transf_rch_cod_rechazo_3         
         );

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados
         LET v_pesos_aiv97 = ret_transf_rch_aivs_viv97 * v_valor_fondo;

         -- incremento del importe total
         LET v_sumario_importe_total = v_sumario_importe_total + v_pesos_aiv97;
         
         --se continua la ejecucion
         CONTINUE FOREACH;
      END IF

      -- ==========================================================================
      LET ret_transf_id_solicitud          = v_id_solicitud                       ; 
      LET ret_transf_id_derechohabiente    = v_id_derechohabiente                 ;        
      LET ret_transf_id_ret_matriz_derecho = v_id_ret_matriz_derecho              ; -- revisar cual es
      LET ret_transf_sec_pension           = tmp_det_transf_sec_pension           ;
      LET ret_transf_diag_registro         = tmp_det_transf_diagnostico_reg       ;
      LET ret_transf_folio                 = p_folio                              ;
      LET ret_transf_estado_solicitud      = 30                                   ; -- integrada/recibida procesar
      LET ret_transf_curp                  = tmp_det_transf_curp                  ;  
      LET ret_transf_nombre_datamart       = tmp_det_transf_nombre_trab_datamart  ;
      LET ret_transf_nombre_afore          = tmp_det_transf_nombre_afore          ;
      LET ret_transf_paterno_afore         = tmp_det_transf_paterno_afore         ;
      LET ret_transf_materno_afore         = tmp_det_transf_materno_afore         ;
      LET ret_transf_tpo_movimiento        = tmp_det_transf_tpo_movimiento        ;
      LET ret_transf_f_inicio_pension      = tmp_det_transf_f_inicio_pension      ; 
      LET ret_transf_f_resolucion          = tmp_det_transf_f_emision_resol       ;
      LET ret_transf_porcentaje_valuacion  = tmp_det_transf_porc_valuacion / 100  ; -- DECIMAL(5,2) ;
      LET ret_transf_semanas_cotizadas     = tmp_det_transf_sem_cotizadas         ;
      LET ret_transf_f_carga_datamart      = tmp_det_transf_f_carga_datamart      ;
      LET ret_transf_estado_sub_viv        = tmp_det_transf_estatus_subcta        ;
      LET ret_transf_aivs_viv97            = tmp_det_transf_aiv97 / 1000000       ;  -- DECIMAL(14,6);
      LET ret_transf_cve_afore             = tmp_det_transf_cve_afore             ;
      LET ret_transf_cod_rechazo           = 0                                    ; -- aceptado
      
      -- ==========================================================================
      -- calculo de las AIVs 97 a pesos
      LET v_pesos_aiv97                    = ret_transf_aivs_viv97 * v_valor_fondo;

      -- incremento del importe total
      LET v_sumario_importe_total          = v_sumario_importe_total + v_pesos_aiv97;
      
      -- se obtiene el id_solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   ret_transf_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
      
      -- 31jul2013. Se verifica la suficiencia de saldos
      -- 16jun2015. Se obtienen los saldos de las subcuentas de vivienda 97 y aportaciones voluntarias
      EXECUTE FUNCTION fn_saldo_dia(NULL,
                                    ret_transf_id_derechohabiente,
                                    4, -- viv97
                                    NULL)
                       INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_97_pesos;

      EXECUTE FUNCTION fn_saldo_dia(NULL,
                                    ret_transf_id_derechohabiente,
                                    55, -- aportaciones voluntarias
                                    NULL)
                       INTO v_resultado_consulta, v_saldo_vol_aivs, v_saldo_vol_pesos;

      -- si el saldo es inferior a lo solicitado
      IF ( ret_transf_aivs_viv97 > (v_saldo_97_aivs + v_saldo_vol_aivs)) THEN
	  
         -- 13feb2014. Se tolera hasta 1 AIV de diferencia
         -- 12012015. Se implementa se tome el maximo permitido de la tabla
         IF (v_saldo_97_aivs + v_saldo_vol_aivs) > 0 THEN 
            IF ( ret_transf_aivs_viv97 > (v_saldo_97_aivs + v_saldo_vol_aivs + v_max_aivs_sobregiro) ) THEN
               -- se inserta el registro en la tabla historica de saldos
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud,
                                                              4                      ,
                                                              11                     ,
                                                              v_saldo_97_aivs        ,
                                                              v_saldo_97_pesos       ,
                                                              p_folio                ,
                                                              TODAY                  ,
                                                              CURRENT HOUR TO SECOND );
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud,
                                                              55                     ,
                                                              11                     ,
                                                              v_saldo_vol_aivs        ,
                                                              v_saldo_vol_pesos       ,
                                                              p_folio                ,
                                                              TODAY                  ,
                                                              CURRENT HOUR TO SECOND );

               -- se rechaza la solicitud por saldo insuficiente
               LET ret_transf_cod_rechazo           = v_monto_viv97_invalido; -- saldo insuficiente
               LET ret_transf_estado_solicitud      = 100; -- rechazada

            END IF	  
         ELSE 
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud,
                                                              4                      ,
                                                              11                     ,
                                                              v_saldo_97_aivs        ,
                                                              v_saldo_97_pesos       ,
                                                              p_folio                ,
                                                              TODAY                  ,
                                                              CURRENT HOUR TO SECOND );
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_transf_id_solicitud,
                                                              55                     ,
                                                              11                     ,
                                                              v_saldo_vol_aivs        ,
                                                              v_saldo_vol_pesos       ,
                                                              p_folio                ,
                                                              TODAY                  ,
                                                              CURRENT HOUR TO SECOND );
            LET ret_transf_cod_rechazo           = v_monto_viv97_invalido; -- saldo insuficiente
            LET ret_transf_estado_solicitud      = 100; -- rechazada
         END IF 
                                                        
      END IF

      -- si la solicitud no fue rechazada por saldo insuficiente
      IF ( ret_transf_estado_solicitud = 30 ) THEN
         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_marca_cuenta(
                 ret_transf_id_derechohabiente
                ,v_marca_transferencia -- marca de transferencia
                ,ret_transf_id_solicitud   --seq_ret_solicitud.CURRVAL
                ,ret_transf_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;
            
         -- si no se pudo marcar, se rechaza el registro
         IF ( v_i_estado_marca > 0 ) THEN
            LET ret_transf_estado_solicitud = 100; -- rechazada
         
            -- si el error es por convencia de marca, se verifica si fue por credito, unificacion o separacion
            IF ( v_i_estado_marca = 501 OR v_i_estado_marca = 502 ) THEN
               -- marca de unificacion
               LET ret_transf_cod_rechazo = v_error_det_marca_unificacion;
            ELSE
               -- se revisa si la marca es por un credito de tipo garantia (anualidad o en garantia)
               SELECT tpo_originacion, marca_inf
               INTO   v_tpo_originacion, v_marca_infonavit
               FROM   cat_tipo_credito
               WHERE  tpo_originacion IN (2,4)
               AND    marca_inf = v_i_estado_marca;
                 
               -- si se encontro, es un credito con alguna garantia
               IF ( v_marca_infonavit IS NOT NULL ) THEN
                  LET ret_transf_cod_rechazo = v_error_det_saldo_en_garantia;
               ELSE
                  -- se revisa si es un credito tradicional
                  SELECT tpo_originacion, marca_inf
                  INTO   v_tpo_originacion, v_marca_infonavit
                  FROM   cat_tipo_credito
                  WHERE  tpo_originacion = 1
                  AND    marca_inf = v_i_estado_marca;
               
                  -- si se encontro
                  IF ( v_marca_infonavit IS NOT NULL ) THEN
                     -- el NSS esta en proceso de credito
                     LET ret_transf_cod_rechazo = v_error_det_marca_credito;               
                  ELSE
                     IF ( v_i_estado_marca = 280 ) THEN
                        -- marca de separacion de cuentas
                        LET ret_transf_cod_rechazo = v_error_det_marca_separacion;
                     ELSE
                        -- si se trata de algun retiro por transferencia
                        IF ( v_i_estado_marca = 806 ) THEN
                           -- el registro esta duplicado
                           LET ret_transf_cod_rechazo = v_error_det_registro_duplicado_archivo;
                        ELSE
                           -- se trata de otra marca
                           LET ret_transf_cod_rechazo = v_error_det_marca_no_convive;
                        END IF -- marca de retiro
                     END IF -- separacion de cuentas
                  END IF -- marca credito tradicional
               END IF  -- marca de credito con garantia
            END IF -- marca de unificacion
           
         END IF -- error al marcar
      END IF -- si no se rechazo por saldo insuficiente


      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
      INSERT INTO ret_transferencia (
        id_solicitud,
        id_derechohabiente,
        id_ret_matriz_derecho,
        sec_pension,
        diag_registro,
        folio,
        estado_solicitud,
        curp,
        nombre_datamart,
        nombre_afore,
        paterno_afore,
        materno_afore,
        tpo_movimiento,
        f_inicio_pension,
        f_resolucion,
        porcentaje_valuacion,
        semanas_cotizadas,
        f_carga_datamart,
        estado_sub_viv,
        aivs_viv97,
        cve_afore,
        cod_rechazo            
      )
      VALUES (
        ret_transf_id_solicitud,
        ret_transf_id_derechohabiente,
        ret_transf_id_ret_matriz_derecho,
        ret_transf_sec_pension,
        ret_transf_diag_registro,
        ret_transf_folio,
        ret_transf_estado_solicitud,
        ret_transf_curp,
        ret_transf_nombre_datamart,
        ret_transf_nombre_afore,
        ret_transf_paterno_afore,
        ret_transf_materno_afore,
        ret_transf_tpo_movimiento,
        ret_transf_f_inicio_pension,
        ret_transf_f_resolucion,
        ret_transf_porcentaje_valuacion,
        ret_transf_semanas_cotizadas,
        ret_transf_f_carga_datamart,
        ret_transf_estado_sub_viv,
        ret_transf_aivs_viv97,
        ret_transf_cve_afore,
        ret_transf_cod_rechazo           
      );
      
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

   END FOREACH;
 
   -- se actualiza el importe total para el registro del encabezado
   UPDATE ret_cza_transferencia
   SET    total_importe = v_sumario_importe_total
   WHERE  folio         = p_folio;

   UPDATE STATISTICS FOR TABLE ret_transferencia_rch;
   UPDATE STATISTICS FOR TABLE ret_transferencia;
   UPDATE STATISTICS FOR TABLE ret_cza_transferencia;
   UPDATE STATISTICS FOR TABLE ret_cza_transferencia_rch;
 
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION
;


