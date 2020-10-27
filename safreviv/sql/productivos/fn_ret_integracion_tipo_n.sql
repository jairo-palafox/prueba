






CREATE FUNCTION "safreviv".fn_ret_integracion_tipo_n(p_usuario_cod CHAR(20),
                                          p_folio DECIMAL(9,0),
                                          p_nombre_archivo VARCHAR(40),
                                          p_pid DECIMAL(9,0),
                                          p_proceso_cod SMALLINT) 
   RETURNING SMALLINT, INTEGER, VARCHAR(255)

-- campos de la tabla de encabezado de retiros por tipo_n de recursos (sin filler)
DEFINE tmp_ret_cza_tpo_registro        char(2);
DEFINE tmp_ret_cza_id_servicio         char(2);
DEFINE tmp_ret_cza_tpo_entidad_origen  char(2);
DEFINE tmp_ret_cza_cve_entidad_origen  char(3);
DEFINE tmp_ret_cza_tpo_entidad_destino char(2);
DEFINE tmp_ret_cza_cve_entidad_destino char(3);
DEFINE tmp_ret_cza_f_operacion         date   ;
DEFINE tmp_ret_cza_resultado_operacion char(2);
DEFINE tmp_ret_cza_motivo_rech_1       char(3);
DEFINE tmp_ret_cza_motivo_rech_2       char(3);
DEFINE tmp_ret_cza_motivo_rech_3       char(3);
 
-- campos de la tabla de detalle de retiros por tipo_n de recursos (sin filler)
DEFINE tmp_ret_det_tpo_registro           CHAR(2)      ;
DEFINE tmp_ret_det_id_servicio            CHAR(2)      ;
DEFINE tmp_ret_det_id_operacion           CHAR(2)      ;
DEFINE tmp_ret_det_nss                    CHAR(11)     ;
DEFINE tmp_ret_det_rfc_icefa              CHAR(13)     ;
DEFINE tmp_ret_det_nci_icefa              CHAR(30)     ;
DEFINE tmp_ret_det_cve_icefa              CHAR(3)     ;  
DEFINE tmp_ret_det_nombre_icefa           CHAR(120)    ;
DEFINE tmp_ret_det_nss_id_presentada      CHAR(11)     ;
DEFINE tmp_ret_det_rfc_id_presentada      CHAR(13)     ;
DEFINE tmp_ret_det_nombre_id_presentada   CHAR(120)    ;
DEFINE tmp_ret_det_docto_probatorio       CHAR(1)      ;
DEFINE tmp_ret_det_num_referencia         CHAR(18)     ;
DEFINE tmp_ret_det_origen_retiro          CHAR(1)      ;
DEFINE tmp_ret_det_tpo_seguro             CHAR(2)      ;
DEFINE tmp_ret_det_tpo_pension            CHAR(2)      ;
DEFINE tmp_ret_det_tpo_prestacion         CHAR(2)      ;
DEFINE tmp_ret_det_regimen                CHAR(8)      ;
DEFINE tmp_ret_det_f_inicio_pension       DATE         ;
DEFINE tmp_ret_det_f_resolucion           DATE         ;
DEFINE tmp_ret_det_porc_valuacion         DECIMAL(5,0) ;
DEFINE tmp_ret_det_actuario               CHAR(7)      ;
DEFINE tmp_ret_det_registro_ppp           CHAR(8)      ;
DEFINE tmp_ret_det_importe_ret92          DECIMAL(15,0);
DEFINE tmp_ret_det_aivs_viv92             DECIMAL(15,0);
DEFINE tmp_ret_det_diagnostico            CHAR(3)      ;
DEFINE tmp_ret_det_cve_afore              CHAR(3)      ;
DEFINE tmp_ret_det_num_id_unico           CHAR(11)     ;    
DEFINE tmp_ret_det_result_operacion       CHAR(2)      ;
DEFINE tmp_ret_det_motivo_rech1           CHAR(3)      ;
DEFINE tmp_ret_det_motivo_rech2           CHAR(3)      ;
DEFINE tmp_ret_det_motivo_rech3           CHAR(3)      ;     

-- encabezado de la tabla historica/integrada de retiros por tipo_n de recursos
DEFINE ret_cza_tipo_n_folio                 DECIMAL(9,0)           ;
DEFINE ret_cza_tipo_n_nombre_archivo        CHAR(20)               ;
DEFINE ret_cza_tipo_n_f_operacion_procesar  DATE                   ;
DEFINE ret_cza_tipo_n_f_carga               DATE                   ; 
DEFINE ret_cza_tipo_n_h_carga               DATETIME HOUR TO SECOND;
DEFINE ret_cza_tipo_n_total_registros       INTEGER                ; 
DEFINE ret_cza_tipo_n_total_ret92           DECIMAL(14,6)          ;
DEFINE ret_cza_tipo_n_total_viv92           DECIMAL(18,6)          ;
  
-- detalle de la tabla historica/integrada de retiros por tipo_n de recursos
DEFINE ret_tipo_n_id_solicitud             DECIMAL(9,0)  ;
DEFINE ret_tipo_n_id_decreto               DECIMAL(9,0)  ;
DEFINE ret_tipo_n_diag_registro            CHAR(3)       ;
DEFINE ret_tipo_n_folio                    DECIMAL(9,0)  ;
DEFINE ret_tipo_n_id_ret_matriz_derecho    SMALLINT      ;
DEFINE ret_tipo_n_nss_icefa                CHAR(11)      ;
DEFINE ret_tipo_n_rfc_icefa                CHAR(13)      ;
DEFINE ret_tipo_n_num_ctr_interno          CHAR(30)      ;
DEFINE ret_tipo_n_cve_icefa                CHAR(3)       ;
DEFINE ret_tipo_n_nombre_icefa             CHAR(120)     ;
DEFINE ret_tipo_n_rfc                      CHAR(13)      ;
DEFINE ret_tipo_n_nombre                   CHAR(120)     ;
DEFINE ret_tipo_n_cve_doc_probatorio       CHAR(1)       ;
DEFINE ret_tipo_n_num_referencia           CHAR(18)      ;
DEFINE ret_tipo_n_origen_retiro            CHAR(1)       ;
DEFINE ret_tipo_n_tpo_seguro               CHAR(2)       ;
DEFINE ret_tipo_n_tpo_pension              CHAR(2)       ;
DEFINE ret_tipo_n_tpo_prestacion           CHAR(2)       ;
DEFINE ret_tipo_n_regimen                  SMALLINT      ;
DEFINE ret_tipo_n_f_inicio_pension         DATE          ;
DEFINE ret_tipo_n_f_resolucion             DATE          ;
DEFINE ret_tipo_n_porcentaje_valuacion     DECIMAL(5,2)  ;
DEFINE ret_tipo_n_actuario                 CHAR(7)       ;
DEFINE ret_tipo_n_num_plan_privado         CHAR(8)       ;
DEFINE ret_tipo_n_importe_sar92            DECIMAL(15,2) ;
DEFINE ret_tipo_n_aivs_viv92               DECIMAL(15,6) ;
DEFINE ret_tipo_n_cve_afore                SMALLINT      ;
DEFINE ret_tipo_n_cod_rechazo              SMALLINT      ;
DEFINE ret_tipo_n_estado_solicitud         SMALLINT      ;

-- rechazo de encabezado
DEFINE ret_cza_tipo_n_rch_folio                     DECIMAL(9,0)            ;
DEFINE ret_cza_tipo_n_rch_nombre_archivo            CHAR(20)                ;
DEFINE ret_cza_tipo_n_rch_f_operacion_procesar      DATE                    ;
DEFINE ret_cza_tipo_n_rch_f_carga                   DATE                    ;
DEFINE ret_cza_tipo_n_rch_hora_carga                DATETIME HOUR TO SECOND ;
DEFINE ret_cza_tipo_n_rch_total_registros           INTEGER                 ;
DEFINE ret_cza_tipo_n_rch_total_ret92               DECIMAL(17,2)           ;
DEFINE ret_cza_tipo_n_rch_total_viv92               DECIMAL(18,6)           ;
DEFINE ret_cza_tipo_n_rch_usuario                   CHAR(20)                ;
DEFINE ret_cza_tipo_n_rch_resultado_operacion       SMALLINT                ;
DEFINE ret_cza_tipo_n_rch_cod_rechazo_1             SMALLINT                ;
DEFINE ret_cza_tipo_n_rch_cod_rechazo_2             SMALLINT                ;
DEFINE ret_cza_tipo_n_rch_cod_rechazo_3             SMALLINT                ;

-- rechazo de detalle
DEFINE ret_ipo_n_rch_id_solicitud           DECIMAL(9,0)  ;
DEFINE ret_ipo_n_rch_id_decreto             DECIMAL(9,0)  ;
DEFINE ret_ipo_n_rch_diag_registro          CHAR(3)       ;
DEFINE ret_ipo_n_rch_folio                  DECIMAL(9,0)  ;
DEFINE ret_ipo_n_rch_id_ret_matriz_derecho  SMALLINT      ;
DEFINE ret_ipo_n_rch_nss_icefa              CHAR(11)      ;
DEFINE ret_ipo_n_rch_rfc_icefa              CHAR(13)      ;
DEFINE ret_ipo_n_rch_num_ctr_interno        CHAR(30)      ;
DEFINE ret_ipo_n_rch_cve_icefa              CHAR(3)       ;
DEFINE ret_ipo_n_rch_nombre_icefa           CHAR(120)     ;
DEFINE ret_ipo_n_rch_rfc                    CHAR(13)      ;
DEFINE ret_ipo_n_rch_nombre                 CHAR(120)     ;
DEFINE ret_ipo_n_rch_cve_doc_probatorio     CHAR(1)       ;
DEFINE ret_ipo_n_rch_num_referencia         CHAR(18)      ;
DEFINE ret_ipo_n_rch_origen_retiro          CHAR(1)       ;
DEFINE ret_ipo_n_rch_tpo_seguro             CHAR(2)       ;
DEFINE ret_ipo_n_rch_tpo_pension            CHAR(2)       ;
DEFINE ret_ipo_n_rch_tpo_prestacion         CHAR(2)       ;
DEFINE ret_ipo_n_rch_regimen                SMALLINT      ;
DEFINE ret_ipo_n_rch_f_inicio_pension       DATE          ;
DEFINE ret_ipo_n_rch_f_resolucion           DATE          ;
DEFINE ret_ipo_n_rch_porcentaje_valuacion   DECIMAL(5,2)  ;
DEFINE ret_ipo_n_rch_actuario               CHAR(7)       ;
DEFINE ret_ipo_n_rch_num_plan_privado       CHAR(8)       ;
DEFINE ret_ipo_n_rch_importe_sar92          DECIMAL(15,2) ;
DEFINE ret_ipo_n_rch_aivs_viv92             DECIMAL(15,6) ;
DEFINE ret_ipo_n_rch_cve_afore              SMALLINT      ;
DEFINE ret_ipo_n_rch_num_id_unico           CHAR(11)      ;
DEFINE ret_ipo_n_rch_estado_solicitud       SMALLINT      ;
DEFINE ret_ipo_n_rch_resultado_operacion    SMALLINT      ;
DEFINE ret_ipo_n_rch_cod_rechazo_1          SMALLINT      ;
DEFINE ret_ipo_n_rch_cod_rechazo_2          SMALLINT      ;
DEFINE ret_ipo_n_rch_cod_rechazo_3          SMALLINT      ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_decreto                         DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);

-- para calcular las AIVs a pesos
DEFINE v_valor_fondo         DECIMAL(19,14);
DEFINE v_pesos_aiv92         DECIMAL(14,6);
DEFINE v_saldo_92_aivs       DECIMAL(16,6); -- saldo del derechohabiente en viv92
DEFINE v_saldo_92_pesos      DECIMAL(12,2); -- saldo del derechohabiente en viv97
DEFINE v_resultado_consulta  SMALLINT;


-- para rechazos
DEFINE v_b_rechazo_encabezado               SMALLINT;
DEFINE v_b_rechazo_detalle                  SMALLINT;
DEFINE v_validar_3_primeros_campos          VARCHAR(6); 
DEFINE v_validar_4_siguientes_campos        varchar(10);

-- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                          SMALLINT; -- clave de afore

DEFINE v_id_ret_matriz_derecho  SMALLINT;
DEFINE v_nci                    CHAR(30);


DEFINE v_sumario_importe_total                 DECIMAL(22,6);    
DEFINE v_sumario_viv92                         DECIMAL(22,6);    
DEFINE v_detalle_viv92                         DECIMAL(22,6);    
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(9,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
DEFINE v_saldo_cuenta                          DECIMAL(14,6);

DEFINE v_motivo_rechazo_1                      SMALLINT;
DEFINE v_motivo_rechazo_2                      SMALLINT;
DEFINE v_motivo_rechazo_3                      SMALLINT;
-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo                       CHAR(30); 
-- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo                SMALLINT; 
   
-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados                    SMALLINT; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados                    SMALLINT; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados                    SMALLINT; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    SMALLINT; -- total de registros de detalle rechazados
  
  
-- codigos de error en encabezado
DEFINE v_error_cza_reg_totales_no_coinciden      SMALLINT;
DEFINE v_error_cza_sum_totales_no_coinciden      SMALLINT;
DEFINE v_error_cza_tpo_registro_invalido         SMALLINT;
DEFINE v_error_cza_id_servicio_invalido          SMALLINT;
DEFINE v_error_cza_sin_precio_fondo              SMALLINT;
DEFINE v_error_cza_sin_fecha_procesar            SMALLINT;
DEFINE v_error_cza_sin_fecha_valuacion           SMALLINT;

DEFINE v_error_cza_sin_tpo_entidad_origen        SMALLINT;
DEFINE v_error_cza_sin_cve_entidad_origen        SMALLINT;
DEFINE v_error_cza_sin_tpo_entidad_destino       SMALLINT;
DEFINE v_error_cza_sin_cve_entidad_destino       SMALLINT;

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
DEFINE v_error_det_nci_diferente                 SMALLINT;
DEFINE v_error_det_registro_duplicado_archivo    SMALLINT;
DEFINE v_monto_viv92_invalido                    SMALLINT; -- saldo insuficiente
DEFINE v_max_aivs_sobregiro                      DECIMAL(12,6);  --cantidad maxima de aivs para sobregiro

-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_tipo_n                            INTEGER; -- 804 de acuerdo a catalogo
-- para validar marca de credito
DEFINE v_tpo_originacion SMALLINT;
DEFINE v_marca_infonavit SMALLINT;


-- para calcular precio del fondo
DEFINE v_fecha_valuacion_nueva      VARCHAR(10); -- fecha de valuacion nueva
DEFINE v_mes_en_texto               VARCHAR(2);
DEFINE v_fecha_valuacion            DATE; -- fecha de valuacion
DEFINE v_mes                        SMALLINT;
DEFINE v_ano                        INT;

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT    ;
DEFINE sql_err                                   INTEGER     ;
DEFINE isam_err                                  INTEGER     ;
DEFINE err_txt                                   VARCHAR(200);
DEFINE v_c_msj                                   VARCHAR(200);

   -- se declara e
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION

   -- se asume que no hay errores
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finalizó correctamente';
 
 
   --SET DEBUG FILE TO "/safreviv_int/BD/debug_ret_tipo_n.trace";
   
   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados  = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados  = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden      = 1000;
   LET v_error_cza_sum_totales_no_coinciden      = 2000;
   LET v_error_cza_tpo_registro_invalido         = 2;
   LET v_error_cza_id_servicio_invalido          = 3;
   LET v_error_cza_sin_precio_fondo              = 4;
   LET v_error_cza_sin_fecha_procesar            = 5;
   LET v_error_cza_sin_fecha_valuacion           = 6;
   LET v_error_cza_sin_tpo_entidad_origen        = 7;
   LET v_error_cza_sin_cve_entidad_origen        = 8;
   LET v_error_cza_sin_tpo_entidad_destino       = 9;
   LET v_error_cza_sin_cve_entidad_destino       = 10;

 
   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 49; -- segun PROCESAR
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_det_id_servicio_invalido          = 3;
   LET v_error_det_id_operacion_invalido         = 4;
   LET v_error_det_matriz_derecho_no_encontrado  = 5;
   LET v_error_det_sec_pension_invalido          = 6;
   LET v_error_det_fec_solicitud_invalido        = 7;
   LET v_error_det_afore_invalido                = 8;
   LET v_error_det_lote_invalido                 = 9;
   LET v_error_det_marca_unificacion             = 597; -- NSS esta en unificacion

   LET v_error_det_saldo_en_garantia             = 961; -- NSS tiene un saldo en garantia
   LET v_error_det_marca_no_convive              = 599; -- NSS esta en otro proceso operativo
   LET v_error_det_en_devolucion_de_subcuenta    = 598; -- NSS esta en un proceso de retiro
   LET v_error_det_subcuenta_no_corresoponde     = 600; -- la subcuenta no corresponde al tipo de retiro
   
   LET v_error_det_marca_credito                 = 100; -- NSS tiene un credito vigente  
   LET v_error_det_marca_separacion              = 201; -- NSS esta en separacion
   LET v_error_det_apaterno_afore_invalido       = 758; -- apellido paterno afore no coincide con el de base de datos
   LET v_error_det_amaterno_afore_invalido       = 759; -- apellido materno afore no coincide con el de base de datos
   LET v_error_det_nombre_afore_invalido         = 760; -- nombre afore no coincide con el de base de datos
   LET v_error_det_nci_diferente                 = 763;
   LET v_error_det_registro_duplicado_archivo    = 768; -- registro duplicado
   LET v_monto_viv92_invalido                    = 766; -- saldo insuficiente

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1
   AND    folio          IS NULL; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_proceso
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    pid         = p_pid;

   SELECT diferencia_maxima
     INTO v_max_aivs_sobregiro
     FROM ret_sobregiro_max_permitido
    WHERE proceso_cod = p_proceso_cod;

   
   -- se inician las variables para marca
   LET v_marca_tipo_n    = 804; -- marca para tipo_n
   LET v_i_estado_marca  = 0;

  --TRACE "cuenta registros en encabezado y detalle";

   --- se cuentan los registros de la tabla temporal de detalle
   SELECT COUNT(*)
   INTO   v_numero_registros
   FROM   safre_tmp:tmp_ret_det_tipo_n;

   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO   v_total_registros
   FROM   safre_tmp:tmp_ret_det_tipo_n;
   
   SELECT total_registros
   INTO   v_sumario_total_registros
   FROM   safre_tmp:tmp_ret_sum_tipo_n;
 
   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN
      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET isam_err       = 0;
      LET v_c_msj        = 'El número de registros cargados contra los reportados en archivo no coinciden';

      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- TRACE "Obtiene los montos del sumario y el detalle";

   
   -- se verifica que los montos sean iguales, la suma del detalle vs el monto declarado en sumario
   SELECT SUM(total_viv92/1000000)
   INTO   v_sumario_viv92
   FROM   safre_tmp:tmp_ret_sum_tipo_n;
   
   SELECT SUM(aivs_viv92/1000000)
   INTO   v_detalle_viv92
   FROM   safre_tmp:tmp_ret_det_tipo_n;

   -- si no coinciden los montos es un error
   IF ( v_sumario_viv92 <> v_detalle_viv92 ) THEN
      LET v_si_resultado = v_error_cza_sum_totales_no_coinciden;
      LET isam_err       = 0;
      LET v_c_msj        = 'El total de AIVs en detalle no coincide con el total de AIVs en el sumario';

      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF


   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   --TRACE "Crea la tabla para los rechazos";
   
   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   DROP TABLE IF EXISTS tmp_codigos_rechazo;
   CREATE TEMP TABLE tmp_codigos_rechazo (
     id_codigo       SMALLINT,
     codigo_rechazo  SMALLINT
   );
   
  --TRACE "busca encabezado";
   
   -- se obtienen los datos del encabezado
   FOREACH
   SELECT 
      tpo_registro                        ,
      id_servicio                         ,
      tpo_entidad_origen                  ,
      cve_entidad_origen                  ,
      tpo_entidad_destino                 ,
      cve_entidad_destino                 ,
      f_operacion                         ,
      resultado_operacion                 ,
      motivo_rech_1                       ,
      motivo_rech_2                       ,
      motivo_rech_3                       
      
   INTO
      tmp_ret_cza_tpo_registro            ,
      tmp_ret_cza_id_servicio             ,
      tmp_ret_cza_tpo_entidad_origen      ,
      tmp_ret_cza_cve_entidad_origen      ,
      tmp_ret_cza_tpo_entidad_destino     ,
      tmp_ret_cza_cve_entidad_destino     ,
      tmp_ret_cza_f_operacion             ,
      tmp_ret_cza_resultado_operacion     ,
      tmp_ret_cza_motivo_rech_1           ,
      tmp_ret_cza_motivo_rech_2           ,
      tmp_ret_cza_motivo_rech_3           
   FROM
      safre_tmp:tmp_ret_cza_tipo_n
 
      -- se asume que no hay error
      LET v_b_rechazo_encabezado = 0;
      
      -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
 
      -- se asignan los datos al registro de encabezado historico
      LET ret_cza_tipo_n_folio                  = p_folio;
      LET ret_cza_tipo_n_nombre_archivo         = p_nombre_archivo;
      LET ret_cza_tipo_n_f_operacion_procesar   = tmp_ret_cza_f_operacion;
      LET ret_cza_tipo_n_f_carga                = TODAY;
      LET ret_cza_tipo_n_h_carga                = CURRENT HOUR TO MINUTE;      
      LET ret_cza_tipo_n_total_ret92            = 0 ;
      LET ret_cza_tipo_n_total_viv92            = 0 ;
	    LET ret_cza_tipo_n_total_registros        = v_numero_registros; -- numero de registros
 
      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;
      LET v_validar_4_siguientes_campos = tmp_ret_cza_tpo_entidad_origen || tmp_ret_cza_cve_entidad_origen || tmp_ret_cza_tpo_entidad_destino ||tmp_ret_cza_cve_entidad_destino;
      
      --TRACE("Validando encabezado");
      -- =========================================================
      -- validando tipo de registro
      IF ( tmp_ret_cza_tpo_registro <> "01" OR tmp_ret_cza_id_servicio <> "04" ) THEN
         -- se activa la bandera de rechazo de encabezado
         LET v_b_rechazo_encabezado = 1;
      
         -- 1	Tipo de Registro	X	02	00	001	-	002	"01" Encabezado de lote
         IF ( tmp_ret_cza_tpo_registro <> "01" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_tpo_registro_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         -- 2	Identificador de Servicio	X	02	00	003	-	004	"04" Retiros
         IF ( tmp_ret_cza_id_servicio <> "04" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_id_servicio_invalido);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      END IF
          
      -- =========================================================
            --TRACE("Validando entidad");
      -- =========================================================
      --tmp_ret_cza_tpo_entidad_origen , tmp_ret_cza_cve_entidad_origen , tmp_ret_cza_tpo_entidad_destino , tmp_ret_cza_cve_entidad_destino
      -- validando tipo de registro
      IF ( v_validar_4_siguientes_campos <> "0300104002" ) THEN
         -- se activa la bandera de rechazo de encabezado
         LET v_b_rechazo_encabezado = 1;
                        
         IF ( tmp_ret_cza_tpo_entidad_origen <> "03" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_tpo_entidad_origen);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         IF ( tmp_ret_cza_cve_entidad_origen <> "001" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_cve_entidad_origen);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF

         IF ( tmp_ret_cza_tpo_entidad_destino <> "04" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_tpo_entidad_destino);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;           
         END IF
         
         -- validando identificador de servicio
         IF ( tmp_ret_cza_cve_entidad_destino <> "002" ) THEN
           INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_cve_entidad_destino);

           -- se incrementa el indice
           LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
         END IF
      END IF
      
      -- se verifica si el encabezado contiene fecha procesar
      IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
        INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_procesar);

        -- se incrementa el indice
        LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;  
      END IF      

      
      -- si hubo rechazo de encabezado no se inserta en historico
      IF ( v_b_rechazo_encabezado = 1 ) THEN
         
         -- se llenan los datos generales de rechazo del encabezado 
          LET ret_cza_tipo_n_rch_folio                 = p_folio                    ; -- DECIMAL(9,0)                    ;
          LET ret_cza_tipo_n_rch_nombre_archivo        = p_nombre_archivo           ; -- CHAR(20)                        ;
          LET ret_cza_tipo_n_rch_f_operacion_procesar  = tmp_ret_cza_f_operacion    ;
          LET ret_cza_tipo_n_rch_f_carga               = TODAY                      ; -- DATE                            ;
          LET ret_cza_tipo_n_rch_hora_carga            = CURRENT HOUR TO SECOND     ; -- DATETIME FRACTION TO FRACTION(3);
          LET ret_cza_tipo_n_rch_total_registros       = 0                          ;             
          LET ret_cza_tipo_n_rch_total_ret92           = 0                          ;
          LET ret_cza_tipo_n_rch_total_viv92           = 0                          ;
          LET ret_cza_tipo_n_rch_usuario               = p_usuario_cod              ; -- CHAR(20) 
          LET ret_cza_tipo_n_rch_resultado_operacion   = 2                          ; -- rechazado

         -- se asignan los primeros tres errores
         LET v_motivo_rechazo_1 = 0    ;
         LET v_motivo_rechazo_2 = 0    ;
         LET v_motivo_rechazo_3 = 0    ;

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
    
         LET ret_cza_tipo_n_rch_cod_rechazo_1  = v_motivo_rechazo_1;
         LET ret_cza_tipo_n_rch_cod_rechazo_2  = v_motivo_rechazo_2;
         LET ret_cza_tipo_n_rch_cod_rechazo_3  = v_motivo_rechazo_3;

         -- se inserta el rechazo
         INSERT INTO ret_cza_tipo_n_rch (
              folio                      ,
              nombre_archivo             ,      
              f_operacion_procesar       ,      
              f_carga                    ,      
              hora_carga                 ,      
              total_registros            ,      
              total_ret92                ,      
              total_viv92                ,      
              usuario                    ,      
              resultado_operacion        ,      
              cod_rechazo_1              ,      
              cod_rechazo_2              ,      
              cod_rechazo_3                     
         )
         VALUES (    
             ret_cza_tipo_n_rch_folio                 ,
             ret_cza_tipo_n_rch_nombre_archivo        ,
             ret_cza_tipo_n_rch_f_operacion_procesar  ,
             ret_cza_tipo_n_rch_f_carga               ,
             ret_cza_tipo_n_rch_hora_carga            ,
             ret_cza_tipo_n_rch_total_registros       ,
             ret_cza_tipo_n_rch_total_ret92           ,
             ret_cza_tipo_n_rch_total_viv92           ,
             ret_cza_tipo_n_rch_usuario               ,
             ret_cza_tipo_n_rch_resultado_operacion   ,
             ret_cza_tipo_n_rch_cod_rechazo_1         ,
             ret_cza_tipo_n_rch_cod_rechazo_2         ,
             ret_cza_tipo_n_rch_cod_rechazo_3
         );
       
         -- se cuenta un encabezado rechazado
         LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;
         
         -- el registro fue rechazado y no se inserta en el historico
         CONTINUE FOREACH;

      END IF
      
      
      -- se inserta en la tabla historica del encabezado de retiros  tipo_n 
      INSERT INTO ret_cza_tipo_n (
         folio                              ,
         nombre_archivo                     ,
         f_operacion_procesar               ,
         f_carga                            ,
         hora_carga                         ,
         total_registros                    ,
         total_ret92                        ,
         total_viv92                        
      ) 
      VALUES (
         ret_cza_tipo_n_folio                 ,
         ret_cza_tipo_n_nombre_archivo        ,
         ret_cza_tipo_n_f_operacion_procesar  ,
         ret_cza_tipo_n_f_carga               ,
         ret_cza_tipo_n_h_carga               ,
         ret_cza_tipo_n_total_registros       ,
         ret_cza_tipo_n_total_ret92           ,
         ret_cza_tipo_n_total_viv92           
      );
      
      -- se cuenta un encabezado insertado
      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;
 
   END FOREACH;
   -- TRACE "Proceso el encabezado";

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;

   IF v_max_aivs_sobregiro IS NULL THEN
       LET v_max_aivs_sobregiro = 0;
   END IF

   
   -- se calcula el primer dia del mes siguiente de la fecha de operacion procesar
   -- MM/DD/YYYY
   LET v_mes = MONTH(ret_cza_tipo_n_f_operacion_procesar);
   LET v_ano = YEAR(ret_cza_tipo_n_f_operacion_procesar);
   -- se incrementa el mes
   IF ( v_mes < 12 ) THEN
      -- mes siguiente
      LET v_mes = v_mes + 1;
   ELSE
      -- el mes que le sigue a diciembre es enero (del siguiente año --20140102 Eneas)
      LET v_mes = 1;
      LET v_ano = v_ano + 1;
   END IF
   
   -- se formatea la fecha
   IF ( v_mes < 10 ) THEN
      LET v_mes_en_texto = "0" || v_mes;
   ELSE
      LET v_mes_en_texto = v_mes;
   END IF
   
   LET v_fecha_valuacion_nueva = v_mes_en_texto || "/01/" || v_ano;
   
   LET v_fecha_valuacion = DATE(v_fecha_valuacion_nueva);
 
   -- se obtiene el valor del fondo del dia
   SELECT precio_fondo
   INTO   v_valor_fondo
   FROM   glo_valor_fondo
   WHERE  fondo = 11
   AND    f_valuacion = v_fecha_valuacion;

--TRACE "foreach del detalle";

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
     tpo_registro                    ,     
     id_servicio                     ,     
     id_operacion                    ,     
     nss_icefa                       ,     
     rfc_icefa                       ,     
     TRIM(nci_icefa)                 ,     
     cve_icefa                       ,     
     nombre_icefa                    ,     
     nss_id_presentada               ,     
     rfc_id_presentada               ,     
     nombre_id_presentada            ,     
     docto_probatorio                ,     
     num_referencia                  ,     
     origen_retiro                   ,     
     tpo_seguro                      ,     
     tpo_pension                     ,     
     tpo_prestacion                  ,     
     regimen                         ,     
     f_inicio_pension                ,     
     f_resolucion                    ,     
     porc_valuacion                  ,     
     actuario                        ,     
     registro_ppp                    ,     
     nvl(importe_ret92,0)            ,     
     nvl(aivs_viv92,0)               ,     
     diagnostico                     ,     
     cve_afore                       ,     
     num_id_unico                    ,
     result_operacion                ,     
     motivo_rech1                    ,     
     motivo_rech2                    ,     
     motivo_rech3                          
                                        
   INTO                                 
    tmp_ret_det_tpo_registro           , 
    tmp_ret_det_id_servicio            ,
    tmp_ret_det_id_operacion           ,
    tmp_ret_det_nss                    ,
    tmp_ret_det_rfc_icefa              ,
    tmp_ret_det_nci_icefa              ,
    tmp_ret_det_cve_icefa              ,
    tmp_ret_det_nombre_icefa           ,
    tmp_ret_det_nss_id_presentada      ,
    tmp_ret_det_rfc_id_presentada      ,
    tmp_ret_det_nombre_id_presentada   ,
    tmp_ret_det_docto_probatorio       ,
    tmp_ret_det_num_referencia         ,
    tmp_ret_det_origen_retiro          ,
    tmp_ret_det_tpo_seguro             ,
    tmp_ret_det_tpo_pension            ,
    tmp_ret_det_tpo_prestacion         ,
    tmp_ret_det_regimen                ,
    tmp_ret_det_f_inicio_pension       ,
    tmp_ret_det_f_resolucion           ,
    tmp_ret_det_porc_valuacion         ,
    tmp_ret_det_actuario               ,
    tmp_ret_det_registro_ppp           ,
    tmp_ret_det_importe_ret92          ,
    tmp_ret_det_aivs_viv92             ,
    tmp_ret_det_diagnostico            ,
    tmp_ret_det_cve_afore              ,
    tmp_ret_det_num_id_unico           ,
    tmp_ret_det_result_operacion       ,
    tmp_ret_det_motivo_rech1           ,
    tmp_ret_det_motivo_rech2           ,
    tmp_ret_det_motivo_rech3       

   FROM
      safre_tmp:tmp_ret_det_tipo_n
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle    = 0;
    
      -- ==========================================================================
      -- ==========================================================================
      -- se busca el NSS en afi_decreto
      -- ==========================================================================
      SELECT id_decreto, TRIM(nci)
      INTO   v_id_decreto, v_nci
      FROM   afi_decreto
      WHERE consec_cuenta = tmp_ret_det_num_id_unico ;
          
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;
      
      --TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;
  
      -- si no se encontro el id_derechohabiente
      IF ( v_id_decreto IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_id_decreto           = 0;
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      LET v_validar_3_primeros_campos = tmp_ret_det_tpo_registro || tmp_ret_det_id_servicio || tmp_ret_det_id_operacion;
      
      -- si la concatenacion no es igual a la esperada, entonces algun campo es incorrecto
      IF ( v_validar_3_primeros_campos <> "030423" AND v_validar_3_primeros_campos <> "030424" ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
                  
                  
         -- 1	Tipo de Registro
         IF ( tmp_ret_det_tpo_registro <> "02" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_tpo_registro_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 2	Identificador de Servicio	
         IF ( tmp_ret_det_id_servicio <> "04" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_servicio_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
         
         -- 3	Identificador de Operación	
         IF ( tmp_ret_det_id_operacion <> "23" AND tmp_ret_det_id_operacion <> "24" ) THEN
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_id_operacion_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

         END IF
      END IF
              
      -- se verifica que los NCI coincidan
      IF ( tmp_ret_det_nci_icefa <> v_nci ) THEN
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nci_diferente);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;         
      END IF
              
--TRACE "obteniendo matriz de derecho";
                  
      -- se obtiene el id_matriz_derecho para la combinacion del registro
      SELECT id_ret_matriz_derecho
      INTO   v_id_ret_matriz_derecho
      FROM   ret_matriz_derecho
      WHERE  tpo_retiro           = "N"
      AND    regimen              = tmp_ret_det_regimen
      AND    tpo_seguro           = tmp_ret_det_tpo_seguro
      AND    tpo_pension          = tmp_ret_det_tpo_pension
      AND    tpo_prestacion       = tmp_ret_det_tpo_prestacion;
         
      -- si no se encontro, se rechaza
      IF ( v_id_ret_matriz_derecho IS NULL ) THEN
          LET v_id_ret_matriz_derecho = 0;    --- PROINFXVII-54, se deja de validar si existe en la matriz de derechos
--         LET v_b_rechazo_detalle    = 1;
--         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_matriz_derecho_no_encontrado);

         -- se incrementa el indice
--         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;

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
               
         -- se asignan los datos al registro de rechazo de detalle   
         LET ret_ipo_n_rch_id_solicitud           = v_id_solicitud                ; -- decimal(9,0) ;             
         LET ret_ipo_n_rch_id_decreto             = v_id_decreto                  ; -- decimal(9,0) ;      
         LET ret_ipo_n_rch_diag_registro          = tmp_ret_det_diagnostico       ;  
         LET ret_ipo_n_rch_folio                  = p_folio                       ;
         LET ret_ipo_n_rch_id_ret_matriz_derecho  = v_id_ret_matriz_derecho       ;
         LET ret_ipo_n_rch_nss_icefa              = tmp_ret_det_nss               ;  
         LET ret_ipo_n_rch_rfc_icefa              = tmp_ret_det_rfc_icefa         ; 
         LET ret_ipo_n_rch_num_ctr_interno        = tmp_ret_det_nci_icefa         ;   
         LET ret_ipo_n_rch_cve_icefa              = tmp_ret_det_cve_icefa         ;
         LET ret_ipo_n_rch_nombre_icefa           = tmp_ret_det_nombre_icefa      ;
         LET ret_ipo_n_rch_rfc                    = tmp_ret_det_rfc_id_presentada ;
         LET ret_ipo_n_rch_nombre                 = tmp_ret_det_nombre_id_presentada;
         LET ret_ipo_n_rch_cve_doc_probatorio     = tmp_ret_det_docto_probatorio  ; 
         LET ret_ipo_n_rch_num_referencia         = tmp_ret_det_num_referencia    ;
         LET ret_ipo_n_rch_origen_retiro          = tmp_ret_det_origen_retiro     ;
         LET ret_ipo_n_rch_tpo_seguro             = tmp_ret_det_tpo_seguro        ;   
         LET ret_ipo_n_rch_tpo_pension            = tmp_ret_det_tpo_pension       ;   
         LET ret_ipo_n_rch_tpo_prestacion         = tmp_ret_det_tpo_prestacion    ;   
         LET ret_ipo_n_rch_regimen                = tmp_ret_det_regimen           ;   
         LET ret_ipo_n_rch_f_inicio_pension       = tmp_ret_det_f_inicio_pension  ;   
         LET ret_ipo_n_rch_f_resolucion           = tmp_ret_det_f_resolucion      ;   
         LET ret_ipo_n_rch_porcentaje_valuacion   = tmp_ret_det_porc_valuacion / 100;   
         LET ret_ipo_n_rch_actuario               = tmp_ret_det_actuario          ;   
         LET ret_ipo_n_rch_num_plan_privado       = tmp_ret_det_registro_ppp      ;   
         LET ret_ipo_n_rch_importe_sar92          = tmp_ret_det_importe_ret92 / 100;  
         LET ret_ipo_n_rch_aivs_viv92             = tmp_ret_det_aivs_viv92  / 1000000;  
         LET ret_ipo_n_rch_cve_afore              = tmp_ret_det_cve_afore         ;
         LET ret_ipo_n_rch_num_id_unico           = tmp_ret_det_num_id_unico      ;
         LET ret_ipo_n_rch_estado_solicitud       = 100                           ;         LET ret_ipo_n_rch_resultado_operacion    = 2                             ;         LET ret_ipo_n_rch_cod_rechazo_1          = v_motivo_rechazo_1            ; 
         LET ret_ipo_n_rch_cod_rechazo_2          = v_motivo_rechazo_2            ; 
         LET ret_ipo_n_rch_cod_rechazo_3          = v_motivo_rechazo_3            ; 
        -- TRACE "Inserta el rechazo >" || tmp_ret_det_num_id_unico || "<";
         -- se inserta el registro de rechazo
         INSERT INTO ret_tipo_n_rch(
            id_solicitud           ,
            id_decreto             ,
            diag_registro          ,
            folio                  ,
            id_ret_matriz_derecho  ,
            nss_icefa              ,
            rfc_icefa              ,
            num_ctr_interno        ,
            cve_icefa              ,
            nombre_icefa           ,
            rfc                    ,
            nombre                 ,
            cve_doc_probatorio     ,
            num_referencia         ,
            origen_retiro          ,
            tpo_seguro             ,
            tpo_pension            ,
            tpo_prestacion         ,
            regimen                ,
            f_inicio_pension       ,
            f_resolucion           ,
            porcentaje_valuacion   ,
            actuario               ,
            num_plan_privado       ,
            importe_sar92          ,
            aivs_viv92             ,
            cve_afore              ,
            num_id_unico           ,
            estado_solicitud       ,
            resultado_operacion    ,
            cod_rechazo_1          ,
            cod_rechazo_2          ,
            cod_rechazo_3           
         )
         VALUES (
            seq_ret_solicitud.NEXTVAL             ,
            ret_ipo_n_rch_id_decreto              ,
            ret_ipo_n_rch_diag_registro           ,
            ret_ipo_n_rch_folio                   ,
            ret_ipo_n_rch_id_ret_matriz_derecho   ,
            ret_ipo_n_rch_nss_icefa               ,
            ret_ipo_n_rch_rfc_icefa               ,
            ret_ipo_n_rch_num_ctr_interno         ,
            ret_ipo_n_rch_cve_icefa               ,
            ret_ipo_n_rch_nombre_icefa            ,
            ret_ipo_n_rch_rfc                     ,
            ret_ipo_n_rch_nombre                  ,
            ret_ipo_n_rch_cve_doc_probatorio      ,
            ret_ipo_n_rch_num_referencia          ,
            ret_ipo_n_rch_origen_retiro           ,
            ret_ipo_n_rch_tpo_seguro              ,
            ret_ipo_n_rch_tpo_pension             ,
            ret_ipo_n_rch_tpo_prestacion          ,
            ret_ipo_n_rch_regimen                 ,
            ret_ipo_n_rch_f_inicio_pension        ,
            ret_ipo_n_rch_f_resolucion            ,
            ret_ipo_n_rch_porcentaje_valuacion    ,
            ret_ipo_n_rch_actuario                ,
            ret_ipo_n_rch_num_plan_privado        ,
            ret_ipo_n_rch_importe_sar92           ,
            ret_ipo_n_rch_aivs_viv92              ,
            ret_ipo_n_rch_cve_afore               ,
            ret_ipo_n_rch_num_id_unico            ,
            ret_ipo_n_rch_estado_solicitud        ,
            ret_ipo_n_rch_resultado_operacion     ,
            ret_ipo_n_rch_cod_rechazo_1           ,
            ret_ipo_n_rch_cod_rechazo_2           ,
            ret_ipo_n_rch_cod_rechazo_3           
         );

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- se obtienen las AIVs del registro rechazado
         LET ret_tipo_n_aivs_viv92 = (tmp_ret_det_aivs_viv92 / 1000000);
         
         -- se convierten a pesos
         LET v_pesos_aiv92 = ret_tipo_n_aivs_viv92 * v_valor_fondo;
         
         -- incremento del importe total
         LET v_sumario_importe_total = v_sumario_importe_total + ret_tipo_n_aivs_viv92;

         -- si fue rechazado no se inserta en el historico
         CONTINUE FOREACH;
      END IF
    
      -- ==========================================================================
      -- REGISTRO ACEPTADO
      -- ==========================================================================
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
      -- TRACE "Inserta el aceptado >" || tmp_ret_det_num_id_unico || "<";
      -- Asignando datos a registro de detalle tipo_n
      LET ret_tipo_n_id_solicitud           = v_id_solicitud                    ;
      LET ret_tipo_n_id_decreto             = v_id_decreto                      ;
      LET ret_tipo_n_diag_registro          = tmp_ret_det_diagnostico           ;
      LET ret_tipo_n_folio                  = p_folio                           ;
      LET ret_tipo_n_id_ret_matriz_derecho  = v_id_ret_matriz_derecho           ;
      LET ret_tipo_n_nss_icefa              = tmp_ret_det_nss                   ;
      LET ret_tipo_n_rfc_icefa              = tmp_ret_det_rfc_icefa             ;
      LET ret_tipo_n_num_ctr_interno        = tmp_ret_det_nci_icefa             ;
      LET ret_tipo_n_cve_icefa              = tmp_ret_det_cve_icefa             ;
      LET ret_tipo_n_nombre_icefa           = tmp_ret_det_nombre_icefa          ;
      LET ret_tipo_n_rfc                    = tmp_ret_det_rfc_id_presentada     ;
      LET ret_tipo_n_nombre                 = tmp_ret_det_nombre_id_presentada  ;   
      LET ret_tipo_n_cve_doc_probatorio     = tmp_ret_det_docto_probatorio      ;
      LET ret_tipo_n_num_referencia         = tmp_ret_det_num_referencia        ;
      LET ret_tipo_n_origen_retiro          = tmp_ret_det_origen_retiro         ;
      LET ret_tipo_n_tpo_seguro             = tmp_ret_det_tpo_seguro            ; 
      LET ret_tipo_n_tpo_pension            = tmp_ret_det_tpo_pension           ; 
      LET ret_tipo_n_tpo_prestacion         = tmp_ret_det_tpo_prestacion        ; 
      LET ret_tipo_n_regimen                = tmp_ret_det_regimen               ; 
      LET ret_tipo_n_f_inicio_pension       = tmp_ret_det_f_inicio_pension      ; 
      LET ret_tipo_n_f_resolucion           = tmp_ret_det_f_resolucion          ; 
      LET ret_tipo_n_porcentaje_valuacion   = tmp_ret_det_porc_valuacion /100   ;    
      LET ret_tipo_n_actuario               = tmp_ret_det_actuario              ;    
      LET ret_tipo_n_num_plan_privado       = tmp_ret_det_registro_ppp          ;    
      LET ret_tipo_n_importe_sar92          = tmp_ret_det_importe_ret92 /100    ;    
      LET ret_tipo_n_aivs_viv92             = tmp_ret_det_aivs_viv92    /1000000;    
      LET ret_tipo_n_cve_afore              = tmp_ret_det_cve_afore             ;
      LET ret_tipo_n_cod_rechazo            = 0                                 ;
      LET ret_tipo_n_estado_solicitud       = 30                                ; -- RECIBIDA [integrada](ret_estado_solicitud)

      -- calculo de las AIVs 97 y 92 a pesos
      LET ret_tipo_n_aivs_viv92            = (tmp_ret_det_aivs_viv92 / 1000000);
      LET v_pesos_aiv92 = ret_tipo_n_aivs_viv92 * v_valor_fondo;

      -- incremento del importe total
      LET v_sumario_importe_total = v_sumario_importe_total + ret_tipo_n_aivs_viv92;

      -- 31jul2013 validacion de suficiencia de saldo
      -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
      EXECUTE FUNCTION fn_saldo_dia_cta_decreto(NULL,
                                                ret_tipo_n_id_decreto,
                                                48,
                                                NULL)
                       INTO v_resultado_consulta, v_saldo_92_aivs, v_saldo_92_pesos;

      -- se calcula la diferencia en AIVs
      IF ( ret_tipo_n_aivs_viv92 > v_saldo_92_aivs ) THEN

         -- 02dic2013. Se tolera hasta 1 AIV de diferencia
         IF ( ret_tipo_n_aivs_viv92 > (v_saldo_92_aivs + v_max_aivs_sobregiro) ) THEN
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_tipo_n_id_solicitud,48,11,
                                                           v_saldo_92_aivs, v_saldo_92_pesos,
                                                           p_folio,TODAY,CURRENT HOUR TO SECOND);

            -- se rechaza la solicitud por insuficiencia de saldo
            LET ret_tipo_n_cod_rechazo      = v_monto_viv92_invalido;
            LET ret_tipo_n_estado_solicitud = 100; -- rechazada

         END IF

														
      END IF
    
      -- si no se rechazo por saldo insuficiente
      IF ( ret_tipo_n_cod_rechazo = 0 ) THEN
         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         
         EXECUTE FUNCTION fn_marca_cuenta_decreto(
                 v_id_decreto
                ,v_marca_tipo_n -- marca de disposicion
                ,ret_tipo_n_id_solicitud  
                ,ret_tipo_n_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;
         
         -- si no se pudo marcar, se rechaza el registro
         IF ( v_i_estado_marca > 0 ) THEN
            LET ret_tipo_n_estado_solicitud = 100; -- rechazada
            
            -- si el error es por convencia de marca, se verifica si fue por credito, unificacion o separacion
            IF ( v_i_estado_marca = 501 OR v_i_estado_marca = 502 ) THEN
               -- marca de unificacion
               LET ret_tipo_n_cod_rechazo = v_error_det_marca_unificacion;
            ELSE
               -- se revisa si la marca es por un credito de tipo garantia (anualidad o en garantia)
               SELECT tpo_originacion, marca_inf
               INTO   v_tpo_originacion, v_marca_infonavit
               FROM   cat_tipo_credito
               WHERE  tpo_originacion IN (2,4)
               AND    marca_inf = v_i_estado_marca;
                 
               -- si se encontro, es un credito con alguna garantia
               IF ( v_marca_infonavit IS NOT NULL ) THEN
                  LET ret_tipo_n_cod_rechazo = v_error_det_saldo_en_garantia;
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
                     LET ret_tipo_n_cod_rechazo = v_error_det_marca_credito;               
                  ELSE
                     IF ( v_i_estado_marca = 280 ) THEN
                        -- marca de separacion de cuentas
                        LET ret_tipo_n_cod_rechazo = v_error_det_marca_separacion;
                     ELSE
                        -- si se trata del mismo tipo de retiro N
                        IF ( v_i_estado_marca = 804 ) THEN
                           -- se marca como registro duplicado
                           LET ret_tipo_n_cod_rechazo = v_error_det_registro_duplicado_archivo;
                        ELSE
                           -- se trata de otra marca
                           LET ret_tipo_n_cod_rechazo = v_error_det_marca_no_convive;
                        END IF -- marca de retiro
                     END IF -- separacion de cuentas
                  END IF -- marca credito tradicional
               END IF  -- marca de credito con garantia
            END IF -- marca de unificacion
         END IF -- error al marcar
      END IF -- rechazo por insuficiencia de saldo

      -- se inserta en la tabla historia de detalle de retiro por tipo_n de recursos
      INSERT INTO ret_tipo_n (
          id_solicitud            ,
          id_decreto      ,
          diag_registro           ,
          folio                   ,
          id_ret_matriz_derecho   ,
          nss_icefa               ,
          rfc_icefa               ,
          num_ctr_interno         ,
          cve_icefa               ,
          nombre_icefa            ,
          rfc                     ,
          nombre                  ,
          cve_doc_probatorio      ,
          num_referencia          ,
          origen_retiro           ,
          f_inicio_pension        ,
          f_resolucion            ,
          porcentaje_valuacion    ,
          actuario                ,
          num_plan_privado        ,
          importe_sar92           ,
          aivs_viv92              ,
          cve_afore               ,
          cod_rechazo             ,
          estado_solicitud         
      )
      VALUES (
         ret_tipo_n_id_solicitud           ,
         --ret_tipo_n_id_derechohabiente     ,
         ret_tipo_n_id_decreto             ,
         ret_tipo_n_diag_registro          ,
         ret_tipo_n_folio                  ,
         ret_tipo_n_id_ret_matriz_derecho  ,
         ret_tipo_n_nss_icefa              ,
         ret_tipo_n_rfc_icefa              ,
         ret_tipo_n_num_ctr_interno        ,
         ret_tipo_n_cve_icefa              ,
         ret_tipo_n_nombre_icefa           ,
         ret_tipo_n_rfc                    ,
         ret_tipo_n_nombre                 ,
         ret_tipo_n_cve_doc_probatorio     ,
         ret_tipo_n_num_referencia         ,
         ret_tipo_n_origen_retiro          ,   
         ret_tipo_n_f_inicio_pension       ,
         ret_tipo_n_f_resolucion           ,
         ret_tipo_n_porcentaje_valuacion   ,
         ret_tipo_n_actuario               ,
         ret_tipo_n_num_plan_privado       ,
         ret_tipo_n_importe_sar92          ,
         ret_tipo_n_aivs_viv92             ,
         ret_tipo_n_cve_afore              ,
         ret_tipo_n_cod_rechazo            ,
         ret_tipo_n_estado_solicitud        
      );
      -- TRACE "Insertado >" || tmp_ret_det_num_id_unico || "<";
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados
   END FOREACH;
    
   -- el importe total se transforma a pesos
   LET v_sumario_importe_total = v_sumario_importe_total * v_valor_fondo;
    
   -- se actualiza el importe total para el registro del encabezado
   UPDATE ret_cza_tipo_n
   SET    total_viv92 = v_sumario_importe_total
   WHERE  folio = p_folio;
       
   -- se actualizan las estadisticas
   UPDATE STATISTICS FOR TABLE ret_tipo_n;
   UPDATE STATISTICS FOR TABLE ret_cza_tipo_n;
   UPDATE STATISTICS FOR TABLE ret_tipo_n_rch;
   UPDATE STATISTICS FOR TABLE ret_cza_tipo_n_rch;
   UPDATE STATISTICS FOR TABLE ret_his_saldo;
      
   -- si no hubo error
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = 'El proceso de integración finalizó correctamente.';

   -- se devuelve el resultado de la integracion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


