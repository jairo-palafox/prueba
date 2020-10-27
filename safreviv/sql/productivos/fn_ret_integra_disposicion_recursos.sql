






CREATE FUNCTION "safreviv".fn_ret_integra_disposicion_recursos(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                                    p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                    p_proceso_cod SMALLINT,p_marca SMALLINT )
   RETURNING INTEGER, INTEGER, VARCHAR(250)


-- campos de la tabla de encabezado de retiros por disposicion de recursos (sin filler)
DEFINE tmp_ret_cza_tpo_registro          CHAR(2);
DEFINE tmp_ret_cza_id_servicio           CHAR(2);
DEFINE tmp_ret_cza_tpo_entidad_origen    CHAR(2);
DEFINE tmp_ret_cza_cve_entidad_origen    CHAR(3);
DEFINE tmp_ret_cza_tpo_entidad_destino   CHAR(2);
DEFINE tmp_ret_cza_cve_entidad_destino   CHAR(3);
DEFINE tmp_ret_cza_f_operacion           DATE;
DEFINE tmp_ret_cza_f_valor_transferencia DATE;
DEFINE tmp_ret_cza_val_participacion     DECIMAL(14);
DEFINE tmp_ret_cza_resultado_operacion   CHAR(2);
DEFINE tmp_ret_cza_motivo_rech_1         CHAR(3);
DEFINE tmp_ret_cza_motivo_rech_2         CHAR(3);
DEFINE tmp_ret_cza_motivo_rech_3         CHAR(3);

-- campos de la tabla de detalle de retiros por disposicion de recursos (sin filler)
DEFINE tmp_ret_det_tpo_registro       CHAR(2);
DEFINE tmp_ret_det_id_servicio        CHAR(2);
DEFINE tmp_ret_det_id_operacion       CHAR(2);
DEFINE tmp_ret_det_nss                CHAR(11);
DEFINE tmp_ret_det_curp               CHAR(18);
DEFINE tmp_ret_det_nombre_afore       CHAR(40);
DEFINE tmp_ret_det_paterno_afore      CHAR(40);
DEFINE tmp_ret_det_materno_afore      CHAR(40);
DEFINE tmp_ret_det_sec_pension        CHAR(2);
DEFINE tmp_ret_det_tpo_retiro         CHAR(1);
DEFINE tmp_ret_det_regimen            CHAR(2);
DEFINE tmp_ret_det_tpo_seguro         CHAR(2);
DEFINE tmp_ret_det_tpo_pension        CHAR(2);
DEFINE tmp_ret_det_tpo_prestacion     CHAR(2);
DEFINE tmp_ret_det_f_inicio_pension   DATE;
DEFINE tmp_ret_det_f_emision_resol    DATE;
DEFINE tmp_ret_det_porc_valuacion     DECIMAL(5);
DEFINE tmp_ret_det_sem_cotizadas      DECIMAL(4);
DEFINE tmp_ret_det_f_solicitud_trab   DATE;
DEFINE tmp_ret_det_cve_doc_probatorio CHAR(1);
DEFINE tmp_ret_det_f_nacimiento       DATE;
DEFINE tmp_ret_det_aseguradora        CHAR(3);
DEFINE tmp_ret_det_actuario_autor     CHAR(7);
DEFINE tmp_ret_det_num_reg_ppp        CHAR(8);
DEFINE tmp_ret_det_periodo_pago       DECIMAL(6);
DEFINE tmp_ret_det_acciones_ret97     DECIMAL(14);
DEFINE tmp_ret_det_acciones_cv        DECIMAL(14);
DEFINE tmp_ret_det_acciones_cuotsol   DECIMAL(14);
DEFINE tmp_ret_det_acciones_ret92     DECIMAL(14);
DEFINE tmp_ret_det_aiv97              DECIMAL(14);
DEFINE tmp_ret_det_aiv92              DECIMAL(14);
DEFINE tmp_ret_det_consec_trab        DECIMAL(11);
DEFINE tmp_ret_det_fondo_subcta_viv72 DECIMAL(14);
DEFINE tmp_ret_det_diagnostico_reg    CHAR(3);
DEFINE tmp_ret_det_estatus_subcta     CHAR(1);
DEFINE tmp_ret_det_result_operacion   CHAR(2);
DEFINE tmp_ret_det_cve_afore          DECIMAL(3);
DEFINE tmp_ret_det_motivo_rech1       CHAR(3);
DEFINE tmp_ret_det_motivo_rech2       CHAR(3);

-- encabezado de la tabla historica/integrada de retiros por disposicion de recursos
DEFINE ret_cza_disposicion_folio                 DECIMAL(9,0);
DEFINE ret_cza_disposicion_nombre_archivo        CHAR(20);
DEFINE ret_cza_disposicion_f_operacion_procesar  DATE;
DEFINE ret_cza_disposicion_f_carga               DATE;
DEFINE ret_cza_disposicion_h_carga               DATETIME HOUR TO SECOND;
DEFINE ret_cza_disposicion_f_valor_transferencia DATE;
DEFINE ret_cza_disposicion_precio_fondo          DECIMAL(14,6);
DEFINE ret_cza_disposicion_total_registros       DECIMAL(6,0);
DEFINE ret_cza_disposicion_total_importe         DECIMAL(18,6);
DEFINE ret_cza_disposicion_usuario               CHAR(20);

-- detalle de la tabla historica/integrada de retiros por disposicion de recursos
DEFINE ret_disposicion_id_solicitud          DECIMAL(9,0);
DEFINE ret_disposicion_id_derechohabiente    DECIMAL(9,0);
DEFINE ret_disposicion_f_solicitud           DATE;
DEFINE ret_disposicion_id_ret_matriz_derecho SMALLINT;
DEFINE ret_disposicion_sec_pension           CHAR(2);
DEFINE ret_disposicion_diag_registro         CHAR(3);
DEFINE ret_disposicion_folio                 DECIMAL(9,0);
DEFINE ret_disposicion_curp                  CHAR(18);
DEFINE ret_disposicion_nombre_afore          CHAR(40);
DEFINE ret_disposicion_paterno_afore         CHAR(40);
DEFINE ret_disposicion_materno_afore         CHAR(40);
DEFINE ret_disposicion_f_inicio_pension      DATE;
DEFINE ret_disposicion_f_resolucion          DATE;
DEFINE ret_disposicion_porcentaje_valuacion  DECIMAL(5,2);
DEFINE ret_disposicion_semanas_cotizadas     INTEGER;
DEFINE ret_disposicion_cve_doc_probatorio    SMALLINT;
DEFINE ret_disposicion_f_nacimiento          DATE;
DEFINE ret_disposicion_aseguradora           CHAR(3);
DEFINE ret_disposicion_actuario              CHAR(7);
DEFINE ret_disposicion_num_plan_privado      CHAR(8);
DEFINE ret_disposicion_periodo_primer_pago   INTEGER;
DEFINE ret_disposicion_aivs_ret97            DECIMAL(14,6);
DEFINE ret_disposicion_aivs_cv               DECIMAL(14,6);
DEFINE ret_disposicion_aivs_cs               DECIMAL(14,6);
DEFINE ret_disposicion_aivs_sar92            DECIMAL(14,6);
DEFINE ret_disposicion_aivs_viv97            DECIMAL(14,6);
DEFINE ret_disposicion_aivs_viv92            DECIMAL(14,6);
DEFINE ret_disposicion_consec_trabajador     DECIMAL(11,0);
DEFINE ret_disposicion_importe_viv72         DECIMAL(14,2);
DEFINE ret_disposicion_estado_sub_viv        SMALLINT;
DEFINE ret_disposicion_cve_afore             SMALLINT;
DEFINE ret_disposicion_cod_rechazo           SMALLINT;
DEFINE ret_disposicion_estado_solicitud      SMALLINT;

-- rechazo de encabezado
DEFINE ret_cza_disposicion_rch_folio                 DECIMAL(9,0);
DEFINE ret_cza_disposicion_rch_nombre_archivo        CHAR(20);
DEFINE ret_cza_disposicion_rch_f_operacion_procesar  DATE;
DEFINE ret_cza_disposicion_rch_f_carga               DATE;
DEFINE ret_cza_disposicion_rch_h_carga               DATETIME HOUR TO SECOND;
DEFINE ret_cza_disposicion_rch_f_valor_transferencia DATE;
DEFINE ret_cza_disposicion_rch_precio_fondo          DECIMAL(14,6);
DEFINE ret_cza_disposicion_rch_total_registros       DECIMAL(6,0);
DEFINE ret_cza_disposicion_rch_total_importe         DECIMAL(18,6);
DEFINE ret_cza_disposicion_rch_usuario               CHAR(20);
DEFINE ret_cza_disposicion_rch_resultado_operacion   SMALLINT;
DEFINE ret_cza_disposicion_rch_cod_rechazo_1         SMALLINT;
DEFINE ret_cza_disposicion_rch_cod_rechazo_2         SMALLINT;
DEFINE ret_cza_disposicion_rch_cod_rechazo_3         SMALLINT;

-- rechazo de detalle
DEFINE ret_dispos_rch_id_solicitud         DECIMAL(9,0);
DEFINE ret_dispos_rch_id_derechohabiente   DECIMAL(9,0);
DEFINE ret_dispos_rch_f_solicitud          DATE;
DEFINE ret_dispos_rch_folio                DECIMAL(9,0);
DEFINE ret_dispos_rch_nss                  CHAR(11);
DEFINE ret_dispos_rch_curp                 CHAR(18);
DEFINE ret_dispos_rch_nombre_afore         CHAR(40);
DEFINE ret_dispos_rch_paterno_afore        CHAR(40);
DEFINE ret_dispos_rch_materno_afore        CHAR(40);
DEFINE ret_dispos_rch_sec_pension          CHAR(2);
DEFINE ret_dispos_rch_tipo_retiro          CHAR(1);
DEFINE ret_dispos_rch_regimen              CHAR(2);
DEFINE ret_dispos_rch_tpo_seguro           CHAR(2);
DEFINE ret_dispos_rch_tpo_pension          CHAR(2);
DEFINE ret_dispos_rch_tpo_prestacion       CHAR(2);
DEFINE ret_dispos_rch_f_inicio_pension     DATE;
DEFINE ret_dispos_rch_f_resolucion         DATE;
DEFINE ret_dispos_rch_porcentaje_valuacion DECIMAL(5,2);
DEFINE ret_dispos_rch_semanas_cotizadas    INTEGER;
DEFINE ret_dispos_rch_cve_doc_probatorio   SMALLINT;
DEFINE ret_dispos_rch_f_nacimiento         DATE;
DEFINE ret_dispos_rch_aseguradora          CHAR(3);
DEFINE ret_dispos_rch_actuario             CHAR(7);
DEFINE ret_dispos_rch_num_plan_privado     CHAR(8);
DEFINE ret_dispos_rch_periodo_primer_pago  INTEGER;
DEFINE ret_dispos_rch_aivs_ret_97          DECIMAL(14,6);
DEFINE ret_dispos_rch_aivs_cv              DECIMAL(14,6);
DEFINE ret_dispos_rch_aivs_cs              DECIMAL(14,6);
DEFINE ret_dispos_rch_aivs_sar92           DECIMAL(14,6);
DEFINE ret_dispos_rch_aivs_viv97           DECIMAL(14,6);
DEFINE ret_dispos_rch_aivs_viv92           DECIMAL(14,6);
DEFINE ret_dispos_rch_consec_trabajador    DECIMAL(11,0);
DEFINE ret_dispos_rch_importe_viv72        DECIMAL(14,2);
DEFINE ret_dispos_rch_diag_registro        CHAR(3);
DEFINE ret_dispos_rch_estado_sub_viv       SMALLINT;
DEFINE ret_dispos_rch_cve_afore            SMALLINT;
DEFINE ret_dispos_rch_estado_solicitud     SMALLINT;
DEFINE ret_dispos_rch_resultado_operacion  SMALLINT;
DEFINE ret_dispos_rch_cod_rechazo_1        SMALLINT;
DEFINE ret_dispos_rch_cod_rechazo_2        SMALLINT;
DEFINE ret_dispos_rch_cod_rechazo_3        SMALLINT;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente DECIMAL(9,0);
DEFINE v_id_solicitud       DECIMAL(9,0);

-- para calcular las AIVs a pesos
DEFINE v_valor_fondo      DECIMAL(14,6);
DEFINE v_pesos_aiv97      DECIMAL(14,6);
DEFINE v_pesos_aiv92      DECIMAL(14,6);
DEFINE v_saldo_92_aivs    DECIMAL(18,6);
DEFINE v_saldo_92_pesos   DECIMAL(18,6);
DEFINE v_saldo_97_aivs    DECIMAL(18,6);
DEFINE v_saldo_97_pesos   DECIMAL(18,6);
DEFINE v_saldo_vol_aivs   DECIMAL(16,6);
DEFINE v_saldo_vol_pesos  DECIMAL(16,6);

DEFINE v_saldo_72_pesos     DECIMAL(18,6);
DEFINE v_resultado_consulta SMALLINT;

-- para rechazos
DEFINE v_b_rechazo_encabezado      SMALLINT;
DEFINE v_b_rechazo_detalle         DECIMAL(6,0);
DEFINE v_validar_3_primeros_campos VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                 SMALLINT; -- clave de afore
-- id matriz derecho
DEFINE v_id_ret_matriz_derecho SMALLINT; -- id de la matriz de derecho de retiros
DEFINE v_grupo                 SMALLINT; -- grupo para revisar que cuentas se pagan por retiro
DEFINE v_paga_viv92            SMALLINT; -- booleana para saber si se paga viv92
DEFINE v_paga_viv97            SMALLINT; -- booleana para saber si se paga viv97
DEFINE v_paga_viv72            SMALLINT; -- booleana para saber si se paga viv72

DEFINE v_sumario_importe_total   DECIMAL(22,6);
DEFINE v_sumario_total_registros DECIMAL(6,0);
DEFINE v_total_registros         DECIMAL(6,0);
DEFINE v_numero_registros        DECIMAL(6,0);
DEFINE v_saldo_cuenta            DECIMAL(14,6);

DEFINE v_motivo_rechazo_1 DECIMAL(6,0);
DEFINE v_motivo_rechazo_2 DECIMAL(6,0);
DEFINE v_motivo_rechazo_3 DECIMAL(6,0);
-- arreglo de codigos de rechazo
DEFINE v_codigos_rechazo        CHAR(30); -- los codigos van de tres en tres
DEFINE v_indice_codigos_rechazo SMALLINT;

-- conteo de rechazos e inserciones
DEFINE v_reg_cza_insertados SMALLINT; -- total de registros de encabezado insertados
DEFINE v_reg_cza_rechazados SMALLINT; -- total de registros de encabezado rechazados
DEFINE v_reg_det_insertados DECIMAL(6,0); -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados DECIMAL(6,0); -- total de registros de detalle rechazados

-- codigos de error en encabezado
DEFINE v_error_cza_reg_totales_no_coinciden DECIMAL(10,5);
DEFINE v_error_cza_tpo_registro_invalido    SMALLINT;
DEFINE v_error_cza_id_servicio_invalido     SMALLINT;
DEFINE v_error_cza_sin_precio_fondo         SMALLINT;
DEFINE v_error_cza_sin_fecha_procesar       SMALLINT;
DEFINE v_error_cza_sin_fecha_valuacion      SMALLINT;
-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado            SMALLINT;
DEFINE v_error_det_tpo_registro_invalido        SMALLINT;
DEFINE v_error_det_id_servicio_invalido         SMALLINT;
DEFINE v_error_det_id_operacion_invalido        SMALLINT;
DEFINE v_error_det_matriz_derecho_no_encontrado SMALLINT;
DEFINE v_error_det_sec_pension_invalido         SMALLINT;
DEFINE v_error_det_fec_solicitud_invalido       SMALLINT;
DEFINE v_error_det_afore_invalido               SMALLINT;
DEFINE v_error_det_lote_invalido                SMALLINT; -- el lote quedo invalido porque hubo rechazos
DEFINE v_error_det_marca_unificacion            SMALLINT; -- el lote quedo invalido porque hubo rechazos
DEFINE v_error_det_marca_credito                SMALLINT; -- el lote quedo invalido porque hubo rechazos
DEFINE v_error_det_marca_separacion             SMALLINT; -- el lote quedo invalido porque hubo rechazos
DEFINE v_error_det_apaterno_afore_invalido      SMALLINT;
DEFINE v_error_det_amaterno_afore_invalido      SMALLINT;
DEFINE v_error_det_nombre_afore_invalido        SMALLINT;
DEFINE v_error_det_saldo_en_garantia            SMALLINT;
DEFINE v_error_det_marca_no_convive             SMALLINT;
DEFINE v_error_det_en_devolucion_de_subcuenta   SMALLINT;
DEFINE v_error_det_subcuenta_no_corresoponde    SMALLINT; -- subcuenta no corresponde a matriz de derechos
DEFINE v_error_det_registro_duplicado_archivo   SMALLINT; -- el registro esta dos veces
DEFINE v_monto_viv92_invalido                   SMALLINT; -- para registrar insuficiencia de saldos
DEFINE v_monto_viv97_invalido                   SMALLINT; -- para registrar insuficiencia de saldos
DEFINE v_max_aivs_sobregiro                     DECIMAL(18,6);  --cantidad maxima de aivs para sobregiro
DEFINE v_tabla                                  CHAR(25);  -- Variable de trabajo para guardar el nombre de las tablas de movimientos
DEFINE v_query                                  CHAR(5000);
DEFINE v_count_movtos_1802                      SMALLINT;
DEFINE v_suma_movtos_1802                       SMALLINT;
DEFINE v_suma_solicitado                        DECIMAL(18,6);
DEFINE v_suma_saldo                             DECIMAL(18,6);
DEFINE v_dif_sumas                              DECIMAL(18,6);

DEFINE v_nombre_af     CHAR(40);
DEFINE v_ap_paterno_af CHAR(40);
DEFINE v_ap_materno_af CHAR(40);

-- para validar marca de credito
DEFINE v_tpo_originacion SMALLINT;
DEFINE v_marca_infonavit SMALLINT;

-- estatus del proceso
DEFINE v_estatus_proceso SMALLINT;

-- para marcar las cuentas
DEFINE v_i_estado_marca    INTEGER;
DEFINE v_marca_disposicion INTEGER; -- 805 de acuerdo a catalogo

-- Control de Excepciones
DEFINE v_si_resultado SMALLINT;
DEFINE sql_err        INTEGER;
DEFINE isam_err       INTEGER;
DEFINE err_txt        VARCHAR(250);
DEFINE v_c_msj        VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION


   SET DEBUG FILE TO "/safreviv_int/BD/debug_ret_disposicion.trace";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_cza_insertados = 0; -- total de registros de encabezado insertados
   LET v_reg_cza_rechazados = 0; -- total de registros de encabezado rechazados
   LET v_reg_det_insertados = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados = 0; -- total de registros de detalle rechazados

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finalizó exitosamente.';

   -- se inician los codigos de error en encabezado
   LET v_error_cza_reg_totales_no_coinciden = 1000;
   LET v_error_cza_tpo_registro_invalido = 2;
   LET v_error_cza_id_servicio_invalido = 3;
   LET v_error_cza_sin_precio_fondo = 4;
   LET v_error_cza_sin_fecha_procesar = 5;
   LET v_error_cza_sin_fecha_valuacion = 6;

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado = 49; -- segun PROCESAR
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
   LET v_error_det_registro_duplicado_archivo = 768; -- registro duplicado en archivo
   LET v_monto_viv92_invalido = 766; -- saldo insuficiente de vivienva 92
   LET v_monto_viv97_invalido = 767; -- saldo insuficiente de vivienva 97

   LET v_query = "";


   -- se inician las variables para marca
   LET v_marca_disposicion = p_marca;
   LET v_i_estado_marca = 0;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio       = p_folio,
          estado      = 2 -- integrado
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 1 -- archivo cargado
   AND    estado      = 1
   AND    folio       IS NULL; -- etapa de carga

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- se asigna el folio al proceso
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  pid         = p_pid;


   SELECT diferencia_maxima
   INTO   v_max_aivs_sobregiro
   FROM   ret_sobregiro_max_permitido
   WHERE  proceso_cod = p_proceso_cod;


   --- se cuentan los registros de la tabla temporal de detalle
   SELECT COUNT(*)
   INTO   v_numero_registros
   FROM   safre_tmp:tmp_ret_det_disposicion;

   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO   v_total_registros
   FROM   safre_tmp:tmp_ret_det_disposicion;

   SELECT total_registros
   INTO   v_sumario_total_registros
   FROM   safre_tmp:tmp_ret_sum_disposicion;

   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN
      -- se rechaza el lote y no integra
      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET v_c_msj = "No coinciden numero de registros cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado   = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;

   CREATE TEMP TABLE 
      tmp_codigos_rechazo (
         id_codigo       SMALLINT,
         codigo_rechazo  SMALLINT
         );

   -- se obtienen los datos del encabezado
   FOREACH
    SELECT
      tpo_registro,id_servicio,
      tpo_entidad_origen,cve_entidad_origen,
      tpo_entidad_destino,cve_entidad_destino,
      f_operacion,f_valor_transferencia,
      val_participacion,resultado_operacion,
      motivo_rech_1,motivo_rech_2,motivo_rech_3
    INTO
      tmp_ret_cza_tpo_registro,tmp_ret_cza_id_servicio,
      tmp_ret_cza_tpo_entidad_origen,tmp_ret_cza_cve_entidad_origen,
      tmp_ret_cza_tpo_entidad_destino,tmp_ret_cza_cve_entidad_destino,
      tmp_ret_cza_f_operacion,tmp_ret_cza_f_valor_transferencia,
      tmp_ret_cza_val_participacion,tmp_ret_cza_resultado_operacion,
      tmp_ret_cza_motivo_rech_1,tmp_ret_cza_motivo_rech_2,tmp_ret_cza_motivo_rech_3
    FROM     safre_tmp:tmp_ret_cza_disposicion

      -- se asume que no hay error
      LET v_b_rechazo_encabezado = 0;

      -- se borra la tabla de errores
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;

      -- se asignan los datos al registro de encabezado historico
      LET ret_cza_disposicion_folio = p_folio;
      LET ret_cza_disposicion_nombre_archivo = p_nombre_archivo;
      LET ret_cza_disposicion_f_operacion_procesar = tmp_ret_cza_f_operacion;
      LET ret_cza_disposicion_f_carga = TODAY;
      LET ret_cza_disposicion_h_carga = CURRENT HOUR TO MINUTE;
      LET ret_cza_disposicion_f_valor_transferencia = tmp_ret_cza_f_valor_transferencia;
      LET ret_cza_disposicion_precio_fondo = tmp_ret_cza_val_participacion / 1000000;
      LET ret_cza_disposicion_total_registros = v_numero_registros; -- numero de registros
      LET ret_cza_disposicion_total_importe = 0; -- importe total -- se sumara al final
      LET ret_cza_disposicion_usuario = p_usuario_cod;

      -- se establece el valor del fondo
      LET v_valor_fondo = ret_cza_disposicion_precio_fondo;

      -- se reinicia el indice de codigos de rechazo
      LET v_indice_codigos_rechazo = 1;

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

      -- se verifica si el encabezado contiene el precio del fondo
      IF ( ret_cza_disposicion_precio_fondo <= 0 ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_precio_fondo);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- se verifica si el encabezado contiene fecha procesar
      IF ( tmp_ret_cza_f_operacion IS NULL ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_procesar);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- se verifica si el encabezado contiene fecha de valuacion
      IF ( tmp_ret_cza_f_valor_transferencia IS NULL ) THEN
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_cza_sin_fecha_valuacion);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- si hubo rechazo de encabezado no se inserta en historico
      IF ( v_b_rechazo_encabezado = 1 ) THEN

         -- se llenan los datos generales de rechazo del encabezado
         LET ret_cza_disposicion_rch_folio = p_folio; -- DECIMAL(9,0)                    ;
         LET ret_cza_disposicion_rch_f_operacion_procesar = tmp_ret_cza_f_operacion; -- DATE                            ;
         LET ret_cza_disposicion_rch_nombre_archivo = p_nombre_archivo; -- CHAR(20)                        ;
         LET ret_cza_disposicion_rch_f_carga = TODAY; -- DATE                            ;
         LET ret_cza_disposicion_rch_h_carga = CURRENT HOUR TO SECOND; -- DATETIME FRACTION TO FRACTION(3);
         LET ret_cza_disposicion_rch_f_valor_transferencia = tmp_ret_cza_f_valor_transferencia; -- DATE                            ;
         LET ret_cza_disposicion_rch_precio_fondo = ret_cza_disposicion_precio_fondo; -- DECIMAL(14,6)                   ;
         LET ret_cza_disposicion_rch_total_registros = 0; -- INTEGER                         ;
         LET ret_cza_disposicion_rch_total_importe = 0; -- DECIMAL(18,6)                   ;
         LET ret_cza_disposicion_rch_usuario = p_usuario_cod; -- CHAR(20)                        ;
         LET ret_cza_disposicion_rch_resultado_operacion = 2; -- rechazado

         -- se asignan los primeros tres errores
         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;

         -- se leen los tres primeros errores
         FOREACH
            SELECT   FIRST 3
                     id_codigo,
                     codigo_rechazo
            INTO     v_indice_codigos_rechazo, 
                     v_codigos_rechazo
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

         LET ret_cza_disposicion_rch_cod_rechazo_1  = v_motivo_rechazo_1;
         LET ret_cza_disposicion_rch_cod_rechazo_2  = v_motivo_rechazo_2;
         LET ret_cza_disposicion_rch_cod_rechazo_3  = v_motivo_rechazo_3;


         -- se inserta el rechazo
         INSERT INTO ret_cza_disposicion_rch (
            folio,f_operacion_procesar,
            nombre_archivo,f_carga,
            hora_carga,f_valor_transferencia,
            precio_fondo,total_registros,
            total_importe,usuario,
            resultado_operacion,cod_rechazo_1,
            cod_rechazo_2,cod_rechazo_3
         )
         VALUES (
            ret_cza_disposicion_rch_folio,ret_cza_disposicion_rch_f_operacion_procesar,
            ret_cza_disposicion_rch_nombre_archivo,ret_cza_disposicion_rch_f_carga,
            ret_cza_disposicion_rch_h_carga,ret_cza_disposicion_rch_f_valor_transferencia,
            ret_cza_disposicion_rch_precio_fondo,ret_cza_disposicion_rch_total_registros,
            ret_cza_disposicion_rch_total_importe,ret_cza_disposicion_rch_usuario,
            ret_cza_disposicion_rch_resultado_operacion,ret_cza_disposicion_rch_cod_rechazo_1,
            ret_cza_disposicion_rch_cod_rechazo_2,ret_cza_disposicion_rch_cod_rechazo_3
         );

         -- se cuenta un encabezado rechazado
         LET v_reg_cza_rechazados = v_reg_cza_rechazados + 1;

         -- el registro fue rechazado y no se inserta en el historico
         CONTINUE FOREACH;
      END IF

      -- se inserta en la tabla historica del encabezado de retiros por disposicion
      INSERT INTO ret_cza_disposicion (
         folio,nombre_archivo,
         f_operacion_procesar,f_carga,
         h_carga,f_valor_transferencia,
         precio_fondo,total_registros,
         total_importe,usuario
      )
      VALUES (
         ret_cza_disposicion_folio,ret_cza_disposicion_nombre_archivo,
         ret_cza_disposicion_f_operacion_procesar,ret_cza_disposicion_f_carga,
         ret_cza_disposicion_h_carga,ret_cza_disposicion_f_valor_transferencia,
         ret_cza_disposicion_precio_fondo,ret_cza_disposicion_total_registros,
         ret_cza_disposicion_total_importe,ret_cza_disposicion_usuario
      );

      -- se cuenta un encabezado insertado
      LET v_reg_cza_insertados = v_reg_cza_insertados + 1;

   END FOREACH;

   -- se inicia el importe total
   LET v_sumario_importe_total = 0;

   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;


   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle = 0;


   IF v_max_aivs_sobregiro IS NULL THEN
      LET v_max_aivs_sobregiro = 0;
   END IF
   -- se obtienen los datos del detalle
   FOREACH
    SELECT
      tpo_registro,id_servicio,id_operacion,nss,
      curp,nombre_afore,paterno_afore,materno_afore,
      sec_pension,tpo_retiro,regimen,tpo_seguro,
      tpo_pension,tpo_prestacion,f_inicio_pension,f_emision_resol,
      porc_valuacion,sem_cotizadas,f_solicitud_trab,cve_doc_probatorio,
      f_nacimiento,aseguradora,actuario_autor,num_reg_ppp,
      periodo_pago,acciones_ret97,acciones_cv,acciones_cuotsol,
      acciones_ret92,NVL(aiv97,0),NVL(aiv92,0),consec_trab,
      NVL(fondo_subcta_viv72,0),diagnostico_reg,estatus_subcta,
      result_operacion,cve_afore,motivo_rech1,motivo_rech2
    INTO
      tmp_ret_det_tpo_registro,tmp_ret_det_id_servicio,
      tmp_ret_det_id_operacion,tmp_ret_det_nss,
      tmp_ret_det_curp,tmp_ret_det_nombre_afore,
      tmp_ret_det_paterno_afore,tmp_ret_det_materno_afore,
      tmp_ret_det_sec_pension,tmp_ret_det_tpo_retiro,
      tmp_ret_det_regimen,tmp_ret_det_tpo_seguro,
      tmp_ret_det_tpo_pension,tmp_ret_det_tpo_prestacion,
      tmp_ret_det_f_inicio_pension,tmp_ret_det_f_emision_resol,
      tmp_ret_det_porc_valuacion,tmp_ret_det_sem_cotizadas,
      tmp_ret_det_f_solicitud_trab,tmp_ret_det_cve_doc_probatorio,
      tmp_ret_det_f_nacimiento,tmp_ret_det_aseguradora,
      tmp_ret_det_actuario_autor,tmp_ret_det_num_reg_ppp,
      tmp_ret_det_periodo_pago,tmp_ret_det_acciones_ret97,
      tmp_ret_det_acciones_cv,tmp_ret_det_acciones_cuotsol,
      tmp_ret_det_acciones_ret92,tmp_ret_det_aiv97,
      tmp_ret_det_aiv92,tmp_ret_det_consec_trab,
      tmp_ret_det_fondo_subcta_viv72,
      tmp_ret_det_diagnostico_reg,tmp_ret_det_estatus_subcta,
      tmp_ret_det_result_operacion,tmp_ret_det_cve_afore,
      tmp_ret_det_motivo_rech1,tmp_ret_det_motivo_rech2
    FROM safre_tmp:tmp_ret_det_disposicion

      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente,nombre_af,ap_paterno_af,ap_materno_af
      INTO   v_id_derechohabiente,v_nombre_af,v_ap_paterno_af,v_ap_materno_af
      FROM   afi_derechohabiente
      WHERE  nss = tmp_ret_det_nss;
      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;

      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;

      LET v_indice_codigos_rechazo = 1;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle = 1;
         LET v_id_derechohabiente = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

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
      SELECT id_ret_matriz_derecho, grupo
      INTO   v_id_ret_matriz_derecho, v_grupo
      FROM   ret_matriz_derecho
      WHERE  tpo_retiro           = tmp_ret_det_tpo_retiro
      AND    regimen              = tmp_ret_det_regimen
      AND    tpo_seguro           = tmp_ret_det_tpo_seguro
      AND    tpo_pension          = tmp_ret_det_tpo_pension
      AND    tpo_prestacion       = tmp_ret_det_tpo_prestacion;

      -- si no se encontro, se rechaza
      IF ( v_id_ret_matriz_derecho IS NULL ) THEN
         LET v_b_rechazo_detalle = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_matriz_derecho_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      ELSE
         -- se verifica que con respecto al tipo de retiro las cuentas solicitadas sean las adecuadas
         -- se asume que no se paga ninguna cuenta
         LET v_paga_viv92 = 0;
         LET v_paga_viv97 = 0;
         LET v_paga_viv72 = 0;

         -- se verifica segun el tipo de retiro, que cuentas debe pagar
         -- vivienda 92
         SELECT subcuenta
         INTO   v_paga_viv92
         FROM   ret_grupo_subcta
         WHERE  grupo     = v_grupo
         AND    subcuenta = 8;

         -- vivienda 97
         SELECT subcuenta
         INTO   v_paga_viv97
         FROM   ret_grupo_subcta
         WHERE  grupo     = v_grupo
         AND    subcuenta = 4;

         -- vivienda 72
         SELECT subcuenta
         INTO   v_paga_viv72
         FROM   ret_grupo_subcta
         WHERE  grupo     = v_grupo
         AND    subcuenta = 40;

         -- se verifica si se solicita vivienda 92 y esta se debe de pagar de acuerdo al tipo de retiro
         IF ( (tmp_ret_det_aiv92 IS NOT NULL AND tmp_ret_det_aiv92 > 0) AND v_paga_viv92 IS NULL ) THEN
            -- se solicito viv92 y el tipo de retiro no la paga
            LET v_b_rechazo_detalle      = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_subcuenta_no_corresoponde);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF

         -- se verifica si se solicita vivienda 97 y esta se debe de pagar de acuerdo al tipo de retiro
         IF ( (tmp_ret_det_aiv97 IS NOT NULL AND tmp_ret_det_aiv97 > 0) AND v_paga_viv97 IS NULL ) THEN
            -- se solicito viv97 y el tipo de retiro no la paga
            LET v_b_rechazo_detalle      = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_subcuenta_no_corresoponde);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF

         -- se verifica si se solicita vivienda 97 y esta se debe de pagar de acuerdo al tipo de retiro
         IF ( (tmp_ret_det_fondo_subcta_viv72 IS NOT NULL AND tmp_ret_det_fondo_subcta_viv72 > 0) AND v_paga_viv72 IS NULL ) THEN
            -- se solicito viv72 y el tipo de retiro no la paga
            LET v_b_rechazo_detalle      = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_subcuenta_no_corresoponde);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
      END IF

      -- la secuencia de pension debe existir y ser numerica
      -- agosto 22, 2012 Secuencia de pension solo se revisa en tipo de retiro E
      IF ( tmp_ret_det_tpo_retiro = "E" ) THEN
         IF ( tmp_ret_det_sec_pension IS NULL OR (tmp_ret_det_sec_pension < "00" OR tmp_ret_det_sec_pension > "99")) THEN
            -- se rechaza
            LET v_b_rechazo_detalle      = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_sec_pension_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
      END IF

      -- la fecha de solicitud del trabajor no puede ser mayor a la del dia
      IF ( tmp_ret_det_f_solicitud_trab > TODAY ) THEN
         -- se rechaza
         LET v_b_rechazo_detalle      = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_fec_solicitud_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- la clave de afore tiene que estar en catalogo
      SELECT afore_cod
      INTO   v_afore_cod
      FROM   cat_afore
      WHERE  afore_cod = tmp_ret_det_cve_afore;

      IF ( v_afore_cod IS NULL ) THEN
         -- se rechaza
         LET v_b_rechazo_detalle      = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_afore_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
----- Modificaciones por el req SACI2017-49
-----   Aqui se debe validar el tipo de retiro J que la solicitud no sobrepase las mil aivs
      IF v_id_derechohabiente IS NOT NULL AND tmp_ret_det_tpo_retiro = "J" THEN
         ---- Se obtienen los saldos para validar si no sobrepasan las 1000 AIVS
         LET v_saldo_97_aivs = 0;
         LET v_saldo_97_pesos  = 0;
         LET v_saldo_vol_aivs = 0;
         LET v_saldo_vol_pesos = 0;
         LET v_saldo_92_aivs = 0;
         LET v_saldo_92_pesos = 0;
         LET v_resultado_consulta = 0;
         LET v_suma_saldo = 0;
         LET v_suma_solicitado = 0;
         LET v_dif_sumas = 0;
         
         EXECUTE FUNCTION fn_saldo_dia(NULL, v_id_derechohabiente, 4, NULL)
                     INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_97_pesos;
         LET v_resultado_consulta = 0;
         EXECUTE FUNCTION fn_saldo_dia(NULL, v_id_derechohabiente, 55, NULL)
                     INTO v_resultado_consulta, v_saldo_vol_aivs, v_saldo_vol_pesos;
         LET v_resultado_consulta = 0;
         EXECUTE FUNCTION fn_saldo_dia(NULL, v_id_derechohabiente, 8, NULL)
                     INTO v_resultado_consulta, v_saldo_92_aivs, v_saldo_92_pesos;
         LET v_suma_solicitado = (tmp_ret_det_aiv97 / 1000000) + (tmp_ret_det_aiv92 / 1000000);
         LET v_suma_saldo = v_saldo_97_aivs + v_saldo_vol_aivs + v_saldo_92_aivs;
         LET v_dif_sumas = v_suma_solicitado - v_suma_saldo;

         FOREACH 
            SELECT tabla
            INTO   v_tabla
            FROM   cat_tab_movimiento
            LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                            " FROM   " || v_tabla || 
                                            " WHERE  movimiento IN (1802,1832,1701,1852,1691,1862,1872, " ||
                                            "                       1711,1892, 721,1912,1731,1952,1751, " ||
                                            "                       1932,1741,192) " ||
                                            " AND    subcuenta IN  (4, 8) " || 
                                            " AND    id_derechohabiente = " || v_id_derechohabiente || 
                                            " UNION ";
         END FOREACH
         LET v_query = TRIM (v_query) || " SELECT COUNT(*) " ||
                                         " FROM   cta_movimiento " ||
                                         " WHERE  movimiento IN (1802,1832,1701,1852,1691,1862,1872, " ||
                                         "                       1711,1892, 721,1912,1731,1952,1751, " ||
                                         "                       1932,1741,192) " ||
                                         " AND    subcuenta IN  (4, 8) " || 
                                         " AND    id_derechohabiente = " || v_id_derechohabiente;
         PREPARE stmt_tmp FROM v_query;
         DECLARE cust_cur CURSOR FOR stmt_tmp;  
         LET v_count_movtos_1802 = 0;
         LET v_suma_movtos_1802 = 0;
         OPEN cust_cur;
            WHILE (1 = 1)
               FETCH cust_cur INTO v_count_movtos_1802;
                  IF (SQLCODE != 100) THEN
                     LET v_suma_movtos_1802 = v_suma_movtos_1802 + v_count_movtos_1802;
                  ELSE 
                     EXIT WHILE;
                  END IF 
            END WHILE
         CLOSE cust_cur;
         FREE cust_cur;
         FREE stmt_tmp;
         IF v_id_derechohabiente = 12000549 THEN 
            trace "La info del caso solicitado: " || v_suma_solicitado || ", saldo: " || v_suma_saldo || ", diferencia: " || v_dif_sumas || ", movimientos: " || v_suma_movtos_1802 || "-----";
         END IF 
         IF  v_dif_sumas > 1000 AND v_suma_movtos_1802 > 0 THEN 
            LET v_b_rechazo_detalle      = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_monto_viv97_invalido);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
         LET v_saldo_97_aivs  = 0;
         LET v_saldo_97_pesos = 0;
         LET v_saldo_vol_aivs = 0;
         LET v_saldo_vol_pesos = 0;
         LET v_saldo_92_aivs = 0;
         LET v_saldo_92_pesos = 0;
         LET v_resultado_consulta = 0;
         LET v_suma_saldo = 0;
         LET v_suma_solicitado = 0;
         LET v_dif_sumas = 0;
         LET v_query = "";
      END IF

---------------      
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

         -- se asignan los datos al registro de rechazo de detalle
         LET ret_dispos_rch_id_solicitud          = v_id_solicitud; -- decimal(9,0) ;
         LET ret_dispos_rch_id_derechohabiente    = v_id_derechohabiente; -- decimal(9,0) ;
         LET ret_dispos_rch_f_solicitud           = tmp_ret_det_f_solicitud_trab; -- date         ;
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
         LET ret_dispos_rch_f_inicio_pension      = tmp_ret_det_f_inicio_pension; -- date         ;
         LET ret_dispos_rch_f_resolucion          = tmp_ret_det_f_emision_resol; -- date         ;
         LET ret_dispos_rch_porcentaje_valuacion  = tmp_ret_det_porc_valuacion / 100; -- decimal(5,2) ;
         LET ret_dispos_rch_semanas_cotizadas     = tmp_ret_det_sem_cotizadas; -- integer      ;
         LET ret_dispos_rch_cve_doc_probatorio    = tmp_ret_det_cve_doc_probatorio; -- smallint     ;
         LET ret_dispos_rch_f_nacimiento          = tmp_ret_det_f_nacimiento; -- date         ;
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
            id_solicitud,id_derechohabiente,
            f_solicitud,folio,
            nss,curp,nombre_afore,
            paterno_afore,materno_afore,
            sec_pension,tipo_retiro,
            regimen,tpo_seguro,
            tpo_pension,tpo_prestacion,
            f_inicio_pension,f_resolucion,
            porcentaje_valuacion,semanas_cotizadas,
            cve_doc_probatorio,f_nacimiento,
            aseguradora,actuario,
            num_plan_privado,periodo_primer_pago,
            aivs_viv97,aivs_viv92,
            consec_trabajador,importe_viv72,
            diag_registro,estado_sub_viv,
            cve_afore,estado_solicitud,
            resultado_operacion,cod_rechazo_1,
            cod_rechazo_2,cod_rechazo_3
         )
         VALUES (
            seq_ret_solicitud.NEXTVAL,ret_dispos_rch_id_derechohabiente,
            ret_dispos_rch_f_solicitud,ret_dispos_rch_folio,
            ret_dispos_rch_nss,ret_dispos_rch_curp,
            ret_dispos_rch_nombre_afore,ret_dispos_rch_paterno_afore,
            ret_dispos_rch_materno_afore,ret_dispos_rch_sec_pension,
            ret_dispos_rch_tipo_retiro,ret_dispos_rch_regimen,
            ret_dispos_rch_tpo_seguro,ret_dispos_rch_tpo_pension,
            ret_dispos_rch_tpo_prestacion,ret_dispos_rch_f_inicio_pension,
            ret_dispos_rch_f_resolucion,ret_dispos_rch_porcentaje_valuacion,
            ret_dispos_rch_semanas_cotizadas,ret_dispos_rch_cve_doc_probatorio,
            ret_dispos_rch_f_nacimiento,ret_dispos_rch_aseguradora,
            ret_dispos_rch_actuario,ret_dispos_rch_num_plan_privado,
            ret_dispos_rch_periodo_primer_pago,ret_dispos_rch_aivs_viv97,
            ret_dispos_rch_aivs_viv92,ret_dispos_rch_consec_trabajador,
            ret_dispos_rch_importe_viv72,ret_dispos_rch_diag_registro,
            ret_dispos_rch_estado_sub_viv,ret_dispos_rch_cve_afore,
            ret_dispos_rch_estado_solicitud,ret_dispos_rch_resultado_operacion,
            ret_dispos_rch_cod_rechazo_1,ret_dispos_rch_cod_rechazo_2,
            ret_dispos_rch_cod_rechazo_3
         );

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- calculo de las AIVs 97 y 92 a pesos
         LET ret_disposicion_aivs_viv97 = (tmp_ret_det_aiv97 / 1000000);
         LET ret_disposicion_aivs_viv92 = (tmp_ret_det_aiv92 / 1000000);

         -- se acumula para el total general aunque haya sido rechazado
         LET v_pesos_aiv97 = ret_disposicion_aivs_viv97 * v_valor_fondo;
         LET v_pesos_aiv92 = ret_disposicion_aivs_viv92 * v_valor_fondo;
         LET ret_disposicion_importe_viv72 = tmp_ret_det_fondo_subcta_viv72 / 100;

         -- incremento del importe total
         --LET v_sumario_importe_total       = v_sumario_importe_total + v_pesos_aiv97 + v_pesos_aiv92 + ret_disposicion_importe_viv72;
         -- no se considera la subcuenta del importe de vivienda 72 segun requerimiento PRODINFXVI-128
         LET v_sumario_importe_total = v_sumario_importe_total + v_pesos_aiv97 + v_pesos_aiv92;

         -- si fue rechazado no se inserta en el historico
         CONTINUE FOREACH;

      END IF

      -- se obtiene el id_solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO   v_id_solicitud
      FROM   systables
      WHERE  tabid = 1;
      LET ret_disposicion_cod_rechazo = 0;
      trace "----------------------- ";
      trace " Aceptado";
      trace " NSS:" || tmp_ret_det_nss;
      trace " Cod Rech:" || ret_disposicion_cod_rechazo;

      -- se verifica que los montos no esten nulos
      IF ( tmp_ret_det_aiv97 IS NULL ) THEN
         LET tmp_ret_det_aiv97 = 0;
      END IF

      IF ( tmp_ret_det_aiv92 IS NULL ) THEN
         LET tmp_ret_det_aiv92 = 0;
      END IF

      IF ( tmp_ret_det_fondo_subcta_viv72 IS NULL ) THEN
         LET tmp_ret_det_fondo_subcta_viv72 = 0;
      END IF

      -- Asignando datos a registro de detalle disposicion
      LET ret_disposicion_id_solicitud          = v_id_solicitud;
      LET ret_disposicion_id_derechohabiente    = v_id_derechohabiente;
      LET ret_disposicion_f_solicitud           = tmp_ret_det_f_solicitud_trab;
      LET ret_disposicion_id_ret_matriz_derecho = v_id_ret_matriz_derecho;
      LET ret_disposicion_sec_pension           = tmp_ret_det_sec_pension;
      LET ret_disposicion_diag_registro         = tmp_ret_det_diagnostico_reg;
      LET ret_disposicion_folio                 = p_folio;
      LET ret_disposicion_curp                  = tmp_ret_det_curp;
      LET ret_disposicion_nombre_afore          = tmp_ret_det_nombre_afore;
      LET ret_disposicion_paterno_afore         = tmp_ret_det_paterno_afore;
      LET ret_disposicion_materno_afore         = tmp_ret_det_materno_afore;
      LET ret_disposicion_f_inicio_pension      = tmp_ret_det_f_inicio_pension;
      LET ret_disposicion_f_resolucion          = tmp_ret_det_f_emision_resol;
      LET ret_disposicion_porcentaje_valuacion  = tmp_ret_det_porc_valuacion / 100;
      LET ret_disposicion_semanas_cotizadas     = tmp_ret_det_sem_cotizadas;
      LET ret_disposicion_cve_doc_probatorio    = tmp_ret_det_cve_doc_probatorio;
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

      -- calculo de las AIVs 97 y 92 a pesos
      LET ret_disposicion_aivs_viv97 = (tmp_ret_det_aiv97 / 1000000);
      LET ret_disposicion_aivs_viv92 = (tmp_ret_det_aiv92 / 1000000);

      LET v_pesos_aiv97 = ret_disposicion_aivs_viv97 * v_valor_fondo;
      LET v_pesos_aiv92 = ret_disposicion_aivs_viv92 * v_valor_fondo;

      -- incremento del importe total
      -- LET v_sumario_importe_total    = v_sumario_importe_total + v_pesos_aiv97 + v_pesos_aiv92 + ret_disposicion_importe_viv72;
      -- No se considera la subcuenta del monto de vivienda 72, segun requerimiento PRODINFXVI-128
      LET v_sumario_importe_total = v_sumario_importe_total + v_pesos_aiv97 + v_pesos_aiv92;
      trace " Despues asignacion";
      trace " Cod Rech:" || ret_disposicion_cod_rechazo;

      -- 30jul2013 SE VERIFICA SUFICIENCIA DE SALDOS
      -- vivienda 97
      IF ( ret_disposicion_aivs_viv97 > 0 ) THEN
         -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                       ret_disposicion_id_derechohabiente,
                                       4,
                                       NULL)
         INTO v_resultado_consulta, v_saldo_97_aivs, v_saldo_97_pesos;
         -- Obtiene el saldo de la subcuenta de aportaciones voluntarias de vivienda,
         -- se verifica si el derechohabiente tiene saldo suficiente para efectuar el retiro
         LET v_saldo_vol_aivs     = 0;
         LET v_saldo_vol_pesos    = 0;
         LET v_resultado_consulta = 0;

         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                       ret_disposicion_id_derechohabiente,
                                       55,
                                       NULL)
         INTO v_resultado_consulta, v_saldo_vol_aivs, v_saldo_vol_pesos;

         -- se verifica si el monto solicitado es mayor al saldo del trabajador
         -- 02dic2013. Se tolera hasta 1 AIV de diferencia
         --IF ( ret_disposicion_aivs_viv97 > ( v_saldo_97_aivs + 1 ) ) THEN
         IF v_saldo_97_aivs + v_saldo_vol_aivs > 0 THEN -- Si el saldo es menor o igual a cero se rechaza la solicitud segun req PRODINFXVI-119
            IF ( ret_disposicion_aivs_viv97 > ( v_saldo_97_aivs + v_saldo_vol_aivs + v_max_aivs_sobregiro ) ) THEN  --se permite el sobregiro
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,4,11,
                                                              v_saldo_97_aivs, v_saldo_97_pesos,
                                                              p_folio,TODAY,CURRENT HOUR TO SECOND);
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,55,11,
                                                              v_saldo_vol_aivs, v_saldo_vol_pesos,
                                                              p_folio,TODAY,CURRENT HOUR TO SECOND);
               -- se rechaza la solicitud por saldo insuficiente
               LET ret_disposicion_cod_rechazo = v_monto_viv97_invalido; -- saldo insuficiente
               LET ret_disposicion_estado_solicitud = 100; -- rechazada

            END IF
         ELSE 
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,4,11,
                                                           v_saldo_97_aivs, v_saldo_97_pesos,
                                                           p_folio,TODAY,CURRENT HOUR TO SECOND);
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,55,11,
                                                           v_saldo_vol_aivs, v_saldo_vol_pesos,
                                                           p_folio,TODAY,CURRENT HOUR TO SECOND);
            -- se rechaza la solicitud por saldo insuficiente
            LET ret_disposicion_cod_rechazo = v_monto_viv97_invalido; -- saldo insuficiente
            LET ret_disposicion_estado_solicitud = 100; -- rechazada
         END IF
      END IF

      IF ( ret_disposicion_aivs_viv92 > 0 ) THEN
         -- se verifica si el derechohabiente tiene saldo suficiente
         EXECUTE FUNCTION fn_saldo_dia(NULL,
                                       ret_disposicion_id_derechohabiente,
                                       8,NULL)
         INTO v_resultado_consulta, v_saldo_92_aivs, v_saldo_92_pesos;

         -- si el saldo solicitado es mayor al poseido
         -- 02dic2013. Se tolera hasta 1 AIV de diferencia
         --IF ( ret_disposicion_aivs_viv92 > ( v_saldo_92_aivs + 1 )) THEN
         IF   v_saldo_92_aivs > 0 THEN 
            IF ( ret_disposicion_aivs_viv92 > ( v_saldo_92_aivs + v_max_aivs_sobregiro )) THEN
               EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,8,11,
                                                              v_saldo_92_aivs, v_saldo_92_pesos,
                                                              p_folio,TODAY,CURRENT HOUR TO SECOND);
               -- se rechaza la solicitud por saldo insuficiente
               LET ret_disposicion_cod_rechazo = v_monto_viv92_invalido; -- saldo insuficiente
               LET ret_disposicion_estado_solicitud = 100; -- rechazada
            END IF
         ELSE
            EXECUTE PROCEDURE fn_ret_inserta_ret_his_saldo(ret_disposicion_id_solicitud,8,11,
                                                           v_saldo_92_aivs, v_saldo_92_pesos,
                                                           p_folio,TODAY,CURRENT HOUR TO SECOND);
            LET ret_disposicion_cod_rechazo = v_monto_viv92_invalido; -- saldo insuficiente
            LET ret_disposicion_estado_solicitud = 100; -- rechazada
         END IF
      END IF

      -- se intenta marcar la cuenta si no fue rechazada por saldo
      IF ( ret_disposicion_cod_rechazo = 0 ) THEN
         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_marca_cuenta(ret_disposicion_id_derechohabiente
                                         ,v_marca_disposicion -- marca de disposicion
                                         ,ret_disposicion_id_solicitud
                                         ,ret_disposicion_folio
                                         ,0 -- estado marca
                                         ,0 -- codigo de rechazo
                                         ,0 -- marca de la causa
                                         ,NULL -- fecha de la causa
                                         ,p_usuario_cod
                                         ,p_proceso_cod)
                                     INTO v_i_estado_marca;

         -- si no se pudo marcar, se rechaza el registro
         IF ( v_i_estado_marca > 0 ) THEN
            LET ret_disposicion_estado_solicitud = 100; -- rechazada

            -- si el error es por convencia de marca, se verifica si fue por credito, unificacion o separacion
            IF ( v_i_estado_marca = 501 OR v_i_estado_marca = 502 ) THEN
               -- marca de unificacion
               LET ret_disposicion_cod_rechazo = v_error_det_marca_unificacion;
            ELSE
               -- se revisa si la marca es por un credito de tipo garantia (anualidad o en garantia)
               SELECT tpo_originacion, marca_inf
               INTO   v_tpo_originacion, v_marca_infonavit
               FROM   cat_tipo_credito
               WHERE  tpo_originacion IN (2,4)
               AND    marca_inf = v_i_estado_marca;

               -- si se encontro, es un credito con alguna garantia
               IF ( v_marca_infonavit IS NOT NULL ) THEN
                  LET ret_disposicion_cod_rechazo = v_error_det_saldo_en_garantia;
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
                     LET ret_disposicion_cod_rechazo = v_error_det_marca_credito;
                  ELSE
                     IF ( v_i_estado_marca = 280 ) THEN
                        -- marca de separacion de cuentas
                        LET ret_disposicion_cod_rechazo = v_error_det_marca_separacion;
                     ELSE
                        -- si se trata del mismo tipo de retiro, es decir, disposicion
                        IF ( v_i_estado_marca = 805 ) THEN
                           -- se marca como registro duplicado
                           LET ret_disposicion_cod_rechazo = v_error_det_registro_duplicado_archivo;
                        ELSE
                           -- se trata de otra marca
                           LET ret_disposicion_cod_rechazo = v_error_det_marca_no_convive;
                        END IF -- marca de retiro
                     END IF -- separacion de cuentas
                  END IF -- marca credito tradicional
               END IF  -- marca de credito con garantia
            END IF -- marca de unificacion

         END IF -- error al marcar
      END IF
      trace "Antes ret_disposicion :" || ret_disposicion_cod_rechazo;
      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
      INSERT INTO ret_disposicion (
         id_solicitud,id_derechohabiente,
         f_solicitud,id_ret_matriz_derecho,
         sec_pension,diag_registro,
         folio,curp,
         nombre_afore,paterno_afore,
         materno_afore,f_inicio_pension,
         f_resolucion,porcentaje_valuacion,
         semanas_cotizadas,cve_doc_probatorio,
         f_nacimiento,aseguradora,
         actuario,num_plan_privado,
         periodo_primer_pago,aivs_viv97,
         aivs_viv92,consec_trabajador,
         importe_viv72,estado_sub_viv,
         cve_afore,cod_rechazo,
         estado_solicitud
      )
      VALUES (
         ret_disposicion_id_solicitud,ret_disposicion_id_derechohabiente,
         ret_disposicion_f_solicitud,ret_disposicion_id_ret_matriz_derecho,
         ret_disposicion_sec_pension,ret_disposicion_diag_registro,
         ret_disposicion_folio,ret_disposicion_curp,
         ret_disposicion_nombre_afore,ret_disposicion_paterno_afore,
         ret_disposicion_materno_afore,ret_disposicion_f_inicio_pension,
         ret_disposicion_f_resolucion,ret_disposicion_porcentaje_valuacion,
         ret_disposicion_semanas_cotizadas,ret_disposicion_cve_doc_probatorio,
         ret_disposicion_f_nacimiento,ret_disposicion_aseguradora,
         ret_disposicion_actuario,ret_disposicion_num_plan_privado,
         ret_disposicion_periodo_primer_pago,ret_disposicion_aivs_viv97,
         ret_disposicion_aivs_viv92,ret_disposicion_consec_trabajador,
         ret_disposicion_importe_viv72,ret_disposicion_estado_sub_viv,
         ret_disposicion_cve_afore,ret_disposicion_cod_rechazo,
         ret_disposicion_estado_solicitud
      );
      trace "Insertado:" || ret_disposicion_cod_rechazo;

      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

   END FOREACH;

   UPDATE STATISTICS FOR TABLE ret_disposicion;
   UPDATE STATISTICS FOR TABLE ret_disposicion_rch;
   UPDATE STATISTICS FOR TABLE ret_cza_disposicion;
   UPDATE STATISTICS FOR TABLE ret_cza_disposicion_rch;
   UPDATE STATISTICS FOR TABLE ret_his_saldo;

   -- se actualiza el importe total para el registro del encabezado
   UPDATE ret_cza_disposicion
   SET    total_importe = v_sumario_importe_total
   WHERE  folio         = p_folio;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


