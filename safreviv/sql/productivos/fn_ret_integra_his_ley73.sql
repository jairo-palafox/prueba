






CREATE FUNCTION "safreviv".fn_ret_integra_his_ley73(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)


-- encabezado de la tabla temporal
-- tmp_ret_cza_his_ley73
DEFINE tmp_ret_cza_l73_tpo_registro               CHAR(2)      ;
DEFINE tmp_ret_cza_l73_cve_ent_origen             CHAR(2)      ;
DEFINE tmp_ret_cza_l73_cve_ent_destino            CHAR(2)      ;
DEFINE tmp_ret_cza_l73_f_transmision              DATE         ;
DEFINE tmp_ret_cza_l73_num_trab_pensionados       DECIMAL(6,0) ;
DEFINE tmp_ret_cza_l73_saldo_viv97_transferido    DECIMAL(13,0);
DEFINE tmp_ret_cza_l73_saldo_viv97_no_transferido DECIMAL(13,0);
DEFINE tmp_ret_cza_l73_saldo_viv97_entregado      DECIMAL(13,0);
DEFINE tmp_ret_cza_l73_filler                     CHAR(461)    ;

-- detalle de la tabla temporal
--tmp_ret_det_his_ley73
DEFINE tmp_ret_det_l73_tpo_registro               CHAR(2)      ;
DEFINE tmp_ret_det_l73_nss                        CHAR(11)     ;
DEFINE tmp_ret_det_l73_curp                       CHAR(18)     ;
DEFINE tmp_ret_det_l73_ap_paterno_trab            CHAR(40)     ;
DEFINE tmp_ret_det_l73_ap_materno_trab            CHAR(40)     ;
DEFINE tmp_ret_det_l73_nombre_trab                CHAR(40)     ;
DEFINE tmp_ret_det_l73_estatus_transferencia      CHAR(1)      ;
DEFINE tmp_ret_det_l73_folio_formulario_pago      CHAR(4)      ;
DEFINE tmp_ret_det_l73_saldo_viv97_transferido    DECIMAL(13,0);
DEFINE tmp_ret_det_l73_saldo_viv97_no_transferido DECIMAL(13,0);
DEFINE tmp_ret_det_l73_f_transferencia            DATE         ;
DEFINE tmp_ret_det_l73_tpo_trabajador             CHAR(1)      ;
DEFINE tmp_ret_det_l73_estatus_demanda_amparo     CHAR(1)      ;
DEFINE tmp_ret_det_l73_num_juicio                 CHAR(5)      ;
DEFINE tmp_ret_det_l73_ano_juicio                 CHAR(4)      ;
DEFINE tmp_ret_det_l73_juzgado                    CHAR(100)    ;
DEFINE tmp_ret_det_l73_num_expediente             CHAR(7)      ;
DEFINE tmp_ret_det_l73_ap_paterno_benef           CHAR(40)     ;
DEFINE tmp_ret_det_l73_ap_materno_benef           CHAR(40)     ;
DEFINE tmp_ret_det_l73_nombre_benef               CHAR(40)     ;
DEFINE tmp_ret_det_l73_num_oficio_infonavit       CHAR(25)     ;
DEFINE tmp_ret_det_l73_f_entrega_recursos         DATE         ;
DEFINE tmp_ret_det_l73_f_entrega_trabajador       DATE         ;
DEFINE tmp_ret_det_l73_saldo_viv97_entregado      DECIMAL(13,0);
DEFINE tmp_ret_det_l73_origen_entrega             CHAR(1)      ;
DEFINE tmp_ret_det_l73_mecanismo_entrega          CHAR(1)      ;
DEFINE tmp_ret_det_l73_banco_receptor             CHAR(2)      ;
DEFINE tmp_ret_det_l73_cuenta_bancaria            CHAR(16)     ;
DEFINE tmp_ret_det_l73_clabe                      CHAR(18)     ;

-- tablas destino
-- ret_his_anexo1
DEFINE ret_his_anexo1_id_solicitud              decimal(9,0) ;
DEFINE ret_his_anexo1_estado_solicitud          smallint     ;
DEFINE ret_his_anexo1_cod_rechazo               smallint     ;
DEFINE ret_his_anexo1_estado_transferencia      CHAR(1)      ;
DEFINE ret_his_anexo1_tpo_pensionado            CHAR(1)      ;
DEFINE ret_his_anexo1_estado_demanda_amparo     CHAR(1)      ;
DEFINE ret_his_anexo1_origen_entrega            CHAR(1)      ;
DEFINE ret_his_anexo1_mecanismo_entrega         CHAR(1)      ;
DEFINE ret_his_anexo1_folio                     decimal(9,0) ;
DEFINE ret_his_anexo1_id_derechohabiente        decimal(9,0) ;
DEFINE ret_his_anexo1_nss                       char(11)     ;
DEFINE ret_his_anexo1_curp                      char(18)     ;
DEFINE ret_his_anexo1_ap_paterno_af             char(40)     ;
DEFINE ret_his_anexo1_ap_materno_af             char(40)     ;
DEFINE ret_his_anexo1_nombre_af                 char(40)     ;
DEFINE ret_his_anexo1_folio_pago                CHAR(4)      ;
DEFINE ret_his_anexo1_pesos_viv97_transf        decimal(13,2);
DEFINE ret_his_anexo1_pesos_viv97_no_transf     decimal(13,2);
DEFINE ret_his_anexo1_f_transferencia           date         ;
DEFINE ret_his_anexo1_num_juicio                CHAR(5)      ;
DEFINE ret_his_anexo1_ano_juicio                CHAR(4)      ;
DEFINE ret_his_anexo1_juzgado                   char(100)    ;
DEFINE ret_his_anexo1_num_expediente            CHAR(7)      ;
DEFINE ret_his_anexo1_ap_paterno_benef          char(40)     ;
DEFINE ret_his_anexo1_ap_materno_benef          char(40)     ;
DEFINE ret_his_anexo1_nombre_benef              char(40)     ;
DEFINE ret_his_anexo1_num_oficio_infonavit      char(25)     ;
DEFINE ret_his_anexo1_f_entrega_recursos_tesofe date         ;
DEFINE ret_his_anexo1_f_entrega_recursos_af     date         ;
DEFINE ret_his_anexo1_saldo_pagado_viv97        decimal(13,2);
DEFINE ret_his_anexo1_banco_receptor            CHAR(2)      ;
DEFINE ret_his_anexo1_num_cuenta                CHAR(16)     ;
DEFINE ret_his_anexo1_clabe                     CHAR(18)     ;

-- variables de soporte al proceso
DEFINE v_id_derechohabiente                 DECIMAL(9,0);
DEFINE v_id_solicitud                       DECIMAL(9,0);
-- =============================================================================
-- para calcular las AIVs a pesos
DEFINE v_valor_fondo                        DECIMAL(14,6);
DEFINE v_pesos_aiv97                        decimal(14,6);
DEFINE v_pesos_aiv92                        decimal(14,6);
DEFINE v_saldo_aivs                         DECIMAL(18,6); -- saldo del derechohabiente en viv92
DEFINE v_saldo_pesos                        DECIMAL(18,6); -- saldo del derechohabiente en viv97
DEFINE v_resultado_consulta                 SMALLINT;

-- para rechazos
DEFINE v_b_rechazo_encabezado               SMALLINT;
DEFINE v_b_rechazo_detalle                  SMALLINT;
DEFINE v_validar_3_primeros_campos          VARCHAR(6); -- se concatenan los 3 primeros campos para validar
DEFINE v_afore_cod                          SMALLINT; -- clave de afore
-- id matriz derecho
DEFINE v_id_ret_matriz_derecho              SMALLINT; -- id de la matriz de derecho de retiros
-- RECUPERADOS
 
DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
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
DEFINE v_error_estatus_transf_invalido           SMALLINT; -- error 
DEFINE v_error_saldo_viv97_transf_invalido       SMALLINT; -- error 
DEFINE v_error_saldo_viv97_no_transf_invalido    SMALLINT; -- error 
DEFINE v_error_f_trans_invalida                  SMALLINT; -- error 
DEFINE v_error_tpo_trabajador_invalido           SMALLINT; -- error 
DEFINE v_error_estatus_demanda_invalido          SMALLINT; -- error 
DEFINE v_error_num_juicio_invalido               SMALLINT; -- error 
DEFINE v_error_ano_juicio_invalido               SMALLINT; -- error 
DEFINE v_error_juzgado_invalido                  SMALLINT; -- error 
DEFINE v_error_num_expediente_invalido           SMALLINT; -- error 
DEFINE v_error_num_oficio_invalido               SMALLINT; -- error 
DEFINE v_error_f_entrega_invalido                SMALLINT; -- error 
DEFINE v_error_saldo_viv97_entregado_invalido    SMALLINT; -- error 
DEFINE v_error_origen_entrega_invalido           SMALLINT; -- error 
DEFINE v_error_mecanismo_entrega_invalido        SMALLINT; -- error 
DEFINE v_error_banco_receptor_invalido           SMALLINT; -- error 
DEFINE v_error_cuenta_bancaria_invalida          SMALLINT; -- error 
DEFINE v_error_clabe_invalida                    SMALLINT; -- error 
DEFINE v_error_curp_invalido                     SMALLINT;
DEFINE v_error_ap_paterno_trabajador_invalido    SMALLINT;
DEFINE v_error_ap_materno_trabajador_invalido    SMALLINT;
DEFINE v_error_nombre_trabajador_invalido        SMALLINT;
DEFINE v_error_folio_formulario_pago_invalido    SMALLINT;

-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 
-- para marcar las cuentas
DEFINE v_i_estado_marca                          INTEGER;
DEFINE v_marca_fc                                INTEGER; -- 808
DEFINE v_movimiento                              SMALLINT; -- clave de movimiento

-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_l73_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_fc.txt";

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
   LET tmp_ret_det_l73_nss = NULL;

   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = P_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1; -- etapa de carga
   
   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion 
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   UPDATE bat_ctr_proceso
   SET    folio       = P_folio
   WHERE  proceso_cod = p_proceso_cod 
   AND    pid         = p_pid;

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
   
   LET v_error_estatus_transf_invalido           = 10;
   LET v_error_saldo_viv97_transf_invalido       = 11;
   LET v_error_saldo_viv97_no_transf_invalido    = 12;
   LET v_error_f_trans_invalida                  = 13;
   LET v_error_tpo_trabajador_invalido           = 14;
   LET v_error_estatus_demanda_invalido          = 15;
   LET v_error_num_juicio_invalido               = 16;
   LET v_error_ano_juicio_invalido               = 17;
   LET v_error_juzgado_invalido                  = 18;
   LET v_error_num_expediente_invalido           = 18;
   LET v_error_num_oficio_invalido               = 20;
   LET v_error_f_entrega_invalido                = 21;
   LET v_error_saldo_viv97_entregado_invalido    = 22;
   LET v_error_origen_entrega_invalido           = 23;
   LET v_error_mecanismo_entrega_invalido        = 24;
   LET v_error_banco_receptor_invalido           = 25;
   LET v_error_cuenta_bancaria_invalida          = 26;
   LET v_error_clabe_invalida                    = 27;
   -- 13dic2013. Se agregan estas validaciones
   LET v_error_curp_invalido                     = 28;
   LET v_error_ap_paterno_trabajador_invalido    = 29;
   LET v_error_ap_materno_trabajador_invalido    = 30;
   LET v_error_nombre_trabajador_invalido        = 31;
   LET v_error_folio_formulario_pago_invalido    = 32;

   
 
 
   -- ===================================================
   -- validaciones encabezado contra detalle
   --- se cuentan los registros de la tabla temporal de detalle
{ 
   SELECT COUNT(*)
   INTO v_numero_registros
   FROM safre_tmp:tmp_ret_det_disposicion;

   -- se cuentan los registros del detalle y se validan contra el detalle del sumario
   SELECT COUNT(*)
   INTO v_total_registros
   FROM
      safre_tmp:tmp_ret_det_disposicion;
   
   SELECT total_registros
   INTO v_sumario_total_registros
   FROM
      safre_tmp:tmp_ret_sum_disposicion;
 
   -- si no coincide el total es un error
   IF ( v_total_registros <> v_sumario_total_registros ) THEN
      --TRACE("No coinciden numero de registros cargados contra los dados en archivo");
      --TRACE("Cargados:");
      --TRACE(v_total_registros);
      --TRACE("En sumario de archivo:");
      --TRACE(v_sumario_total_registros);

      -- se rechaza el lote y no integra
      --RETURN v_error_cza_reg_totales_no_coinciden; -- codigo de error
      LET v_si_resultado = v_error_cza_reg_totales_no_coinciden;
      LET v_c_msj = "No coinciden numero de registros cargados contra los dados en archivo.";
      RETURN v_si_resultado, isam_err, v_c_msj;
   END IF

 }
 
   -- integracion de encabezado
   FOREACH
   SELECT
      tpo_registro               ,
      cve_ent_origen             ,
      cve_ent_destino            ,
      f_transmision              ,
      num_trab_pensionados       ,
      saldo_viv97_transferido    ,
      saldo_viv97_no_transferido ,
      saldo_viv97_entregado      
   INTO
      tmp_ret_cza_l73_tpo_registro               ,
      tmp_ret_cza_l73_cve_ent_origen             ,
      tmp_ret_cza_l73_cve_ent_destino            ,
      tmp_ret_cza_l73_f_transmision              ,
      tmp_ret_cza_l73_num_trab_pensionados       ,
      tmp_ret_cza_l73_saldo_viv97_transferido    ,
      tmp_ret_cza_l73_saldo_viv97_no_transferido ,
      tmp_ret_cza_l73_saldo_viv97_entregado      
   FROM
   safre_tmp:tmp_ret_cza_his_ley73
 
   END FOREACH;
   
   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;
   
   CREATE TEMP TABLE tmp_codigos_rechazo (
   id_codigo       SMALLINT,
   codigo_rechazo SMALLINT
   );
   
   -- integracion de detalle
   FOREACH
   SELECT
      nss                        ,
      curp                       ,
      ap_paterno_trab            ,
      ap_materno_trab            ,
      nombre_trab                ,
      estatus_transferencia      ,
      folio_formulario_pago      ,
      saldo_viv97_transferido    ,
      saldo_viv97_no_transferido ,
      f_transferencia            ,
      tpo_trabajador             ,
      estatus_demanda_amparo     ,
      num_juicio                 ,
      ano_juicio                 ,
      juzgado                    ,
      num_expediente             ,
      ap_paterno_benef           ,
      ap_materno_benef           ,
      nombre_benef               ,
      num_oficio_infonavit       ,
      f_entrega_recursos         ,
      f_entrega_trabajador       ,
      saldo_viv97_entregado      ,
      origen_entrega             ,
      mecanismo_entrega          ,
      banco_receptor             ,
      cuenta_bancaria            ,
      clabe                      
   INTO
      tmp_ret_det_l73_nss                        ,
      tmp_ret_det_l73_curp                       ,
      tmp_ret_det_l73_ap_paterno_trab            ,
      tmp_ret_det_l73_ap_materno_trab            ,
      tmp_ret_det_l73_nombre_trab                ,
      tmp_ret_det_l73_estatus_transferencia      ,
      tmp_ret_det_l73_folio_formulario_pago      ,
      tmp_ret_det_l73_saldo_viv97_transferido    ,
      tmp_ret_det_l73_saldo_viv97_no_transferido ,
      tmp_ret_det_l73_f_transferencia            ,
      tmp_ret_det_l73_tpo_trabajador             ,
      tmp_ret_det_l73_estatus_demanda_amparo     ,
      tmp_ret_det_l73_num_juicio                 ,
      tmp_ret_det_l73_ano_juicio                 ,
      tmp_ret_det_l73_juzgado                    ,
      tmp_ret_det_l73_num_expediente             ,
      tmp_ret_det_l73_ap_paterno_benef           ,
      tmp_ret_det_l73_ap_materno_benef           ,
      tmp_ret_det_l73_nombre_benef               ,
      tmp_ret_det_l73_num_oficio_infonavit       ,
      tmp_ret_det_l73_f_entrega_recursos         ,
      tmp_ret_det_l73_f_entrega_trabajador       ,
      tmp_ret_det_l73_saldo_viv97_entregado      ,
      tmp_ret_det_l73_origen_entrega             ,
      tmp_ret_det_l73_mecanismo_entrega          ,
      tmp_ret_det_l73_banco_receptor             ,
      tmp_ret_det_l73_cuenta_bancaria            ,
      tmp_ret_det_l73_clabe                      
   FROM
   safre_tmp:tmp_ret_det_his_ley73
   
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;
   
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
      
      -- se inicia el contador     
      LET v_indice_codigos_rechazo = 1;

      -- se busca el derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente
      FROM   afi_derechohabiente
      WHERE  nss = tmp_ret_det_l73_nss;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;
         LET v_id_derechohabiente   = 0;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
      
      -- el CURP debe existir
      IF ( tmp_ret_det_l73_curp IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_curp_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- Apellido Paterno del trabajador debe existir
      IF ( tmp_ret_det_l73_ap_paterno_trab IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_ap_paterno_trabajador_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- Apellido Materno del trabajador debe existir
      IF ( tmp_ret_det_l73_ap_materno_trab IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_ap_materno_trabajador_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- Nombre del trabajador debe existir
      IF ( tmp_ret_det_l73_nombre_trab IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_nombre_trabajador_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      -- Folio del formulario de pago debe existir
      IF ( tmp_ret_det_l73_folio_formulario_pago IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_folio_formulario_pago_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      
      -- estatus de transferencia debe ser 0 o 1
      IF ( tmp_ret_det_l73_estatus_transferencia < 0 OR tmp_ret_det_l73_estatus_transferencia > 1 ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_estatus_transf_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;      
      END IF

      -- si estatus de transferencia es 1, la fecha de transferencia debe ser valida
      IF ( tmp_ret_det_l73_estatus_transferencia = 1 ) THEN
         IF ( tmp_ret_det_l73_f_transferencia IS NULL ) THEN
            -- se marca la bandera de rechazo de detalle
            LET v_b_rechazo_detalle    = 1;

            INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_f_trans_invalida);

            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF
      END IF

      -- tipo de trabajador pensionado puede ser 0,1,2
      IF ( tmp_ret_det_l73_tpo_trabajador < 0 AND tmp_ret_det_l73_tpo_trabajador > 2 ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_tpo_trabajador_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF

      -- estatus de demanda pueder ser 0,1,2,3
      IF ( tmp_ret_det_l73_estatus_demanda_amparo < 0 AND tmp_ret_det_l73_estatus_demanda_amparo > 3 ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_estatus_demanda_invalido);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF


      
      -- ================================================================
      -- VERIFICACION DE ERRORES ENCONTRADOS
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
         -- se rechaza la solicitud
         LET ret_his_anexo1_estado_solicitud = 100; -- rechazada
      ELSE
         LET v_motivo_rechazo_1 = 0;
         LET ret_his_anexo1_estado_solicitud = 10; -- aceptada
      END IF
      
      -- se obtiene el numero de solicitud
      SELECT seq_ret_solicitud.NEXTVAL
      INTO v_id_solicitud
      FROM systables
      WHERE tabid = 1;

      -- se transfieren los datos a registro
      LET ret_his_anexo1_id_solicitud              = v_id_solicitud; -- decimal(9,0) ;
      LET ret_his_anexo1_cod_rechazo               = v_motivo_rechazo_1;
      LET ret_his_anexo1_estado_transferencia      = tmp_ret_det_l73_estatus_transferencia; -- smallint     ;
      LET ret_his_anexo1_tpo_pensionado            = tmp_ret_det_l73_tpo_trabajador; -- smallint     ;
      LET ret_his_anexo1_estado_demanda_amparo     = tmp_ret_det_l73_estatus_demanda_amparo; -- smallint     ;
      LET ret_his_anexo1_origen_entrega            = tmp_ret_det_l73_origen_entrega; -- smallint     ;
      LET ret_his_anexo1_mecanismo_entrega         = tmp_ret_det_l73_mecanismo_entrega; -- smallint     ;
      LET ret_his_anexo1_folio                     = p_folio; -- decimal(9,0) ;
      LET ret_his_anexo1_id_derechohabiente        = v_id_derechohabiente; -- decimal(9,0) ;
      LET ret_his_anexo1_nss                       = tmp_ret_det_l73_nss; -- char(11)     ;
      LET ret_his_anexo1_curp                      = tmp_ret_det_l73_curp; -- char(18)     ;
      LET ret_his_anexo1_ap_paterno_af             = tmp_ret_det_l73_ap_paterno_trab; -- char(40)     ;
      LET ret_his_anexo1_ap_materno_af             = tmp_ret_det_l73_ap_materno_trab; -- char(40)     ;
      LET ret_his_anexo1_nombre_af                 = tmp_ret_det_l73_nombre_trab; -- char(40)     ;
      LET ret_his_anexo1_folio_pago                = tmp_ret_det_l73_folio_formulario_pago; -- smallint     ;
      LET ret_his_anexo1_pesos_viv97_transf        = tmp_ret_det_l73_saldo_viv97_transferido / 100; -- decimal(13,2);
      LET ret_his_anexo1_pesos_viv97_no_transf     = tmp_ret_det_l73_saldo_viv97_no_transferido / 100; -- decimal(13,2);
      LET ret_his_anexo1_f_transferencia           = tmp_ret_det_l73_f_transferencia; -- date         ;
      LET ret_his_anexo1_num_juicio                = tmp_ret_det_l73_num_juicio; -- integer      ;
      LET ret_his_anexo1_ano_juicio                = tmp_ret_det_l73_ano_juicio; -- smallint     ;
      LET ret_his_anexo1_juzgado                   = tmp_ret_det_l73_juzgado; -- char(100)    ;
      LET ret_his_anexo1_num_expediente            = tmp_ret_det_l73_num_expediente; -- decimal(7,0) ;
      LET ret_his_anexo1_ap_paterno_benef          = tmp_ret_det_l73_ap_paterno_benef; -- char(40)     ;
      LET ret_his_anexo1_ap_materno_benef          = tmp_ret_det_l73_ap_materno_benef; -- char(40)     ;
      LET ret_his_anexo1_nombre_benef              = tmp_ret_det_l73_nombre_benef; -- char(40)     ;
      LET ret_his_anexo1_num_oficio_infonavit      = tmp_ret_det_l73_num_oficio_infonavit; -- char(25)     ;
      LET ret_his_anexo1_f_entrega_recursos_tesofe = tmp_ret_det_l73_f_entrega_recursos; -- date         ;
      LET ret_his_anexo1_f_entrega_recursos_af     = tmp_ret_det_l73_f_entrega_trabajador; -- date         ;
      LET ret_his_anexo1_saldo_pagado_viv97        = tmp_ret_det_l73_saldo_viv97_entregado / 100; -- decimal(13,2);
      LET ret_his_anexo1_banco_receptor            = tmp_ret_det_l73_banco_receptor; -- smallint     ;
      LET ret_his_anexo1_num_cuenta                = tmp_ret_det_l73_cuenta_bancaria; -- decimal(16,0);
      LET ret_his_anexo1_clabe                     = tmp_ret_det_l73_clabe; -- decimal(18,0);

      
      -- se inserta en tabla destino
      INSERT INTO ret_his_anexo1 (
         id_solicitud              ,
         estado_solicitud          ,
         cod_rechazo               ,
         estado_transferencia      ,
         tpo_pensionado            ,
         estado_demanda_amparo     ,
         origen_entrega            ,
         mecanismo_entrega         ,
         folio                     ,
         id_derechohabiente        ,
         nss                       ,
         curp                      ,
         ap_paterno_af             ,
         ap_materno_af             ,
         nombre_af                 ,
         folio_pago                ,
         pesos_viv97_transf        ,
         pesos_viv97_no_transf     ,
         f_transferencia           ,
         num_juicio                ,
         ano_juicio                ,
         juzgado                   ,
         num_expediente            ,
         ap_paterno_benef          ,
         ap_materno_benef          ,
         nombre_benef              ,
         num_oficio_infonavit      ,
         f_entrega_recursos_tesofe ,
         f_entrega_recursos_af     ,
         saldo_pagado_viv97        ,
         banco_receptor            ,
         num_cuenta                ,
         clabe                     
      ) VALUES (
         ret_his_anexo1_id_solicitud              ,
         ret_his_anexo1_estado_solicitud          ,
         ret_his_anexo1_cod_rechazo               ,
         ret_his_anexo1_estado_transferencia      ,
         ret_his_anexo1_tpo_pensionado            ,
         ret_his_anexo1_estado_demanda_amparo     ,
         ret_his_anexo1_origen_entrega            ,
         ret_his_anexo1_mecanismo_entrega         ,
         ret_his_anexo1_folio                     ,
         ret_his_anexo1_id_derechohabiente        ,
         ret_his_anexo1_nss                       ,
         ret_his_anexo1_curp                      ,
         ret_his_anexo1_ap_paterno_af             ,
         ret_his_anexo1_ap_materno_af             ,
         ret_his_anexo1_nombre_af                 ,
         ret_his_anexo1_folio_pago                ,
         ret_his_anexo1_pesos_viv97_transf        ,
         ret_his_anexo1_pesos_viv97_no_transf     ,
         ret_his_anexo1_f_transferencia           ,
         ret_his_anexo1_num_juicio                ,
         ret_his_anexo1_ano_juicio                ,
         ret_his_anexo1_juzgado                   ,
         ret_his_anexo1_num_expediente            ,
         ret_his_anexo1_ap_paterno_benef          ,
         ret_his_anexo1_ap_materno_benef          ,
         ret_his_anexo1_nombre_benef              ,
         ret_his_anexo1_num_oficio_infonavit      ,
         ret_his_anexo1_f_entrega_recursos_tesofe ,
         ret_his_anexo1_f_entrega_recursos_af     ,
         ret_his_anexo1_saldo_pagado_viv97        ,
         ret_his_anexo1_banco_receptor            ,
         ret_his_anexo1_num_cuenta                ,
         ret_his_anexo1_clabe                     
      );
   
   END FOREACH;
   
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_his_anexo1;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_l73_nss;
END FUNCTION;


