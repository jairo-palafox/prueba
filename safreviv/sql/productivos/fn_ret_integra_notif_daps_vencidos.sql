






CREATE FUNCTION "safreviv".fn_ret_integra_notif_daps_vencidos(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                  p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                  p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250)

-- detalle de la tabla temporal
--tmp_ret_not_dap_vencido
DEFINE tmp_ret_not_dap_vencido_num_docto                  CHAR(10)      ;
DEFINE tmp_ret_not_dap_vencido_posicion                   CHAR(3)     ;
DEFINE tmp_ret_not_dap_vencido_anio                       CHAR(4)     ;
DEFINE tmp_ret_not_dap_vencido_division                   CHAR(4)     ;
DEFINE tmp_ret_not_dap_vencido_descripcion                CHAR(8)     ;
DEFINE tmp_ret_not_dap_vencido_f_liquidacion              DATE     ;
DEFINE tmp_ret_not_dap_vencido_usuario                    CHAR(8)      ;
DEFINE tmp_ret_not_dap_vencido_referencia                 CHAR(11)      ;
DEFINE tmp_ret_not_dap_vencido_txt_docto                  CHAR(25);
DEFINE tmp_ret_not_dap_vencido_acreedor                   CHAR(8);
DEFINE tmp_ret_not_dap_vencido_nombre                     CHAR(33)         ;
DEFINE tmp_ret_not_dap_vencido_vp                         CHAR(2)      ;
DEFINE tmp_ret_not_dap_vencido_bp                         CHAR(2)      ;
DEFINE tmp_ret_not_dap_vencido_importe                    DECIMAL(10,2)      ;
DEFINE tmp_ret_not_dap_vencido_moneda                     CHAR(4)      ;
DEFINE tmp_ret_not_dap_vencido_anulacion                  CHAR(10)    ;
DEFINE tmp_ret_not_dap_vencido_anio_dos                   CHAR(4)      ;
DEFINE tmp_ret_not_dap_vencido_doc_comp                   CHAR(10)     ;
DEFINE tmp_ret_not_dap_vencido_anio_tres                  CHAR(4)     ;
DEFINE tmp_ret_not_dap_vencido_estatus                    CHAR(7)     ;
DEFINE tmp_ret_not_dap_vencido_num_cta                    CHAR(16)     ;
DEFINE tmp_ret_not_dap_vencido_f_conta1                   DATE         ;
DEFINE tmp_ret_not_dap_vencido_f_conta2                   DATE         ;
DEFINE tmp_ret_not_dap_vencido_f_conta3                   DATE         ;
DEFINE tmp_ret_not_dap_vencido_f_conta4                   DATE         ;

-- tablas destino
-- ret_notif_dap_vencido
DEFINE ret_not_dap_vencido_id_solicitud               DECIMAL(9,0)     ;
DEFINE ret_not_dap_vencido_folio                      DECIMAL(9,0)    ;
DEFINE ret_not_dap_vencido_num_docto                  CHAR(10)      ;
DEFINE ret_not_dap_vencido_posicion                   CHAR(3)     ;
DEFINE ret_not_dap_vencido_anio                       CHAR(4)     ;
DEFINE ret_not_dap_vencido_division                   CHAR(4)     ;
DEFINE ret_not_dap_vencido_descripcion                CHAR(8)     ;
DEFINE ret_not_dap_vencido_f_liquidacion              DATE     ;
DEFINE ret_not_dap_vencido_usuario                    CHAR(8)      ;
DEFINE ret_not_dap_vencido_referencia                 CHAR(11)      ;
DEFINE ret_not_dap_vencido_txt_docto                  CHAR(25);
DEFINE ret_not_dap_vencido_acreedor                   CHAR(8);
DEFINE ret_not_dap_vencido_nombre                     CHAR(33)         ;
DEFINE ret_not_dap_vencido_vp                         CHAR(2)      ;
DEFINE ret_not_dap_vencido_bp                         CHAR(2)      ;
DEFINE ret_not_dap_vencido_importe                    DECIMAL(10,2)      ;
DEFINE ret_not_dap_vencido_moneda                     CHAR(4)      ;
DEFINE ret_not_dap_vencido_anulacion                  CHAR(10)    ;
DEFINE ret_not_dap_vencido_anio_dos                   CHAR(4)      ;
DEFINE ret_not_dap_vencido_doc_comp                   CHAR(10)     ;
DEFINE ret_not_dap_vencido_anio_tres                  CHAR(4)     ;
DEFINE ret_not_dap_vencido_estatus                    CHAR(7)     ;
DEFINE ret_not_dap_vencido_num_cta                    CHAR(16)     ;
DEFINE ret_not_dap_vencido_f_conta1                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta2                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta3                   DATE         ;
DEFINE ret_not_dap_vencido_f_conta4                   DATE         ;
DEFINE ret_not_dap_vencido_estado_solicitud           SMALLINT         ;

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
      
      RETURN v_si_resultado, isam_err, err_txt;
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

   
 
 
 
   
   -- integracion de detalle
   FOREACH
   SELECT
        num_docto,  
        posicion,   
        anio,   
        division,
        descripcion,
        substr(f_liquidacion,4,2)||"/"||substr(f_liquidacion,1,2)||"/"||substr(f_liquidacion,7,4),
        usuario,
        referencia,
        txt_docto,
        acreedor,
        nombre,
        vp,
        bp,
        REPLACE(importe,",",""),
        moneda,
        anulacion,
        anio_dos,
        doc_comp,
        anio_tres,
        estatus,
        num_cta,
        f_conta1,
        f_conta2,
        f_conta3,
        f_conta4
   INTO
        tmp_ret_not_dap_vencido_num_docto,
        tmp_ret_not_dap_vencido_posicion,
        tmp_ret_not_dap_vencido_anio,
        tmp_ret_not_dap_vencido_division,
        tmp_ret_not_dap_vencido_descripcion,
        tmp_ret_not_dap_vencido_f_liquidacion,
        tmp_ret_not_dap_vencido_usuario,
        tmp_ret_not_dap_vencido_referencia,
        tmp_ret_not_dap_vencido_txt_docto,
        tmp_ret_not_dap_vencido_acreedor,
        tmp_ret_not_dap_vencido_nombre,
        tmp_ret_not_dap_vencido_vp,
        tmp_ret_not_dap_vencido_bp,
        tmp_ret_not_dap_vencido_importe,
        tmp_ret_not_dap_vencido_moneda,
        tmp_ret_not_dap_vencido_anulacion,
        tmp_ret_not_dap_vencido_anio_dos,
        tmp_ret_not_dap_vencido_doc_comp,
        tmp_ret_not_dap_vencido_anio_tres,
        tmp_ret_not_dap_vencido_estatus,
        tmp_ret_not_dap_vencido_num_cta,
        tmp_ret_not_dap_vencido_f_conta1,
        tmp_ret_not_dap_vencido_f_conta2,
        tmp_ret_not_dap_vencido_f_conta3,
        tmp_ret_not_dap_vencido_f_conta4
   FROM
   safre_tmp:tmp_ret_not_dap_vencido
   WHERE num_docto is not null
   
      -- se busca el derechohabiente
      SELECT max(nvl(id_solicitud,0))
      INTO   v_id_solicitud
      FROM   ret_solo_infonavit
      WHERE  nss = tmp_ret_not_dap_vencido_referencia;

      -- se transfieren los datos a registro
      LET ret_not_dap_vencido_id_solicitud              = v_id_solicitud; -- decimal(9,0) ;
      LET ret_not_dap_vencido_folio                     = p_folio;
      LET ret_not_dap_vencido_num_docto                 = tmp_ret_not_dap_vencido_num_docto;
      LET ret_not_dap_vencido_posicion                  = tmp_ret_not_dap_vencido_posicion;
      LET ret_not_dap_vencido_anio                      = tmp_ret_not_dap_vencido_anio;
      LET ret_not_dap_vencido_division                  = tmp_ret_not_dap_vencido_division;
      LET ret_not_dap_vencido_descripcion               = tmp_ret_not_dap_vencido_descripcion;
      LET ret_not_dap_vencido_f_liquidacion             = tmp_ret_not_dap_vencido_f_liquidacion;
      LET ret_not_dap_vencido_usuario                   = tmp_ret_not_dap_vencido_usuario;
      LET ret_not_dap_vencido_referencia                = tmp_ret_not_dap_vencido_referencia;
      LET ret_not_dap_vencido_txt_docto                 = tmp_ret_not_dap_vencido_txt_docto;
      LET ret_not_dap_vencido_acreedor                  = tmp_ret_not_dap_vencido_acreedor;
      LET ret_not_dap_vencido_nombre                    = tmp_ret_not_dap_vencido_nombre;
      LET ret_not_dap_vencido_vp                        = tmp_ret_not_dap_vencido_vp;
      LET ret_not_dap_vencido_bp                        = tmp_ret_not_dap_vencido_bp;
      LET ret_not_dap_vencido_importe                   = tmp_ret_not_dap_vencido_importe;
      LET ret_not_dap_vencido_moneda                    = tmp_ret_not_dap_vencido_moneda;
      LET ret_not_dap_vencido_anulacion                 = tmp_ret_not_dap_vencido_anulacion;
      LET ret_not_dap_vencido_anio_dos                  = tmp_ret_not_dap_vencido_anio_dos;
      LET ret_not_dap_vencido_doc_comp                  = tmp_ret_not_dap_vencido_doc_comp;
      LET ret_not_dap_vencido_anio_tres                 = tmp_ret_not_dap_vencido_anio_tres;
      LET ret_not_dap_vencido_estatus                   = tmp_ret_not_dap_vencido_estatus;
      LET ret_not_dap_vencido_num_cta                   = tmp_ret_not_dap_vencido_num_cta;
      LET ret_not_dap_vencido_f_conta1                  = tmp_ret_not_dap_vencido_f_conta1;
      LET ret_not_dap_vencido_f_conta2                  = tmp_ret_not_dap_vencido_f_conta2;
      LET ret_not_dap_vencido_f_conta3                  = tmp_ret_not_dap_vencido_f_conta3;
      LET ret_not_dap_vencido_f_conta4                  = tmp_ret_not_dap_vencido_f_conta4;
      let ret_not_dap_vencido_estado_solicitud          = 20 ;  --- cargada


      
      -- se inserta en tabla destino
      INSERT INTO ret_not_dap_vencido (
                    id_solicitud,
                    folio,
                    num_docto,
                    posicion,
                    anio,
                    division,
                    descripcion,
                    f_liquidacion,
                    usuario,
                    referencia,
                    txt_docto,
                    acreedor,
                    nombre,
                    vp,
                    bp,
                    importe,
                    moneda,
                    anulacion,
                    anio_dos,
                    doc_comp,
                    anio_tres,
                    estatus,
                    num_cta,
                    f_conta1,
                    f_conta2,
                    f_conta3,
                    f_conta4,
                    estado_solicitud
      ) VALUES (
                    ret_not_dap_vencido_id_solicitud,
                    ret_not_dap_vencido_folio,
                    ret_not_dap_vencido_num_docto,
                    ret_not_dap_vencido_posicion,
                    ret_not_dap_vencido_anio,
                    ret_not_dap_vencido_division,
                    ret_not_dap_vencido_descripcion,
                    ret_not_dap_vencido_f_liquidacion,
                    ret_not_dap_vencido_usuario,
                    ret_not_dap_vencido_referencia,
                    ret_not_dap_vencido_txt_docto,
                    ret_not_dap_vencido_acreedor,
                    ret_not_dap_vencido_nombre,
                    ret_not_dap_vencido_vp,
                    ret_not_dap_vencido_bp,
                    ret_not_dap_vencido_importe,
                    ret_not_dap_vencido_moneda,
                    ret_not_dap_vencido_anulacion,
                    ret_not_dap_vencido_anio_dos,
                    ret_not_dap_vencido_doc_comp,
                    ret_not_dap_vencido_anio_tres,
                    ret_not_dap_vencido_estatus,
                    ret_not_dap_vencido_num_cta,
                    ret_not_dap_vencido_f_conta1,
                    ret_not_dap_vencido_f_conta2,
                    ret_not_dap_vencido_f_conta3,
                    ret_not_dap_vencido_f_conta4,
                    ret_not_dap_vencido_estado_solicitud
      );
   
   END FOREACH;
   
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_not_dap_vencido;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


