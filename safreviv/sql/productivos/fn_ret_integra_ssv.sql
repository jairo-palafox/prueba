






CREATE FUNCTION "safreviv".fn_ret_integra_ssv(p_usuario_cod CHAR(20), p_folio DECIMAL(9,0),
                                                p_nombre_archivo VARCHAR(40), p_pid DECIMAL(9,0),
                                                p_proceso_cod SMALLINT) 
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)


-- detalle de la tabla temporal
--tmp_ret_det_his_ley73
DEFINE tmp_registro_ssv_nss                    CHAR(20);  
DEFINE tmp_registro_ssv_rfc                    CHAR(20);  
DEFINE tmp_registro_ssv_compensa               CHAR(20);  
DEFINE tmp_registro_ssv_doc_comp               CHAR(20);  
DEFINE tmp_registro_ssv_ejercicio              CHAR(20);  
DEFINE tmp_registro_ssv_no_documento           CHAR(20);  
DEFINE tmp_registro_ssv_fch_contable           CHAR(20);  
DEFINE tmp_registro_ssv_fch_doc                CHAR(20);  
DEFINE tmp_registro_ssv_referencia             CHAR(20);  
DEFINE tmp_registro_ssv_clase_doc              CHAR(20);  
DEFINE tmp_registro_ssv_periodo                CHAR(20);  
DEFINE tmp_registro_ssv_dh                     CHAR(20);  
DEFINE tmp_registro_ssv_importe                CHAR(20);  
DEFINE tmp_registro_ssv_cta_mayor              CHAR(20);  
DEFINE tmp_registro_ssv_vp                     CHAR(20);  
DEFINE tmp_registro_ssv_titular_cuenta         CHAR(60);  
DEFINE tmp_registro_ssv_grupo                  CHAR(20);  
DEFINE tmp_registro_ssv_via_pago               CHAR(20);  
DEFINE tmp_registro_ssv_delegacion             CHAR(20);          
DEFINE tmp_registro_ssv_clabe                  CHAR(20);

-- tablas destino
-- ret_his_anexo1
DEFINE registro_ssv_id_solicitud                DECIMAL(9,0);
DEFINE registro_ssv_folio                       DECIMAL(9,0);
DEFINE registro_ssv_estado_solicitud            INTEGER; 
DEFINE registro_ssv_cod_rechazo                 INTEGER;
DEFINE registro_ssv_nss                         CHAR(11);  
DEFINE registro_ssv_rfc                         CHAR(13);  
DEFINE registro_ssv_compensa                    CHAR(8); 
DEFINE registro_ssv_doc_comp                    CHAR(10);  
DEFINE registro_ssv_ejercicio                   CHAR(4);  
DEFINE registro_ssv_no_documento                CHAR(10);  
DEFINE registro_ssv_fch_contable                DATE;  
DEFINE registro_ssv_fch_doc                     DATE;  
DEFINE registro_ssv_referencia                  CHAR(16);  
DEFINE registro_ssv_clase_doc                   CHAR(2); 
DEFINE registro_ssv_periodo                     CHAR(2);  
DEFINE registro_ssv_dh                          CHAR(2);
DEFINE registro_ssv_importe                     DECIMAL(12,2);  
DEFINE registro_ssv_cta_mayor                   CHAR(10);  
DEFINE registro_ssv_vp                          CHAR(1); 
DEFINE registro_ssv_titular_cuenta              CHAR(60);  
DEFINE registro_ssv_grupo                       SMALLINT;  
DEFINE registro_ssv_via_pago                    CHAR(8); 
DEFINE registro_ssv_delegacion                  CHAR(24);          
DEFINE registro_ssv_clabe                       CHAR(18);
DEFINE registro_ssv_tipo_sol                    CHAR(04);

-- variables de soporte al proceso
DEFINE v_id_solicitud                          DECIMAL(9,0);
DEFINE v_importe                               DECIMAL(12,2);
DEFINE v_anio_cont                             CHAR(4);
DEFINE v_mes_cont                              CHAR(2);
DEFINE v_dia_cont                              CHAR(2);
DEFINE v_anio_doc                              CHAR(4);
DEFINE v_mes_doc                               CHAR(2);
DEFINE v_dia_doc                               CHAR(2);
DEFINE v_fecha                                 CHAR(8);
DEFINE v_id_derechohabiente                    DECIMAL(9,0);
DEFINE v_marca_cargo_ssv                       SMALLINT;
DEFINE v_i_estado_marca                        SMALLINT;
DEFINE v_ape_paterno                           CHAR(40);
DEFINE v_ape_materno                           CHAR(40);
DEFINE v_nombre                                CHAR(40);
DEFINE v_posicion_signo                        SMALLINT;
DEFINE v_posicion_segundo_signo                SMALLINT;
DEFINE v_f_nacimiento                          DATE;
DEFINE v_sin_paterno                           SMALLINT;
DEFINE v_sin_materno                           SMALLINT;
-- =============================================================================

-- para rechazos
DEFINE v_b_rechazo_detalle                     SMALLINT;
 
DEFINE v_sumario_importe_total                 DECIMAL(22,6);
DEFINE v_sumario_total_registros               DECIMAL(9,0) ;
DEFINE v_total_registros                       DECIMAL(2,0) ;
DEFINE v_numero_registros                      DECIMAL(9,0) ;
DEFINE v_existe_en_hist                        SMALLINT;

-- arreglo de codigos de rechazo
DEFINE v_indice_codigos_rechazo                INTEGER; 

-- conteo de rechazos e inserciones
DEFINE v_reg_det_insertados                    INTEGER; -- total de registros de detalle insertados
DEFINE v_reg_det_rechazados                    INTEGER; -- total de registros de detalle rechazados
 
-- codigos de error en detalle
DEFINE v_error_det_nss_no_encontrado             INTEGER;
DEFINE v_error_det_tpo_registro_invalido         INTEGER;
DEFINE v_error_monto_invalido                    INTEGER; -- error 
DEFINE v_error_marca                             INTEGER;
DEFINE v_error_datos_insuficientes               INTEGER;
-- estatus del proceso
DEFINE v_estatus_proceso                         SMALLINT;
 
-- Control de Excepciones
DEFINE v_si_resultado                            SMALLINT;
DEFINE sql_err                                   INTEGER;
DEFINE isam_err                                  INTEGER;
DEFINE err_txt                                   VARCHAR(250);
DEFINE v_c_msj                                   VARCHAR(250);


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt 
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt, tmp_registro_ssv_nss;
   END EXCEPTION

   -- se establece el archivo para el debug
   --SET DEBUG FILE TO "/safreviv_int/BD/integra_ssv_siaff.txt";

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados
   LET v_indice_codigos_rechazo = 0;
   LET v_importe = 0;

   -- Inicializa las variables temporales
   LET tmp_registro_ssv_nss = NULL;  
   LET tmp_registro_ssv_rfc = NULL;
   LET tmp_registro_ssv_compensa = NULL;
   LET tmp_registro_ssv_doc_comp = NULL;
   LET tmp_registro_ssv_ejercicio = NULL;
   LET tmp_registro_ssv_no_documento = NULL;
   LET tmp_registro_ssv_fch_contable = NULL;
   LET tmp_registro_ssv_fch_doc = NULL;
   LET tmp_registro_ssv_referencia = NULL;
   LET tmp_registro_ssv_clase_doc = NULL;
   LET tmp_registro_ssv_periodo = NULL;
   LET tmp_registro_ssv_dh = NULL;
   LET tmp_registro_ssv_importe = NULL;
   LET tmp_registro_ssv_cta_mayor = NULL;
   LET tmp_registro_ssv_vp = NULL;
   LET tmp_registro_ssv_titular_cuenta = NULL;
   LET tmp_registro_ssv_grupo = NULL;
   LET tmp_registro_ssv_via_pago = NULL;
   LET tmp_registro_ssv_delegacion = NULL;
   LET tmp_registro_ssv_clabe = NULL;
   LET v_anio_cont = NULL;
   LET v_anio_doc = NULL;
   LET v_mes_cont = NULL;
   LET v_mes_doc = NULL;
   LET v_dia_cont = NULL;
   LET v_dia_doc = NULL;
   LET v_ape_paterno = NULL;
   LET v_ape_materno = NULL;
   LET v_nombre = NULL;
   LET v_f_nacimiento = NULL;


   
   -- se asume que el proceso termina bien
   LET v_estatus_proceso    = 0;
   LET v_si_resultado       = 0;
   LET isam_err             = 0;
   LET v_c_msj              = 'El proceso finalizó exitosamente.';
   LET tmp_registro_ssv_nss = NULL;

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

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_monto_invalido                    = 11;
   LET v_error_marca                             = 12;
   LET v_error_datos_insuficientes               = 13;
   LET registro_ssv_estado_solicitud             = 0;     
   LET registro_ssv_cod_rechazo                  = 0;
   LET v_id_derechohabiente                      = 0;
   LET registro_ssv_id_solicitud                 = 0;
   LET registro_ssv_folio                        = p_folio;
   LET v_marca_cargo_ssv                         = 819;
   
   -- ===================================================
   -- validaciones encabezado contra detalle
   --- se cuentan los registros de la tabla temporal de detalle
   -- trace on;  
   -- integracion de detalle
   FOREACH
   SELECT nss, rfc, compensa, doc_comp, ejercicio, no_documento, fch_contable,
          fch_doc, referencia, clase_doc, periodo, dh, importe, cta_mayor, vp,
          titular_cuenta, grupo, via_pago, delegacion, clabe
   INTO   tmp_registro_ssv_nss, tmp_registro_ssv_rfc, tmp_registro_ssv_compensa,
          tmp_registro_ssv_doc_comp, tmp_registro_ssv_ejercicio,
          tmp_registro_ssv_no_documento, tmp_registro_ssv_fch_contable,
          tmp_registro_ssv_fch_doc, tmp_registro_ssv_referencia,
          tmp_registro_ssv_clase_doc, tmp_registro_ssv_periodo, 
          tmp_registro_ssv_dh, tmp_registro_ssv_importe,
          tmp_registro_ssv_cta_mayor, tmp_registro_ssv_vp,
          tmp_registro_ssv_titular_cuenta, tmp_registro_ssv_grupo,
          tmp_registro_ssv_via_pago, tmp_registro_ssv_delegacion,
          tmp_registro_ssv_clabe
   FROM
   safre_tmp:tmp_ret_anexo1_cargos_ssv_siaff
   
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle = 0;


      -- se busca el derechohabiente
      -- se busca que exista en el historico del Anexo 1
      LET v_existe_en_hist = 0;
      SELECT COUNT(*)
      INTO   v_existe_en_hist
      FROM   afi_derechohabiente -- ret_his_anexo1
      WHERE  nss = tmp_registro_ssv_nss;

      LET registro_ssv_estado_solicitud = 10;
      LET registro_ssv_cod_rechazo = 0;
      LET registro_ssv_tipo_sol    = "0.0";
      LET v_sin_paterno = 0;
      LET v_sin_materno = 0;
      LET v_posicion_signo = 0;
      LET v_ape_paterno = NULL;
      LET v_ape_materno = NULL;
      LET v_nombre = NULL;
      -- si no se encontro el id_derechohabiente
      IF ( v_existe_en_hist = 0 OR v_existe_en_hist IS NULL ) THEN
         IF (INSTR(tmp_registro_ssv_titular_cuenta,"$")) > 0 THEN 
            LET v_posicion_signo = INSTR(tmp_registro_ssv_titular_cuenta,"$");
            IF v_posicion_signo > 0 THEN 
               IF v_posicion_signo = 1 THEN 
                  LET v_ape_paterno = NULL;
                  LET v_sin_paterno = 1;
               END IF 
               IF v_sin_paterno = 0 THEN 
                  LET  v_ape_paterno = SUBSTR(tmp_registro_ssv_titular_cuenta,1,(v_posicion_signo - 1));
               END IF 
               LET v_posicion_segundo_signo = INSTR(tmp_registro_ssv_titular_cuenta,"$", 1, 2);
               IF  (v_posicion_segundo_signo - v_posicion_signo) =  1 THEN 
                  LET v_ape_materno = NULL;
                  LET v_sin_materno = 1;
               END IF  
               IF v_sin_materno = 0 THEN 
                  LET v_ape_materno = SUBSTR(tmp_registro_ssv_titular_cuenta,(v_posicion_signo + 1), (v_posicion_segundo_signo - (v_posicion_signo + 1)));
                  -- LET v_ape_materno = tmp_registro_ssv_titular_cuenta[v_posicion_signo+1,INSTR(tmp_registro_ssv_titular_a cuenta,"$",v_posicion_signo + 1) - 1];
               END IF 
               LET v_nombre = SUBSTR(tmp_registro_ssv_titular_cuenta,(v_posicion_segundo_signo + 1), LENGTH(tmp_registro_ssv_titular_cuenta));
               --LET v_nombre = tmp_registro_ssv_titular_cuenta[INSTR(tmp_registro_ssv_titular_a cuenta,"$",v_posicion_signo + 1) + 1, LENGTH(tmp_registro_ssv_titular_a)];
               --- DAMOS DE ALTA EL REGISTRO EN AFI-DERECHOHABIENTE
               EXECUTE FUNCTION fn_fnacimiento(tmp_registro_ssv_nss, NULL, NULL) INTO v_f_nacimiento;
               SELECT seq_derechohabiente.NEXTVAL
               INTO   v_id_derechohabiente
               FROM   systables
               WHERE  tabid = 1;
               IF v_sin_paterno = 0 THEN 
                  INSERT INTO afi_derechohabiente (
                                     id_derechohabiente,nss,ind_nrp,f_nacimiento,nombre_imss
                                    ,nombre_af,ap_paterno_af,ap_materno_af,sexo,tipo_trabajador
                                    ,origen_afiliacion,id_credito,folio_lote,f_apertura_inf
                                    ,f_apertura,ind_estado_cuenta,rfc) 
                         VALUES (v_id_derechohabiente,tmp_registro_ssv_nss, 0, v_f_nacimiento,
                                 tmp_registro_ssv_titular_cuenta,v_nombre,v_ape_paterno,v_ape_materno,
                                 0,'I','B',0,p_folio, today, today, 0,tmp_registro_ssv_rfc);
                  LET registro_ssv_tipo_sol = "1.1";
               ELSE 
                  LET registro_ssv_estado_solicitud = 100; -- rechazada
                  LET registro_ssv_cod_rechazo = v_error_datos_insuficientes;
                  LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
                  LET registro_ssv_tipo_sol = "1.1";
                  LET v_id_derechohabiente = 0;
               END IF 
            END IF 
            
         ELSE     
            LET registro_ssv_estado_solicitud = 100; -- rechazada
            LET registro_ssv_cod_rechazo = v_error_datos_insuficientes;
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
            LET registro_ssv_tipo_sol = "1.1";
            LET v_id_derechohabiente = 0;
         END IF 
      ELSE 
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente
         FROM   afi_derechohabiente -- ret_his_anexo1
         WHERE  nss = tmp_registro_ssv_nss;

      END IF
      
      -- el monto debe ser mayor a cero
      
      LET v_importe = tmp_registro_ssv_importe;
--      LET v_importe = 1000;

      IF v_importe  <= 0  THEN
         -- se marca la bandera de rechazo de detalle
         LET registro_ssv_estado_solicitud = 100; -- rechazada
         LET registro_ssv_cod_rechazo = v_error_monto_invalido;
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      END IF
    
      -- se obtiene el numero de solicitud
      SELECT seq_ret_cargos_ssv_siaff.NEXTVAL
      INTO v_id_solicitud
      FROM systables
      WHERE tabid = 1;

      -- se transfieren los datos a registro

      LET registro_ssv_id_solicitud                = v_id_solicitud;
      LET registro_ssv_folio                       = p_folio;
      LET registro_ssv_nss                         = tmp_registro_ssv_nss;
      LET registro_ssv_rfc                         = tmp_registro_ssv_rfc;
      LET registro_ssv_compensa                    = tmp_registro_ssv_compensa;
      LET registro_ssv_doc_comp                    = tmp_registro_ssv_doc_comp;
      LET registro_ssv_ejercicio                   = tmp_registro_ssv_ejercicio;
      LET registro_ssv_no_documento                = tmp_registro_ssv_no_documento;
      IF tmp_registro_ssv_fch_contable IS NOT NULL THEN 
         LET registro_ssv_fch_contable             = mdy(tmp_registro_ssv_fch_contable[3,4],tmp_registro_ssv_fch_contable[1,2],tmp_registro_ssv_fch_contable[5,8]);
      ELSE 
         LET registro_ssv_fch_contable = NULL;
      END IF
      IF tmp_registro_ssv_fch_doc IS NOT NULL THEN 
         LET registro_ssv_fch_doc                  = mdy(tmp_registro_ssv_fch_doc[3,4],tmp_registro_ssv_fch_doc[1,2],tmp_registro_ssv_fch_doc[5,8]);
      ELSE 
         LET registro_ssv_fch_doc = NULL;
      END IF  
      LET registro_ssv_referencia                  = tmp_registro_ssv_referencia;
      LET registro_ssv_clase_doc                   = tmp_registro_ssv_clase_doc;
      LET registro_ssv_periodo                     = tmp_registro_ssv_periodo;
      LET registro_ssv_dh                          = tmp_registro_ssv_dh;
      LET registro_ssv_importe                     = v_importe;
      LET registro_ssv_cta_mayor                   = tmp_registro_ssv_cta_mayor;
      LET registro_ssv_vp                          = tmp_registro_ssv_vp;
      LET registro_ssv_titular_cuenta              = tmp_registro_ssv_titular_cuenta;
      LET registro_ssv_grupo                       = tmp_registro_ssv_grupo;
      LET registro_ssv_via_pago                    = tmp_registro_ssv_via_pago;
      LET registro_ssv_delegacion                  = tmp_registro_ssv_delegacion;
      LET registro_ssv_clabe                       = tmp_registro_ssv_clabe;
      LET v_i_estado_marca = 1;
      IF registro_ssv_estado_solicitud = 10 THEN
         EXECUTE FUNCTION fn_marca_cuenta(
                 v_id_derechohabiente
                ,v_marca_cargo_ssv -- marca de cargos de la SSV
                ,v_id_solicitud    -- seq_ret_cargos_ssv_siaff.CURRVAL
                ,p_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;

         IF v_i_estado_marca <> 0 THEN 
            LET registro_ssv_estado_solicitud = 100; -- rechazada
            LET registro_ssv_cod_rechazo = v_error_marca;
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
            LET registro_ssv_tipo_sol = "1.1";
         END IF 
      END IF       
      -- se inserta en tabla destino
      INSERT INTO ret_cargos_ssv_siaff (
          id_solicitud, folio, estado_solicitud, cod_rechazo,
          nss, rfc, compensa, doc_comp, ejercicio, no_documento, fch_contable, 
          fch_doc, referencia, clase_doc, periodo, dh, importe, cta_mayor, vp,
          titular_cuenta, grupo, via_pago, delegacion, clabe, tipo_sol
      ) VALUES (
          registro_ssv_id_solicitud, registro_ssv_folio,
          registro_ssv_estado_solicitud, registro_ssv_cod_rechazo, 
          registro_ssv_nss, registro_ssv_rfc, registro_ssv_compensa,
          registro_ssv_doc_comp, registro_ssv_ejercicio,
          registro_ssv_no_documento, registro_ssv_fch_contable,
          registro_ssv_fch_doc, registro_ssv_referencia, 
          registro_ssv_clase_doc, registro_ssv_periodo, registro_ssv_dh,
          registro_ssv_importe, registro_ssv_cta_mayor, registro_ssv_vp,
          registro_ssv_titular_cuenta, registro_ssv_grupo, registro_ssv_via_pago,
          registro_ssv_delegacion, registro_ssv_clabe, registro_ssv_tipo_sol);
      LET v_reg_det_insertados = v_reg_det_insertados + 1;
   END FOREACH;
   --trace off;
   -- actualizacion de estadisticas de las tablas destino
   UPDATE STATISTICS FOR TABLE ret_cargos_ssv_siaff;
  
   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_registro_ssv_nss;
END FUNCTION;


