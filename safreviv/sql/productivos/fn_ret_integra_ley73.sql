






CREATE FUNCTION "safreviv".fn_ret_integra_ley73(  p_usuario_cod    CHAR(20)
                                      , p_folio          DECIMAL(9,0)
                                      , p_nombre_archivo VARCHAR(40)
                                      , p_pid            DECIMAL(9,0)
                                      , p_proceso_cod    SMALLINT
                                      )
   RETURNING INTEGER, INTEGER, VARCHAR(250), CHAR(11)

   -- campos de la tabla de detalle de retiros de Ley 73 (sin filler)
   DEFINE tmp_ret_det_nss                          CHAR(11);
   DEFINE tmp_ret_det_est_ssv                      CHAR(04);
   DEFINE tmp_ret_det_desc_st_ssv                  CHAR(40);
   DEFINE tmp_ret_det_est_jfca                     CHAR(04);
   DEFINE tmp_ret_det_desc_jfca                    CHAR(40);
   DEFINE tmp_ret_det_tpo_proceso                  CHAR(04);
   DEFINE tmp_ret_det_desc_tpo_proceso             CHAR(40);
   DEFINE tmp_ret_det_nombre_afore                 CHAR(40);
   DEFINE tmp_ret_det_uno_apellido_afore           CHAR(40);
   DEFINE tmp_ret_det_dos_apellido_afore           CHAR(40);
   DEFINE tmp_ret_det_beneficiario                 CHAR(20);
   DEFINE tmp_ret_det_nombre_benef                 CHAR(40);
   DEFINE tmp_ret_det_ape_pat_benef                CHAR(40);
   DEFINE tmp_ret_det_ape_mat_benef                CHAR(40);
   DEFINE tmp_ret_det_curp                         CHAR(18);
   DEFINE tmp_ret_det_rfc                          CHAR(13);
   DEFINE tmp_ret_det_entidad_fed                  CHAR(02);
   DEFINE tmp_ret_det_f_inicio_tram                CHAR(10);
   DEFINE tmp_ret_det_f_autorizacion_pago          CHAR(10);
   DEFINE tmp_ret_det_num_doc_cta_pago_fico        CHAR(15);
   DEFINE tmp_ret_det_eje_fis_cpp_fico             CHAR(04);
   DEFINE tmp_ret_det_no_doc_pago_fico             CHAR(15);
   DEFINE tmp_ret_det_f_pago_fico                  CHAR(10);
   DEFINE tmp_ret_det_imp_pago_fico                CHAR(19);
   DEFINE tmp_ret_det_ref_pago_fico                CHAR(15);
   DEFINE tmp_ret_det_num_caso_adai                CHAR(10);
   DEFINE tmp_ret_det_num_laudo                    CHAR(09);
   DEFINE tmp_ret_det_num_junta_esp                CHAR(02);
   DEFINE tmp_ret_det_imp_pago_ant                 CHAR(19);
   DEFINE tmp_ret_det_f_pago_ant                   CHAR(10);
   DEFINE tmp_ret_det_cve_banco                    CHAR(05);
   DEFINE tmp_ret_det_cuenta_bancaria              CHAR(18);
   DEFINE tmp_ret_det_imp_transf_ssv               CHAR(19);
   DEFINE tmp_ret_det_f_transf_ssv                 CHAR(10);
   DEFINE tmp_ret_det_ssv_dif_ini_legacy           CHAR(27);
   DEFINE tmp_ret_det_f_marca                      CHAR(10);
   DEFINE tmp_ret_det_ssv_erro_fico                CHAR(27);
   DEFINE tmp_ret_det_ssv_cve_afore                CHAR(03);
   DEFINE tmp_ret_det_ssv_imp97_pesos              CHAR(24);
   DEFINE tmp_ret_det_ssv_imp97_ivs                CHAR(24);
   DEFINE tmp_ret_det_ssv_imp92_pesos              CHAR(24);
   DEFINE tmp_ret_det_ssv_imp92_ivs                CHAR(24);
   DEFINE tmp_ret_det_f_valuacion                  CHAR(24);
   DEFINE tmp_ret_det_tpo_cambio                   CHAR(10);                                                

-- detalle de la tabla historica/integrada de retiros de Ley 73
-- ret_ley73
   DEFINE ret_ley73_id_solicitud                   DECIMAL(9,0)          ;
   DEFINE ret_ley73_id_derechohabiente             DECIMAL(9,0)          ;
   DEFINE ret_ley73_tpo_proceso                    SMALLINT              ;
   DEFINE ret_ley73_f_solicitud                    DATE                  ;
   DEFINE ret_ley73_estado_solicitud               SMALLINT              ;
   DEFINE ret_ley73_folio                          DECIMAL(9,0)          ;
   DEFINE ret_ley73_aivs_viv92                     DECIMAL(18,6)         ;
   DEFINE ret_ley73_aivs_viv97                     DECIMAL(18,6)         ;
   DEFINE ret_ley73_importe_viv92                  DECIMAL(12,2)         ;
   DEFINE ret_ley73_importe_viv97                  DECIMAL(12,2)         ;
   DEFINE ret_ley73_cod_retorno                    SMALLINT              ;
   DEFINE ret_ley73_nrp                            SMALLINT              ;
   DEFINE ret_ley73_f_valuacion                    DATE                  ;
   DEFINE ret_ley73_f_captura                      DATE                  ;
   DEFINE ret_ley73_h_captura                      DATETIME DAY TO SECOND;
   DEFINE ret_ley73_usuario                        CHAR(20)              ;
   DEFINE ret_ley73_marca_juridico                 SMALLINT              ;
   DEFINE ret_ley73_estado_jfca                    SMALLINT              ;
   DEFINE ret_ley73_cod_rechazo                    SMALLINT              ;
   DEFINE ret_ley73_estado_interno                 SMALLINT              ;
   DEFINE ret_ley73_tpo_cambio                     DECIMAL(6,5)          ;
   DEFINE ret_ley73_importe_tesofe                 DECIMAL(18,2)         ;
   
   -- detalle de la tabla historica/integrada ret_autoriza
   DEFINE ret_autoriza_id_solicitud                DECIMAL(9,0)      ;
   DEFINE ret_autoriza_clabe                       CHAR(18)          ;
   DEFINE ret_autoriza_banco                       DECIMAL(5,0)      ;
   DEFINE ret_autoriza_entidad_federativa          SMALLINT          ;
   DEFINE ret_autoriza_f_autorizacion              DATE              ;
   DEFINE ret_autoriza_f_marca                     DATE              ;
   DEFINE ret_autoriza_cve_afore                   SMALLINT          ;
   DEFINE ret_autoriza_caso_adai                   CHAR(10)          ;
   DEFINE ret_autoriza_num_laudo                   CHAR(9)           ;
   DEFINE ret_autoriza_num_junta                   CHAR(2)           ;

   -- detalle de la tabla historica/integrada ret_beneficiario
   DEFINE ret_beneficiario_id_solicitud            DECIMAL(9,0)     ;
   DEFINE ret_beneficiario_tpo_beneficiario        SMALLINT         ;
   DEFINE ret_beneficiario_nombre_beneficiario     CHAR(40)         ;
   DEFINE ret_beneficiario_paterno_beneficiario    CHAR(40)         ;
   DEFINE ret_beneficiario_materno_beneficiario    CHAR(40)         ;
   DEFINE ret_beneficiario_f_registro              DATE             ;

   -- detalle de la tabla historica/integrada ret_pago_trm
   DEFINE ret_pago_trm_id_solicitud            DECIMAL(9)      ;
   DEFINE ret_pago_trm_id_derechohabiente      DECIMAL(9)      ;
   DEFINE ret_pago_trm_estatus_trm             SMALLINT        ;
   DEFINE ret_pago_trm_estatus_jfca            SMALLINT        ;
   DEFINE ret_pago_trm_tpo_proceso             SMALLINT        ;
   DEFINE ret_pago_trm_f_marca                 DATE            ;
   DEFINE ret_pago_trm_f_pago                  DATE            ;
   DEFINE ret_pago_trm_importe_pagado          DECIMAL(12,2)   ;
   DEFINE ret_pago_trm_doc_pago_fico           CHAR(10)        ;
   DEFINE ret_pago_trm_doc_por_pagar_fico      CHAR(15)        ;
   DEFINE ret_pago_trm_f_pago_fico             DATE            ;
   DEFINE ret_pago_trm_ef_cta_por_pagar_fico   SMALLINT        ;
   DEFINE ret_pago_trm_referencia_pago_fico    CHAR(7)         ;
   DEFINE ret_pago_trm_tpo_beneficiario        SMALLINT        ;
   DEFINE ret_pago_trm_nombre_beneficiario     CHAR(40)        ;
   DEFINE ret_pago_trm_paterno_beneficiario    CHAR(40)        ;
   DEFINE ret_pago_trm_materno_beneficiario    CHAR(40)        ;
   DEFINE ret_pago_trm_clabe                   CHAR(18)        ;
   DEFINE ret_pago_trm_cve_banco               SMALLINT        ;
   DEFINE ret_pago_trm_entidad_federativa      SMALLINT        ;
   DEFINE ret_pago_trm_f_autorizacion          date            ;
   DEFINE ret_pago_trm_cve_afore               SMALLINT        ;
   DEFINE ret_pago_trm_caso_adai               INTEGER         ;
   DEFINE ret_pago_trm_num_laudo               CHAR(9)         ;
   DEFINE ret_pago_trm_num_junta               CHAR(2)         ;
   DEFINE ret_pago_trm_resultado_operacion     SMALLINT        ;
   DEFINE ret_pago_trm_cod_rechazo_1           SMALLINT        ;
   DEFINE ret_pago_trm_cod_rechazo_2           SMALLINT        ;

   -- detalle de la tabla historica/integrada ret_pago_juridico
   DEFINE ret_pago_juridico_id_solicitud        DECIMAL(9,0)   ;
   DEFINE ret_pago_juridico_id_derechohabiente  DECIMAL(9,0)   ;
   DEFINE ret_pago_juridico_tipo_resolucion     SMALLINT       ;
   DEFINE ret_pago_juridico_f_marca_tj          DATE           ;
   DEFINE ret_pago_juridico_f_pago              DATE           ;
   DEFINE ret_pago_juridico_importe_pagado      DECIMAL(10,2)  ;
   DEFINE ret_pago_juridico_num_registro        SMALLINT       ;
   DEFINE ret_pago_juridico_cod_rechazo         SMALLINT       ;

   -- variables de soporte al proceso
   DEFINE v_id_derechohabiente                     DECIMAL(9,0);
   DEFINE v_rec_tot_saldo_viv92                    DECIMAL(19,2);
   DEFINE v_rec_tot_saldo_viv97                    DECIMAL(19,2);
   DEFINE v_b_rechazo_encabezado                   DECIMAL(9,0);
   --variable de solicitd
   DEFINE v_id_solicitud                           DECIMAL(9,0);
   DEFINE v_precio_fondo                           DECIMAL(14,6);
   --define v_ret_ley73_id_derechohabiente           DECIMAL(9,0);
-- =============================================================================

   DEFINE v_tmp_ret_det_pesos                      DECIMAL(19,3);
   DEFINE v_tmp_ret_det_aivs                       DECIMAL(19,6);
   DEFINE v_tmp_ret_det_aivs_aux                   DECIMAL(19,6);
   DEFINE v_valuacion_paso                         DECIMAL(19,2);
   DEFINE v_pesos_archivo_paso                     DECIMAL(19,2);
   DEFINE v_pesos_viv97_paso                       DECIMAL(19,2);
   DEFINE v_pesos_viv92_paso                       DECIMAL(19,2);

   -- para rechazos
   DEFINE v_b_rechazo_detalle                      SMALLINT;

-- RECUPERADOS
   DEFINE v_numero_registros                         DECIMAL(9,0);
   --rechazos
   DEFINE v_motivo_rechazo_1                         SMALLINT;
   DEFINE v_motivo_rechazo_2                         SMALLINT;
   DEFINE v_motivo_rechazo_3                         SMALLINT;
   -- arreglo de codigos de rechazo
   DEFINE v_codigos_rechazo                          CHAR(30); -- los codigos van de tres en tres
   DEFINE v_indice_codigos_rechazo                   SMALLINT;

   -- conteo de rechazos e inserciones
   DEFINE v_reg_det_insertados                       DECIMAL(9,0); -- total de registros de detalle insertados
   DEFINE v_reg_det_rechazados                       DECIMAL(9,0); -- total de registros de detalle rechazados


   -- codigos de error en detalle
   DEFINE v_error_det_importe_tesofe_invalido         SMALLINT;
   DEFINE v_error_det_nss_no_encontrado               SMALLINT;
   DEFINE v_error_det_tpo_registro_invalido           SMALLINT;
   DEFINE v_error_det_id_servicio_invalido            SMALLINT;
   DEFINE v_error_det_id_operacion_invalido           SMALLINT;
   DEFINE v_error_det_estatus_ssv_no_existe           SMALLINT;
   DEFINE v_error_det_tot_impviv72_no_capturado       SMALLINT;
   DEFINE v_error_det_tpo_proceso_no_existe           SMALLINT;
   DEFINE v_error_det_ssv_imp97_pesos_igual_cero      SMALLINT;
   DEFINE v_error_det_ssv_imp97_aivs_igual_cero       SMALLINT;
   DEFINE v_error_det_ssv_imp92_pesos_igual_cero      SMALLINT;
   DEFINE v_error_det_ssv_imp92_aivs_igual_cero       SMALLINT;
   DEFINE v_error_det_f_valuacion_no_existe           SMALLINT;
   DEFINE v_error_det_sin_aivs                        SMALLINT; -- no se recibiendo AIVs de viv92 o viv97
   DEFINE v_error_det_tpo_cambio                      SMALLINT;
   DEFINE v_error_det_pesos97_mal_valuados            SMALLINT;
   DEFINE v_error_det_pesos92_mal_valuados            SMALLINT;

   -- codigos de error en sumario
   DEFINE v_error_sum_totales_no_coinciden          SMALLINT;

   -- estatus del proceso
   DEFINE v_estatus_proceso                         SMALLINT;

   -- para marcar las cuentas
   DEFINE v_i_estado_marca                          INTEGER;
   DEFINE v_marca_ley73                             INTEGER; -- 802 de acuerdo a catalogo
   
   DEFINE v_diferencia_imp_pago_fico                DECIMAL(18);

   -- Control de Excepciones
   DEFINE v_si_resultado                            SMALLINT;
   DEFINE sql_err                                   INTEGER;
   DEFINE isam_err                                  INTEGER;
   DEFINE err_txt                                   VARCHAR(250);
   DEFINE v_c_msj                                   VARCHAR(250);
   DEFINE r_bnd_edo_act_archivo                     SMALLINT;


   -- se configura el retorno de los valores
   ON EXCEPTION SET sql_err, isam_err, err_txt
      LET v_si_resultado = sql_err;

      RETURN v_si_resultado, isam_err, err_txt, tmp_ret_det_nss;
   END EXCEPTION

   LET tmp_ret_det_nss             = "";

   --SET DEBUG FILE TO "/safreviv_int/BD/debug_ret_ley73.txt";
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/debug_ret_ley73.txt";

    -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE glo_ctr_archivo
   SET    folio = p_folio ,
          estado = 2     -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    opera_cod      = 1 -- archivo cargado
   AND    estado         = 1 -- etapa de carga
   AND    nombre_archivo = p_nombre_archivo;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_operacion
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;

   -- Agregar folio a operacion de integracion
   UPDATE bat_ctr_proceso
   SET    folio       = p_folio
   WHERE  proceso_cod = p_proceso_cod
   AND    pid         = p_pid;
   
   -- Actualiza el estado del archivo procesado
   CALL fn_act_edo_archivo(p_nombre_archivo,p_folio,2,p_usuario_cod) 
         RETURNING r_bnd_edo_act_archivo;

   ---- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados        = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados        = 0; -- total de registros de detalle rechazados
   LET v_rec_tot_saldo_viv92       = 0;
   LET v_rec_tot_saldo_viv97       = 0;
   LET v_precio_fondo              = 0;
    -- NSS que presento error

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;
   LET v_si_resultado    = 0;
   LET isam_err          = 0;
   LET v_c_msj           = 'El proceso finalizó exitosamente.';

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado                =   7;
   LET v_error_det_tpo_registro_invalido            = 321;
   LET v_error_det_id_servicio_invalido             = 322;
   LET v_error_det_id_operacion_invalido            = 323;
   LET v_error_det_estatus_ssv_no_existe            = 324;
   LET v_error_det_tot_impviv72_no_capturado        = 325;
   LET v_error_det_tpo_proceso_no_existe            = 326;
   LET v_error_sum_totales_no_coinciden             = 327;
   LET v_error_det_ssv_imp97_pesos_igual_cero       = 328;
   LET v_error_det_ssv_imp97_aivs_igual_cero        = 329;
   LET v_error_det_ssv_imp92_pesos_igual_cero       = 330;
   LET v_error_det_ssv_imp92_aivs_igual_cero        = 331;
   LET v_error_det_f_valuacion_no_existe            = 332;
   LET v_error_det_sin_aivs                         = 333;
   LET v_error_det_tpo_cambio                       = 334;
   LET v_error_det_pesos97_mal_valuados             = 335;
   LET v_error_det_pesos92_mal_valuados             = 336;
   LET v_error_det_importe_tesofe_invalido          = 805;
   

   -- se inician las variables para marca
   LET v_marca_ley73        = 803; -- marca para ley 73
   LET v_i_estado_marca     = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;

   CREATE TEMP TABLE tmp_codigos_rechazo (
                                         id_derechohabiente  decimal(9,0)
                                         ,id_codigo          SMALLINT
                                         ,codigo_rechazo     SMALLINT
                                         );

   -- se inicia la variable que almacenaria el id_solicitud
   LET v_id_solicitud = 0;

   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
       nss
      ,est_ssv
      ,desc_st_ssv
      ,est_jfca
      ,desc_jfca
      ,tpo_proceso
      ,desc_tpo_proceso
      ,nombre_afore
      ,uno_apellido_afore
      ,dos_apellido_afore
      ,beneficiario
      ,nombre_benef
      ,ape_pat_benef
      ,ape_mat_benef
      ,curp
      ,rfc
      ,entidad_fed
      ,f_inicio_tram
      ,f_autorizacion_pago
      ,num_doc_cta_pago_fico
      ,eje_fis_cpp_fico
      ,no_doc_pago_fico
      ,f_pago_fico
      ,imp_pago_fico
      ,ref_pago_fico
      ,num_caso_adai
      ,num_laudo
      ,num_junta_esp
      ,imp_pago_ant
      ,f_pago_ant
      ,cve_banco
      ,cuenta_bancaria
      ,imp_transf_ssv
      ,f_transf_ssv
      ,ssv_dif_ini_legacy
      ,f_marca
      ,ssv_erro_fico
      ,ssv_cve_afore
      ,ssv_imp97_pesos
      ,ssv_imp97_ivs
      ,ssv_imp92_pesos
      ,ssv_imp92_ivs
      ,f_valuacion
      ,tpo_cambio
    INTO
       tmp_ret_det_nss
      ,tmp_ret_det_est_ssv
      ,tmp_ret_det_desc_st_ssv
      ,tmp_ret_det_est_jfca
      ,tmp_ret_det_desc_jfca
      ,tmp_ret_det_tpo_proceso
      ,tmp_ret_det_desc_tpo_proceso
      ,tmp_ret_det_nombre_afore
      ,tmp_ret_det_uno_apellido_afore
      ,tmp_ret_det_dos_apellido_afore
      ,tmp_ret_det_beneficiario
      ,tmp_ret_det_nombre_benef
      ,tmp_ret_det_ape_pat_benef
      ,tmp_ret_det_ape_mat_benef
      ,tmp_ret_det_curp
      ,tmp_ret_det_rfc
      ,tmp_ret_det_entidad_fed
      ,tmp_ret_det_f_inicio_tram
      ,tmp_ret_det_f_autorizacion_pago
      ,tmp_ret_det_num_doc_cta_pago_fico
      ,tmp_ret_det_eje_fis_cpp_fico
      ,tmp_ret_det_no_doc_pago_fico
      ,tmp_ret_det_f_pago_fico
      ,tmp_ret_det_imp_pago_fico
      ,tmp_ret_det_ref_pago_fico
      ,tmp_ret_det_num_caso_adai
      ,tmp_ret_det_num_laudo
      ,tmp_ret_det_num_junta_esp
      ,tmp_ret_det_imp_pago_ant
      ,tmp_ret_det_f_pago_ant
      ,tmp_ret_det_cve_banco
      ,tmp_ret_det_cuenta_bancaria
      ,tmp_ret_det_imp_transf_ssv
      ,tmp_ret_det_f_transf_ssv
      ,tmp_ret_det_ssv_dif_ini_legacy
      ,tmp_ret_det_f_marca
      ,tmp_ret_det_ssv_erro_fico
      ,tmp_ret_det_ssv_cve_afore
      ,tmp_ret_det_ssv_imp97_pesos
      ,tmp_ret_det_ssv_imp97_ivs
      ,tmp_ret_det_ssv_imp92_pesos
      ,tmp_ret_det_ssv_imp92_ivs
      ,tmp_ret_det_f_valuacion
      ,tmp_ret_det_tpo_cambio            
   FROM
      safre_tmp:tmp_ret_det_ley73

      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle    = 0;
      LET v_id_derechohabiente   = 0;
      LET v_tmp_ret_det_aivs     = 0;
      LET v_tmp_ret_det_aivs_aux = 0;
      LET v_valuacion_paso       = 0;
      LET v_pesos_archivo_paso   = 0;
      

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_ret_det_nss;

      -- para el id solicitud se obtiene de la secuencia
      LET v_id_solicitud = 0;

      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;

      LET v_indice_codigos_rechazo = 1;

       --trace("Validando registro de detalle");
      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         -- se marca la bandera de rechazo de detalle
         LET v_b_rechazo_detalle    = 1;

         INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_nss_no_encontrado);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         --trace("v_error_det_nss_no_encontrado "||v_error_det_nss_no_encontrado);
      END IF

      -- la estatus ssv debe existir
      IF ( tmp_ret_det_est_ssv IS NULL OR LENGTH(TRIM(tmp_ret_det_est_ssv)) = 0) THEN
         -- se rechaza
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_estatus_ssv_no_existe);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         --trace("v_error_det_estatus_ssv_no_existe "||v_error_det_estatus_ssv_no_existe);
      END IF

	  -----------------------------------------------------------------------------------------------------
	  -- Requerimiento 878
	  -- Validar grupos: 0101, 0201, 0114, 0102, 0103, 0124, 0401, 0402, 0403, 0404, 0411, 0412, 0413, 0414
	  -----------------------------------------------------------------------------------------------------
      IF ( tmp_ret_det_tpo_proceso IS NULL OR LENGTH(TRIM(tmp_ret_det_tpo_proceso)) = 0 OR
           tmp_ret_det_tpo_proceso NOT IN ('0101', '0201', '0114', '0102', '0103', '0124','0401',
                                          '0402', '0403', '0404', '0411', '0412', '0413', '0414')) THEN
         -- se rechaza
         LET v_b_rechazo_detalle    = 1;
         INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_proceso_no_existe);

         -- se incrementa el indice
         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         --trace("v_error_det_tpo_proceso_no_existe "||v_error_det_tpo_proceso_no_existe);
      END IF
      
      
      -----------------------------------------------------------------------------------------------------
	  -- Requerimiento 878
	  -- Validar para los grupos: 0101, 0103 y 0124
	  -----------------------------------------------------------------------------------------------------
	    LET ret_ley73_importe_tesofe = tmp_ret_det_imp_transf_ssv;
		IF tmp_ret_det_tpo_proceso IN('0102','0103','0124') THEN
      		----------------------------------------------------------------------------------------------------------------------------------
			-- Leer el campo: Id. 33. Importe TESOFE: Sera el importe a debitar del saldo TESOFE 47 (Saldo transferido al gobierno federal),
			-- que forma parte  del id. 24 “Importe de pago FICO” más el importe pagado de la SSV 92 y SSV 97 (AIVS pagadas por el tipo de cambio)
			----------------------------------------------------------------------------------------------------------------------------------
			IF ret_ley73_importe_tesofe < 0 THEN
				-- se rechaza
				LET v_b_rechazo_detalle    = 1;
				INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_importe_tesofe_invalido);
				-- se incrementa el indice
				LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
			ELSE
				----------------------------------------------------------------------------------------------------------------------------------
				-- Validar id 24 imp_pago_fico: Ya que el mismo estará conformado para cada registro por:
				--                              El importe del id 33. Importe Tesofe (valudado en pesos)
				--                      +       (Importe AIVS 92 (id 42) + Importe AIVS 97 (id 40)  por el tipo de cambio del id 44 “Tipo de cambio de operación” Importe de pago en FICO)
				--
				--                              Es decir id 33 + ((id 42 + id 40) * id 44)
				-- Ajustar la validación para aceptar una variación del calculo realizado en SACI contra el dato del importe del 
				-- Id. 24, de hasta +/- 10 pesos, considerando los redondeos de los pagos anteriores.
				----------------------------------------------------------------------------------------------------------------------------------
				LET v_diferencia_imp_pago_fico = (tmp_ret_det_ssv_imp92_ivs + tmp_ret_det_ssv_imp97_ivs) * tmp_ret_det_tpo_cambio;
		        LET v_diferencia_imp_pago_fico = v_diferencia_imp_pago_fico + ret_ley73_importe_tesofe;
				LET v_diferencia_imp_pago_fico = tmp_ret_det_imp_pago_fico - v_diferencia_imp_pago_fico;
				LET v_diferencia_imp_pago_fico = ROUND(v_diferencia_imp_pago_fico,2);
				IF ( v_diferencia_imp_pago_fico > 10 OR v_diferencia_imp_pago_fico < -10 ) THEN
					-- se rechaza
					LET v_b_rechazo_detalle    = 1;
					INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_importe_tesofe_invalido);
					-- se incrementa el indice
					LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
				END IF
		    END IF			
		END IF
		-- Para estos grupos el importe tesofe sera el contenido en el campo id 24 y no en el id 33
		IF tmp_ret_det_tpo_proceso IN('0401','0402','0403','0404','0411','0412','0413','0414') THEN
			LET ret_ley73_importe_tesofe = tmp_ret_det_imp_pago_fico;
		END IF

	     -- se verifica que se hayan recibido AIVs de viv92 o viv97
         LET tmp_ret_det_ssv_imp97_ivs = Replace(tmp_ret_det_ssv_imp97_ivs, ",", "");
         IF LENGTH(TRIM(tmp_ret_det_ssv_imp97_ivs)) = 0 THEN
             LET v_tmp_ret_det_aivs        = 0;
         ELSE 
             LET v_tmp_ret_det_aivs        = (tmp_ret_det_ssv_imp97_ivs);
         END IF 
         
         -- se verifica viv92
         LET tmp_ret_det_ssv_imp92_ivs =  Replace(tmp_ret_det_ssv_imp92_ivs, ",", "");
         IF LENGTH(TRIM(tmp_ret_det_ssv_imp92_ivs)) = 0 THEN
             LET v_tmp_ret_det_aivs_aux    =  0;
         ELSE
             LET v_tmp_ret_det_aivs_aux    =  (tmp_ret_det_ssv_imp92_ivs);
         END IF
         
         LET tmp_ret_det_ssv_imp97_pesos =  Replace(tmp_ret_det_ssv_imp97_pesos, ",", "");
         IF LENGTH(TRIM(tmp_ret_det_ssv_imp97_pesos)) = 0 THEN
             LET v_pesos_viv97_paso = 0;
         ELSE 
             LET v_pesos_viv97_paso = (tmp_ret_det_ssv_imp97_pesos);
         END IF
         
         LET tmp_ret_det_ssv_imp92_pesos =  Replace(tmp_ret_det_ssv_imp92_pesos, ",", "");
         IF LENGTH(TRIM(tmp_ret_det_ssv_imp92_pesos)) = 0 THEN
             LET v_pesos_viv92_paso = 0;
         ELSE 
             LET v_pesos_viv92_paso = (tmp_ret_det_ssv_imp92_pesos);
         END IF 

      -- Requerimiento 878
      -- verificacion que no sea proceso 0401,0402,0403,0404,0411,0412,0413,0414; pues por defecto no deberian traer aiv's
      -- si es proceso 0102, 0103 o 0124 solo se valida el imp_pago_fico (Si se rechaza por el, aqui no entraría, si se acepta no se
      -- necesita validar, por lo que es inecesario en ambos casos el aplicar esta validación). 
	     IF tmp_ret_det_tpo_proceso NOT IN('0102','0103','0124','0401','0402','0403','0404','0411','0412','0413','0414') THEN
	     
	       -- si no se tiene viv97 ni viv92
         IF ( v_tmp_ret_det_aivs <= 0 AND v_tmp_ret_det_aivs_aux <= 0 ) THEN
            -- se rechaza
            LET v_b_rechazo_detalle    = 1;
            INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_sin_aivs);
         
            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
            --trace("v_error_det_ssv_imp97_aivs_igual_cero "||v_error_det_ssv_imp97_aivs_igual_cero);
         END IF

         IF ( (v_tmp_ret_det_aivs > 0 AND v_pesos_viv97_paso = 0) OR
              (v_tmp_ret_det_aivs = 0 AND v_pesos_viv97_paso > 0) ) THEN 
            -- se rechaza
            LET v_b_rechazo_detalle    = 1;
            IF v_tmp_ret_det_aivs = 0 THEN 
                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_ssv_imp97_aivs_igual_cero);
            ELSE
                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_ssv_imp97_pesos_igual_cero);
            END IF
            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF 

         IF ( (v_tmp_ret_det_aivs_aux > 0 AND v_pesos_viv92_paso = 0) OR
              (v_tmp_ret_det_aivs_aux = 0 AND v_pesos_viv92_paso > 0) ) THEN 
            -- se rechaza
            LET v_b_rechazo_detalle    = 1;
            IF v_tmp_ret_det_aivs_aux = 0 THEN 
                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_ssv_imp92_aivs_igual_cero);
            ELSE 
                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_ssv_imp92_pesos_igual_cero);
            END IF
            -- se incrementa el indice
            LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         END IF 
      
      END IF
      
      -- se verifica que se tenga fecha de valuacion
      -- se modifica para tomar del mismo archivo el precio para la valuacion de las aivs  140911 RPR
      LET v_precio_fondo = 0;
      
      -- Cambio el tipo de dato del layout
      --LET tmp_ret_det_tpo_cambio =  Replace(tmp_ret_det_tpo_cambio, ",", "");
      --IF LENGTH(TRIM(tmp_ret_det_tpo_cambio)) = 0 THEN
      --    LET v_precio_fondo = 0;
      --ELSE
      --    LET v_precio_fondo    =  (tmp_ret_det_tpo_cambio);
      --END IF

			-- Requerimiento 878
			-- Escenarios en los que puede no venir vivienda 97
			-- Grupos: 0102, 0103, 0124, 0402, 0403, 0412, 0413, 0414
			-- Grupo 0404 solo afecta Tesofe
      LET v_precio_fondo    =  (tmp_ret_det_tpo_cambio);
      IF ( v_precio_fondo <= 0 OR v_precio_fondo IS NULL ) AND tmp_ret_det_tpo_proceso <> '0404' THEN
         -- Si es de este grupo
         IF tmp_ret_det_tpo_proceso IN ('0102','0103','0124','0402','0403','0412','0413','0414') THEN
            -- Si pertenece a los grupos mencionados y ademas viene importe aiv de vivienda 97 se rechaza si no trae precio de fondo
            IF v_tmp_ret_det_aivs > 0 THEN
                -- se rechaza
         		    --trace ("se rechazo ");
                --trace (v_id_derechohabiente);
                --trace (v_indice_codigos_rechazo);
                --trace ( v_error_det_f_valuacion_no_existe);
                LET v_b_rechazo_detalle    = 1;
                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_cambio);
                -- se incrementa el indice
         				LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
            END IF -- v_tmp_ret_det_aivs > 0
         ELSE
         		IF tmp_ret_det_tpo_proceso IN ('0401','0411') THEN
         		   -- Si pertenece a los grupos mencionados y ademas viene importe aiv de vivienda 97 se rechaza si no trae precio de fondo
	             IF v_tmp_ret_det_aivs > 0 THEN
	                -- se rechaza
	         		    --trace ("se rechazo ");
	                --trace (v_id_derechohabiente);
	                --trace (v_indice_codigos_rechazo);
	                --trace ( v_error_det_f_valuacion_no_existe);
	                LET v_b_rechazo_detalle    = 1;
	                INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_cambio);
	                -- se incrementa el indice
	         				LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
	             END IF -- v_tmp_ret_det_aivs > 0
	             -- Si pertenece a los grupos mencionados y ademas viene importe aiv de vivienda 92 se rechaza si no trae precio de fondo
               IF v_tmp_ret_det_aivs_aux > 0 THEN
                  -- se rechaza
         		      --trace ("se rechazo ");
                  --trace (v_id_derechohabiente);
                  --trace (v_indice_codigos_rechazo);
                  --trace ( v_error_det_f_valuacion_no_existe);
                  LET v_b_rechazo_detalle    = 1;
                  INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_cambio);
                  -- se incrementa el indice
         				  LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
               END IF -- v_tmp_ret_det_aivs > 0
            ELSE
               -- se rechaza
         		   --trace ("se rechazo ");
               --trace (v_id_derechohabiente);
               --trace (v_indice_codigos_rechazo);
               --trace ( v_error_det_f_valuacion_no_existe);
               LET v_b_rechazo_detalle    = 1;
               INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_tpo_cambio);
               -- se incrementa el indice
         		   LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
         		END IF -- tmp_ret_det_tpo_proceso IN ('0401','411')
         END IF -- tmp_ret_det_tpo_proceso IN ('0102','0103','0124','0402','0403','0412','0413','0414')
      END IF -- ( v_precio_fondo <= 0 OR v_precio_fondo IS NULL ) AND tmp_ret_det_tpo_proceso <> '0404'

      -- Se validan los pesos, que sean exactamente lo que resulte de multiplicar las AIVS por el tipo de cambio
--      trace("Validando Importe " || tmp_ret_det_nss || ":");
--      trace("Aivs 97 Archivo :" || v_tmp_ret_det_aivs);
--      trace("Aivs 92 Archivo :" || v_tmp_ret_det_aivs_aux);
--      trace("Tipo de Cambio  :" || v_precio_fondo);
      IF v_precio_fondo > 0 THEN
          IF v_tmp_ret_det_aivs > 0 THEN
              LET v_valuacion_paso = ROUND(v_tmp_ret_det_aivs * v_precio_fondo,2);
              LET tmp_ret_det_ssv_imp97_pesos = Replace(tmp_ret_det_ssv_imp97_pesos, ",", "");
              LET v_pesos_archivo_paso        = (tmp_ret_det_ssv_imp97_pesos);
--              trace("   Pesos 97 archivo  :" || tmp_ret_det_ssv_imp97_pesos);
--              trace("   Comparacion 97    :" || v_valuacion_paso || ": :" || v_pesos_archivo_paso || ":");
              IF v_valuacion_paso <> v_pesos_archivo_paso THEN
                  LET v_b_rechazo_detalle    = 1;
                  INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_pesos97_mal_valuados);
                  -- se incrementa el indice
                  LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
              END IF 
          END IF  
          IF v_tmp_ret_det_aivs_aux > 0 THEN
              LET v_valuacion_paso = ROUND(v_tmp_ret_det_aivs_aux * v_precio_fondo,2);
              LET tmp_ret_det_ssv_imp92_pesos = Replace(tmp_ret_det_ssv_imp92_pesos, ",", "");
              LET v_pesos_archivo_paso        = (tmp_ret_det_ssv_imp92_pesos);
--              trace("   Pesos 92 archivo  :" || tmp_ret_det_ssv_imp92_pesos);
--              trace("   Comparacion 92    :" || v_valuacion_paso || ": :" || v_pesos_archivo_paso || ":");
              IF v_valuacion_paso <> v_pesos_archivo_paso THEN
                  LET v_b_rechazo_detalle    = 1;
                  INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_pesos92_mal_valuados);
                  -- se incrementa el indice
                  LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
              END IF 
          END IF
      END IF
      
--      IF NOT EXISTS (SELECT precio_fondo
--                  FROM glo_valor_fondo
--                 WHERE fondo   =  11
--                   AND f_valuacion = tmp_ret_det_f_valuacion) or  tmp_ret_det_f_valuacion is null THEN
--         -- se rechaza
--         --trace ("se rechazo ");
--         --trace (v_id_derechohabiente);
--         --trace (v_indice_codigos_rechazo);
--         --trace ( v_error_det_f_valuacion_no_existe);
--         LET v_b_rechazo_detalle    = 1;
--         INSERT INTO tmp_codigos_rechazo VALUES (v_id_derechohabiente,v_indice_codigos_rechazo, v_error_det_f_valuacion_no_existe);

--         -- se incrementa el indice
--         LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
--         --trace("v_error_det_ssv_imp92_aivs_igual_cero "||v_error_det_ssv_imp92_aivs_igual_cero);
--      END IF

       --trace("v_b_rechazo_detalle "||v_b_rechazo_detalle);
      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN

         LET v_motivo_rechazo_1 = 0;
         LET v_motivo_rechazo_2 = 0;
         LET v_motivo_rechazo_3 = 0;

         -- se leen los tres primeros errores
         FOREACH
            SELECT FIRST 3 id_codigo,  codigo_rechazo
              INTO v_indice_codigos_rechazo, v_codigos_rechazo
              FROM tmp_codigos_rechazo
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

          SELECT FIRST 1 id_derechohabiente
            INTO v_id_derechohabiente
            FROM afi_derechohabiente
           WHERE nss = tmp_ret_det_nss
             AND NOT(id_derechohabiente IS NULL );

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- si fue rechazado no se inserta en el historico
         --CONTINUE FOREACH;
      END IF

      SELECT sum(monto_pesos)
        INTO v_rec_tot_saldo_viv97
        FROM afi_derechohabiente afi,
             cta_movimiento cta
       WHERE afi.id_derechohabiente = cta.id_derechohabiente
         AND nss                = tmp_ret_det_nss
         AND fondo_inversion    = 11
         AND subcuenta          = 4;

      SELECT sum(monto_pesos)
        INTO v_rec_tot_saldo_viv92
        FROM afi_derechohabiente afi,
             cta_movimiento cta
       WHERE afi.id_derechohabiente = cta.id_derechohabiente
         AND nss                = tmp_ret_det_nss
         AND fondo_inversion    = 11
         AND subcuenta          = 8;

      -- se obtiene el id_solicitud
      SELECT seq_ret_solicitud.NEXTVAL
        INTO v_id_solicitud
        FROM systables
       WHERE tabid = 1;

     SELECT id_derechohabiente
       INTO v_id_derechohabiente
       FROM afi_derechohabiente
      WHERE nss = tmp_ret_det_nss;

      --trace("Asignando datos a registro de detalle Ley 73");
      --trace (v_id_solicitud             );
      --trace (v_id_derechohabiente       );
      --trace (tmp_ret_det_tpo_proceso    );
      --trace (tmp_ret_det_f_inicio_tram  );
      --trace ("15"                       );
      --trace (p_folio                    );
      --trace (tmp_ret_det_ssv_imp92_ivs  );
      --trace (tmp_ret_det_ssv_imp97_ivs  );
      --trace (tmp_ret_det_ssv_imp92_pesos);
      --trace (tmp_ret_det_ssv_imp97_pesos);
      --trace ("0"                        );
      --trace ("NULL"                       );
      --trace ( TODAY                     );
      --trace ( TODAY                     );
      --trace (CURRENT HOUR TO SECOND     );
      --trace (p_usuario_cod              );
      --trace ("NULL"                       );
      --trace (tmp_ret_det_est_jfca       );
      --trace ("0"                        );
      --trace (tmp_ret_det_est_ssv        );
      IF ( v_id_derechohabiente IS NULL ) THEN
         LET v_id_derechohabiente = 0;
      END IF

      ---- se asignan los datos al registro de rechazo de detalle
      LET ret_ley73_id_solicitud         = v_id_solicitud                  ;
      LET ret_ley73_id_derechohabiente   = v_id_derechohabiente            ;
      LET ret_ley73_tpo_proceso          = tmp_ret_det_tpo_proceso         ;

      IF tmp_ret_det_f_inicio_tram = "00.00.0000" or tmp_ret_det_f_inicio_tram is null
      OR tmp_ret_det_f_inicio_tram[4,5]  > 31 or  tmp_ret_det_f_inicio_tram[1,2] > 13 or  tmp_ret_det_f_inicio_tram[7,10] < 1950 THEN
         LET tmp_ret_det_f_inicio_tram   = "01/01/0001"  ;
      END IF

      LET ret_ley73_f_solicitud        = tmp_ret_det_f_inicio_tram;

        --trace("Asignando datos a registro salio");
      --LET ret_ley73_estado_solicitud     = "15"                            ;
      LET ret_ley73_folio                = p_folio                         ;

      if trim(tmp_ret_det_ssv_imp92_ivs) is null or  trim(tmp_ret_det_ssv_imp92_ivs)  = "" then
         LET  ret_ley73_aivs_viv92 = 0;
      else
         LET ret_ley73_aivs_viv92           = (tmp_ret_det_ssv_imp92_ivs)       ;
      end if

      if trim(tmp_ret_det_ssv_imp97_ivs) is null or  trim(tmp_ret_det_ssv_imp97_ivs)  = "" then
         LET  ret_ley73_aivs_viv97 = 0;
      else
         LET ret_ley73_aivs_viv97           = (tmp_ret_det_ssv_imp97_ivs)       ;
      end if

      if trim(tmp_ret_det_ssv_imp92_pesos) is null or  trim(tmp_ret_det_ssv_imp92_pesos)  = "" then
         LET  ret_ley73_importe_viv92 = 0;
      else
         LET ret_ley73_importe_viv92           = (tmp_ret_det_ssv_imp92_pesos)       ;
      end if

      if trim(tmp_ret_det_ssv_imp97_pesos) is null or  trim(tmp_ret_det_ssv_imp97_pesos)  = "" then
         LET  ret_ley73_importe_viv97 = 0;
      else
         LET ret_ley73_importe_viv97           = (tmp_ret_det_ssv_imp97_pesos)       ;
      end if

      if tmp_ret_det_f_valuacion   = "00.00.0000" or tmp_ret_det_f_valuacion is null
      or tmp_ret_det_f_valuacion[4,5]  > 31 or  tmp_ret_det_f_valuacion[1,2] > 13 or  tmp_ret_det_f_valuacion[7,10] < 1950 THEN
         LET tmp_ret_det_f_valuacion   = "01/01/0001"  ;
      end if

      IF tmp_ret_det_f_valuacion = "01/01/0001" THEN
         LET ret_ley73_f_valuacion        = tmp_ret_det_f_valuacion;
      ELSE
         LET ret_ley73_f_valuacion       = tmp_ret_det_f_valuacion ;
         --LET v_precio_fondo              = 0 ;

         --SELECT precio_fondo
         --  INTO v_precio_fondo
         --  FROM glo_valor_fondo
         -- WHERE fondo       = 11
         --   AND f_valuacion = tmp_ret_det_f_valuacion;

         LET ret_ley73_importe_viv92        = tmp_ret_det_ssv_imp92_ivs * v_precio_fondo ;
         LET ret_ley73_importe_viv97        = tmp_ret_det_ssv_imp97_ivs * v_precio_fondo ;
      END IF

      LET ret_ley73_tpo_cambio           = v_precio_fondo                  ;
      LET ret_ley73_cod_retorno          = "0"                             ;
      LET ret_ley73_nrp                  = NULL                            ;
      LET ret_ley73_f_captura            = "01/01/0001"                    ;
      LET ret_ley73_h_captura            = CURRENT HOUR TO SECOND          ;
      LET ret_ley73_usuario              = p_usuario_cod                   ; -- antes "Contingencia"
      LET ret_ley73_marca_juridico       = NULL                            ;
      LET ret_ley73_estado_jfca          = tmp_ret_det_est_jfca            ;
      LET v_codigos_rechazo = 0;

      SELECT FIRST 1  id_codigo,  codigo_rechazo
        INTO v_indice_codigos_rechazo, v_codigos_rechazo
        FROM tmp_codigos_rechazo
       WHERE id_derechohabiente =  v_id_derechohabiente             ;

      IF v_codigos_rechazo <> 0 OR v_id_derechohabiente = 0 Then
         LET ret_ley73_estado_solicitud = "100"                            ;
         IF v_id_derechohabiente = 0 THEN
             LET ret_ley73_cod_rechazo      = v_error_det_nss_no_encontrado ;
         ELSE
             LET ret_ley73_cod_rechazo      = v_codigos_rechazo                ;
         END IF
      ELSE
         LET ret_ley73_estado_solicitud = "15"                             ;
         LET ret_ley73_cod_rechazo      = "0"                              ;
      END IF

      LET ret_ley73_estado_interno  = tmp_ret_det_est_ssv             ;

      --if  ( tmp_ret_det_est_ssv in (14,17,18,19,21,23,24,25) AND ret_ley73_tpo_proceso = 101 AND ret_ley73_estado_solicitud = "15") THEN
      --   LET ret_ley73_estado_solicitud = "15";
      --ELSE
      --   if not ret_ley73_estado_solicitud = "100" THEN
      --      LET ret_ley73_estado_solicitud = "18";
      --   end if
      --end if


      --trace ("|"||v_id_solicitud       ||"|"      );
      LET ret_autoriza_id_solicitud        =  v_id_solicitud                     ;

      --trace ("|"||tmp_ret_det_cuenta_bancaria ||"|");
      LET ret_autoriza_clabe               =  tmp_ret_det_cuenta_bancaria        ;

      --trace ("|"||tmp_ret_det_cve_banco   ||"|"   );
      LET ret_autoriza_banco               =  tmp_ret_det_cve_banco              ;

      --trace ("|"||tmp_ret_det_entidad_fed ||"|"   );
      LET ret_autoriza_entidad_federativa  =  tmp_ret_det_entidad_fed            ;

        --trace (tmp_ret_det_f_autorizacion_pago);
        --trace ("NULL"                      );
        --trace (tmp_ret_det_ssv_cve_afore );
        --trace (tmp_ret_det_num_caso_adai );
        --trace (tmp_ret_det_num_laudo     );
        --trace (tmp_ret_det_num_junta_esp );

     --trace ("tmp_ret_det_f_autorizacion_pago"||tmp_ret_det_f_autorizacion_pago);
      if tmp_ret_det_f_autorizacion_pago   = "00.00.0000" or tmp_ret_det_f_autorizacion_pago is null
      or tmp_ret_det_f_autorizacion_pago[4,5]  > 31 or  tmp_ret_det_f_autorizacion_pago[1,2] > 13 or  tmp_ret_det_f_autorizacion_pago[7,10] < 1950 THEN
         LET tmp_ret_det_f_autorizacion_pago   = "01/01/0001"  ;
      end if

      LET ret_autoriza_f_autorizacion        = tmp_ret_det_f_autorizacion_pago;

      LET ret_autoriza_f_marca             =  "01/01/0001"                       ; --checar con franco si va
      LET ret_autoriza_cve_afore           =  tmp_ret_det_ssv_cve_afore          ;
      LET ret_autoriza_caso_adai           =  tmp_ret_det_num_caso_adai          ;
      LET ret_autoriza_num_laudo           =  tmp_ret_det_num_laudo              ;
      LET ret_autoriza_num_junta           =  tmp_ret_det_num_junta_esp          ;

      --trace("Asignando datos a registro de ret_beneficiario");
      --trace (v_id_solicitud            );
      LET ret_beneficiario_id_solicitud          =  v_id_solicitud              ;

      --trace (tmp_ret_det_beneficiario  );
      IF tmp_ret_det_beneficiario = "BENEFICIARIO" THEN
         LET ret_beneficiario_tpo_beneficiario      =  1    ;
      ELSE
         IF tmp_ret_det_beneficiario = "TITULAR" THEN
            LET ret_beneficiario_tpo_beneficiario      =  2    ;
         ELSE
            LET ret_beneficiario_tpo_beneficiario      =  NULL    ;
         END IF
      END IF


      --trace (tmp_ret_det_nombre_benef  );
      LET ret_beneficiario_nombre_beneficiario   =  tmp_ret_det_nombre_benef    ;
      --trace (tmp_ret_det_ape_pat_benef );
      LET ret_beneficiario_paterno_beneficiario  =  tmp_ret_det_ape_pat_benef   ;
      --trace (tmp_ret_det_ape_mat_benef );
      LET ret_beneficiario_materno_beneficiario  =  tmp_ret_det_ape_mat_benef   ;
      --trace (TODAY                     );
      LET ret_beneficiario_f_registro            =  "01/01/0001"                ;  --checar con franco si va

            --trace("Asignando datos a registro de ret_pago_trm");
      LET ret_pago_trm_id_solicitud              =  v_id_solicitud                   ;
      LET ret_pago_trm_id_derechohabiente        =  v_id_derechohabiente             ;
      LET ret_pago_trm_estatus_trm               =  NULL                             ;  --checar con franco si va
      LET ret_pago_trm_estatus_jfca              =  tmp_ret_det_est_jfca             ;  --checar con franco si va
      LET ret_pago_trm_tpo_proceso               =  NULL                             ;  --checar con franco si va
      LET ret_pago_trm_f_marca                   =  "01/01/0001"                     ;  --checar con franco si va
      LET ret_pago_trm_f_pago                    =  "01/01/0001"                     ;  --checar con franco si va
      LET ret_pago_trm_importe_pagado            =  NULL                             ;  --checar con franco si va
      LET ret_pago_trm_doc_pago_fico             = tmp_ret_det_no_doc_pago_fico      ;
      LET ret_pago_trm_doc_por_pagar_fico        = tmp_ret_det_num_doc_cta_pago_fico ;


      if tmp_ret_det_f_pago_fico       = "00.00.0000" or tmp_ret_det_f_pago_fico is null
      or tmp_ret_det_f_pago_fico[4,5]  > 31 or  tmp_ret_det_f_pago_fico[1,2] > 13 or  tmp_ret_det_f_pago_fico[7,10] < 1950 THEN
         LET tmp_ret_det_f_pago_fico   = "01/01/0001"  ;
      end if

      LET ret_pago_trm_f_pago_fico        = tmp_ret_det_f_pago_fico;

      --LET ret_pago_trm_f_pago_fico               = tmp_ret_det_f_pago_fico         ;
      LET ret_pago_trm_ef_cta_por_pagar_fico     = tmp_ret_det_eje_fis_cpp_fico      ;
      LET ret_pago_trm_referencia_pago_fico      = tmp_ret_det_ref_pago_fico         ;
      LET ret_pago_trm_tpo_beneficiario          = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_nombre_beneficiario       = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_paterno_beneficiario      = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_materno_beneficiario      = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_clabe                     = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_cve_banco                 = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_entidad_federativa        = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_f_autorizacion            = "01/01/0001"                      ;  --checar con franco si va
      LET ret_pago_trm_cve_afore                 = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_caso_adai                 = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_num_laudo                 = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_num_junta                 = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_resultado_operacion       = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_cod_rechazo_1             = NULL                              ;  --checar con franco si va
      LET ret_pago_trm_cod_rechazo_2             = NULL                              ;  --checar con franco si va
      --trace("Asignando datos a registro de ret_pago_juridico");
      LET ret_pago_juridico_id_solicitud         =  v_id_solicitud                   ;
      LET ret_pago_juridico_id_derechohabiente   =  v_id_derechohabiente             ;
      LET ret_pago_juridico_tipo_resolucion      =  '0'                              ;  --checar con franco si va


       if tmp_ret_det_f_marca   = "00.00.0000" or tmp_ret_det_f_marca is null
       or tmp_ret_det_f_marca[4,5]  > 31 or  tmp_ret_det_f_marca[1,2] > 13 or  tmp_ret_det_f_marca[7,10] < 1950 THEN
         LET tmp_ret_det_f_marca   = "01/01/0001"  ;
      end if

      LET ret_pago_juridico_f_marca_tj       = tmp_ret_det_f_marca;

      if tmp_ret_det_f_pago_ant   = "00.00.0000" or tmp_ret_det_f_pago_ant is null
      or tmp_ret_det_f_pago_ant[4,5]  > 31 or  tmp_ret_det_f_pago_ant[1,2] > 13 or  tmp_ret_det_f_pago_ant[7,10] < 1950 THEN
         LET tmp_ret_det_f_pago_ant   = "01/01/0001"  ;
      end if

      LET ret_pago_juridico_f_pago        = tmp_ret_det_f_pago_ant;

      LET ret_pago_juridico_importe_pagado       =  tmp_ret_det_imp_pago_ant         ;
      LET ret_pago_juridico_num_registro         =  NULL                             ;  --checar con franco si va
      LET ret_pago_juridico_cod_rechazo          =  NULL                             ;  --checar con franco si va

      --trace("Asignando datos a registro de detalle salio");

      IF ret_ley73_estado_solicitud  = 15 then
         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_marca_cuenta(
                 v_id_derechohabiente
                ,v_marca_ley73 -- marca de disposicion
                ,v_id_solicitud
                ,p_folio
                ,0 -- estado marca
                ,0 -- codigo de rechazo
                ,0 -- marca de la causa
                ,NULL -- fecha de la causa
                ,p_usuario_cod
                ,p_proceso_cod)
            INTO v_i_estado_marca;

      ELSE
         LET isam_err = 1000;
         LET v_c_msj  = 'El proceso finalizó con registros rechazados \nConsultar detalle en consulta de saldos';
      END IF


      -- se inserta en la tabla historia de retiro de Ley 73
     INSERT INTO ret_ley73(
                              id_solicitud
                             ,id_derechohabiente
                             ,tpo_proceso
                             ,f_solicitud
                             ,estado_solicitud
                             ,folio
                             ,aivs_viv92
                             ,aivs_viv97
                             ,importe_viv92
                             ,importe_viv97
                             ,cod_retorno
                             ,nrp
                             ,f_valuacion
                             ,f_captura
                             ,h_captura
                             ,usuario
                             ,marca_juridico
                             ,estado_jfca
                             ,cod_rechazo
                             ,estado_interno
                             ,tpo_cambio
                             ,importe_tesofe
                             )
         VALUES (
                  ret_ley73_id_solicitud
                 ,ret_ley73_id_derechohabiente
                 ,ret_ley73_tpo_proceso
                 ,ret_ley73_f_solicitud
                 ,ret_ley73_estado_solicitud
                 ,ret_ley73_folio
                 ,ret_ley73_aivs_viv92
                 ,ret_ley73_aivs_viv97
                 ,ret_ley73_importe_viv92
                 ,ret_ley73_importe_viv97
                 ,ret_ley73_cod_retorno
                 ,ret_ley73_nrp
                 ,ret_ley73_f_valuacion
                 ,ret_ley73_f_captura
                 ,ret_ley73_h_captura
                 ,ret_ley73_usuario
                 ,ret_ley73_marca_juridico
                 ,ret_ley73_estado_jfca
                 ,ret_ley73_cod_rechazo
                 ,ret_ley73_estado_interno    
                 ,ret_ley73_tpo_cambio
                 ,ret_ley73_importe_tesofe    );

       --trace("posterior al insert ");

           INSERT INTO ret_autoriza(
                                       id_solicitud
                                      ,clabe
                                      ,banco
                                      ,entidad_federativa
                                      ,f_autorizacion
                                      ,f_marca
                                      ,cve_afore
                                      ,caso_adai
                                      ,num_laudo
                                      ,num_junta
                                     )
          VALUES ( ret_autoriza_id_solicitud
                  ,ret_autoriza_clabe
                  ,ret_autoriza_banco
                  ,ret_autoriza_entidad_federativa
                  ,ret_autoriza_f_autorizacion
                  ,ret_autoriza_f_marca
                  ,ret_autoriza_cve_afore
                  ,ret_autoriza_caso_adai
                  ,ret_autoriza_num_laudo
                  ,ret_autoriza_num_junta
                 );
       --trace("posterior al autoriza ");


           INSERT INTO ret_beneficiario (
                                       id_solicitud
                                      ,tpo_beneficiario
                                      ,nombre_beneficiario
                                      ,paterno_beneficiario
                                      ,materno_beneficiario
                                      ,f_registro
                                     )
          VALUES (  ret_beneficiario_id_solicitud
                   ,ret_beneficiario_tpo_beneficiario
                   ,ret_beneficiario_nombre_beneficiario
                   ,ret_beneficiario_paterno_beneficiario
                   ,ret_beneficiario_materno_beneficiario
                   ,ret_beneficiario_f_registro
                 );

          --trace("posterior al beneficiario ");


                     INSERT INTO ret_pago_juridico (
                                       id_solicitud
                                      ,id_derechohabiente
                                      ,tipo_resolucion
                                      ,f_marca_tj
                                      ,f_pago
                                      ,importe_pagado
                                      ,num_registro
                                      ,cod_rechazo
                                     )
          VALUES ( ret_pago_juridico_id_solicitud
                                      ,ret_pago_juridico_id_derechohabiente
                                      ,ret_pago_juridico_tipo_resolucion
                                      ,ret_pago_juridico_f_marca_tj
                                      ,ret_pago_juridico_f_pago
                                      ,ret_pago_juridico_importe_pagado
                                      ,ret_pago_juridico_num_registro
                                      ,ret_pago_juridico_cod_rechazo
                 );
                 --trace("posterior al ret_pago_juridico ");

          INSERT INTO ret_pago_trm (   id_solicitud
                                      ,id_derechohabiente
                                      ,folio
                                      ,estatus_trm
                                      ,estatus_jfca
                                      ,tpo_proceso
                                      ,f_marca
                                      ,f_pago
                                      ,importe_pagado
                                      ,doc_pago_fico
                                      ,doc_por_pagar_fico
                                      ,f_pago_fico
                                      ,ef_cta_por_pagar_fico
                                      ,referencia_pago_fico
                                      ,tpo_beneficiario
                                      ,nombre_beneficiario
                                      ,paterno_beneficiario
                                      ,materno_beneficiario
                                      ,clabe
                                      ,cve_banco
                                      ,entidad_federativa
                                      ,f_autorizacion
                                      ,cve_afore
                                      ,caso_adai
                                      ,num_laudo
                                      ,num_junta
                                      ,resultado_operacion
                                      ,cod_rechazo_1
                                      ,cod_rechazo_2
                                     )
          VALUES (  ret_pago_trm_id_solicitud
                                      ,ret_pago_trm_id_derechohabiente
                                      ,p_folio
                                      ,ret_pago_trm_estatus_trm
                                      ,ret_pago_trm_estatus_jfca
                                      ,ret_pago_trm_tpo_proceso
                                      ,ret_pago_trm_f_marca
                                      ,ret_pago_trm_f_pago
                                      ,ret_pago_trm_importe_pagado
                                      ,ret_pago_trm_doc_pago_fico
                                      ,ret_pago_trm_doc_por_pagar_fico
                                      ,ret_pago_trm_f_pago_fico
                                      ,ret_pago_trm_ef_cta_por_pagar_fico
                                      ,ret_pago_trm_referencia_pago_fico
                                      ,ret_pago_trm_tpo_beneficiario
                                      ,ret_pago_trm_nombre_beneficiario
                                      ,ret_pago_trm_paterno_beneficiario
                                      ,ret_pago_trm_materno_beneficiario
                                      ,ret_pago_trm_clabe
                                      ,ret_pago_trm_cve_banco
                                      ,ret_pago_trm_entidad_federativa
                                      ,ret_pago_trm_f_autorizacion
                                      ,ret_pago_trm_cve_afore
                                      ,ret_pago_trm_caso_adai
                                      ,ret_pago_trm_num_laudo
                                      ,ret_pago_trm_num_junta
                                      ,ret_pago_trm_resultado_operacion
                                      ,ret_pago_trm_cod_rechazo_1
                                      ,ret_pago_trm_cod_rechazo_2
                 );
      --trace("posterior al ret_pago_juridico ");
      --trace("posterior al ret_pago_trm ");
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

   END FOREACH;
   --trace("fin FOREACH ");
   UPDATE STATISTICS FOR TABLE ret_ley73;
   UPDATE STATISTICS FOR TABLE ret_pago_trm;
   UPDATE STATISTICS FOR TABLE ret_pago_juridico;
   UPDATE STATISTICS FOR TABLE ret_beneficiario;
   UPDATE STATISTICS FOR TABLE ret_autoriza;
   
   LET v_c_msj = v_c_msj||'. Registros insertados: '||v_reg_det_insertados||'. Registros rechazados: '||v_reg_det_rechazados;
   
   -- si no se preliquidaron registros
   IF ( v_reg_det_insertados = v_reg_det_rechazados ) THEN
      -- se marca el procesoe en error
      LET v_si_resultado = 1000;
      LET isam_err       = 0;
      LET v_c_msj        = v_c_msj||"Error. No se preliquidaron solicitudes para el folio.";
   END IF;

   -- se devuelve el resultado de la ejecucion
   RETURN v_si_resultado, isam_err, v_c_msj, tmp_ret_det_nss;
END FUNCTION;


