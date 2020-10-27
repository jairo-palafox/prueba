






CREATE FUNCTION "safreviv".fn_copia_transferencia( p_usuario_cod     CHAR(20)     ,
                                        p_folio           DECIMAL(10,0),
                                        p_nombre_archivo  VARCHAR(40,0),
                                        p_pid             DECIMAL(9,0) ,
                                        p_proceso_cod     SMALLINT  
                                      )
   RETURNING SMALLINT, INTEGER, VARCHAR(255)
 
   --tabla destino 
   -- campos de la tabla de detalle de retiros por transferencia (sin filler)
   DEFINE tmp_det_transf_tpo_registro         CHAR(2)     ;
   DEFINE tmp_det_transf_id_servicio          CHAR(2)     ;
   DEFINE tmp_det_transf_id_operacion         CHAR(2)     ;
   DEFINE tmp_det_transf_nss                  CHAR(11)    ;
   DEFINE tmp_det_transf_curp                 CHAR(18)    ;
   DEFINE tmp_det_transf_nombre_trab_datamart CHAR(50)    ;
   DEFINE tmp_det_transf_nombre_afore         CHAR(40)    ;
   DEFINE tmp_det_transf_paterno_afore        CHAR(40)    ;
   DEFINE tmp_det_transf_materno_afore        CHAR(40)    ;
   DEFINE tmp_det_transf_sec_pension          CHAR(2)     ;
   DEFINE tmp_det_transf_tpo_movimiento       CHAR(3)     ;
   DEFINE tmp_det_transf_regimen              CHAR(2)     ;
   DEFINE tmp_det_transf_tpo_retiro           CHAR(1)     ;
   DEFINE tmp_det_transf_tpo_seguro           CHAR(2)     ;
   DEFINE tmp_det_transf_tpo_pension          CHAR(2)     ;
   DEFINE tmp_det_transf_tpo_prestacion       CHAR(2)     ;
   DEFINE tmp_det_transf_f_inicio_pension     DATE        ;
   DEFINE tmp_det_transf_f_emision_resol      DATE        ;
   DEFINE tmp_det_transf_porc_valuacion       DECIMAL(5)  ;
   DEFINE tmp_det_transf_sem_cotizadas        DECIMAL(4)  ;
   DEFINE tmp_det_transf_f_carga_datamart     DECIMAL(8)  ;
   DEFINE tmp_det_transf_diagnostico_reg      CHAR(3)     ;
   DEFINE tmp_det_transf_estatus_subcta       CHAR(1)     ;
   DEFINE tmp_det_transf_periodo_pago         DECIMAL(6)  ;
   DEFINE tmp_det_transf_acciones_ret97       DECIMAL(14) ;
   DEFINE tmp_det_transf_acciones_cv          DECIMAL(14) ;
   DEFINE tmp_det_transf_acciones_cuotsol     DECIMAL(14) ;
   DEFINE tmp_det_transf_aiv97                DECIMAL(14) ;
   DEFINE tmp_det_transf_result_operacion     CHAR(2)     ;
   DEFINE tmp_det_transf_cve_afore            DECIMAL(3)  ;
   DEFINE tmp_det_transf_motivo_rech1         CHAR(3)     ;
   DEFINE tmp_det_transf_motivo_rech2         CHAR(3)     ;
   -- campos agregados para la carga inicial
   DEFINE tmp_det_transf_f_carga_infonavit    DATE        ;
   DEFINE tmp_det_transf_f_transferencia      DATE        ;
   DEFINE tmp_det_transf_monto_aivs           DECIMAL(9,0);
   DEFINE tmp_det_transf_monto_pesos          DECIMAL(9,0);
-- =================================================================================


-- detalle de la tabla historica/integrada de retiros por transferencia
-- ret_transferencia
   DEFINE ret_transf_id_solicitud         DECIMAL(9,0) ;
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

   -- campos de la tabla de encabezado de transferencia
   DEFINE ret_cza_transferencia_folio                 decimal(9,0)           ;
   DEFINE ret_cza_transferencia_nombre_archivo        char(20)               ;
   DEFINE ret_cza_transferencia_f_operacion_procesar  date                   ;
   DEFINE ret_cza_transferencia_f_carga               date                   ;
   DEFINE ret_cza_transferencia_h_carga               datetime hour to minute;
   DEFINE ret_cza_transferencia_f_valor_transferencia date                   ;
   DEFINE ret_cza_transferencia_precio_fondo          decimal(14,6)          ;
   DEFINE ret_cza_transferencia_total_registros       integer                ;
   DEFINE ret_cza_transferencia_total_importe         decimal(22,2)          ;
   DEFINE ret_cza_transferencia_usuario               char(20)               ;


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
 
 -- para marcar las cuentas
 DEFINE v_i_estado_marca                          INTEGER;
 DEFINE v_marca_transferencia                     INTEGER; -- 806 transferencia de recurso

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

   --SET DEBUG FILE TO ("/ds/safreviv_int/BD/debug_ret_ci_transferencia.txt");
--TRACE "actualizando glo_ctr_archivo";
--TRACE "proceso_cod: " || p_proceso_cod;
--TRACE "archivo: " || p_nombre_archivo;
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

   -- se asume que no hay errores
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = 'El proceso finalizó correctamente';

   -- se inician los contadores de registros insertados y rechazados
   LET v_reg_det_insertados  = 0; -- total de registros de detalle insertados
   LET v_reg_det_rechazados  = 0; -- total de registros de detalle rechazados

   -- se asume que el proceso termina bien
   LET v_estatus_proceso = 0;

   -- se inician los codigos de error en detalle
   LET v_error_det_nss_no_encontrado             = 1;
   LET v_error_det_tpo_registro_invalido         = 2;
   LET v_error_det_id_servicio_invalido          = 3;
   LET v_error_det_id_operacion_invalido         = 4;
   LET v_error_det_matriz_derecho_no_encontrado  = 5;
   LET v_error_det_sec_pension_invalido          = 6;
   LET v_error_det_fec_solicitud_invalido        = 7;
   LET v_error_det_afore_invalido                = 8;

   -- se inician las variables para marca
   LET v_marca_transferencia = 806; -- marca para transferencia de recurso
   LET v_i_estado_marca      = 0;

   -- se asume que no hay rechazos
   LET v_b_rechazo_encabezado = 0;

   -- se crea una tabla temporal de codigos de error
   LET v_indice_codigos_rechazo = 1;    
 
   -- se inicia el importe total
   LET v_sumario_importe_total = 0;
   
   -- se inicia la variable que almacenaria el id_transferencia
   LET v_id_solicitud = 0;
   
   
   -- se asume que no hay rechazos en el detalle del archivo
   LET v_b_rechazo_detalle    = 0;

   CREATE TEMP TABLE tmp_codigos_rechazo (
   id_codigo       SMALLINT,
   codigo_rechazo SMALLINT
   );

   -- se obtienen los datos del detalle
   FOREACH
   SELECT
      tpo_registro         ,
      id_servicio          ,
      id_operacion         ,
      nss                  ,
      curp                 ,
      nombre_trab_datamart ,
      nombre_afore         ,
      paterno_afore        ,
      materno_afore        ,
      sec_pension          ,
      tpo_movimiento       ,
      regimen              ,
      tpo_retiro           ,
      tpo_seguro           ,
      tpo_pension          ,
      tpo_prestacion       ,
      f_inicio_pension     ,
      f_emision_resol      ,
      porc_valuacion       ,
      sem_cotizadas        ,
      f_carga_datamart     ,
      diagnostico_reg      ,
      estatus_subcta       ,
      periodo_pago         ,
      acciones_ret97       ,
      acciones_cv          ,
      acciones_cuotsol     ,
      aiv97                ,
      result_operacion     ,
      cve_afore            ,
      motivo_rech1         ,
      motivo_rech2         ,
      f_carga_infonavit    ,
      f_transferencia      ,
      monto_aivs           ,
      monto_pesos          
   INTO
      tmp_det_transf_tpo_registro         ,
      tmp_det_transf_id_servicio          ,
      tmp_det_transf_id_operacion         ,
      tmp_det_transf_nss                  ,
      tmp_det_transf_curp                 ,
      tmp_det_transf_nombre_trab_datamart ,
      tmp_det_transf_nombre_afore         ,
      tmp_det_transf_paterno_afore        ,
      tmp_det_transf_materno_afore        ,
      tmp_det_transf_sec_pension          ,
      tmp_det_transf_tpo_movimiento       ,
      tmp_det_transf_regimen              ,
      tmp_det_transf_tpo_retiro           ,
      tmp_det_transf_tpo_seguro           ,
      tmp_det_transf_tpo_pension          ,
      tmp_det_transf_tpo_prestacion       ,
      tmp_det_transf_f_inicio_pension     ,
      tmp_det_transf_f_emision_resol      ,
      tmp_det_transf_porc_valuacion       ,
      tmp_det_transf_sem_cotizadas        ,
      tmp_det_transf_f_carga_datamart     ,
      tmp_det_transf_diagnostico_reg      ,
      tmp_det_transf_estatus_subcta       ,
      tmp_det_transf_periodo_pago         ,
      tmp_det_transf_acciones_ret97       ,
      tmp_det_transf_acciones_cv          ,
      tmp_det_transf_acciones_cuotsol     ,
      tmp_det_transf_aiv97                ,
      tmp_det_transf_result_operacion     ,
      tmp_det_transf_cve_afore            ,
      tmp_det_transf_motivo_rech1         ,
      tmp_det_transf_motivo_rech2         ,
      tmp_det_transf_f_carga_infonavit    ,
      tmp_det_transf_f_transferencia      ,
      tmp_det_transf_monto_aivs           ,
      tmp_det_transf_monto_pesos          
   FROM
      safre_mig:tmp_det_transferencia
      
      -- se asume que no hay rechazos en el detalle del archivo
      LET v_b_rechazo_detalle    = 0;

      -- se obtiene el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente
        FROM afi_derechohabiente
       WHERE nss = tmp_det_transf_nss;    
      
      
      
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
         -- el id_transferencia se obtiene de la secuencia de retiros
         LET v_id_solicitud = 0;
      
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================

      ----TRACE("Validando registro de detalle");
      -- validando el registro
      DELETE FROM tmp_codigos_rechazo WHERE 1=1;
     
      LET v_indice_codigos_rechazo = 1;

      -- si no se encontro el id_derechohabiente
      IF ( v_id_derechohabiente IS NULL ) THEN
         LET v_id_derechohabiente = 0;
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
       
      -- se obtiene el id_matriz_derecho para la combinacion del registro
      SELECT id_ret_matriz_derecho
        INTO v_id_ret_matriz_derecho
        FROM ret_matriz_derecho
       WHERE tpo_retiro      = tmp_det_transf_tpo_retiro
         AND regimen         = tmp_det_transf_regimen
         AND tpo_seguro      = tmp_det_transf_tpo_seguro
         AND tpo_pension     = tmp_det_transf_tpo_pension
         AND tpo_prestacion  = tmp_det_transf_tpo_prestacion;
         
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
      -- Agosto 22, 2102. Clave de afore no se valida en carga inicial de transferencia
      --SELECT afore_cod
      --  INTO v_afore_cod
      --  FROM cat_afore
      -- WHERE afore_cod = tmp_det_transf_cve_afore;
      --   
      --IF ( v_afore_cod IS NULL ) THEN
      --   -- se rechaza
      --   LET v_b_rechazo_detalle    = 1;
      --   INSERT INTO tmp_codigos_rechazo VALUES (v_indice_codigos_rechazo, v_error_det_afore_invalido);
      --   --se asigan un cero por que la clave de afore no e admite nula 
      --   LET  tmp_det_transf_cve_afore = 0 ;
      --   -- se incrementa el indice
      --   LET v_indice_codigos_rechazo = v_indice_codigos_rechazo + 1;
      --
      --END IF

    
      -- si el registro se rechaza
      IF ( v_b_rechazo_detalle = 1 ) THEN
      
      
         --TRACE  ("v_b_rechazo_detalle   "||v_b_rechazo_detalle);
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
         FROM tmp_codigos_rechazo
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
         
            --TRACE ("RECHAZO 1= " ||v_motivo_rechazo_1  );
            --TRACE ("RECHAZO 2= " ||v_motivo_rechazo_2  );
            --TRACE ("RECHAZO 3= " ||v_motivo_rechazo_3  );
               
         -- ==============================================================
         -- ==============================================================
         
         
         -- le falta el ID de transferencia o de solicitud
         
         
         
         -- ==============================================================
         -- ==============================================================
       
         LET ret_transf_rch_id_derechohabiente    = v_id_derechohabiente; -- decimal(9,0) ;
         LET ret_transf_rch_folio                 = p_folio; -- decimal(9,0) ;
         LET ret_transf_rch_id_ret_matriz_derecho = v_id_ret_matriz_derecho; -- smallint     ;
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
         IF( tmp_det_transf_cve_afore IS NULL )THEN 
         	 --se asigan un cero por que la clave de afore no e admite nula 
           LET  tmp_det_transf_cve_afore = 0 ;
         END IF 
         LET ret_transf_rch_cve_afore             = tmp_det_transf_cve_afore; -- smallint     ;
         LET ret_transf_rch_cod_rechazo           = 0; -- cual es?
         LET ret_transf_rch_cod_rechazo_1         = v_motivo_rechazo_1; -- smallint     ;
         LET ret_transf_rch_cod_rechazo_2         = v_motivo_rechazo_2; -- smallint     ;
         LET ret_transf_rch_cod_rechazo_3         = v_motivo_rechazo_3; -- smallint     ;
         
            
         IF (ret_transf_rch_id_derechohabiente  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_id_derechohabiente" ||ret_transf_rch_id_derechohabiente  );                        
            LET ret_transf_rch_id_derechohabiente =  00;
         END IF 
         
         
          IF (ret_transf_rch_folio  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_id_derechohabiente" ||ret_transf_rch_id_derechohabiente  );                        
            LET ret_transf_rch_id_derechohabiente =  00;
         END IF 
         
         
          IF (ret_transf_rch_sec_pension  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_sec_pension" ||ret_transf_rch_sec_pension  );                        
            LET ret_transf_rch_sec_pension =  00;
         END IF 
         
         
          IF (ret_transf_rch_diag_registro  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_diag_registro" ||ret_transf_rch_diag_registro  );                        
            LET ret_transf_rch_diag_registro =  00;
         END IF 
         
         
          IF (ret_transf_rch_f_inicio_pension  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_f_inicio_pension" ||ret_transf_rch_f_inicio_pension  );                        
            LET ret_transf_rch_f_inicio_pension =  00;
         END IF 
         
          IF (ret_transf_rch_estado_sub_viv  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_estado_sub_viv" ||ret_transf_rch_estado_sub_viv  );                        
            LET ret_transf_rch_estado_sub_viv =  00;
         END IF 
            
            
          IF (ret_transf_rch_cve_afore  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_cve_afore" ||ret_transf_rch_cve_afore  );                        
            LET ret_transf_rch_cve_afore =  00;
         END IF 
         
          IF (ret_transf_rch_cod_rechazo  IS NULL) THEN      	
         	  --TRACE ("ret_transf_rch_cod_rechazo" ||ret_transf_rch_cod_rechazo  );                        
            LET ret_transf_rch_cod_rechazo =  00;
         END IF 

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

         -- se cuenta un registro de detalle rechazado
         LET v_reg_det_rechazados  = v_reg_det_rechazados + 1; -- total de registros de detalle rechazados

         -- si fue rechazado no se inserta en el historico
         CONTINUE FOREACH;
      END IF

      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      
      ----TRACE("Asignando datos a registro de detalle transferencia");
      
      LET ret_transf_id_solicitud          = v_id_solicitud; 
      LET ret_transf_id_derechohabiente    = v_id_derechohabiente;      
      LET ret_transf_id_ret_matriz_derecho = v_id_ret_matriz_derecho; -- revisar cual es
      LET ret_transf_sec_pension           = tmp_det_transf_sec_pension;
      LET ret_transf_diag_registro         = tmp_det_transf_diagnostico_reg;
      LET ret_transf_folio                 = p_folio;
      LET ret_transf_estado_solicitud      = 30; -- integrada/recibida procesar
      LET ret_transf_curp                  = tmp_det_transf_curp; 
      LET ret_transf_nombre_datamart       = tmp_det_transf_nombre_trab_datamart;
      LET ret_transf_nombre_afore          = tmp_det_transf_nombre_afore;
      LET ret_transf_paterno_afore         = tmp_det_transf_paterno_afore;
      LET ret_transf_materno_afore         = tmp_det_transf_materno_afore;
      LET ret_transf_tpo_movimiento        = tmp_det_transf_tpo_movimiento;
      LET ret_transf_f_inicio_pension      = tmp_det_transf_f_inicio_pension; 
      LET ret_transf_f_resolucion          = tmp_det_transf_f_emision_resol;
      LET ret_transf_porcentaje_valuacion  = tmp_det_transf_porc_valuacion / 100; -- DECIMAL(5,2) ;
      LET ret_transf_semanas_cotizadas     = tmp_det_transf_sem_cotizadas;
      LET ret_transf_f_carga_datamart      = tmp_det_transf_f_carga_datamart;
      LET ret_transf_estado_sub_viv        = tmp_det_transf_estatus_subcta;
      LET ret_transf_aivs_viv97            = tmp_det_transf_aiv97 / 1000000; -- DECIMAL(14,6);
      LET ret_transf_cve_afore             = tmp_det_transf_cve_afore;
      LET ret_transf_cod_rechazo           = 0; -- verificar cual es
      
                           
                           
     IF (ret_transf_id_derechohabiente  IS NULL) THEN      	
     	  --TRACE ("ret_transf_id_derechohabiente" ||ret_transf_id_derechohabiente  );                        
        LET ret_transf_id_derechohabiente =  00;
     END IF 

     IF (ret_transf_sec_pension  IS NULL) THEN 
     	
     	--TRACE ("ret_transf_sec_pension" ||ret_transf_sec_pension  );
        LET ret_transf_sec_pension =  00;
     END IF 
                        
                        
     IF (ret_transf_diag_registro  IS NULL) THEN 
     	--TRACE ("ret_transf_diag_registro" ||ret_transf_diag_registro  );
        LET ret_transf_diag_registro =  00 ;
     END IF 
                        
                        
     IF (ret_transf_folio  IS NULL) THEN 
     	--TRACE ("ret_transf_folio" ||ret_transf_folio  );
        LET ret_transf_folio =  00 ;
     END IF 


     IF (ret_transf_estado_solicitud  IS NULL) THEN 
     	--TRACE ("ret_transf_estado_solicitud" ||ret_transf_estado_solicitud  );
        LET ret_transf_estado_solicitud =  00 ;
     END IF 
                        
     IF (ret_transf_f_inicio_pension  IS NULL) THEN 
     	--TRACE ("ret_transf_f_inicio_pension" ||ret_transf_f_inicio_pension  );
        LET ret_transf_f_inicio_pension = TODAY  ;
     END IF 
                        
     IF (ret_transf_estado_sub_viv  IS NULL) THEN 
     	--TRACE ("ret_transf_estado_sub_viv" ||ret_transf_estado_sub_viv  );
        LET ret_transf_estado_sub_viv  =  00 ;
     END IF 

     IF (ret_transf_cve_afore  IS NULL) THEN 
     	--TRACE ("ret_transf_cve_afore" ||ret_transf_cve_afore  );
        LET ret_transf_cve_afore  =  00 ;
     END IF 
     
     IF (ret_transf_cod_rechazo  IS NULL) THEN 
     	--TRACE ("ret_transf_cod_rechazo" ||ret_transf_cod_rechazo  );
        LET ret_transf_cod_rechazo  =  00 ;
     END IF 


      
      
      -- ==========================================================================
      -- ==========================================================================
      -- ==========================================================================
      -- calculo de las AIVs 97 a pesos
      ----TRACE("Asignando AIVs a pesos");
           
      ----TRACE("multiplicando AIVs por valor del fondo");
      --LET v_pesos_aiv97 = ret_transf_aivs_viv97 * v_valor_fondo;

      -- incremento del importe total
      --TRACE("acumulando importe total");
      --LET v_sumario_importe_total = v_sumario_importe_total + v_pesos_aiv97;
    
      -- se inserta en la tabla historia de detalle de retiro por disposicion de recursos
      INSERT INTO safre_viv:ret_transferencia (
         id_solicitud      ,
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
      )
      VALUES (
         seq_ret_solicitud.NEXTVAL        ,
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
      );
      
      -- se cuenta un registro insertado
      LET v_reg_det_insertados  = v_reg_det_insertados + 1; -- total de registros de detalle insertados

      -- se marca la cuenta
      LET v_i_estado_marca = 0;
      {
      EXECUTE FUNCTION fn_marca_cuenta(
              ret_transf_id_derechohabiente
             ,v_marca_transferencia -- marca de transferencia
             ,seq_ret_solicitud.CURRVAL  
             ,ret_transf_folio
             ,0 -- estado marca
             ,0 -- codigo de rechazo
             ,0 -- marca de la causa
             ,NULL -- fecha de la causa
             ,p_usuario_cod)
         INTO v_i_estado_marca;
}
   END FOREACH;
 
 

   -- se actualiza el importe total para el registro del encabezado
   UPDATE safre_viv:ret_cza_transferencia
   SET    total_importe = v_sumario_importe_total
   WHERE  folio = p_folio;
 
   --TRACE("UPDATE STATISTICS FOR TABLE ret_transferencia");
   -- se actualizan las estadisticas de la tabla
   UPDATE STATISTICS FOR TABLE ret_transferencia;
 
   -- si hubo rechazos en la integracion, el lote se rechaza completo
   IF ( v_reg_det_rechazados > 0 OR v_b_rechazo_encabezado > 0 ) THEN
      LET v_estatus_proceso = 100; -- rechazado
      
      -- se desmarcan todas las cuentas porque el archivo sera rechazado
      FOREACH
      SELECT
         id_solicitud   ,
         id_derechohabiente    
      INTO
         v_id_solicitud             ,
         ret_transf_id_derechohabiente    
      FROM ret_transferencia
      WHERE folio = p_folio
  
           -- se desmarca la cuenta en cuestion
         -- se marca la cuenta
         LET v_i_estado_marca = 0;
         EXECUTE FUNCTION fn_desmarca_cuenta(
                 ret_transf_id_derechohabiente
                ,v_marca_transferencia -- marca de transferencia
                ,v_id_solicitud -- identificador de registro de archivo o lote
                ,0 -- estado marca
                ,0 -- marca de la causa
                ,p_usuario_cod)
            INTO v_i_estado_marca;
            
      END FOREACH;
      
      --TRACE("en el ifUPDATE STATISTICS FOR TABLE ret_transferencia");
  
      UPDATE STATISTICS FOR TABLE safre_viv:ret_transferencia;
       
      -- se borran los registros de la tabla de detalle
      DELETE FROM safre_viv:ret_transferencia
      WHERE  folio = p_folio;
      
      -- se borran los registros del encabezado
      DELETE FROM safre_viv:ret_cza_transferencia
      WHERE  folio = p_folio;

   END IF
 
   -- ======================================================
   -- se crea el registro de encabezado de transferencia
   LET ret_cza_transferencia_folio                 = p_folio; -- decimal(9,0) not null ,
   LET ret_cza_transferencia_nombre_archivo        = p_nombre_archivo; -- char(20),
   LET ret_cza_transferencia_f_operacion_procesar  = TODAY; -- date,
   LET ret_cza_transferencia_f_carga               = TODAY; -- date,
   LET ret_cza_transferencia_h_carga               = CURRENT HOUR TO MINUTE; -- datetime hour to minute,
   LET ret_cza_transferencia_f_valor_transferencia = TODAY; -- date,
   LET ret_cza_transferencia_precio_fondo          = 0; -- decimal(14,6),
   LET ret_cza_transferencia_total_registros       = 0; -- integer,
   LET ret_cza_transferencia_total_importe         = 0; -- decimal(22,2),
   LET ret_cza_transferencia_usuario               = p_usuario_cod; -- char(20)

   -- se obtiene el numero de registros
   SELECT COUNT(*)
   INTO ret_cza_transferencia_total_registros
   FROM ret_transferencia
   WHERE folio = p_folio;
   
   INSERT INTO ret_cza_transferencia (
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
      ret_cza_transferencia_folio                 ,
      ret_cza_transferencia_nombre_archivo        ,
      ret_cza_transferencia_f_operacion_procesar  ,
      ret_cza_transferencia_f_carga               ,
      ret_cza_transferencia_h_carga               ,
      ret_cza_transferencia_f_valor_transferencia ,
      ret_cza_transferencia_precio_fondo          ,
      ret_cza_transferencia_total_registros       ,
      ret_cza_transferencia_total_importe         ,
      ret_cza_transferencia_usuario               
   );

 
   -- se devuelve el resultado de la ejecucion del proceso
   RETURN v_si_resultado, isam_err, v_c_msj;

END FUNCTION
;


