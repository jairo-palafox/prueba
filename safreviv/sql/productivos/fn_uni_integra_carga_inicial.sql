






CREATE FUNCTION "safreviv".fn_uni_integra_carga_inicial(p_usuario_cod    CHAR(20), 
                                             p_proceso_cod    SMALLINT, 
                                             p_nombre_archivo CHAR(18),
                                             p_folio          DECIMAL(10), 
                                             p_pid            DECIMAL(9,0)) 
  RETURNING INTEGER, 
            INTEGER, 
            VARCHAR(250)

-- Variables utilizadas para el encabezado UNI solo IMSS
DEFINE v_enc_tpo_registro                    CHAR(2);
DEFINE v_enc_ide_servicio                    CHAR(2);
DEFINE v_enc_ide_operacion                   CHAR(2);
DEFINE v_enc_fec_presentacion                CHAR(8);
DEFINE v_enc_date_presentacion               DATE;
DEFINE v_enc_consec_lote_dia                 DECIMAL(3,0);
DEFINE v_enc_resultado_operacion             CHAR(2);

-- Variables utilizadas para el detalle unificadora UNI solo IMSS
DEFINE v_det_tipo_registro                   CHAR(2);
DEFINE v_det_contador_servicio               DECIMAL(9,0);
DEFINE v_det_evento                          CHAR(2);
DEFINE v_det_curp_unificador                 CHAR(18);
DEFINE v_det_nss_unificador                  CHAR(11);
DEFINE v_det_rfc_unificador                  CHAR(13);
DEFINE v_det_paterno_unificador              CHAR(40);
DEFINE v_det_materno_unificador              CHAR(40);
DEFINE v_det_nombre_unificador               CHAR(40);
DEFINE v_det_nombre_imssunificador           CHAR(50);
DEFINE v_det_indicador_credito43bis          CHAR(1);
DEFINE v_det_diagnostico_unificacion         CHAR(2);
DEFINE v_det_fec_diagnostico                 CHAR(8);
DEFINE v_det_date_diagnostico                DATE;
DEFINE v_det_codigo_resultado                CHAR(2);
DEFINE v_det_motivo_rechazo                  CHAR(3);

-- Variables utilizadas para el detalle unificadas UNI solo IMSS
DEFINE v_det2_tpo_registro                   CHAR(2);                    
DEFINE v_det2_contador_servicio              DECIMAL(9,0);
DEFINE v_det2_evento                         CHAR(2);
DEFINE v_det2_nss_unificador                 CHAR(11);
DEFINE v_det2_nss_unificado                  CHAR(11);
DEFINE v_det2_curp_unificado                 CHAR(18);
DEFINE v_det2_rfc_unificado                  CHAR(13);
DEFINE v_det2_app_unificado                  CHAR(40);
DEFINE v_det2_apm_unificado                  CHAR(40);
DEFINE v_det2_nom_unificado                  CHAR(40);
DEFINE v_det2_nom_segun_imss                 CHAR(50);
DEFINE v_det2_ide_credito_infonavit          CHAR(1);
DEFINE v_det2_codigo_resultado               CHAR(2);
DEFINE v_det2_motivo_rechazo                 CHAR(2);

-- Variables utilizadas para el sumario unificadas UNI solo IMSS
DEFINE v_sum_tipo_registro                   CHAR(2);
DEFINE v_sum_total_registro                  DECIMAL(9,0);
DEFINE v_sum_total_nss_unificador            DECIMAL(9,0);
DEFINE v_sum_total_nss_unificados            DECIMAL(9,0);

-- Variables que se retornan
DEFINE v_i_resultado                         INTEGER;
DEFINE v_si_numero_solicitudes_totales       INTEGER;
DEFINE v_si_solicitudes_aceptadas_unificador INTEGER;
DEFINE v_si_solicitudes_aceptadas_unificadas INTEGER;

-- Control de Excepciones
DEFINE sql_err                               INTEGER;
DEFINE isam_err                              INTEGER;
DEFINE err_txt                               CHAR(200);
--                                           
DEFINE v_d_id_referencia                     DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador       DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado        DECIMAL(9,0);
DEFINE v_diagnostico_unificador              SMALLINT;
DEFINE v_diagnostico_unificadas              SMALLINT;
DEFINE v_diagnostico_rechazo                 SMALLINT;
DEFINE v_estado_familia                      SMALLINT;
DEFINE v_estado_familia_unificador           SMALLINT;
DEFINE v_estado_familia_unificado            SMALLINT;

   ON EXCEPTION SET sql_err, isam_err, err_txt
         LET v_i_resultado                         = sql_err;
         LET v_si_numero_solicitudes_totales       = 0;
         LET v_si_solicitudes_aceptadas_unificador = 0;
         LET v_si_solicitudes_aceptadas_unificadas = 0;
         
         RETURN v_i_resultado, isam_err, err_txt;
   END EXCEPTION
   
   -- Variables que almacenan informacion para su validacion
   LET v_i_resultado                         = 0;
   LET v_si_numero_solicitudes_totales       = 0;
   LET v_si_solicitudes_aceptadas_unificador = 0;
   LET v_si_solicitudes_aceptadas_unificadas = 0;
   LET v_d_id_referencia                     = 0;
   
   LET v_diagnostico_unificador              = 0;
   LET v_diagnostico_unificadas              = 0;
   LET v_diagnostico_rechazo                 = 0;
   LET v_estado_familia                      = 0;
   LET v_estado_familia_unificador           = 0;
   LET v_estado_familia_unificado            = 0;
   LET sql_err                               = NULL;
   LET isam_err                              = NULL;
   LET err_txt                               = NULL;
   LET v_estado_familia                      = 0;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni_integra_carga_inicial.txt";
   --TRACE ON;
    
   LET err_txt = "Al recuperar datos detalle tmp_enc_cta_unificar_op21";
   FOREACH
      SELECT tpo_registro,
             ide_servicio,
             ide_operacion,
             to_date(fec_presentacion,'%Y%m%d'),
             consec_lote_dia,
             resultado_operacion
        INTO v_enc_tpo_registro,
             v_enc_ide_servicio,
             v_enc_ide_operacion,
             v_enc_date_presentacion,
             v_enc_consec_lote_dia,
             v_enc_resultado_operacion
        FROM safre_mig:tmp_enc_unificacion
          
      -- Acumula la referencia
      LET v_d_id_referencia = v_d_id_referencia + 1;
      
      LET err_txt = "Valida tipo de registro para el registro inicial 01";
      
      IF (v_enc_tpo_registro <> "01" OR v_enc_tpo_registro IS NULL) THEN
         -- ERROR de encabezado.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  1,
                                                  1,
                                                  v_d_id_referencia,
                                                  "No",
                                                  10,
                                                  v_enc_tpo_registro);
      END IF
      
      LET err_txt = "Al insertar encabezado en uni_cza_unificacion";
      
      INSERT INTO uni_cza_unificacion(id_czaunifica,
                                      folio_unificacion,
                                      tipo_registro,
                                      servicio,
                                      operacion,
                                      tipo_entidad_origen,
                                      clave_entidad_origen,
                                      tipo_entidad_destino,
                                      clave_entidad_destino,
                                      f_presentacion,
                                      consecutivo_dia,
                                      resultado_operacion,
                                      periodo_unificacion)
             VALUES(seq_uni_cza_unificacion.NEXTVAL,  --id_czaunifica,
                    p_folio,
                    v_enc_tpo_registro,
                    v_enc_ide_servicio,
                    v_enc_ide_operacion,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    v_enc_date_presentacion,
                    v_enc_consec_lote_dia,
                    v_enc_resultado_operacion,
                    NULL);
   END FOREACH;
---------------------------CTAS UNIFICADOR----------------------------- 
   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   
   FOREACH
      SELECT tpo_registro,                    
             contador_servicio,               
             evento,
             nss_unificador,
             curp_unificadora,                
             rfc_unificador,                  
             app_unificador,                  
             apm_unificador,                  
             nombre_unificador,               
             nombre_segun_imss,
             ide_credito_infonavit,
             diagnostico_unificacion,
             to_date(fecha_diagnostico,'%Y%m%d')
        INTO v_det_tipo_registro,           
             v_det_contador_servicio,
             v_det_evento,
             v_det_nss_unificador,
             v_det_curp_unificador,         
             v_det_rfc_unificador,          
             v_det_paterno_unificador,      
             v_det_materno_unificador,      
             v_det_nombre_unificador,       
             v_det_nombre_imssunificador,
             v_det_indicador_credito43bis,
             v_det_diagnostico_unificacion,         
             v_det_date_diagnostico
        FROM tmp_det_unificador
          
      -- Acumula la referencia
      LET v_d_id_referencia           = v_d_id_referencia + 1;
      LET v_diagnostico_unificador    = 1; --Aceptada
      LET v_diagnostico_rechazo       = 1;
      LET v_estado_familia_unificador = 1;
      
      LET err_txt = "Valida tipo de registro para el registro inicial 02";
      
      IF (v_det_tipo_registro <> "02" OR v_det_tipo_registro IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_d_id_referencia,
                                                  "No",
                                                  12,
                                                  v_det_tipo_registro);
         LET v_diagnostico_unificador    = 2; --Rechazada
         LET v_diagnostico_rechazo       = 12;
         LET v_estado_familia_unificador =  2;
      END IF
      
      --Recupera el id_derechohabiente
      SELECT id_derechohabiente
        INTO v_id_derechohabiente_unificador
        FROM afi_derechohabiente
       WHERE nss = v_det_nss_unificador;
      
      -- Valida el id_derechohabiente
      LET err_txt = "En id_derechohabiente, no existe";
      
      IF(v_id_derechohabiente_unificador IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_d_id_referencia,
                                                  "No",
                                                  13,
                                                  v_det_nss_unificador);
         LET v_diagnostico_unificador    = 2; --Rechazada   
         LET v_diagnostico_rechazo       = 13;
         LET v_estado_familia_unificador =  2;
      END IF
      
      --"Al insertar encabezado en uni_det_unificador";
      LET err_txt = "Al insertar encabezado en uni_det_unificador";
      
      INSERT INTO uni_det_unificador(id_unificador,
                                     folio_unificacion,
                                     id_derechohabiente,
                                     tipo_registro,
                                     contador_servicio,
                                     tipo_entidad_solicitante,
                                     cve_entidad_solicitante,
                                     tipo_entidad_unificador,
                                     clave_entidad_unificador,
                                     curp_unificador,
                                     nss_unificador,
                                     rfc_unificador,
                                     paterno_unificador,
                                     materno_unificador,
                                     nombre_unificador,
                                     nombre_imssunificador,
                                     sexo_unificador,
                                     entidad_nacunificador,
                                     f_nac_unificador,
                                     tpo_docto_probatorio,
                                     clave_afore_receptora,
                                     numero_cuentasasoc,
                                     estatus_convocatoria,
                                     resultado_operacion,
                                     ident_movimiento,
                                     estatus_traspaso,
                                     estatus_retiro,
                                     cve_afore_aclaracion,
                                     ind_credito43bis,
                                     estado_familia,
                                     estado_unificacion,
                                     diagnostico,
                                     f_liquidacion,
                                     f_notificacion,
                                     f_aplicacion, 
                                     folio_liquidacion)
             VALUES(seq_uni_det_unificador.NEXTVAL, --id_unificador
                    p_folio,                                  
                    v_id_derechohabiente_unificador,                      
                    v_det_tipo_registro,          
                    v_det_contador_servicio,      
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    v_det_curp_unificador,        
                    v_det_nss_unificador,         
                    v_det_rfc_unificador,         
                    v_det_paterno_unificador,     
                    v_det_materno_unificador,     
                    v_det_nombre_unificador,      
                    v_det_nombre_imssunificador,  
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    NULL,                         
                    v_det_indicador_credito43bis, 
                    v_estado_familia_unificador,       
                    v_diagnostico_unificador,
                    v_diagnostico_rechazo,         
                    v_det_date_diagnostico,                         
                    NULL,
                    NULL,       
                    NULL);

---------------------------CTAS UNIFICADAS----------------------------- 
      --"Al recuperar datos detalle tmp_det_cta_unificadas_op21";
      LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadas_op21";

      FOREACH
         SELECT tpo_registro,                    
                contador_servicio,
                evento,
                nss_unificador,
                nss_unificado,
                curp_unificado,
                rfc_unificado,
                app_unificado,
                apm_unificado,
                nom_unificado,
                nom_segun_imss,
                ide_credito_infonavit
           INTO v_det2_tpo_registro,
                v_det2_contador_servicio,
                v_det2_evento,
                v_det2_nss_unificador,
                v_det2_nss_unificado,
                v_det2_curp_unificado,
                v_det2_rfc_unificado,
                v_det2_app_unificado,
                v_det2_apm_unificado,
                v_det2_nom_unificado,
                v_det2_nom_segun_imss,
                v_det2_ide_credito_infonavit
           FROM tmp_det_unificado
          WHERE nss_unificador = v_det_nss_unificador
             
         -- Acumula la referencia
         LET v_d_id_referencia          = v_d_id_referencia + 1;
         LET v_diagnostico_unificadas   = 1; --Aceptadas
         LET v_diagnostico_rechazo      = 1;
         LET v_estado_familia_unificado = 1;
         
         --"Valida tipo de registro para el registro inicial 03";
         LET err_txt = "Valida tipo de registro para el registro inicial 03";
         
         IF (v_det2_tpo_registro <> "03" OR v_det2_tpo_registro IS NULL) THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_d_id_referencia,
                                                     "No",
                                                     14,
                                                     v_det2_tpo_registro);
            LET v_diagnostico_unificadas   = 2; --Rechazada
            LET v_diagnostico_rechazo      = 14;
            LET v_estado_familia_unificado =  2;
         END IF
         
         --"Recupera el id derechohabiente unificado";
         LET err_txt = "Recupera el id derechohabiente unificado";
         
         SELECT id_derechohabiente
           INTO v_id_derechohabiente_unificado
           FROM afi_derechohabiente
          WHERE nss = v_det2_nss_unificado;
         
         -- Valida el id_derechohabiente
         LET err_txt = "En id_derechohabiente, no existe en unificado";
         
         IF(v_id_derechohabiente_unificado IS NULL)THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_d_id_referencia,
                                                     "No",
                                                     15,
                                                     v_det2_nss_unificado);
            LET v_diagnostico_unificadas   = 2; --Rechazada
            LET v_diagnostico_rechazo      = 15;
            LET v_estado_familia_unificado =  2;
         END IF

         -- "Al insertar encabezado en uni_det_unificado";
         LET err_txt = "Al insertar encabezado en uni_det_unificado";
         
         INSERT INTO uni_det_unificado(id_unificado,
                                       id_unificador,
                                       folio_unificacion,
                                       id_derechohabiente,
                                       tipo_registro,
                                       contador_serviciocta1,
                                       tipo_entidadcta1,
                                       cve_entidadcta1,
                                       curpcta1,
                                       nsscta1,
                                       rfccta1,
                                       paternocta1,
                                       maternocta1,
                                       nombrecta1,
                                       nombre_imsscta1,
                                       sexocta1,
                                       ent_nacimiento,
                                       f_nacimientocta1,
                                       estatus_traspasocta1,
                                       estatus_retirocta1,
                                       estatus_convocatoria,
                                       nsscta2,
                                       diagnostico_uni,
                                       resultado_operacion,
                                       afore_aclaracion,
                                       credito43bis,
                                       estado_unificacion,
                                       diagnostico)
                VALUES(seq_uni_det_unificado.NEXTVAL, --id_unificado
                       seq_uni_det_unificador.CURRVAL, --id_unificador
                       p_folio,                                  
                       v_id_derechohabiente_unificado,
                       v_det2_tpo_registro,         
                       v_det2_contador_servicio,    
                       NULL,                        
                       NULL,                        
                       v_det2_curp_unificado,       
                       v_det2_nss_unificado,        
                       v_det2_rfc_unificado,        
                       v_det2_app_unificado,        
                       v_det2_apm_unificado,        
                       v_det2_nom_unificado,        
                       v_det2_nom_segun_imss,       
                       NULL,                        
                       NULL,                        
                       NULL,                        
                       NULL,                        
                       NULL,                        
                       NULL,                        
                       v_det2_nss_unificador,       
                       NULL,                        
                       NULL,                        
                       NULL,                        
                       v_det2_ide_credito_infonavit,
                       v_diagnostico_unificadas,                        
                       v_diagnostico_rechazo);
         
         -- "Valida para marcar la cuenta unificado";
         LET err_txt = "Valida para marcar la cuenta unificado";
         
         IF v_diagnostico_unificadas = 1 AND v_estado_familia_unificado = 1 THEN
            -- "Al marcar unificación imss unificado";

            INSERT INTO sfr_marca_historica
            VALUES (v_id_derechohabiente_unificado,  --- id_derechohabiente
                    502,                             --- marca             
                    seq_uni_det_unificado.CURRVAL,   --- n_referencia      
                    v_enc_date_presentacion,         --- f_inicio          
                    "00:00:00",                      --- h_inicio          
                    NULL,                            --- f_fin
                    p_folio,                         --- folio             
                    2310,                            --- proceso_marca
                    null,                            --- proceso_desmarca
                    0,                               --- estado_marca      
                    0,                               --- rch_cod           
                    0,                               --- marca_causa
                    null,                            --- f_marca_causa
                    null,                            --- f_vigencia        
                    p_usuario_cod,                   --- usuario_marca     
                    p_proceso_cod                    --- usuario_desmarca
                   );
                    
            INSERT INTO sfr_marca_activa
            VALUES (v_id_derechohabiente_unificado,  --- id_derechohabiente
                    502,                             --- marca             
                    seq_uni_det_unificado.CURRVAL,   --- n_referencia      
                    v_enc_date_presentacion,         --- f_inicio          
                    "00:00:00",                      --- h_inicio          
                    p_folio,                         --- folio             
                    2310,                            --- proceso_marca     
                    0,                               --- marca_causa       
                    null,                            --- f_marca_causa     
                    null,                            --- f_vigencia        
                    p_usuario_cod                    --- usuario_marca     
                   );
                    
            INSERT INTO uni_pre_unificado
            VALUES (seq_uni_det_unificado.CURRVAL,   --- id_pre_unificado  
                    seq_uni_det_unificador.CURRVAL,  --- id_pre_unificador 
                    v_id_derechohabiente_unificado,  --- id_derechohabiente
                    seq_uni_det_unificado.CURRVAL,   --- id_unificado      
                    v_det2_nss_unificado,            --- nss               
                    v_det2_nom_segun_imss,           --- nombre            
                    1,                               --- estado            
                    1                                --- diagnostico       
                   );
         END IF           
      END FOREACH;
         
      -- "Valida para marcar la cuenta unificador";
      LET err_txt = "Valida para marcar la cuenta unificador";
      
      IF v_diagnostico_unificador = 1 AND v_estado_familia_unificador = 1 THEN

         -- "Al marcar unificación imss unificador";
                        
         INSERT INTO sfr_marca_historica
         VALUES (v_id_derechohabiente_unificador, --- id_derechohabiente
                 501,                             --- marca             
                 seq_uni_det_unificador.CURRVAL,  --- n_referencia      
                 v_enc_date_presentacion,         --- f_inicio          
                 "00:00:00",                      --- h_inicio          
                 NULL,                            --- f_fin
                 p_folio,                         --- folio             
                 2310,                            --- proceso_marca
                 null,                            --- proceso_desmarca
                 0,                               --- estado_marca      
                 0,                               --- rch_cod           
                 0,                               --- marca_causa
                 null,                            --- f_marca_causa
                 null,                            --- f_vigencia        
                 p_usuario_cod,                   --- usuario_marca     
                 p_proceso_cod                    --- usuario_desmarca
                );
                
         INSERT INTO sfr_marca_activa
         VALUES (v_id_derechohabiente_unificador, --- id_derechohabiente
                 501,                             --- marca             
                 seq_uni_det_unificador.CURRVAL,  --- n_referencia      
                 v_enc_date_presentacion,         --- f_inicio          
                 "00:00:00",                      --- h_inicio          
                 p_folio,                         --- folio             
                 2310,                            --- proceso_marca     
                 0,                               --- marca_causa       
                 null,                            --- f_marca_causa     
                 null,                            --- f_vigencia        
                 p_usuario_cod                    --- usuario_marca     
                );

         INSERT INTO uni_pre_unificador
         VALUES (seq_uni_det_unificador.CURRVAL,  --- id_pre_unificador 
                 p_folio,                         --- folio_lote        
                 v_id_derechohabiente_unificador, --- id_derechohabiente
                 NULL,                            --- nrp
                 NULL,                            --- tipo_trabajador
                 v_det_nss_unificador,            --- nss_correcto      
                 v_det_nombre_imssunificador,     --- nombre_correcto   
                 1,                               --- estado_familia    
                 1,                               --- estado            
                 1,                               --- diagnostico       
                 v_det_date_diagnostico,          --- f_proceso         
                 TODAY                            --- f_movimiento      
                );
      END IF
         
      -- Conteo de solicitudes unificador
      IF v_diagnostico_unificador = 1 THEN
         LET v_si_solicitudes_aceptadas_unificador = v_si_solicitudes_aceptadas_unificador + 1;
      END IF
      
      IF v_diagnostico_unificadas = 1 THEN
         LET v_si_solicitudes_aceptadas_unificadas = v_si_solicitudes_aceptadas_unificadas + 1;
      END IF
      
      IF v_estado_familia_unificador = 1 AND v_estado_familia_unificado = 1 THEN
         LET v_estado_familia = 1;
      ELSE
         LET v_estado_familia = 2;
      END IF
                  
   END FOREACH;
   
   -- "Al recuperar datos detalle tmp_sum_cta_unificar_op21";
   LET err_txt = "Al recuperar datos detalle tmp_sum_cta_unificar_op21";
   
   FOREACH
      SELECT tpo_registro,
             cant_registro_detalle_02,
             cant_registro_detalle_03
        INTO v_sum_tipo_registro,
             v_sum_total_nss_unificador,
             v_sum_total_nss_unificados
        FROM safre_mig:tmp_sum_unificacion
          
      -- Acumula la referencia
      LET v_d_id_referencia = v_d_id_referencia + 1;
      
      -- "Valida tipo de registro para el registro inicial 09";
      LET err_txt = "Valida tipo de registro 09";
      
      IF (v_sum_tipo_registro <> "09" OR v_sum_tipo_registro IS NULL) THEN
         -- ERROR de encabezado.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  9,
                                                  9,
                                                  v_d_id_referencia,
                                                  "No",
                                                  16,
                                                  v_sum_tipo_registro);
      END IF

      -- "Al insertar encabezado en uni_sum_unificacion";
      LET err_txt = "Al insertar encabezado en uni_sum_unificacion";
      INSERT INTO uni_sum_unificacion(id_sum_unifica,
                                                folio_unificacion,
                                                tipo_registro,
                                                total_registro,
                                                total_nss_unificador,
                                                total_nss_unificados)
             VALUES(seq_uni_sum_unificacion.NEXTVAL,  --id_sum_unifica,
                    p_folio,
                    v_sum_tipo_registro,
                    NULL,
                    v_sum_total_nss_unificador,
                    v_sum_total_nss_unificados);
   
      LET v_sum_total_registro = v_sum_total_nss_unificador + v_sum_total_nss_unificados;
   END FOREACH;
    
   -- "Al actualizar glo_ctr_archivo";
   LET err_txt = "Al actualizar glo_ctr_archivo";
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE safre_mig:glo_ctr_archivo
      SET folio = p_folio, 
          estado = 2           
    WHERE proceso_cod    = p_proceso_cod
      AND opera_cod      = 1 -- archivo cargado
      AND estado         = 1; -- etapa de carga
   
   UPDATE safre_viv:bat_ctr_operacion 
      SET nom_archivo = p_nombre_archivo
    WHERE proceso_cod = p_proceso_cod
      AND opera_cod   = 2
      AND pid         = p_pid;      
          
   -- " Registros: "||v_sum_total_registro;
   LET err_txt = " Registros: "||v_sum_total_registro;
   
   UPDATE statistics FOR TABLE safre_viv:uni_cza_unificacion;
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificador;
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificado;
   UPDATE statistics FOR TABLE safre_viv:uni_sum_unificacion;
   UPDATE statistics FOR TABLE safre_viv:uni_pre_unificador;
   UPDATE statistics FOR TABLE safre_viv:uni_pre_unificado;
   
   RETURN v_i_resultado,
          isam_err,
          err_txt;
END FUNCTION
;


