






CREATE FUNCTION "safreviv".fn_uni_integra_imss(p_usuario_cod    CHAR(20),
                                    p_proceso_cod    SMALLINT, 
                                    p_nombre_archivo CHAR(40),
                                    p_folio          DECIMAL(9,0), 
                                    p_pid            DECIMAL(9,0)) 

  RETURNING INTEGER,
            CHAR(200),
            INTEGER,
            INTEGER

-- 27112014 PRODINF-561
-- 26082015 PRODINF-906

-- Variables utilizadas para el encabezado UNI solo IMSS
DEFINE v_enc_folio_unificacion               DECIMAL(9,0);
DEFINE v_enc_tpo_registro                    CHAR(2);
DEFINE v_enc_ide_servicio                    CHAR(2);
DEFINE v_enc_ide_operacion                   CHAR(2);
DEFINE v_enc_tpo_entidad_origen              CHAR(2);
DEFINE v_enc_cve_entidad_origen              CHAR(3);
DEFINE v_enc_tpo_entidad_destino             CHAR(2);
DEFINE v_enc_cve_entidad_destino             CHAR(3);
DEFINE v_enc_fec_presentacion                CHAR(8);
DEFINE v_enc_date_presentacion               DATE;
DEFINE v_enc_consec_lote_dia                 DECIMAL(3,0);
DEFINE v_enc_resultado_operacion             CHAR(2);
DEFINE v_enc_periodo_unificacion             CHAR(6);
-- Variables utilizadas para el detalle unificadora UNI solo IMSS
DEFINE v_det_folio_unificacion               DECIMAL(9,0);
DEFINE v_det_id_derechohabiente              DECIMAL(9,0);
DEFINE v_det_tipo_registro                   CHAR(2);
DEFINE v_det_contador_servicio               DECIMAL(9,0);
DEFINE v_det_tipo_entidad_solicitante        CHAR(2);
DEFINE v_det_cve_entidad_solicitante         CHAR(3);
DEFINE v_det_tipo_entidad_unificador         CHAR(2);
DEFINE v_det_clave_entidad_unificador        CHAR(3);
DEFINE v_det_curp_unificador                 CHAR(18);
DEFINE v_det_nss_unificador                  CHAR(11);
DEFINE v_det_rfc_unificador                  CHAR(13);
DEFINE v_det_paterno_unificador              CHAR(40);
DEFINE v_det_materno_unificador              CHAR(40);
DEFINE v_det_nombre_unificador               CHAR(40);
DEFINE v_det_nombre_imssunificador           CHAR(50);
DEFINE v_det_sexo_unificador                 CHAR(1);
DEFINE v_det_entidad_nacunificador           CHAR(2);
DEFINE v_det_fec_nac_unificador              CHAR(8);
DEFINE v_det_date_nac_unificador             DATE;
DEFINE v_det_tpo_docto_probatorio            CHAR(1);
DEFINE v_det_clave_afore_receptora           CHAR(3);
DEFINE v_det_numero_cuentasasoc              DECIMAL(2,0);
DEFINE v_det_estatus_convocatoria            CHAR(1);
DEFINE v_det_resultado_operacion             CHAR(2);
DEFINE v_det_ident_movimiento                CHAR(2);
DEFINE v_det_estatus_traspaso                CHAR(2);
DEFINE v_det_estatus_retiro                  CHAR(2);
DEFINE v_det_cve_afore_aclaracion            CHAR(3);
DEFINE v_det_indicador_credito43bis          CHAR(1);
DEFINE v_det_estado_unificacion              DECIMAL(2,0);
DEFINE v_det_diagnostico                     DECIMAL(3,0);
DEFINE v_det_fec_liquidacion                 CHAR(8);
DEFINE v_det_date_liquidacion                DATE;
DEFINE v_det_fec_notificacion                CHAR(8);
DEFINE v_det_date_notificacion               DATE;
DEFINE v_det_folio_liquidacion               DECIMAL(9,0);
-- Variables utilizadas para el detalle unificadas UNI solo IMSS
DEFINE v_det2_folio_unificacion              DECIMAL(9,0);
DEFINE v_det2_id_derechohabiente             DECIMAL(9,0);
DEFINE v_det2_tipo_registro                  CHAR(2);
DEFINE v_det2_contador_serviciocta1          DECIMAL(9,0);
DEFINE v_nss_unificador_traajador            CHAR(11);
DEFINE v_det2_tipo_entidadcta1               CHAR(2);
DEFINE v_det2_cve_entidadcta1                CHAR(3);
DEFINE v_det2_curpcta1                       CHAR(18);
DEFINE v_det2_nsscta1                        CHAR(11);
DEFINE v_det2_rfccta1                        CHAR(13);
DEFINE v_det2_paternocta1                    CHAR(40);
DEFINE v_det2_maternocta1                    CHAR(40);
DEFINE v_det2_nombrecta1                     CHAR(40);
DEFINE v_det2_nombre_imsscta1                CHAR(50);
DEFINE v_det2_sexocta1                       CHAR(1);
DEFINE v_det2_ent_nacimiento                 CHAR(2);
DEFINE v_det2_fec_nacimientocta1             CHAR(8);
DEFINE v_det2_date_nacimientocta1            DATE;
DEFINE v_det2_estatus_traspasocta1           CHAR(2);
DEFINE v_det2_estatus_retirocta1             CHAR(2);
DEFINE v_det2_estatus_convocatoria           CHAR(1);
DEFINE v_det2_nsscta2                        CHAR(11);
DEFINE v_det2_diagnostico_uni                CHAR(2);
DEFINE v_det2_resultado_operacion            CHAR(2);
DEFINE v_det2_afore_aclaracion               CHAR(3);
DEFINE v_det2_credito43bis                   CHAR(1);
DEFINE v_det2_estado_unificacion             CHAR(2);
-- Variables utilizadas para el sumario unificadas UNI solo IMSS
DEFINE v_sum_folio_unificacion               DECIMAL(9,0);
DEFINE v_sum_tipo_registro                   CHAR(2);
DEFINE v_sum_total_registro                  DECIMAL(9,0);
DEFINE v_sum_total_nss_unificador            DECIMAL(9,0);
DEFINE v_sum_total_nss_unificados            DECIMAL(9,0);
-- Variables que se retornan
DEFINE v_i_resultado                         SMALLINT;
DEFINE v_si_numero_solicitudes_totales       SMALLINT;
DEFINE v_si_solicitudes_aceptadas_unificador SMALLINT;
DEFINE v_si_solicitudes_aceptadas_unificadas SMALLINT;
-- Control de Excepciones
DEFINE sql_err                               INTEGER;
DEFINE isam_err                              INTEGER;
DEFINE err_txt                               CHAR(200);
--                                           
DEFINE v_si_resultado                        SMALLINT;
DEFINE v_resultado                           SMALLINT;

DEFINE v_secuencia_encabezado                DECIMAL(9,0);
DEFINE v_secuencia_unificador                DECIMAL(9,0);
DEFINE v_secuencia_unificados                DECIMAL(9,0);
DEFINE v_secuencia_sumario                   DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador       DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado        DECIMAL(9,0);
DEFINE v_diagnostico_unificador              SMALLINT;
DEFINE v_diagnostico_unificadas              SMALLINT;
DEFINE v_diagnostico_rechazo                 SMALLINT;
DEFINE v_estado_familia                      SMALLINT;
DEFINE v_estado_familia_unificador           SMALLINT;
DEFINE v_estado_familia_unificado            SMALLINT;
-- Variable para marca de cuenta             
DEFINE v_i_estado_marca                      INTEGER;
DEFINE v_marca_activa                        SMALLINT;
DEFINE v_res_desmarca                        SMALLINT;
--Variables de marca afiliación
DEFINE v_marca_afi                           SMALLINT;
DEFINE v_n_referencia_afi                    DECIMAL(9,0);
DEFINE v_marca_afi_ado                       SMALLINT;    
DEFINE v_n_referencia_afi_ado                DECIMAL(9,0);
-- Totales 
DEFINE v_total_unificadores                  INTEGER;
DEFINE v_total_unificados                    INTEGER;  
DEFINE v_existe_unificado                    SMALLINT;
DEFINE v_existe_unificador                   SMALLINT;
DEFINE v_unificador_valido                   SMALLINT;
DEFINE v_unificado_valido                    SMALLINT;

   ON EXCEPTION SET sql_err,
                    isam_err
                    
      LET v_i_resultado        = sql_err;
      LET v_total_unificadores = 0;
      LET v_total_unificados   = 0;

      RETURN v_i_resultado, 
             err_txt,
             v_total_unificadores,
             v_total_unificados;
   END EXCEPTION

  --Se habilita el LOG del SP
--  SET DEBUG FILE TO '/safreviv/uni/sql/integra_imss.sql';
--  TRACE ON;

LET v_i_resultado                         = 0;
LET v_resultado                           = 0;
LET v_si_numero_solicitudes_totales       = 0;
LET v_si_solicitudes_aceptadas_unificador = 0;
LET v_si_solicitudes_aceptadas_unificadas = 0;
LET v_si_resultado                        = 0;
LET sql_err                               = NULL;
LET isam_err                              = NULL;
LET err_txt                               = NULL;
LET v_estado_familia                      = 0;
LET v_marca_activa                        = 0;
LET v_total_unificadores                  = 0;
LET v_total_unificados                    = 0;
LET v_existe_unificado                    = 0;
LET v_existe_unificador                   = 0;
LET v_unificador_valido                   = 1;
LET v_unificado_valido                    = 1;
LET v_marca_afi                           = 0;
LET v_n_referencia_afi                    = 0;
LET v_marca_afi_ado                       = 0;
LET v_n_referencia_afi_ado                = 0;

   --trace "Al recuperar datos detalle tmp_enc_cta_unificar_op21";
   
   LET err_txt = "Al recuperar datos detalle tmp_enc_cza_unificar_op21";
   
   FOREACH
      SELECT tpo_registro,
             ide_servicio,
             ide_operacion,
             tpo_entidad_origen,
             cve_entidad_origen,
             tpo_entidad_destino,
             cve_entidad_destino,
             fec_presentacion,
             consec_lote_dia,
             resultado_operacion,
             periodo_unificacion
      INTO   v_enc_tpo_registro,
             v_enc_ide_servicio,
             v_enc_ide_operacion,
             v_enc_tpo_entidad_origen,
             v_enc_cve_entidad_origen,
             v_enc_tpo_entidad_destino,
             v_enc_cve_entidad_destino,
             v_enc_fec_presentacion,
             v_enc_consec_lote_dia,
             v_enc_resultado_operacion,
             v_enc_periodo_unificacion
      FROM   safre_tmp:tmp_enc_cta_unificar_op21
          
      --Recupera la referencia para insertar rechazos
      SELECT seq_uni_cza_unificacion.NEXTVAL
      INTO   v_secuencia_encabezado
      FROM   systables
      WHERE  tabid = 1;
      
      --trace "Valida tipo de registro para el registro inicial 01";
      LET err_txt = "Valida tipo de registro para el registro inicial 01";
      
      IF (v_enc_tpo_registro <> "01" OR v_enc_tpo_registro IS NULL) THEN
         -- ERROR de encabezado.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  1,
                                                  1,
                                                  v_secuencia_encabezado,
                                                  "No",
                                                  10,
                                                  v_enc_tpo_registro);
      END IF
      
      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                                v_enc_fec_presentacion)
              INTO v_resultado,
                   v_enc_date_presentacion;
      
      -- Valida la fecha de registro
      --trace "Fecha de presentacion";
      LET err_txt = "Fecha de presentacion";
      
      IF(v_enc_date_presentacion > TODAY OR v_enc_date_presentacion IS NULL)THEN
         -- ERROR de encabezado.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  1,
                                                  1,
                                                  v_secuencia_encabezado,
                                                  "No",
                                                  11,
                                                  v_enc_date_presentacion);
      END IF
      -- [Error]
      --trace "Al insertar encabezado en uni_cza_unificacion";
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
             VALUES(v_secuencia_encabezado,
                    p_folio,
                    v_enc_tpo_registro,
                    v_enc_ide_servicio,
                    v_enc_ide_operacion,
                    v_enc_tpo_entidad_origen,
                    v_enc_cve_entidad_origen,
                    v_enc_tpo_entidad_destino,
                    v_enc_cve_entidad_destino,
                    v_enc_date_presentacion,
                    v_enc_consec_lote_dia,
                    v_enc_resultado_operacion,
                    v_enc_periodo_unificacion);
   END FOREACH;
---------------------------CTAS UNIFICADOR----------------------------- 
   --trace "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   FOREACH
      SELECT tpo_registro,                    
             contador_servicio,               
             tpo_entidad_unificacion,         
             cve_entidad_unifica,             
             tpo_entidad_localiza_nss,        
             cve_entidad_localiza_nss,        
             curp_unificadora,                
             nss_unificador,                  
             rfc_unificador,                  
             app_unificador,                  
             apm_unificador,                  
             nombre_unificador,               
             nombre_segun_imss,               
             sexo_unificador,                 
             entidad_nacimiento_unificadora,  
             fecha_nacimiento_unificadora,    
             tpo_documento_probatorio,        
             cve_afore_receptora,             
             num_cta_asociadas,               
             estatus_convocatoria,            
             codigo_resultado,                
             ide_movimiento,                  
             estatus_nss_unificador_t,        
             estatus_nss_unificador_r,        
             cve_afore_recibo,                
             ide_credito_infonavit            
      INTO   v_det_tipo_registro,           
             v_det_contador_servicio,       
             v_det_tipo_entidad_solicitante,
             v_det_cve_entidad_solicitante, 
             v_det_tipo_entidad_unificador, 
             v_det_clave_entidad_unificador,
             v_det_curp_unificador,         
             v_det_nss_unificador,          
             v_det_rfc_unificador,          
             v_det_paterno_unificador,      
             v_det_materno_unificador,      
             v_det_nombre_unificador,       
             v_det_nombre_imssunificador,   
             v_det_sexo_unificador,         
             v_det_entidad_nacunificador,   
             v_det_fec_nac_unificador,      
             v_det_tpo_docto_probatorio,    
             v_det_clave_afore_receptora,   
             v_det_numero_cuentasasoc,      
             v_det_estatus_convocatoria,    
             v_det_resultado_operacion,     
             v_det_ident_movimiento,        
             v_det_estatus_traspaso,        
             v_det_estatus_retiro,          
             v_det_cve_afore_aclaracion,    
             v_det_indicador_credito43bis   
      FROM   safre_tmp:tmp_det_cta_unificadora_op21
          
      --Recupera la referencia para insertar rechazos
      SELECT seq_uni_det_unificador.NEXTVAL
      INTO   v_secuencia_unificador
      FROM   systables
      WHERE  tabid = 1;
      
      LET v_diagnostico_unificador = 1; --Aceptada
      LET v_diagnostico_rechazo = 1;
      LET v_estado_familia_unificador = 1;
     
      --trace "Valida tipo de registro para el registro inicial 02";
      LET err_txt = "Valida tipo de registro para el registro inicial 02";
      
      IF (v_det_tipo_registro <> "02" OR v_det_tipo_registro IS NULL) THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_secuencia_unificador,
                                                  "No",
                                                  12,
                                                  v_det_tipo_registro);
                                                  
         LET v_diagnostico_unificador = 2; -- Rechazada
         LET v_diagnostico_rechazo = 12;   -- TIPO DE REGISTRO INCORRECTO PARA EL DETALLE UNIFICADOR
         LET v_estado_familia_unificador =  2;
      END IF

      -- Corrige fecha de YYYYMMDD a MMDDYYY
      EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                                v_det_fec_nac_unificador)
              INTO v_resultado,
                   v_det_date_nac_unificador;
      
      --Recupera el id_derechohabiente
      SELECT id_derechohabiente
      INTO   v_id_derechohabiente_unificador
      FROM   afi_derechohabiente
      WHERE  nss = v_det_nss_unificador;
      
      -- Valida el id_derechohabiente
      --trace "En id_derechohabiente, no existe";
      LET err_txt = "En id_derechohabiente, no existe " || v_det_fec_nac_unificador || " - " || v_id_derechohabiente_unificador ;
      
      IF(v_id_derechohabiente_unificador IS NULL)THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_secuencia_unificador,
                                                  "No",
                                                  13,
                                                  v_det_nss_unificador);

         LET v_diagnostico_unificador = 2; -- Rechazada   
         LET v_diagnostico_rechazo = 13;   -- NO EXISTE EL DERECHOHABIENTE UNIFICADOR
         LET v_estado_familia_unificador =  2;
         
         LET v_unificador_valido = 2;
      END IF
      
      --Se consulta si el unificador tiene marca 511 (Prospecto UNIFICADOR)
      SELECT marca, 
             n_referencia
      INTO   v_marca_afi, 
             v_n_referencia_afi
      FROM   sfr_marca_activa
      WHERE  marca = 511
      AND    id_derechohabiente = v_id_derechohabiente_unificador;
      
      IF v_marca_afi = 511 THEN      
         EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificador,
                                              v_marca_afi,
                                              v_n_referencia_afi,
                                              0,
                                              501,
                                              p_usuario_cod,
                                              p_proceso_cod)
         INTO v_res_desmarca;     
      END IF
      -------
      SELECT marca
      INTO   v_marca_activa
      FROM   sfr_marca_activa
      WHERE  marca = 501
      AND    id_derechohabiente = v_id_derechohabiente_unificador;
      
      --## Se consulta si la cuenta ya tiene marca activa
      IF v_marca_activa = 501 THEN
         -- Si la cuenta ya tiene marca se rechaza
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_secuencia_unificador,
                                                  "EM",
                                                  41,
                                                  v_det_nss_unificador);
                                                  
         LET v_diagnostico_unificador = 2; -- Rechazada   
         LET v_diagnostico_rechazo =   40; -- ERROR AL MARCAR AL UNIFICADOR 
         LET v_estado_familia_unificador =  2;
      ELSE          
         IF v_unificador_valido = 1 THEN
            --Si la cuenta no tiene marca, se marca.
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_unificador,
                                             501,  -- marca de unificador IMSS
                                             v_secuencia_unificador,
                                             p_folio,
                                             0,    -- estado marca
                                             0,    -- codigo de rechazo
                                             0,    -- marca de la causa
                                             NULL, -- fecha de la causa
                                             p_usuario_cod,
                                             p_proceso_cod)
            INTO v_i_estado_marca;
            
            IF v_i_estado_marca <> 0 THEN 
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        2,
                                                        2,
                                                        v_secuencia_unificador,
                                                        "EM",
                                                        40,
                                                        v_det_nss_unificador);
                                                        
               LET v_diagnostico_unificador    = 2; -- Rechazada   
               LET v_diagnostico_rechazo       = 40;
               LET v_estado_familia_unificador =  2;
               
               UPDATE uni_pre_unificador 
               SET    estado =  20
               WHERE id_derechohabiente = v_id_derechohabiente_unificador;
      
            ELSE
               UPDATE uni_pre_unificador 
               SET    estado =  10
               WHERE id_derechohabiente = v_id_derechohabiente_unificador;
            END IF
         END IF   
      END IF

      --trace "Al insertar encabezado en uni_det_unificador";
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
                                     folio_liquidacion)
             VALUES(v_secuencia_unificador, --id_unificador
                    p_folio,                                  
                    v_id_derechohabiente_unificador,                      
                    v_det_tipo_registro,                      
                    v_det_contador_servicio,                  
                    v_det_tipo_entidad_solicitante,           
                    v_det_cve_entidad_solicitante,            
                    v_det_tipo_entidad_unificador,            
                    v_det_clave_entidad_unificador,           
                    v_det_curp_unificador,                    
                    v_det_nss_unificador,                     
                    v_det_rfc_unificador,                     
                    v_det_paterno_unificador,                 
                    v_det_materno_unificador,                 
                    v_det_nombre_unificador,                  
                    v_det_nombre_imssunificador,              
                    v_det_sexo_unificador,                    
                    v_det_entidad_nacunificador,              
                    v_det_date_nac_unificador,                 
                    v_det_tpo_docto_probatorio,               
                    v_det_clave_afore_receptora,              
                    v_det_numero_cuentasasoc,                 
                    v_det_estatus_convocatoria,               
                    v_det_resultado_operacion,                
                    v_det_ident_movimiento,                   
                    v_det_estatus_traspaso,                   
                    v_det_estatus_retiro,                     
                    v_det_cve_afore_aclaracion,               
                    v_det_indicador_credito43bis,
                    NULL,
                    v_diagnostico_unificador,
                    v_diagnostico_rechazo,
                    NULL,                                     
                    NULL,                                     
                    NULL
                    );


      SELECT COUNT(*)
      INTO   v_existe_unificador
      FROM   uni_det_unificador
      WHERE  folio_unificacion =  p_folio 
      AND    nss_unificador = v_det_nss_unificador;      
      
      IF v_existe_unificador > 1 THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_secuencia_unificados,
                                                  "EM",
                                                  40,
                                                  v_det_nss_unificador);          
                                                  
         LET v_diagnostico_unificador = 2; -- Rechazada                                       
         LET v_diagnostico_rechazo = 40;   -- LA MARCA UNIFICADOR IMSS NO ESTA ACTIVA         
         LET v_estado_familia_unificador =  2;                                                
                                                           
         UPDATE uni_det_unificador
            SET estado_unificacion = v_diagnostico_unificador,
                diagnostico        = v_diagnostico_rechazo,
                estado_familia     = v_estado_familia_unificador
         WHERE  id_unificador = v_secuencia_unificador --id_unificador
         AND    folio_unificacion = p_folio;      
                  
      END IF
---------------------------CTAS UNIFICADAS----------------------------- 
      --trace "Al recuperar datos detalle tmp_det_cta_unificadas_op21";
      LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadas_op21";

      FOREACH
         SELECT tpo_registro,                    
                contador_servicio,
                nss_unificador_traajador,
                tpo_entidad_cta,
                cve_entidad_cta,
                curp_cta_trabajador_1,
                nss_cta_trabajador_1,
                rfc_cta_trabajador_1,
                app_trabajador_1,
                apm_trabajador_1,
                nom_trabajajdor_1,
                nom_segun_imss_1,
                sexo_unificador_1,
                entidad_nacimiento_1,
                fec_nacimiento_1,
                estatus_unificar_t_1,
                estatus_unificar_r_1,
                estatus_convocatoria_1,
                nss_cta_trabajador_2,
                diagnostico_unificacion,
                resultado_operacion,
                cve_afore_recibio_1,
                ide_credito_infonavit
         INTO   v_det2_tipo_registro,
                v_det2_contador_serviciocta1,
                v_nss_unificador_traajador,
                v_det2_tipo_entidadcta1,
                v_det2_cve_entidadcta1,
                v_det2_curpcta1,
                v_det2_nsscta1,
                v_det2_rfccta1,
                v_det2_paternocta1,
                v_det2_maternocta1,
                v_det2_nombrecta1,
                v_det2_nombre_imsscta1,
                v_det2_sexocta1,
                v_det2_ent_nacimiento,
                v_det2_fec_nacimientocta1,
                v_det2_estatus_traspasocta1,
                v_det2_estatus_retirocta1,
                v_det2_estatus_convocatoria,
                v_det2_nsscta2,
                v_det2_diagnostico_uni,
                v_det2_resultado_operacion,
                v_det2_afore_aclaracion,
                v_det2_credito43bis
         FROM   safre_tmp:tmp_det_cta_unificadas_op21
         WHERE  nss_unificador_traajador = v_det_nss_unificador
             
         --Recupera la secuencia 
         SELECT seq_uni_det_unificado.NEXTVAL
         INTO   v_secuencia_unificados
         FROM   systables
         WHERE  tabid = 1;

         LET v_diagnostico_unificadas = 1; --Aceptadas
         LET v_diagnostico_rechazo = 1;
         LET v_estado_familia_unificado = 1;
         
         --trace "Valida tipo de registro para el registro inicial 03";
         LET err_txt = "Valida tipo de registro para el registro inicial 03";
         
         IF (v_det2_tipo_registro <> "03" OR v_det2_tipo_registro IS NULL) THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_secuencia_unificados,
                                                     "No",
                                                     14,
                                                     v_det2_tipo_registro);
                                                     
            LET v_diagnostico_unificadas = 2; -- Rechazada
            LET v_diagnostico_rechazo = 14;   -- TIPO DE REGISTRO INCORRECTO PARA EL DETALLE UNIFICADO
            LET v_estado_familia_unificado =  2;
         END IF
         
         LET err_txt = "Recupera el id derechohabiente unificado";
         
         --Recupera el id_derechohabiente
         SELECT id_derechohabiente
         INTO   v_id_derechohabiente_unificado
         FROM   afi_derechohabiente
         WHERE  nss = v_det2_nsscta1;
         
         -- Valida el id_derechohabiente
         LET err_txt = "En id_derechohabiente, no existe en unificado" || " - " || v_det2_nsscta1  || " - " ||  v_det2_fec_nacimientocta1;
         
         IF(v_id_derechohabiente_unificado IS NULL)THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_secuencia_unificados,
                                                     "No",
                                                     15,
                                                     v_det2_nsscta1);
                                                     
            LET v_diagnostico_unificadas = 2; -- Rechazada
            LET v_diagnostico_rechazo = 15;   -- NO EXISTE EL DERECHOHABIENTE UNIFICADO
            LET v_estado_familia_unificado = 2;
            LET v_unificado_valido = 2;
         END IF
         
         --Se consulta si el unificador tiene marca 512
         SELECT marca, 
                n_referencia
         INTO   v_marca_afi_ado, 
                v_n_referencia_afi_ado
         FROM   sfr_marca_activa
         WHERE  marca = 512
         AND    id_derechohabiente = v_id_derechohabiente_unificado;
         
         IF v_marca_afi_ado = 512 THEN      
            EXECUTE FUNCTION fn_desmarca_cuenta (v_id_derechohabiente_unificado,
                                                 v_marca_afi_ado,
                                                 v_n_referencia_afi_ado,
                                                 0,
                                                 502,
                                                 p_usuario_cod,
                                                 p_proceso_cod)
                                                 
            INTO v_res_desmarca;
         END IF

         SELECT marca
         INTO   v_marca_activa
         FROM   sfr_marca_activa
         WHERE  marca = 502
         AND    id_derechohabiente = v_id_derechohabiente_unificado;         
         
         --## Se consulta si la cuenta ya tiene marca activa
         IF v_marca_activa = 502 THEN
            -- Si la cuenta ya tiene marca se rechaza
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        3,
                                                        3,
                                                        v_secuencia_unificados,
                                                        "No",
                                                        41,
                                                        v_det2_nsscta1);
               LET v_diagnostico_unificadas   = 2;  -- Rechazada
               LET v_diagnostico_rechazo      = 41; -- ERROR AL MARCAR EL UNIFICADO
               LET v_estado_familia_unificado = 2;
         ELSE
            IF v_unificado_valido = 1 THEN
               --Si la cuenta no tiene marca, se marca.
               EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_unificado,
                                                502,  -- marca de unificado IMSS
                                                v_secuencia_unificados,
                                                p_folio,
                                                0,    -- estado marca
                                                0,    -- codigo de rechazo
                                                0,    -- marca de la causa
                                                NULL, -- fecha de la causa
                                                p_usuario_cod,
                                                p_proceso_cod)
               INTO v_i_estado_marca;
               
               IF v_i_estado_marca <> 0 THEN 
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           3,
                                                           3,
                                                           v_secuencia_unificados,
                                                           "EM",
                                                           41,
                                                           v_det2_nsscta1);
                  LET v_diagnostico_unificadas = 2; -- Rechazada
                  LET v_diagnostico_rechazo = 41;   -- LA MARCA UNIFICADO IMSS NO ESTA ACTIVA
                  LET v_estado_familia_unificado = 2;
                  
                  UPDATE uni_pre_unificado
                  SET    estado =  20
                  WHERE id_derechohabiente = v_id_derechohabiente_unificado;

               ELSE
                  UPDATE uni_pre_unificado
                  SET    estado =  10
                  WHERE id_derechohabiente = v_id_derechohabiente_unificado;
               END IF
            END IF
         END IF
         
         -- Corrige fecha de YYYYMMDD a MMDDYYY
         EXECUTE PROCEDURE sp_cambia_formato_fecha(p_proceso_cod,
                                                   v_det2_fec_nacimientocta1)
                 INTO v_resultado,
                      v_det2_date_nacimientocta1;                  
         
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
                VALUES(v_secuencia_unificados,
                       v_secuencia_unificador, --id_unificador
                       p_folio,                                  
                       v_id_derechohabiente_unificado,
                       v_det2_tipo_registro,
                       v_det2_contador_serviciocta1,
                       v_det2_tipo_entidadcta1,
                       v_det2_cve_entidadcta1,
                       v_det2_curpcta1,
                       v_det2_nsscta1,
                       v_det2_rfccta1,
                       v_det2_paternocta1,
                       v_det2_maternocta1,
                       v_det2_nombrecta1,
                       v_det2_nombre_imsscta1,
                       v_det2_sexocta1,
                       v_det2_ent_nacimiento,
                       v_det2_date_nacimientocta1,
                       v_det2_estatus_traspasocta1,
                       v_det2_estatus_retirocta1,
                       v_det2_estatus_convocatoria,
                       v_det2_nsscta2,
                       v_det2_diagnostico_uni,
                       v_det2_resultado_operacion,
                       v_det2_afore_aclaracion,
                       v_det2_credito43bis,
                       v_diagnostico_unificadas,
                       v_diagnostico_rechazo);

         -- Verifica si existe unificado con mismo unificador con el folio asignado al proceso
         SELECT COUNT(*)
         INTO   v_existe_unificado
         FROM   uni_det_unificado
         WHERE  folio_unificacion =  p_folio 
         AND    nsscta1 = v_det2_nsscta1
         AND    nsscta2 = v_det2_nsscta2;      
         
         IF v_existe_unificado > 1 THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_secuencia_unificados,
                                                     "No",
                                                     40,
                                                     v_det2_nsscta1);
            
            UPDATE uni_det_unificado
               SET estado_unificacion = 2,
                   diagnostico        = 40
            WHERE  id_unificador = v_secuencia_unificador --id_unificador
            AND    id_unificado  = v_secuencia_unificados;
            
         END IF
         
         --trace "Valida para marcar la cuenta unificado";
         IF v_estado_familia_unificador = 1 THEN 
            IF v_estado_familia_unificado = 1 THEN
               LET v_estado_familia = 1;
            ELSE
               LET v_estado_familia = 2;
            END IF   
         ELSE
            LET v_estado_familia = 2;
         END IF

         --trace "Al actualizar el estado de la familia";
         LET err_txt = "Al actualizar el estado de la familia";

         --Actualiza estado de familia
         UPDATE uni_det_unificador
            SET estado_familia = v_estado_familia
         WHERE  id_unificador = v_secuencia_unificador
         AND    (estado_familia IS NULL OR   estado_familia = 1);
         
         LET v_total_unificados = v_total_unificados + 1;
         LET v_existe_unificado = 0;
         
      END FOREACH; --Unificados       
      LET v_total_unificadores = v_total_unificadores + 1;
      LET v_existe_unificador = 0;
   END FOREACH;  --Unificador 
--------------------------------------------------------------------------------
--Sumario    
   LET err_txt = "Al recuperar datos detalle tmp_sum_cta_unificar_op21";
   
   FOREACH
      SELECT tpo_registro,
             cant_registro_detalle,
             tot_nss_unificadores,
             tot_ctas_unificadas
      INTO   v_sum_tipo_registro,
             v_sum_total_registro,
             v_sum_total_nss_unificador,
             v_sum_total_nss_unificados
      FROM   safre_tmp:tmp_sum_cta_unificar_op21
          
      --Recupera la secuencia 
      SELECT seq_uni_sum_unificacion.NEXTVAL
      INTO   v_secuencia_sumario
      FROM   systables
      WHERE  tabid = 1;
      
      --trace "Valida tipo de registro para el registro inicial 09";
      LET err_txt = "Valida tipo de registro 09";
      
      IF (v_sum_tipo_registro <> "09" OR v_sum_tipo_registro IS NULL) THEN
         -- ERROR de encabezado.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  9,
                                                  9,
                                                  v_secuencia_sumario,
                                                  "No",
                                                  16,
                                                  v_sum_tipo_registro);
      END IF
      
      -- [Error]
      --trace "Al insertar encabezado en uni_sum_unificacion";
      LET err_txt = "Al insertar encabezado en uni_sum_unificacion";
      
      INSERT INTO uni_sum_unificacion(id_sum_unifica,
                                      folio_unificacion,
                                      tipo_registro,
                                      total_registro,
                                      total_nss_unificador,
                                      total_nss_unificados)
             VALUES(v_secuencia_sumario,
                    p_folio,
                    v_sum_tipo_registro,
                    v_sum_total_registro,
                    v_sum_total_nss_unificador,
                    v_sum_total_nss_unificados);
   END FOREACH;
   
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   
   LET err_txt = "Al actualizar glo_ctr_archivo"|| " - " ||p_proceso_cod||" - " || p_pid ;      
	   
	   UPDATE glo_ctr_archivo
	      SET folio = p_folio,
	          estado = 2, 
	          opera_cod = 2
	   WHERE  nombre_archivo = p_nombre_archivo;
   
   UPDATE bat_ctr_operacion 
      SET folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;      

   LET err_txt = " Registros: "||v_sum_total_registro;
   
   UPDATE STATISTICS FOR TABLE uni_cza_unificacion;
   UPDATE STATISTICS FOR TABLE uni_det_unificador;
   UPDATE STATISTICS FOR TABLE uni_det_unificado;
   UPDATE STATISTICS FOR TABLE uni_sum_unificacion;
   
   RETURN v_i_resultado, 
          err_txt,
          v_total_unificadores,
          v_total_unificados;
        
END FUNCTION;


