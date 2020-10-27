






CREATE FUNCTION "safreviv".fn_uni_integra_recurrente(p_usuario_cod CHAR(20),
                                          p_proceso_cod SMALLINT, 
                                          p_nombre_archivo CHAR(18),
                                          p_folio DECIMAL(9,0), 
                                          p_pid DECIMAL(9,0))

  RETURNING INTEGER, 
            CHAR(200), 
            INTEGER,
            INTEGER,
            INTEGER

-- Variables utilizadas para el encabezado UNI solo IMSS
DEFINE v_enc_folio_unificacion        DECIMAL(9,0);
DEFINE v_enc_tpo_registro             CHAR(2);
DEFINE v_enc_ide_servicio             CHAR(2);
DEFINE v_enc_ide_operacion            CHAR(2);
DEFINE v_enc_tpo_entidad_origen       CHAR(2);
DEFINE v_enc_cve_entidad_origen       CHAR(3);
DEFINE v_enc_tpo_entidad_destino      CHAR(2);
DEFINE v_enc_cve_entidad_destino      CHAR(3);
DEFINE v_enc_fec_presentacion         CHAR(8);
DEFINE v_enc_date_presentacion        DATE;
DEFINE v_enc_consec_lote_dia          DECIMAL(3,0);
DEFINE v_enc_resultado_operacion      CHAR(2);
DEFINE v_enc_periodo_unificacion      CHAR(6);
-- Variables utilizadas para el detalle unificadora UNI solo IMSS
DEFINE v_det_folio_unificacion        DECIMAL(9,0);
DEFINE v_det_id_derechohabiente       DECIMAL(9,0);
DEFINE v_det_tipo_registro            CHAR(2);
DEFINE v_det_contador_servicio        DECIMAL(9,0);
DEFINE v_det_tipo_entidad_solicitante CHAR(2);
DEFINE v_det_cve_entidad_solicitante  CHAR(3);
DEFINE v_det_tipo_entidad_unificador  CHAR(2);
DEFINE v_det_clave_entidad_unificador CHAR(3);
DEFINE v_det_curp_unificador          CHAR(18);
DEFINE v_det_nss_unificador           CHAR(11);
DEFINE v_det_rfc_unificador           CHAR(13);
DEFINE v_det_paterno_unificador       CHAR(40);
DEFINE v_det_materno_unificador       CHAR(40);
DEFINE v_det_nombre_unificador        CHAR(40);
DEFINE v_det_nombre_imssunificador    CHAR(50);
DEFINE v_det_sexo_unificador          CHAR(1);
DEFINE v_det_entidad_nacunificador    CHAR(2);
DEFINE v_det_fec_nac_unificador       CHAR(8);
DEFINE v_det_date_nac_unificador      DATE;
DEFINE v_det_tpo_docto_probatorio     CHAR(1);
DEFINE v_det_clave_afore_receptora    CHAR(3);
DEFINE v_det_numero_cuentasasoc       DECIMAL(2,0);
DEFINE v_det_estatus_convocatoria     CHAR(1);
DEFINE v_det_resultado_operacion      CHAR(2);
DEFINE v_det_ident_movimiento         CHAR(2);
DEFINE v_det_estatus_traspaso         CHAR(2);
DEFINE v_det_estatus_retiro           CHAR(2);
DEFINE v_det_cve_afore_aclaracion     CHAR(3);
DEFINE v_det_indicador_credito43bis   CHAR(1);
DEFINE v_det_ind_aplicacion           SMALLINT;
DEFINE v_det_estado_unificacion       DECIMAL(2,0);
DEFINE v_det_diagnostico              DECIMAL(3,0);
DEFINE v_det_fec_liquidacion          CHAR(8);
DEFINE v_det_date_liquidacion         DATE;
DEFINE v_det_fec_notificacion         CHAR(8);
DEFINE v_det_date_notificacion        DATE;
DEFINE v_det_folio_liquidacion        DECIMAL(9,0);
-- Variables utilizadas para el detalle unificadas UNI solo IMSS
DEFINE v_det2_folio_unificacion       DECIMAL(9,0);
DEFINE v_det2_id_derechohabiente      DECIMAL(9,0);
DEFINE v_det2_tipo_registro           CHAR(2);
DEFINE v_det2_contador_serviciocta1   DECIMAL(9,0);
DEFINE v_nss_unificador_traajador     CHAR(11);
DEFINE v_det2_tipo_entidadcta1        CHAR(2);
DEFINE v_det2_cve_entidadcta1         CHAR(3);
DEFINE v_det2_curpcta1                CHAR(18);
DEFINE v_det2_nsscta1                 CHAR(11);
DEFINE v_det2_rfccta1                 CHAR(13);
DEFINE v_det2_paternocta1             CHAR(40);
DEFINE v_det2_maternocta1             CHAR(40);
DEFINE v_det2_nombrecta1              CHAR(40);
DEFINE v_det2_nombre_imsscta1         CHAR(50);
DEFINE v_det2_sexocta1                CHAR(1);
DEFINE v_det2_ent_nacimiento          CHAR(2);
DEFINE v_det2_fec_nacimientocta1      CHAR(8);
DEFINE v_det2_date_nacimientocta1     DATE;
DEFINE v_det2_estatus_traspasocta1    CHAR(2);
DEFINE v_det2_estatus_retirocta1      CHAR(2);
DEFINE v_det2_estatus_convocatoria    CHAR(1);
DEFINE v_det2_nsscta2                 CHAR(11);
DEFINE v_det2_diagnostico_uni         CHAR(2);
DEFINE v_det2_resultado_operacion     CHAR(2);
DEFINE v_det2_afore_aclaracion        CHAR(3);
DEFINE v_det2_credito43bis            CHAR(1);
DEFINE v_det2_estado_unificacion      CHAR(2);
-- Variables utilizadas para el sumario unificadas UNI solo IMSS
DEFINE v_sum_folio_unificacion        DECIMAL(9,0);
DEFINE v_sum_tipo_registro            CHAR(2);
DEFINE v_sum_total_registro           DECIMAL(9,0);
DEFINE v_sum_total_nss_unificador     DECIMAL(9,0);
DEFINE v_sum_total_nss_unificados     DECIMAL(9,0);
-- Variables que se retornan
DEFINE v_i_resultado                         SMALLINT;
DEFINE v_si_numero_solicitudes_totales       INTEGER;
DEFINE v_si_solicitudes_aceptadas_unificador INTEGER;
DEFINE v_si_solicitudes_aceptadas_unificadas INTEGER;

-- Control de Excepciones
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         CHAR(200);
--                                     
DEFINE v_si_resultado                  SMALLINT;
DEFINE v_d_id_referencia               DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado  DECIMAL(9,0);
DEFINE v_diagnostico_unificador        SMALLINT;
DEFINE v_diagnostico_unificadas        SMALLINT;
DEFINE v_diagnostico_rechazo           SMALLINT;
DEFINE v_diagnostico_rechazo_ado       SMALLINT;
DEFINE v_estado_familia                SMALLINT;
DEFINE v_estado_familia_unificador     SMALLINT;
DEFINE v_estado_familia_unificado      SMALLINT;
-- Variable para marca de cuenta       
DEFINE v_i_estado_marca                INTEGER;
DEFINE v_marca_activa                  SMALLINT;
--Variables funcion valida credito UNIFICADOR 
DEFINE r_dor_resultado                 SMALLINT;
DEFINE r_dor_id_derechohabiente        DECIMAL(9,0);
DEFINE r_dor_nss                       CHAR(11);
DEFINE r_dor_tpo_originacion           SMALLINT;
DEFINE r_dor_tpo_credito               SMALLINT;
DEFINE r_dor_num_credito               DECIMAL(10,0);
DEFINE r_dor_f_otorga                  DATE;
DEFINE r_dor_f_liquida                 DATE;
DEFINE r_dor_valida                    SMALLINT;

--Variables funcion valida credito unificado
DEFINE r_ado_resultado                 SMALLINT;
DEFINE r_ado_id_derechohabiente        DECIMAL(9,0);
DEFINE r_ado_nss                       CHAR(11);
DEFINE r_ado_tpo_originacion           SMALLINT;
DEFINE r_ado_tpo_credito               SMALLINT;
DEFINE r_ado_num_credito               DECIMAL(10,0);
DEFINE r_ado_f_otorga                  DATE;
DEFINE r_ado_f_liquida                 DATE;
DEFINE r_ado_valida                    SMALLINT;

DEFINE v_tpo_credito                   SMALLINT;
DEFINE v_tpo_credito_edo               SMALLINT;
DEFINE v_contador_tpo_credito          SMALLINT;
DEFINE v_contador_tpo_credito_edo      SMALLINT;
DEFINE v_secuencia_unificador          DECIMAL (9,0);
DEFINE v_secuencia_unificado           DECIMAL (9,0);       

DEFINE v_edo_uni_dor                   DECIMAL (9,0);
DEFINE v_edo_uni_ado                   DECIMAL (9,0);
DEFINE v_id_unificador_update          DECIMAL (9,0);
                                       
DEFINE v_marca_ag                      SMALLINT;

ON EXCEPTION SET sql_err, isam_err
      LET v_i_resultado = sql_err;
      LET v_sum_total_nss_unificador = 0;
      LET v_sum_total_nss_unificados = 0;
      LET v_sum_total_registro       = 0;
      RETURN v_i_resultado, 
             err_txt, 
             v_sum_total_nss_unificador,
             v_sum_total_nss_unificados,            
             v_sum_total_registro;                  
END EXCEPTION

-- Variables que almacenan informacion para su validacion
LET v_i_resultado                         = 0;
LET v_si_numero_solicitudes_totales       = 0;
LET v_si_solicitudes_aceptadas_unificador = 0;
LET v_si_solicitudes_aceptadas_unificadas = 0;
LET v_sum_total_nss_unificador            = 0; 
LET v_sum_total_nss_unificados            = 0;
LET v_sum_total_registro                  = 0;
LET v_d_id_referencia                     = 0;
LET v_si_resultado                        = 0;
LET sql_err                               = NULL;
LET isam_err                              = NULL;
LET err_txt                               = NULL;
LET v_estado_familia                      = 0;
LET v_estado_familia_unificador           = 0;
LET v_estado_familia_unificado            = 0;
LET v_marca_activa                        = 0;
LET v_det_tipo_registro                   = "00";
LET v_secuencia_unificador                = 0;
LET v_secuencia_unificado                 = 0;
LET v_edo_uni_dor                         = 0; 
LET v_edo_uni_ado                         = 0;
LET v_id_unificador_update                = 0;
LET v_marca_ag                            = 0;


   SET DEBUG FILE TO "/safreviv_int/BD/trace_uni_integra_recurrente.txt";
   TRACE ON;
   
   SELECT MAX(id_referencia) 
   INTO v_d_id_referencia
   FROM uni_det_rechazos;

   --Verifica que el id_referencia no venga nullo
   --en caso de ser contrario, se asigna el valor que trae
   IF(v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
      LET v_d_id_referencia = 0;
   END IF
   -- [Error]
   --trace "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   
   FOREACH
      -----CTAS UNIFICADOR-----
      SELECT tipo_registro,
             nss_unificador
      INTO   v_det_tipo_registro,
             v_det_nss_unificador
      FROM   safre_tmp:tmp_det_uni_recurrente
      GROUP BY 2,1

      -- Acumula la referencia
      LET v_d_id_referencia           = v_d_id_referencia + 1;
      LET v_diagnostico_unificador    = 1; --Aceptada
      LET v_diagnostico_rechazo       = 1;
      LET v_estado_familia_unificador = 1;

      --trace "Valida tipo de registro para el registro inicial 02";
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
      INTO   v_id_derechohabiente_unificador
      FROM   afi_derechohabiente
      WHERE  nss = v_det_nss_unificador;

      -- Valida el id_derechohabiente
      --trace "En id_derechohabiente, no existe";
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

      SELECT marca
      INTO   v_marca_activa
      FROM   sfr_marca_activa
      WHERE  marca = 501
      AND    id_derechohabiente = v_id_derechohabiente_unificador;
         
      IF v_marca_activa = 0 THEN
         -- ERROR de detalle.
         EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                  2,
                                                  2,
                                                  v_d_id_referencia,
                                                  "No",
                                                  17,
                                                  v_det_nss_unificador);
                                                  
         LET v_diagnostico_unificador    = 2; --Rechazada   
         LET v_diagnostico_rechazo       = 17;
         LET v_estado_familia_unificador =  2;
      END IF

      SELECT seq_uni_det_unificador.NEXTVAL
      INTO   v_secuencia_unificador
      FROM   systables
      WHERE  tabid = 1;

      --trace "Al insertar encabezado en uni_det_unificador";
      LET err_txt = "Al insertar en uni_det_unificador";
      INSERT INTO uni_det_unificador(id_unificador,
                                     folio_unificacion,
                                     id_derechohabiente,
                                     tipo_registro,
                                     nss_unificador,
                                     estado_familia,
                                     estado_unificacion,
                                     diagnostico)
      VALUES(v_secuencia_unificador,
             p_folio,
             v_id_derechohabiente_unificador,
             v_det_tipo_registro,
             v_det_nss_unificador,
             v_estado_familia_unificador, 
             v_diagnostico_unificador,
             v_diagnostico_rechazo);
                 
      LET v_sum_total_nss_unificador = v_sum_total_nss_unificador + 1;  
      
      --Registros 03 Detalles Unificados
      ---------------------------CTAS UNIFICADAS----------------------------- 
      --trace "Al recuperar datos detalle tmp_det_cta_unificadas_op21";
      LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadas_op21";
      SELECT MAX(id_referencia) 
      INTO   v_d_id_referencia
      FROM   uni_det_rechazos;
       -- Verifica que el id_referencia no venga nullo
       -- en caso de ser contrario, se asigna el valor que trae
        IF(v_d_id_referencia IS NULL OR v_d_id_referencia = 0) THEN
           LET v_d_id_referencia = 0;
        END IF
      FOREACH
         SELECT tipo_registro,
                nss_unificado
         INTO   v_det2_tipo_registro,
                v_det2_nsscta1         
         FROM   safre_tmp:tmp_det_uni_recurrente
         WHERE nss_unificador = v_det_nss_unificador

         -- Acumula la referencia
         LET v_d_id_referencia          = v_d_id_referencia + 1;
         LET v_diagnostico_unificadas   = 1; --Aceptadas
         LET v_diagnostico_rechazo_ado  = 1;
         LET v_estado_familia_unificado = 1;

         --trace "Valida tipo de registro para el registro inicial 03";
         LET err_txt = "Valida tipo de registro para el registro inicial 03";
         IF (v_det2_tipo_registro <> "02" OR v_det2_tipo_registro IS NULL) THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_d_id_referencia,
                                                     "No",
                                                     14,
                                                     v_det2_tipo_registro);
            LET v_diagnostico_unificadas   = 2; --Rechazada
            LET v_diagnostico_rechazo_ado  = 14;
            LET v_estado_familia_unificado =  2;
         END IF

         --trace "Recupera el id derechohabiente unificado";
         LET err_txt = "Recupera el id derechohabiente unificado";

         --Recupera el id_derechohabiente
         SELECT id_derechohabiente
           INTO v_id_derechohabiente_unificado
           FROM afi_derechohabiente
          WHERE nss = v_det2_nsscta1;

         -- Valida el id_derechohabiente
         --trace "En id_derechohabiente, no existe en unificado";
         LET err_txt = "En id_derechohabiente, no existe en unificado";
         IF(v_id_derechohabiente_unificado IS NULL)THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_d_id_referencia,
                                                     "No",
                                                     15,
                                                     v_det2_nsscta1);
            LET v_diagnostico_unificadas = 2; --Rechazada
            LET v_diagnostico_rechazo_ado = 15;
            LET v_estado_familia_unificado = 2;
         END IF

         --Valida si el UNIFICADO tiene crédito
         -- c)	0 el derechohabiente tiene un crédito vigente
         -- d)	1 no hay un crédito vigente ni liquidado 
         EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente_unificado,0)
         INTO  r_ado_resultado,
               r_ado_tpo_originacion,
               r_ado_tpo_credito,
               r_ado_num_credito,
               r_ado_f_otorga,
               r_ado_f_liquida;

         --Si el derechohabiente no tiene credito
         IF r_ado_resultado = 1 THEN 
            LET  v_tpo_credito_edo = 0;
         ELSE
            --Si el derechohabiente tiene credito
            IF r_ado_resultado = 0 THEN 
               SELECT COUNT(*)
               INTO   v_contador_tpo_credito_edo
               FROM   cta_credito
               WHERE  id_derechohabiente = v_id_derechohabiente_unificado;
         
               IF v_contador_tpo_credito_edo > 1 THEN
                  -- se rechaza porque la unificacion es de Solo Infonavit
                  EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                           6,
                                                           6,
                                                           v_d_id_referencia,
                                                           "Mas de un credito",
                                                           37,
                                                           v_det2_nsscta1);
                  -- se rechaza el unificado
                  LET v_diagnostico_unificadas  = 2;
                  LET v_diagnostico_rechazo_ado = 37;

                  --Se rechaza la familia
                  LET v_estado_familia_unificado  = 2;
                  
                  LET v_contador_tpo_credito_edo = v_contador_tpo_credito_edo + 1;  --Acumulado total de creditos
               ELSE              
                  LET v_tpo_credito_edo = r_ado_tpo_credito;
               END IF
            END IF --Si DH tiene credito 
            LET v_contador_tpo_credito_edo = v_contador_tpo_credito_edo + 1 ;           
			   END IF

         
         SELECT marca
         INTO   v_marca_activa
         FROM   sfr_marca_activa
         WHERE  marca = 502
         AND    id_derechohabiente = v_id_derechohabiente_unificado;

         IF v_marca_activa = 0 THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     3,
                                                     3,
                                                     v_d_id_referencia,
                                                     "No",
                                                     18,
                                                     v_det2_nsscta1);

            LET v_diagnostico_unificadas   = 2; --Rechazada
            LET v_diagnostico_rechazo_ado  = 18;
            LET v_estado_familia_unificado = 2;
         END IF

   ----Se agrega rechazo por marcas de uso de garantías 
         SELECT marca      
         INTO   v_marca_ag
         FROM   sfr_marca_activa
         WHERE  marca IN (221,223,225)
         AND    id_derechohabiente = v_id_derechohabiente_unificado
         GROUP BY 1;
                     
         IF v_marca_ag >0 THEN
            -- ERROR de detalle.
            EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                     2,
                                                     2,
                                                     v_d_id_referencia,
                                                     "AG",
                                                     39,
                                                     v_det_nss_unificador);
                                                     
            LET v_diagnostico_unificador    = 2; --Rechazada   
            LET v_diagnostico_rechazo       = 39;
            LET v_estado_familia_unificador = 2;
         END IF
   ----



         
         SELECT seq_uni_det_unificado.NEXTVAL
         INTO   v_secuencia_unificado
         FROM   systables
         WHERE  tabid = 1;

         -- [Error]
         --trace "Al insertar encabezado en uni_det_unificado";
         LET err_txt = "Al insertar encabezado en uni_det_unificado";
            INSERT INTO uni_det_unificado(id_unificado,
                                          id_unificador,
                                          folio_unificacion,
                                          id_derechohabiente,
                                          nsscta1,
                                          nsscta2,
                                          estado_unificacion,
                                          diagnostico)
             VALUES(v_secuencia_unificado,
                    v_secuencia_unificador,
                    p_folio,                                  
                    v_id_derechohabiente_unificado,
                    v_det2_nsscta1,
                    v_det_nss_unificador,
                    v_diagnostico_unificadas,
                    v_diagnostico_rechazo_ado
                    );
         
         --IF v_estado_familia_unificador = 1 THEN 
         --   IF v_estado_familia_unificado = 1 THEN
         --      LET v_estado_familia = 1;
         --   ELSE
         --      LET v_estado_familia = 2;
         --   END IF   
         --ELSE
         --   LET v_estado_familia = 2;
         --END IF
         --
         --LET err_txt = "Al actualizar el estado de la familia, en folio " || p_folio || " id_dor " || v_secuencia_unificador;
         --
         --trace "Al actualizar el estado de la familia";
         --IF v_estado_familia_unificador = 2 THEN 
         --   IF v_diagnostico_unificadas = 2 THEN 
         --      UPDATE uni_det_unificador
         --      SET    estado_familia    = v_diagnostico_unificadas
         --      WHERE  folio_unificacion = p_folio
         --      AND    id_unificador     = v_secuencia_unificador
         --      AND    (estado_familia IS NULL 
         --      OR     estado_familia = 1);
         --   END IF 
         --END IF
         
         LET v_sum_total_nss_unificados = v_sum_total_nss_unificados + 1 ;
         
         LET v_diagnostico_unificadas   = 1; --Aceptadas
         LET v_diagnostico_rechazo_ado  = 1;
         LET v_estado_familia_unificado = 1;
      END FOREACH; 
--#################################################
      --Se agrega validación para checar si UNIFICADOR tiene credito
      --Valida si el UNIFICADOR tiene crédito
      EXECUTE FUNCTION fn_credito_vivienda(v_id_derechohabiente_unificador,0)
      INTO  r_dor_resultado,
            r_dor_tpo_originacion,
            r_dor_tpo_credito,
            r_dor_num_credito,
            r_dor_f_otorga,
            r_dor_f_liquida;
      
      IF r_dor_resultado = 1 THEN 
         LET  v_tpo_credito = 0;
      ELSE
         --Si el UNIFICADOR tiene credito 
         IF r_dor_resultado = 0 THEN
            SELECT COUNT(*)
            INTO   v_contador_tpo_credito
            FROM   cta_credito
            WHERE  id_derechohabiente = v_id_derechohabiente_unificador;
      
            IF v_contador_tpo_credito > 1 THEN
               -- se rechaza porque la unificacion es de Solo Infonavit
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        5,
                                                        5,
                                                        v_d_id_referencia,
                                                        "Mas de un credito",
                                                        36,
                                                        v_det_nss_unificador);
      
               -- se rechaza el UNIFICADOR
               LET v_diagnostico_unificador    = 2; --Rechazada   
               LET v_diagnostico_rechazo       = 36;
               LET v_estado_familia_unificador =  2;

               --trace "Al actualizar el estado de la familia";
               LET err_txt = "Al actualizar el estado de la familia en unificador ";
               UPDATE uni_det_unificador
               SET    estado_familia      = v_estado_familia_unificador,
                      estado_unificacion  = v_diagnostico_unificador,
                      diagnostico         = v_diagnostico_rechazo
               WHERE  folio_unificacion   = p_folio
               AND    id_unificador       = v_secuencia_unificador;

               LET v_contador_tpo_credito = v_contador_tpo_credito + 1;
            ELSE
               LET v_tpo_credito = r_dor_tpo_credito;
            END IF
         END IF
			END IF

      LET v_sum_total_registro = v_sum_total_registro + 1 ;
   END FOREACH;
      
   FOREACH 
      SELECT a.estado_unificacion, 
             b.estado_unificacion,
             a.id_unificador 
      INTO   v_edo_uni_dor, 
             v_edo_uni_ado,
             v_id_unificador_update 
      FROM   uni_det_unificador a, 
             uni_det_unificado b
      WHERE  a.id_unificador = b.id_unificador 
      AND    a.folio_unificacion = p_folio
      
      IF v_edo_uni_dor  = 2 OR v_edo_uni_ado = 2 THEN 
         UPDATE uni_det_unificador    
         SET    estado_familia    = 2
         WHERE  folio_unificacion = p_folio
         AND    id_unificador     = v_id_unificador_update
         AND    estado_familia = 1;                    
      ELSE                
         CONTINUE FOREACH;      
      END IF 
   
      LET v_edo_uni_dor           = 0; 
      LET v_edo_uni_ado           = 0;
      LET v_id_unificador_update  = 0;
      
   END FOREACH;
   
    --trace "Al actualizar glo_ctr_archivo";
    LET err_txt = "Al actualizar glo_ctr_archivo";
    -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
    SET    folio  = p_folio,
           estado = 2
    WHERE  proceso_cod = p_proceso_cod
    AND    opera_cod   = 1 -- archivo cargado
    AND    estado      = 1; -- etapa de carga
    
    UPDATE bat_ctr_operacion 
    SET    folio       = p_folio,
           nom_archivo = p_nombre_archivo
    WHERE  proceso_cod = p_proceso_cod
    AND    opera_cod   = 2
    AND    pid         = p_pid;      

   --trace " Registros: "||v_sum_total_registro;
   LET err_txt = " Registros: "||v_sum_total_registro;

   UPDATE STATISTICS FOR TABLE uni_det_unificador;
   UPDATE STATISTICS FOR TABLE uni_det_unificado;

 RETURN v_i_resultado, 
        err_txt, 
        v_sum_total_nss_unificador, 
        v_sum_total_nss_unificados,
        v_sum_total_registro;
END FUNCTION;


