






CREATE FUNCTION "safreviv".fn_uni_integra_procesar_rechazadas(p_usuario_cod    CHAR   (20), 
                                                   p_folio          DECIMAL(10), 
                                                   p_nombre_archivo CHAR   (18), 
                                                   p_pid            DECIMAL(9,0),
                                                   p_proceso_cod    SMALLINT) 
  RETURNING INTEGER, 
            INTEGER, 
            CHAR(200)

-- Variables utilizadas para el detalle unificadora UNI solo IMSS
DEFINE v_det_tpo_registro               CHAR(2);
DEFINE v_det_curp_unificadora           CHAR(18);
DEFINE v_det_nss_unificador             CHAR(11);
DEFINE v_det_rfc_unificador             CHAR(13);
DEFINE v_det_app_unificador             CHAR(40);
DEFINE v_det_apm_unificador             CHAR(40);
DEFINE v_det_nombre_unificador          CHAR(40);
DEFINE v_det_num_cuentas_asociadas      INTEGER;
-- Variables utilizadas para el detalle unificadas UNI solo IMSS
DEFINE v_det2_tpo_registro              CHAR(2);
DEFINE v_det2_nss_unificador            CHAR(11);
DEFINE v_det2_curp_unificado            CHAR(18);
DEFINE v_det2_nss_unificado             CHAR(11);
DEFINE v_det2_rfc_unificado             CHAR(13);
DEFINE v_det2_app_unificado             CHAR(40);
DEFINE v_det2_apm_unificado             CHAR(40);
DEFINE v_det2_nom_unificado             CHAR(40);
DEFINE v_det2_diagnostico_unificacion   CHAR(2);
-- Variables que se retornan
DEFINE v_i_resultado                    INTEGER;
DEFINE v_si_numero_solicitudes_totales  INTEGER;
DEFINE v_sum_total_nss_unificador       INTEGER;
DEFINE v_sum_total_nss_unificados       INTEGER;
-- Control de Excepciones
DEFINE sql_err                          INTEGER;
DEFINE isam_err                         INTEGER;
DEFINE err_txt                          CHAR(200);
--                                      
DEFINE v_si_resultado                   SMALLINT;
DEFINE v_d_id_referencia                DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificador  DECIMAL(9,0);
DEFINE v_id_derechohabiente_unificado   DECIMAL(9,0);
DEFINE v_diagnostico_unificador         SMALLINT;
DEFINE v_diagnostico_unificadas         SMALLINT;
DEFINE v_diagnostico_rechazo            SMALLINT;
DEFINE v_estado_familia                 SMALLINT;
DEFINE v_estado_familia_unificador      SMALLINT;
DEFINE v_estado_familia_unificado       SMALLINT;
-- Variable para marca de cuenta        
DEFINE v_i_estado_marca                 INTEGER;
DEFINE v_id_unificador                  DECIMAL(9,0);
DEFINE v_id_unificado                   DECIMAL(9,0);
DEFINE v_si_marca_infonavit             SMALLINT;    

ON EXCEPTION SET sql_err, 
                 isam_err, 
                 err_txt
                 
      LET v_i_resultado = sql_err;
      LET v_si_numero_solicitudes_totales = 0;

      RETURN v_i_resultado,
             isam_err,
             err_txt;
END EXCEPTION

-- Variables que almacenan informacion para su validacion
LET v_i_resultado                         = 0;
LET v_si_numero_solicitudes_totales       = 0;
LET v_sum_total_nss_unificador            = 0;
LET v_sum_total_nss_unificados            = 0;

LET v_d_id_referencia                     = 0;
LET v_si_resultado                        = 0;
LET sql_err                               = NULL;
LET isam_err                              = NULL;
LET err_txt                               = NULL;
LET v_estado_familia                      = 0;

   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni_integra_PROCESAR_aceptadas.txt";
---------------------------CTAS UNIFICADOR----------------------------- 

   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   
   FOREACH
      SELECT nss_unificador
      INTO   v_det_nss_unificador
      FROM   safre_mig:tmp_det_unificador
          
      -- Acumula la referencia
      LET v_d_id_referencia           = v_d_id_referencia + 1;
      LET v_diagnostico_unificador    = 1; --Aceptada
      LET v_diagnostico_rechazo       = 1;
      LET v_estado_familia_unificador = 1;
      
      ----Recupera el id_derechohabiente del UNIFICADOR 
            
      SELECT a.id_unificador,
             a.id_derechohabiente
      INTO   v_id_unificador,
             v_id_derechohabiente_unificador
      FROM   uni_det_unificador a
      WHERE  a.nss_unificador = v_det_nss_unificador
      AND    a.estado_familia = 1
      AND    a.diagnostico = 1;
      
      LET v_si_marca_infonavit = 501;

      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_unificador,
                                          v_si_marca_infonavit, -- marca de infonavit
                                          v_id_unificador,
                                          0,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
              INTO v_si_resultado;
          
      LET v_sum_total_nss_unificador = v_sum_total_nss_unificador + 1;
      
---------------------------CTAS UNIFICADAS----------------------------- 
      FOREACH
         SELECT nss_unificado
         INTO   v_det2_nss_unificado
         FROM   safre_mig:tmp_det_unificado
         WHERE  nss_unificador = v_det_nss_unificador
             
         -- Acumula la referencia
         LET v_d_id_referencia          = v_d_id_referencia + 1;
         LET v_diagnostico_unificadas   = 1; --Aceptadas
         LET v_diagnostico_rechazo      = 1;
         LET v_estado_familia_unificado = 1;
                 
         ----Recupera el id_derechohabiente del unificado 
         SELECT a.id_unificado,
                a.id_derechohabiente
         INTO   v_id_unificado,
                v_id_derechohabiente_unificado     
         FROM   uni_det_unificado a
         WHERE  a.id_unificador = v_id_unificador
         AND    a.nsscta1 = v_det2_nss_unificado
         AND    a.estado_unificacion = 1
         AND    a.diagnostico = 1;

         LET v_si_marca_infonavit = 502;

         EXECUTE FUNCTION fn_desmarca_cuenta(v_id_derechohabiente_unificado,
                                             v_si_marca_infonavit, -- marca de infonavit
                                             v_id_unificado,
                                             0,
                                             0,
                                             p_usuario_cod,
                                             p_proceso_cod)
                 INTO v_si_resultado;

         LET v_sum_total_nss_unificados      = v_sum_total_nss_unificados + 1;
         LET v_si_numero_solicitudes_totales = v_si_numero_solicitudes_totales + 1;
         
         UPDATE uni_det_unificado
         SET    estado_unificacion = 2,
                diagnostico = 31
         WHERE  id_unificador = v_id_unificador
         AND    id_unificado  = v_id_unificado;
      END FOREACH;
      
      UPDATE uni_det_unificador
      SET    estado_familia = 2,
             estado_unificacion = 2,
             diagnostico = 31
      WHERE  id_unificador = v_id_unificador;
         
      LET v_si_numero_solicitudes_totales = v_si_numero_solicitudes_totales + 1;
   END FOREACH;
    
    --trace "Al actualizar glo_ctr_archivo";
   LET err_txt = "Al actualizar glo_ctr_archivo";
   -- Se asigna el folio al archivo y se indica que ha sido integrado
   UPDATE safre_mig:glo_ctr_archivo
   SET    folio = p_folio,
          estado = 2 -- integrado
   WHERE  proceso_cod    = p_proceso_cod
   AND    estado         = 1; -- etapa de carga
      
   UPDATE safre_mig:bat_ctr_operacion 
   SET    folio       = p_folio,
          nom_archivo = p_nombre_archivo
   WHERE  proceso_cod = p_proceso_cod
   AND    opera_cod   = 2
   AND    pid         = p_pid;      
       
   LET err_txt = " Registros: "||v_si_numero_solicitudes_totales;
   
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificador;
   UPDATE statistics FOR TABLE safre_viv:uni_det_unificado;
   
   RETURN v_i_resultado, 
          isam_err, 
          err_txt;
END FUNCTION
;


