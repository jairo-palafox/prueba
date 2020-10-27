






CREATE FUNCTION "safreviv".fn_uni_integra_marca_masiva(p_usuario_cod CHAR(20),
                                          p_proceso_cod SMALLINT, 
                                          p_nombre_archivo CHAR(18),
                                          p_folio DECIMAL(9,0), 
                                          p_pid DECIMAL(9,0))

  RETURNING INTEGER, 
            CHAR(200), 
            INTEGER,
            INTEGER,
            INTEGER


DEFINE v_det_tipo_registro            CHAR(2);
DEFINE v_det_nss_unificador           CHAR(11);
DEFINE v_det2_tipo_registro           CHAR(2);
DEFINE v_det2_nsscta1                 CHAR(11);
DEFINE v_sum_total_registro           DECIMAL(9,0);
DEFINE v_sum_total_nss_unificador     DECIMAL(9,0);
DEFINE v_sum_total_nss_unificados     DECIMAL(9,0);
-- Variables que se retornan
DEFINE v_i_resultado                         SMALLINT;

-- Control de Excepciones
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         CHAR(200);
--                                     
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
DEFINE v_des_id_unificador             DECIMAL(9,0);
DEFINE v_des_dor_id_derechohabiente    DECIMAL(9,0);
DEFINE v_des_dor_resultado_operacion   SMALLINT    ;  
DEFINE v_des_id_unificado              DECIMAL(9,0);
DEFINE v_des_ado_id_derechohabiente    DECIMAL(9,0);
DEFINE v_des_ado_resultado_operacion   SMALLINT    ;  

DEFINE v_secuencia_unificador          DECIMAL (9,0);
DEFINE v_secuencia_unificado           DECIMAL (9,0);       

DEFINE v_edo_uni_dor                   DECIMAL (9,0);
DEFINE v_edo_uni_ado                   DECIMAL (9,0);
DEFINE v_id_unificador_update          DECIMAL (9,0);
DEFINE v_estado_marca                  SMALLINT;
DEFINE v_res_desmarca                  SMALLINT;

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
LET v_sum_total_nss_unificador            = 0; 
LET v_sum_total_nss_unificados            = 0;
LET v_sum_total_registro                  = 0;
LET v_d_id_referencia                     = 0;
LET sql_err                               = NULL;
LET isam_err                              = NULL;
LET err_txt                               = NULL;
LET v_estado_familia                      = 0;
LET v_estado_familia_unificador           = 0;
LET v_estado_familia_unificado            = 0;
LET v_det_tipo_registro                   = "00";
LET v_secuencia_unificador                = 0;
LET v_secuencia_unificado                 = 0;
LET v_edo_uni_dor                         = 0; 
LET v_edo_uni_ado                         = 0;
LET v_id_unificador_update                = 0;


   --SET DEBUG FILE TO "/safreviv_int/BD/trace_uni_marca_masiva.txt";
   --TRACE ON;
   
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
      
      SELECT seq_uni_det_unificador.NEXTVAL
      INTO   v_secuencia_unificador
      FROM   systables
      WHERE  tabid = 1;

      IF v_diagnostico_unificador = 1 THEN 
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
                 INTO v_estado_marca;
         
         --trace "Resultado marca :" || v_estado_marca;
         IF v_estado_marca <> 0 THEN 
             EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                      2,
                                                      2,
                                                      v_secuencia_unificador,
                                                      "EM",
                                                      40,
                                                      v_det_nss_unificador);
                                                      
             LET v_diagnostico_unificador    = 2; -- Rechazada   
             LET v_diagnostico_rechazo       = 40;
             LET v_estado_familia_unificador = 2;
         END IF
      END IF

      LET err_txt = "Al insertar en Unificador";
      INSERT INTO uni_det_marca_unificador(id_unificador,
                                           folio,
                                           id_derechohabiente,
                                           nss_unificador,
                                           estado_familia,
                                           resultado_operacion,
                                           diagnostico,
                                           fecha_marca,
                                           usuario_marca)	
      VALUES(v_secuencia_unificador,
             p_folio,
             v_id_derechohabiente_unificador,
             v_det_nss_unificador,
             v_estado_familia_unificador, 
             v_diagnostico_unificador,
             v_diagnostico_rechazo,
             TODAY,
             p_usuario_cod);
                 
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
         INTO   v_id_derechohabiente_unificado
         FROM   afi_derechohabiente
         WHERE  nss = v_det2_nsscta1;

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
         
         SELECT seq_uni_det_unificado.NEXTVAL
         INTO   v_secuencia_unificado
         FROM   systables
         WHERE  tabid = 1;

         IF v_diagnostico_unificadas = 1 THEN 
            --Se marca la cuenta
            EXECUTE FUNCTION fn_marca_cuenta(v_id_derechohabiente_unificado,
                                             502,  -- marca de unificado IMSS
                                             v_secuencia_unificado,
                                             p_folio,
                                             0,    -- estado marca
                                             0,    -- codigo de rechazo
                                             0,    -- marca de la causa
                                             NULL, -- fecha de la causa
                                             p_usuario_cod,
                                             p_proceso_cod)
            INTO v_estado_marca;
   
            IF v_estado_marca <> 0 THEN 
               EXECUTE PROCEDURE sp_uni_inserta_rechazo(p_folio,
                                                        3,
                                                        3,
                                                        v_secuencia_unificado,
                                                        "EM",
                                                        41,
                                                        v_det2_nsscta1);
               LET v_diagnostico_unificadas = 2; -- Rechazada
               LET v_diagnostico_rechazo = 41;   -- LA MARCA UNIFICADO IMSS NO ESTA ACTIVA
               LET v_estado_familia_unificado = 2;
   
            END IF 
         END IF

         LET err_txt = "Al insertar Unificado";
         
         INSERT INTO uni_det_marca_unificado(id_unificado,
                                             id_unificador,
                                             id_derechohabiente,
                                             nss_unificado,
                                             resultado_operacion,
                                             diagnostico,
                                             fecha_marca,
                                             usuario_marca
                                             )                                         
         VALUES(v_secuencia_unificado,
                v_secuencia_unificador,
                v_id_derechohabiente_unificado,
                v_det2_nsscta1,
                v_diagnostico_unificadas,
                v_diagnostico_rechazo_ado,
                TODAY,
                p_usuario_cod);

         LET v_sum_total_nss_unificados = v_sum_total_nss_unificados + 1 ;

         IF v_diagnostico_unificadas  = 2 THEN 
            UPDATE uni_det_marca_unificador    
            SET    estado_familia    = 2
            WHERE  folio = p_folio
            AND    id_unificador     = v_secuencia_unificador
            AND    estado_familia = 1;
         ELSE                
            CONTINUE FOREACH;      
         END IF 
      END FOREACH; 
      LET v_sum_total_registro = v_sum_total_registro + 1 ;

----
      FOREACH 
         SELECT id_unificador,
                id_derechohabiente,
                resultado_operacion
         INTO   v_des_id_unificador,
                v_des_dor_id_derechohabiente,
                v_des_dor_resultado_operacion
         FROM   uni_det_marca_unificador
         WHERE  folio = p_folio
         AND    estado_familia = 2
      
         IF v_des_id_unificador > 0 THEN 
            --DESMARCAR UNIFICADOR 
            LET err_txt = "Al desmarcar Unificador " || v_des_dor_id_derechohabiente;
            EXECUTE FUNCTION fn_desmarca_cuenta (v_des_dor_id_derechohabiente,
                                                 501,
                                                 v_des_id_unificador,
                                                 30,
                                                 0,
                                                 p_usuario_cod,
                                                 p_proceso_cod)
           INTO v_res_desmarca;
         END IF 
         
         FOREACH
            SELECT id_unificado,
                   id_derechohabiente,
                   resultado_operacion
            INTO   v_des_id_unificado,
                   v_des_ado_id_derechohabiente,
                   v_des_ado_resultado_operacion
            FROM   uni_det_marca_unificado
            WHERE  id_unificador  = v_des_id_unificador
            
            IF v_des_id_unificado > 0 THEN 
               --DESMARCAR UNIFICADO
               LET err_txt = "Al desmarcar Unificado " || v_des_ado_id_derechohabiente;
               EXECUTE FUNCTION fn_desmarca_cuenta (v_des_ado_id_derechohabiente,
                                                    502,
                                                    v_des_id_unificado,
                                                    30,
                                                    0,
                                                    p_usuario_cod,
                                                    p_proceso_cod)
               INTO v_res_desmarca;

            END IF   
         END FOREACH;
      END FOREACH;
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


