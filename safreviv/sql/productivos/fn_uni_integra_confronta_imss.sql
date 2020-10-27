






CREATE FUNCTION "safreviv".fn_uni_integra_confronta_imss(p_usuario_cod    CHAR(20), 
                                              p_proceso_cod    SMALLINT, 
                                              p_nombre_archivo CHAR(18),
                                              p_folio          DECIMAL(10), 
                                              p_pid            DECIMAL(9,0)) 
       RETURNING INTEGER,
                 CHAR(200),
                 INTEGER,
                 INTEGER,
                 INTEGER

-- 27112014 PRODINF-561

-- Variables utilizadas para el detalle uni_det_unificado temporal
DEFINE v_tmp_nss_unificador_traajador CHAR(11);
DEFINE v_tmp_nss_cta_trabajador_1     CHAR(11);
DEFINE v_tmp_diagnostico_unificacion  CHAR(2);
-- Variables utilizadas para el detalle uni_det_unificado
DEFINE v_do_id_unificado        DECIMAL(9,0);
DEFINE v_do_id_unificador       DECIMAL(9,0);
DEFINE v_do_id_derechohabiente  DECIMAL(9,0);
DEFINE v_do_nsscta1             CHAR(11);
DEFINE v_do_nsscta2             CHAR(11);
DEFINE v_do_estado_unificacion  SMALLINT;
DEFINE v_do_diagnostico         SMALLINT;
-- Variables utilizadas para el detalle uni_det_unificador
DEFINE v_id_unificador          DECIMAL(9,0);
DEFINE v_dor_id_derechohabiente DECIMAL(9,0);
DEFINE v_estado_familia         SMALLINT;
-- Variables que se retornan
DEFINE v_i_resultado            SMALLINT;
DEFINE v_si_01                  INTEGER;
DEFINE v_si_02                  INTEGER	;
DEFINE v_si_05                  INTEGER;
DEFINE v_si_resultado           SMALLINT;
DEFINE v_c_msj                  CHAR(200);
-- Control de Excepciones       
DEFINE sql_err                  INTEGER;
DEFINE isam_err                 INTEGER;
DEFINE err_txt                  CHAR(200);
-- Variable para marca de cuenta
DEFINE v_i_estado_marca         INTEGER;
DEFINE v_si_marca_infonavit     SMALLINT;
DEFINE v_estado_marca           SMALLINT;
DEFINE v_res_desmarca           SMALLINT;


ON EXCEPTION SET sql_err, isam_err
      LET v_i_resultado = sql_err;
      LET v_si_01 = 0;
      LET v_si_02 = 0;
      LET v_si_05 = 0;
      RETURN v_i_resultado, 
             err_txt, 
             v_si_01, 
             v_si_02, 
             v_si_05;
END EXCEPTION

-- Variables que almacenan informacion para su validacion
LET v_i_resultado   = 0;
LET v_si_01         = 0;
LET v_si_02         = 0;
LET v_si_05         = 0;
LET sql_err         = NULL;
LET isam_err        = NULL;
LET err_txt         = NULL;
LET v_estado_marca  = 0;
LET v_res_desmarca  = 0;
     

   --SET DEBUG FILE TO "/home/safreviv/trace_uni_integra_confronta_IMSS.txt";
   --TRACE ON;
   
---------------------------CTAS UNIFICADOR----------------------------- 
   --trace "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   LET err_txt = "Al recuperar datos detalle tmp_det_cta_unificadora_op21";
   FOREACH
      SELECT a.nss_unificador_traajador,
             a.nss_cta_trabajador_1,
             a.diagnostico_unificacion
      INTO   v_tmp_nss_unificador_traajador,
             v_tmp_nss_cta_trabajador_1,
             v_tmp_diagnostico_unificacion
      FROM   safre_tmp:tmp_det_cta_unificadas_op21 a
      
      LET err_txt = "verifica que no venga en nulo el diagnostico";
      
      IF v_tmp_diagnostico_unificacion IS NOT NULL THEN
         LET err_txt = "UNIFICADOR " || v_tmp_nss_unificador_traajador || p_folio;
         
         SELECT id_unificador,
                id_derechohabiente,
                estado_familia
         INTO   v_id_unificador,
                v_dor_id_derechohabiente,
                v_estado_familia
         FROM   uni_det_unificador
         WHERE  nss_unificador = v_tmp_nss_unificador_traajador
         AND    estado_familia IN (1,2)
         AND    folio_unificacion = p_folio
         AND    diagnostico <> 40;
         
         LET err_txt = "Unificado " || v_id_unificador || " - " || v_tmp_nss_cta_trabajador_1;

         SELECT id_unificado,
                id_unificador,
                id_derechohabiente,
                nsscta1,
                nsscta2,
                estado_unificacion,
                diagnostico
           INTO v_do_id_unificado,
                v_do_id_unificador,
                v_do_id_derechohabiente,
                v_do_nsscta1,
                v_do_nsscta2,
                v_do_estado_unificacion,
                v_do_diagnostico
           FROM uni_det_unificado
          WHERE id_unificador = v_id_unificador 
          AND   nsscta1 = v_tmp_nss_cta_trabajador_1
          AND   folio_unificacion = p_folio
          AND    diagnostico <> 40;

            IF v_tmp_diagnostico_unificacion = '01' THEN
               LET v_si_01 = v_si_01 + 1;
               
               LET err_txt = "Entro a 01";
               IF v_do_estado_unificacion = 1 AND v_estado_familia = 1 THEN
            	 
                  LET err_txt = "Actualizo la tabla uni_det_unificado 1-01";
                  UPDATE uni_det_unificado
                     SET diagnostico_uni = v_tmp_diagnostico_unificacion,
                         diagnostico = 30
                   WHERE id_unificado = v_do_id_unificado;
                  
                  LET err_txt = "Actualizo la tabla uni_det_unificador 1-01";
                  UPDATE uni_det_unificador
                     SET diagnostico = 30,
                         estado_familia = 1
                         --estado_unificacion = v_tmp_diagnostico_unificacion
                   WHERE id_unificador = v_do_id_unificador;
               ELSE
                  IF v_do_estado_unificacion = 1 THEN
                  
                      LET err_txt = "Actualizo la tabla uni_det_unificado 2-01";
                      UPDATE uni_det_unificado
                         SET diagnostico_uni = v_tmp_diagnostico_unificacion
                       WHERE id_unificado = v_do_id_unificado;
                      
                  
                      LET err_txt = "Actualizo la tabla uni_det_unificador 2-01";
                      UPDATE uni_det_unificador
                         SET estado_familia = 2
                       WHERE id_unificador = v_do_id_unificador;
                  ELSE
                     IF v_do_estado_unificacion = 2 THEN
                  
                        LET err_txt = "Actualizo la tabla uni_det_unificado 2-01";
                        UPDATE uni_det_unificado
                           SET diagnostico_uni = v_tmp_diagnostico_unificacion
                         WHERE id_unificado = v_do_id_unificado;
                        
                        LET err_txt = "Actualizo la tabla uni_det_unificador 2-01";
                        UPDATE uni_det_unificador
                           SET estado_familia = 2
                         WHERE id_unificador = v_do_id_unificador;
                     END IF
                  END IF
               END IF
            ELSE -- '01'
               IF v_tmp_diagnostico_unificacion = '02' THEN
                  LET v_si_02 = v_si_02 + 1;
                  LET v_estado_marca = 30 ;
                  
                  LET err_txt = "Entro a 02";
                  IF v_do_estado_unificacion = 1 OR v_do_estado_unificacion = 2 THEN
                  
                     LET err_txt = "Actualizo la tabla uni_det_unificado 2-01";
                     
                     UPDATE uni_det_unificado
                        SET diagnostico_uni = v_tmp_diagnostico_unificacion,
                            estado_unificacion = 2
                      WHERE id_unificado = v_do_id_unificado;
                                          
                     --trace "Actualizo la tabla uni_det_unificador 2-01";
                     LET err_txt = "Actualizo la tabla uni_det_unificador 2-01";
                     
                     UPDATE uni_det_unificador
                        SET estado_unificacion = 2,
                            estado_familia = 2
                      WHERE id_unificador = v_do_id_unificador;
                      
                     IF p_proceso_cod IS NULL THEN 
                        LET p_proceso_cod = 2309;
                     END IF 
                      
                     EXECUTE FUNCTION fn_desmarca_cuenta (v_do_id_derechohabiente,
                                                          502,
                                                          v_do_id_unificado,
                                                          30,
                                                          0,
                                                          p_usuario_cod,
                                                          p_proceso_cod)
                             INTO v_res_desmarca;

                     EXECUTE FUNCTION fn_desmarca_cuenta (v_dor_id_derechohabiente,
                                                          501,
                                                          v_do_id_unificador,
                                                          30,
                                                          0,
                                                          p_usuario_cod,
                                                          p_proceso_cod)
                             INTO v_res_desmarca;
                             
                     --Desmarca la cuenta para el unificado
                     {EXECUTE FUNCTION fn_uni_posliquida_imss(p_usuario_cod,            ---p_usuario_cod
                                                             p_folio,                  ---p_folio_liquida
                                                             p_proceso_cod,            ---p_proceso_cod
                                                             v_do_id_derechohabiente,  ---p_id_derecho_unificado
                                                             v_dor_id_derechohabiente, ---p_id_derecho_unificador
                                                             v_estado_marca            --estado de la marca = 30
                                                             )
                             INTO v_si_resultado,
                                  isam_err,
                                  v_c_msj;
                      }

                  END IF
               --#Se agrega la opción para los PENDIENTES
               ELSE -- '02'
                  IF v_tmp_diagnostico_unificacion = '04' OR v_tmp_diagnostico_unificacion = '05' THEN
                     LET v_si_05 = v_si_05 + 1;
                     
                     LET err_txt = "Entro a 05";
                     
                     IF v_do_estado_unificacion = 1 AND v_estado_familia = 1 THEN
                     
                        LET err_txt = "Actualizo la tabla uni_det_unificado 2-01";
                        
                        UPDATE uni_det_unificado
                           SET diagnostico_uni = v_tmp_diagnostico_unificacion,
                               estado_unificacion = 3
                         WHERE id_unificado = v_do_id_unificado;
                     
                        LET err_txt = "Actualizo la tabla uni_det_unificador 2-01";
                        UPDATE uni_det_unificador
                           SET estado_unificacion = 3,
                               estado_familia = 3
                         WHERE id_unificador = v_do_id_unificador;                                                  
                     ELSE
                        IF v_estado_familia <> 1 THEN
                        	 
                           LET err_txt = "Actualizo la tabla uni_det_unificado 2-02";
                           UPDATE uni_det_unificado
                              SET diagnostico_uni = v_tmp_diagnostico_unificacion,
                                  estado_unificacion = 3
                            WHERE id_unificado = v_do_id_unificado;
                            
                           LET err_txt = "Actualizo la tabla uni_det_unificador 2-02";
                           UPDATE uni_det_unificador
                              SET estado_unificacion = 3,
                                  estado_familia = 3
                            WHERE id_unificador = v_do_id_unificador;
                        END IF                   
                     END IF                     
                  END IF -- '05'                  
               END IF -- '02'
            END IF -- Nulo
      END IF -- Nulo      
   END FOREACH;
    
  --  -- Se asigna el folio al archivo y se indica que ha sido integrado
    UPDATE glo_ctr_archivo
       SET folio       = p_folio, 
           estado      = 2, -- integrado
           opera_cod   = 4
     WHERE proceso_cod = p_proceso_cod
       AND opera_cod   = 3 -- archivo cargado
       AND estado      = 1; -- etapa de carga
    
    UPDATE bat_ctr_operacion 
       SET folio       = p_folio,
           nom_archivo = p_nombre_archivo
     WHERE proceso_cod = p_proceso_cod
       AND opera_cod   = 4
       AND pid         = p_pid;
{       
    UPDATE glo_folio
       SET opera_cod   = 4
     WHERE proceso_cod = p_proceso_cod
       AND opera_cod   = 2 -- archivo cargado
       AND status      = 0; -- etapa de carga   
}   
 RETURN v_i_resultado, 
        err_txt, 
        v_si_01, 
        v_si_02, 
        v_si_05;
        
END FUNCTION
;


