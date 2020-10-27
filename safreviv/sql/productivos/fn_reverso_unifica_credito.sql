






CREATE FUNCTION "safreviv".fn_reverso_unifica_credito(p_id_proceso            SMALLINT,
                                           p_id_derecho_unificado  DECIMAL(9,0), 
                                           p_id_derecho_unificador DECIMAL(9,0))
RETURNING INTEGER
-- Declaración de variables
DEFINE v_id_cre_acreditado             DECIMAL(9,0);
DEFINE v_id_cre_ctr_archivo            DECIMAL(9,0);
DEFINE v_id_derechohabiente            DECIMAL(9,0);
DEFINE v_tpo_credito                   SMALLINT;
DEFINE v_id_cre_acreditado_unificador  DECIMAL(9,0);
DEFINE v_id_cre_ctr_archivo_unificador DECIMAL(9,0);
DEFINE v_a_id_credito                  SMALLINT;
DEFINE v_a_f_credito                   DATE;
DEFINE v_ctr_folio_archivo             DECIMAL(9,0);
DEFINE v_ctr_folio_archivo_unificado   DECIMAL(9,0);
--
DEFINE v_c_id_derechohabiente	         DECIMAL(9,0);
DEFINE v_c_proceso_cod	               SMALLINT;
DEFINE v_c_tpo_credito	               SMALLINT;
DEFINE v_tpo_originacion               SMALLINT;
DEFINE v_c_num_credito	               DECIMAL(10,0);
DEFINE v_c_f_credito	                 DATE;
--                                     
DEFINE v_si_marca_infonavit            SMALLINT;
DEFINE v_si_marca_procesar             SMALLINT;
DEFINE v_error                         INTEGER;
-- Control de Excepciones
DEFINE v_i_resultado                   SMALLINT;
DEFINE sql_err                         INTEGER;
DEFINE isam_err                        INTEGER;
DEFINE err_txt                         VARCHAR(255);

-- En caso de error se establecen códigos de error
   ON EXCEPTION SET v_error
      RETURN 0; -- ERROR
   END EXCEPTION WITH RESUME;
   --ON EXCEPTION SET sql_err, isam_err, err_txt
   --   LET v_i_resultado = sql_err;
   --   RETURN sql_err, isam_err, err_txt;
   --END EXCEPTION
   
   
   --Se habilita el LOG del SP
   --SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_unifica_cuenta_reverso.trace';
   --TRACE ON;

   FOREACH 
	    SELECT id_cre_acreditado,
             id_cre_ctr_archivo,
             id_derechohabiente,
             tpo_credito,
             tpo_originacion
   	  INTO   v_id_cre_acreditado,
             v_id_cre_ctr_archivo,
             v_id_derechohabiente,
             v_tpo_credito,
             v_tpo_originacion
   	  FROM   cre_acreditado
   	  WHERE  id_derechohabiente = p_id_derecho_unificado
   	  AND    estado = 230
   	     
   	  SELECT folio_archivo
   	  INTO   v_ctr_folio_archivo_unificado
   	  FROM   cre_ctr_archivo
   	  WHERE  id_cre_ctr_archivo = v_id_cre_ctr_archivo;   
   	  
   	  -- Se actualiza la tabla cre_acreditado a un estado original antes de la unificación
   	  UPDATE cre_acreditado
   	     SET estado = 220
   	  WHERE  id_cre_acreditado = v_id_cre_acreditado 
      AND   id_cre_ctr_archivo = v_id_cre_ctr_archivo;
       
        -- Se elimina el registro historico de cre_his_acreditado del id insertado
      DELETE
      FROM   cre_his_acreditado
      WHERE  id_cre_acreditado = v_id_cre_acreditado
      AND    id_cre_ctr_archivo = v_id_cre_ctr_archivo
      AND    estado = 230;
        
        -- Se elimina el registro de la tabla cre_acreditado para el unificador
        
      SELECT id_cre_acreditado,
             id_cre_ctr_archivo
   	  INTO   v_id_cre_acreditado_unificador,
             v_id_cre_ctr_archivo_unificador
   	  FROM   cre_acreditado
   	  WHERE  id_derechohabiente = p_id_derecho_unificador
   	  AND    estado = 220;
      
      SELECT id_credito,
             f_credito  
      INTO   v_a_id_credito,
             v_a_f_credito
      FROM   afi_derechohabiente
      WHERE  id_derechohabiente = p_id_derecho_unificador;
         
      UPDATE afi_derechohabiente
         SET id_credito = v_a_id_credito,
             f_credito  = v_a_f_credito
      WHERE  id_derechohabiente = p_id_derecho_unificado;

      UPDATE afi_derechohabiente
         SET id_credito = 0,
             f_credito  = TODAY
      WHERE  id_derechohabiente = p_id_derecho_unificador;
                  
      DELETE
      FROM   cre_acreditado
      WHERE  estado = 220
      AND    id_derechohabiente = p_id_derecho_unificador
      AND    id_cre_acreditado  = v_id_cre_acreditado_unificador
      AND    id_cre_ctr_archivo = v_id_cre_ctr_archivo_unificador;
                 
        -- Se busca el tipo de credito del derechohabiente
      SELECT marca_inf,
             marca_prc
      INTO   v_si_marca_infonavit,
             v_si_marca_procesar
      FROM   cat_tipo_credito
      WHERE  tpo_credito     = v_tpo_credito
      AND    tpo_originacion = v_tpo_originacion;
      
      -- Se ejecuta el reverso de la desmarca para el id unificado INFONAVIT 
      EXECUTE PROCEDURE sp_reversa_desmarca(p_id_derecho_unificado,
                                            v_si_marca_infonavit,
                                            v_id_cre_acreditado,
                                            v_ctr_folio_archivo_unificado);
                                            
      -- Se ejecuta el reverso de la desmarca para el id unificado en PROCESAR 
      IF p_id_proceso = 2301 OR p_id_proceso = 2314 THEN
         EXECUTE PROCEDURE sp_reversa_desmarca(p_id_derecho_unificado,
                                               v_si_marca_procesar,
                                               v_id_cre_acreditado,
                                               v_ctr_folio_archivo_unificado);
      END IF

      DELETE
      FROM   cta_his_credito
      WHERE  id_derechohabiente = p_id_derecho_unificado
      AND    estado = 4 
      AND    proceso_cod = p_id_proceso;
          
      SELECT *
      INTO   v_c_id_derechohabiente,
             v_c_proceso_cod,
             v_c_tpo_credito,
             v_c_num_credito,
             v_c_f_credito	       
      FROM   cta_credito
      WHERE  id_derechohabiente = p_id_derecho_unificador
      GROUP BY 1,2,3,4,5;        
                 
      DELETE
      FROM   cta_credito
      WHERE  id_derechohabiente = p_id_derecho_unificador;
         
      INSERT INTO cta_credito(id_derechohabiente,
                              proceso_cod,
                              tpo_credito,
                              num_credito,
                              f_credito)
             VALUES(p_id_derecho_unificado,
                    v_c_proceso_cod,
                    v_c_tpo_credito,
                    v_c_num_credito,
                    v_c_f_credito);
        
      SELECT folio_archivo
   	  INTO   v_ctr_folio_archivo
   	  FROM   cre_ctr_archivo
   	  WHERE  id_cre_ctr_archivo = v_id_cre_ctr_archivo_unificador;                          
      
      --Reversa desmarca de crédito del UNIFICADOR INFONAVIT 
      EXECUTE PROCEDURE sp_reversa_marca(p_id_derecho_unificador, 
                                         v_si_marca_infonavit,
                                         v_id_cre_acreditado_unificador,
                                         v_ctr_folio_archivo);
                                         
      --Reversa desmarca de crédito del UNIFICADOR PROCESAR
      IF p_id_proceso = 2301 OR p_id_proceso = 2314 THEN                                   
         EXECUTE PROCEDURE sp_reversa_marca(p_id_derecho_unificador, 
                                            v_si_marca_procesar,
                                            v_id_cre_acreditado_unificador,
                                            v_ctr_folio_archivo);
      END IF

         --#Se actualiza el estado en uni_cre_unificado
      UPDATE uni_cre_unificado
         SET estado = 3
      WHERE  id_cre_acreditado = v_id_cre_acreditado
      AND    estado = 1;
         
      --#Se actualiza el estado en uni_cre_unificador   
      UPDATE uni_cre_unificador
         SET estado = 3
      WHERE  id_cre_acreditado = v_id_cre_acreditado_unificador
      AND    estado = 1;
   END FOREACH;
   RETURN 1;
END FUNCTION
;


