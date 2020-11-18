CREATE FUNCTION fn_rev_preliquida_rest_siaffbancofico_l73(p_folio_liquida DECIMAL(10,0),
                                            p_proceso_cod        SMALLINT,
                                            p_opera_cod          SMALLINT,
                                            p_usuario_cod        VARCHAR(20),
                                            p_pid                DECIMAL(9,0),
                                            p_estado_solicitud   SMALLINT)
RETURNING SMALLINT, INTEGER, INTEGER, CHAR(254);

-- variables para el control de excepciones
DEFINE v_estatus    SMALLINT; 
DEFINE v_sql_error  INTEGER;
DEFINE v_isam_error INTEGER;
DEFINE v_msg_error  CHAR(254);
DEFINE v_id_solicitud DECIMAL(9,0);


   -- en caso de excepcion
   ON EXCEPTION SET v_sql_error, v_isam_error, v_msg_error
      -- error
      LET v_estatus = -1;
      
      RETURN v_estatus, v_sql_error, v_isam_error, v_msg_error;
   END EXCEPTION;
   
--SET DEBUG FILE TO '/ds/safreviv_int/BD/fn_rev_preliquida_rest_siaffbancofico_l73.trace';

   -- se asume proceso correcto
   LET v_estatus = 0;
   LET v_sql_error = 0;
   LET v_isam_error = 0;
   LET v_msg_error = "Reverso correcto";

   -- se obtienen los registros que corresponden al folio en turno
   FOREACH cu_reverso_preliq_restitucion FOR 
   SELECT id_referencia
     INTO v_id_solicitud
     FROM ret_preliquida
    WHERE folio_liquida = p_folio_liquida
    
    -- se borran las solicitudes que se insertaron en la tabla ret_rendimiento_restitucion
    DELETE FROM ret_rendimiento_restitucion WHERE id_solicitud = v_id_solicitud;
    
    -- se devuelven las solicitudes al estatus anterior
    UPDATE ret_solicitud_generico
    SET    estado_solicitud  = p_estado_solicitud, -- se recibe de parametro
           folio_restitucion = null 
    WHERE  id_solicitud     = v_id_solicitud;
     
    UPDATE ret_ley73_generico
    SET    estado_solicitud = p_estado_solicitud -- se recibe de parametro
    WHERE  id_solicitud     = v_id_solicitud;
    
    UPDATE ret_beneficiario_juridico
    SET    estado_solicitud = p_estado_solicitud, -- se recibe de parametro
           consec_beneficiario = null
    WHERE  id_solicitud     = v_id_solicitud;
    
   END FOREACH;
   
   -- se borran los datos de la tabla de preliquidacion
   DELETE FROM ret_preliquida WHERE folio_liquida = p_folio_liquida;   

   -- la preliquidacion es la primera etapa por lo que se reversa el proceso completo
   --coloca el folio como reversado
   UPDATE glo_folio SET status         = 10
    WHERE proceso_cod    = p_proceso_cod
      AND opera_cod      = p_opera_cod
      AND folio          = p_folio_liquida;

   --coloca operacion 1 como reversado
   UPDATE bat_ctr_operacion SET folio = p_folio_liquida,
         estado_cod   = 10
    WHERE folio        = p_folio_liquida
      AND proceso_cod  = p_proceso_cod
      AND opera_cod    = 1;

   --coloca operacion 2 como reversado
   UPDATE bat_ctr_operacion SET estado_cod = 10
    WHERE folio        = p_folio_liquida
      AND proceso_cod  = p_proceso_cod
      AND opera_cod    = 2;

   --coloca operacion 2 como reversado
   UPDATE bat_ctr_operacion SET estado_cod = 10
    WHERE folio        = p_folio_liquida
      AND proceso_cod  = p_proceso_cod
      AND opera_cod    = 3;

   UPDATE bat_ctr_proceso SET  estado_cod   = 10
    WHERE folio        = p_folio_liquida
      AND proceso_cod  = p_proceso_cod;             
            
   -- se devuelve el resultado del procesamiento
   RETURN v_estatus, v_sql_error, v_isam_error, v_msg_error;                                    
END FUNCTION;                                                                                                                                                                                                        