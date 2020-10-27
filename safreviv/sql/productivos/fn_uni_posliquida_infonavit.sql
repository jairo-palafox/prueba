






CREATE FUNCTION "safreviv".fn_uni_posliquida_infonavit(p_usuario_cod CHAR(20),
                                            p_folio_liquida DECIMAL(9,0),
                                            p_proceso_cod SMALLINT,
                                            p_id_derecho_unificado DECIMAL(9,0),
                                            p_id_derecho_unificador DECIMAL(9,0)) 
   RETURNING SMALLINT, INTEGER, CHAR(200)

DEFINE v_id_derechohabiente    DECIMAL(9,0);
DEFINE v_folio_unificacion     DECIMAL(9,0);
DEFINE v_id_unificador         DECIMAL(9,0);
DEFINE v_id_unificado          DECIMAL(9,0);
DEFINE v_si_resultado        SMALLINT;

-- Control de Excepciones
DEFINE sql_err INTEGER;
DEFINE isam_err INTEGER;
DEFINE err_txt  CHAR(200);
DEFINE v_c_msj  CHAR(200);

    ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = -206;
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni.posliquida_INFONAVIT.txt";
 
 LET v_si_resultado = 0;
 LET isam_err = 0;
 LET v_c_msj = "1";

 --trace "Previo a foreach para buscar datos con folio: "||p_folio_liquida;
 
 LET v_c_msj = "Previo a foreach para buscar datos con folio: "||p_folio_liquida;

 FOREACH
    --SELECT id_derechohabiente,
    --       folio_unificacion,
    --       id_inf_unificador
    --  INTO v_id_derechohabiente, 
    --       v_folio_unificacion ,
    --       v_id_unificador
    --FROM uni_inf_unificador
    --WHERE diagnostico = 4  -- solo solicitudes aceptadas y liquidadas
    -- AND folio_liquidacion = p_folio_liquida
    
    SELECT a.id_inf_unificador
     INTO v_id_unificador     
     FROM uni_inf_unificador a
     WHERE a.id_derechohabiente = p_id_derecho_unificador
     AND a.estado_unificacion = 1
     AND a.diagnostico = 4

      --trace("función desmarcar");
      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificador,
                                          503, -- marca de infonavit
                                          v_id_unificador,
                                          0,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;
      --trace "Verifica v:resultado desmarca.."||v_si_resultado;
      LET v_c_msj = "Solicitud desmarcada";
      
      -- Actualiza el estado de la solicitud a 4 liquidado
      --UPDATE uni_inf_unificador
      --   SET diagnostico = 5 -- liquidados
      -- WHERE diagnostico = 4 -- Aceptadas y liquidadas
      --   AND folio_liquidacion = p_folio_liquida;
 
 --END FOREACH;
 
 FOREACH
   -- SELECT id_derechohabiente,
   --        folio_unificacion,
   --        id_inf_unificado
   --   INTO v_id_derechohabiente, 
   --        v_folio_unificacion ,
   --        v_id_unificado 
   -- FROM uni_inf_unificado
   --WHERE diagnostico = 4  -- solo solicitudes aceptadas y liquidadas
   --  AND folio_unificacion = p_folio_liquida
   
   SELECT a.id_inf_unificado
     INTO v_id_unificado     
     FROM uni_inf_unificado a
    WHERE a.id_unificador = v_id_unificador
      AND a.id_derechohabiente = p_id_derecho_unificado
      AND a.estado_unificacion = 1
      AND a.diagnostico = 4

      --trace("función desmarcar");
      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificado,
                                          504, -- marca de infonavit
                                          v_id_unificado,
                                          0,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;
      --trace "Verifica v:resultado desmarca.."||v_si_resultado;
      LET v_c_msj = "Solicitud desmarcada";
      
      -- Actualiza el estado de la solicitud a 4 liquidado
      --UPDATE uni_inf_unificado
      --   SET diagnostico = 5 -- liquidados
      -- WHERE diagnostico = 4 -- Aceptadas y liquidadas
      --   AND folio_unificacion = p_folio_liquida;
   END FOREACH;
 END FOREACH;
 
 --trace("termina proceso");
 LET v_c_msj = "proceso terminado exitosamente";
 
 RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


