






CREATE FUNCTION "safreviv".fn_uni_posliquida_imss(p_usuario_cod           CHAR(20),
                                       p_folio_liquida         DECIMAL(9,0),
                                       p_proceso_cod           SMALLINT,
                                       p_id_derecho_unificado  DECIMAL(9,0),
                                       p_id_derecho_unificador DECIMAL(9,0),
                                       p_estado_marca          SMALLINT )
   RETURNING SMALLINT, INTEGER, CHAR(200)

   DEFINE v_id_unificador      DECIMAL(9,0);
   DEFINE v_id_unificado       DECIMAL(9,0);
   DEFINE v_folio_unificacion  DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   -- 
   DEFINE v_si_marca_infonavit  SMALLINT;
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
 
 --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace_uni_posliquida_IMSS.txt";
 --TRACE ON;
 
   LET v_si_resultado = 0;
   LET isam_err = 0;
   LET v_c_msj = "1";

   LET v_c_msj = "Previo a foreach para buscar datos con folio: "||p_folio_liquida;
   
   FOREACH
      SELECT a.id_pre_unificador
      INTO   v_id_unificador     
      FROM   uni_pre_unificador a
      WHERE  a.id_derechohabiente = p_id_derecho_unificador
      AND    a.estado = 1
      AND    a.diagnostico = 1
      
      LET v_c_msj = "Dentro de proceso foreach";
      LET v_c_msj = "Se verifica si se pago completamente la solicitud";
        
      LET v_c_msj = "Verifica si cumple la regla y desmarca";

      LET v_si_marca_infonavit = 501;

      EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificador,
                                          v_si_marca_infonavit, -- marca de infonavit
                                          v_id_unificador,
                                          p_estado_marca,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
         INTO v_si_resultado;

      LET v_c_msj = "Unificador desmarcada";
        
      FOREACH
         SELECT a.id_pre_unificado
           INTO v_id_unificado     
           FROM uni_pre_unificado a
          WHERE a.id_derechohabiente = p_id_derecho_unificado
            AND a.estado = 1
            AND a.diagnostico = 1
         
         LET v_c_msj = "Dentro de proceso foreach";
         LET v_c_msj = "Se verifica si se pago completamente la solicitud";
           
         LET v_c_msj = "Verifica si cumple la regla y desmarca";

         LET v_si_marca_infonavit = 502;

         EXECUTE FUNCTION fn_desmarca_cuenta(p_id_derecho_unificado,
                                             v_si_marca_infonavit, -- marca de infonavit
                                             v_id_unificado,
                                             p_estado_marca,
                                             0,
                                             p_usuario_cod,
                                             p_proceso_cod)
            INTO v_si_resultado;

         LET v_c_msj = "Unificador desmarcada";
      END FOREACH;
   END FOREACH;
 
   LET v_c_msj = "proceso terminado exitosamente";
   
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


