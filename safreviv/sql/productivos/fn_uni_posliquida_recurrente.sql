






CREATE FUNCTION "safreviv".fn_uni_posliquida_recurrente(p_usuario_cod           CHAR(20),
                                             p_folio_liquida         DECIMAL(9,0),
                                             p_proceso_cod           SMALLINT,
                                             v_id_dh_unificado       DECIMAL(9,0),
                                             v_id_dh_unificador      DECIMAL(9,0)                                             
                                             )
   RETURNING SMALLINT, INTEGER, CHAR(200)

   DEFINE v_id_unificador      DECIMAL(9,0);
   DEFINE v_id_unificado       DECIMAL(9,0);
   DEFINE v_n_referencia_dor   DECIMAL(9,0);
   DEFINE v_n_referencia_ado   DECIMAL(9,0);   
   DEFINE v_folio_unificacion  DECIMAL(9,0);
   DEFINE v_id_derechohabiente DECIMAL(9,0);
   --DEFINE v_id_dh_unificador   DECIMAL(9,0);
   --DEFINE v_id_dh_unificado    DECIMAL(9,0);
   
   -- 
   DEFINE v_si_marca_infonavit  SMALLINT;
   DEFINE v_si_resultado        SMALLINT;
   
   -- Control de Excepciones
   DEFINE sql_err  INTEGER;
   DEFINE isam_err INTEGER;
   DEFINE err_txt  CHAR(200);
   DEFINE v_c_msj  CHAR(200);

   ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = -206;
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION
 
-- SET DEBUG FILE TO "/safreviv/uni/unl/posliquida_recurrente.trace";
-- TRACE ON;
 
   LET v_si_resultado = 0;
   LET isam_err       = 0;
   LET v_c_msj        = "1";

   SELECT a.n_referencia
   INTO   v_n_referencia_dor     
   FROM   sfr_marca_activa a
   WHERE  a.id_derechohabiente = v_id_dh_unificador
   AND    a.marca = 501;
   
   LET v_c_msj = "Dentro de proceso foreach";
   LET v_c_msj = "Se verifica si se pago completamente la solicitud";
     
   LET v_c_msj = "Verifica si cumple la regla y desmarca";

   LET v_si_marca_infonavit = 501;

   IF v_n_referencia_dor IS NOT NULL THEN    
      EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh_unificador,
                                          v_si_marca_infonavit, -- marca de infonavit
                                          v_n_referencia_dor,
                                          0,
                                          0,
                                          p_usuario_cod,
                                          p_proceso_cod)
      INTO v_si_resultado;

      LET v_c_msj = "Unificador desmarcada";
   END IF 
        
   SELECT a.n_referencia
   INTO   v_n_referencia_ado     
   FROM   sfr_marca_activa a
   WHERE  a.id_derechohabiente = v_id_dh_unificado
   AND    a.marca = 502;
   
   LET v_c_msj = "Dentro de proceso foreach";           
   LET v_c_msj = "Verifica si cumple la regla y desmarca";

   LET v_si_marca_infonavit = 502;

   EXECUTE FUNCTION fn_desmarca_cuenta(v_id_dh_unificado,
                                       v_si_marca_infonavit, -- marca de infonavit
                                       v_n_referencia_ado,
                                       0,
                                       0,
                                       p_usuario_cod,
                                       p_proceso_cod)
   INTO v_si_resultado;

   LET v_c_msj = "Unificador desmarcada";
   LET v_c_msj = "proceso terminado exitosamente";
   
   RETURN v_si_resultado, isam_err, v_c_msj;
END FUNCTION;


