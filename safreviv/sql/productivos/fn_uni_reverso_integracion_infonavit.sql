






CREATE FUNCTION "safreviv".fn_uni_reverso_integracion_infonavit(p_folio DECIMAL(9,0) ) 
                                        RETURNING SMALLINT, INTEGER,  VARCHAR(250)
                                        
DEFINE v_r_reverso_id_derechohabiente DECIMAL(9,0);
DEFINE v_r_reverso_marca              SMALLINT;
DEFINE v_r_reverso_id_referencia      DECIMAL(9,0);
DEFINE v_r_reverso_folio              DECIMAL(9,0);   
-- Control de Excepciones
DEFINE sql_err                        INTEGER;
DEFINE isam_err                       INTEGER;
DEFINE err_txt                        CHAR(200);  
DEFINE v_si_resultado                 SMALLINT;                      
                                        
   ON EXCEPTION SET sql_err, isam_err, err_txt
      --LET v_si_resultado = -206;
      LET v_si_resultado = sql_err;
      
      RETURN v_si_resultado, isam_err, err_txt;
   END EXCEPTION    
   --SET DEBUG FILE TO "/ds/safreviv_int/BD/trace.uni._reverso_integra_INFONAVIT.txt";      
  --trace on;
FOREACH
SELECT id_derechohabiente, marca, n_referencia, folio
   INTO v_r_reverso_id_derechohabiente, v_r_reverso_marca,
        v_r_reverso_id_referencia, v_r_reverso_folio
  FROM sfr_marca_activa
   WHERE folio = p_folio
     AND marca = 503
     
     EXECUTE PROCEDURE sp_reversa_marca (v_r_reverso_id_derechohabiente, 
                                v_r_reverso_marca,
                                v_r_reverso_id_referencia,
                                 v_r_reverso_folio);
END FOREACH;        
                                
FOREACH
SELECT id_derechohabiente, marca, n_referencia, folio
   INTO v_r_reverso_id_derechohabiente, v_r_reverso_marca,
        v_r_reverso_id_referencia, v_r_reverso_folio
  FROM sfr_marca_activa
   WHERE folio = p_folio
     AND marca = 504
     
     EXECUTE PROCEDURE sp_reversa_marca (v_r_reverso_id_derechohabiente, 
                                v_r_reverso_marca,
                                v_r_reverso_id_referencia,
                                 v_r_reverso_folio);
END FOREACH;

RETURN v_si_resultado, isam_err, err_txt;
END FUNCTION ;


