






CREATE FUNCTION "safreviv".fn_insert_autoriza_tramite(p_id_derechohabiente DECIMAL(9,0)                        
                                          ,p_id_beneficiario   SMALLINT      
                                          ,p_nombre            CHAR(40)      
                                          ,p_ape_pat           CHAR(20)      
                                          ,p_ape_mat           CHAR(20)      
                                          ,p_cve_bancaria      CHAR(18)      
                                          ,p_cve_banco         CHAR(18)      
                                          ,p_entidad           CHAR(2)       
                                          ,p_f_aut             DATE          
                                          --,p_tpo_proceso       SMALLINT    
                                          ,p_f_marca           DATE          
                                          ,p_cve_afore         SMALLINT      
                                          ,p_caso_adai         SMALLINT      
                                          ,p_num_laudo         SMALLINT      
                                          ,p_num_junta         SMALLINT      
                                        )
                 RETURNING INTEGER  

   DEFINE v_id_solicitud      DECIMAL(9,0);
   DEFINE v_estado_solicitud  SMALLINT;    
   
   DEFINE sql_err             INTEGER;            
   DEFINE isam_err            INTEGER;           
   DEFINE error_info          CHAR(70);    
   DEFINE v_fecha_hoy         DATE;
   DEFINE v_hora_actual       DATETIME HOUR TO SECOND ;

--   ON EXCEPTION
--      SET sql_err, isam_err, error_info
--
--      LET v_fecha_hoy    = TODAY;
--      LET v_hora_actual  = CURRENT;
--      
--
--      RETURN sql_err;
--   END EXCEPTION
   
      --SET DEBUG file to "/ds/safreviv_int/BD/fn_insert_autoriza_ley73.trace";
   --TRACE on;

   LET  v_id_solicitud     = 0;
   LET  v_estado_solicitud = 10;

    SELECT MAX(id_solicitud) 
      INTO v_id_solicitud
      FROM ret_ley73
     WHERE  id_derechohabiente = p_id_derechohabiente
       AND estado_solicitud = v_estado_solicitud  ;

 INSERT INTO ret_autoriza(
        id_solicitud        
       ,clabe               
       ,banco               
       ,entidad_federativa  
       ,f_autorizacion      
       ,f_marca             
       ,cve_afore           
       ,caso_adai           
       ,num_laudo           
       ,num_junta           
       )
VALUES(
      v_id_solicitud   
      ,p_cve_bancaria  
      ,p_cve_banco     
      ,p_entidad       
      ,p_f_aut         
      ,p_f_marca       
      ,p_cve_afore     
      ,p_caso_adai     
      ,p_num_laudo     
      ,p_num_junta
      );

INSERT INTO ret_beneficiario(
       tpo_beneficiario    
       ,nombre_beneficiario 
       ,paterno_beneficiario
       ,materno_beneficiario
       ,f_registro          
      )
VALUES(
--       v_id_solicitud
       p_id_beneficiario
      ,p_nombre
      ,p_ape_pat
      ,p_ape_mat
      ,TODAY);
      
EXECUTE PROCEDURE sp_cambio_status_ley73(p_id_derechohabiente 
                                       ,v_id_solicitud       
                                       ,'15'      --p_estado_solicitud   
                                       ,0         --p_cod_rechazo        
                                       ,0         --p_viv97_val          
                                       ,0         --p_viv92_val          
                                       ,0         --p_aivs97             
                                       ,0         --p_aivs92             
                                       ,'A'   );   
                                      
--IF sql_err = 100 THEN 
	RETURN 0;
--END IF
                                       
END FUNCTION;


