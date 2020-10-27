






CREATE PROCEDURE "safreviv".sp_dpe_inserta_rechazo(
   p_rch_folio             DECIMAL(9,0)
   ,p_rch_tipo_registro     SMALLINT    
   ,p_rch_id_dpe_referencia DECIMAL(9,0)
   ,p_rch_result_operacion  CHAR(2)     
   ,p_rch_diagnostico       SMALLINT    
   ,p_rch_campo_valor       CHAR(50)    
)

   INSERT INTO dpe_rch_archivo
     (
        id_rechazo
       ,folio 
       ,tipo_registro 
       ,id_dpe_referencia 
       ,result_operacion 
       ,diagnostico 
       ,campo_valor
     )
    VALUES
     (
       seq_dpe_rch_archivo.NEXTVAL
      ,p_rch_folio            
      ,p_rch_tipo_registro    
      ,p_rch_id_dpe_referencia
      ,p_rch_result_operacion 
      ,p_rch_diagnostico      
      ,p_rch_campo_valor      
     );

END PROCEDURE -- sp_dpe_pre_integra

;


