






CREATE PROCEDURE "safreviv".sp_liquida_cambio_nss_vol(
   p_f_liquida      DATE,                    
   p_id_derecho     decimal(9,0),            
   p_subcuenta      SMALLINT,                
   p_fondo          SMALLINT,                
   p_movimiento     SMALLINT,                
   p_folio_liquida  decimal(9,0),            
   p_id_referencia  decimal(9,0),            
   p_monto_acciones decimal(16,6),           
   p_monto_pesos    decimal(12,2),           
   p_f_valor        DATE,                    
   p_f_registro     DATE,                    
   p_h_registro     datetime hour to SECOND, 
   p_origen         char(20)                 
   )

   INSERT INTO cta_movimiento VALUES(
      p_f_liquida,     
      p_id_derecho,    
      p_subcuenta,     
      p_fondo,         
      p_movimiento,    
      p_folio_liquida, 
      p_id_referencia, 
      p_monto_acciones,
      p_monto_pesos,   
      p_f_valor,       
      p_f_registro,    
      p_h_registro,    
      p_origen        
      );

END PROCEDURE;


