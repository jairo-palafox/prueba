






CREATE PROCEDURE "safreviv".sp_liquida_execp_portabilidad()

    DEFINE v_f_liquida            date;                   
    DEFINE v_id_derechohabiente   decimal(9,0);           
    DEFINE v_subcuenta            smallint;               
    DEFINE v_fondo_inversion      smallint;               
    DEFINE v_movimiento           smallint;               
    DEFINE v_folio_liquida        decimal(9,0);           
    DEFINE v_id_referencia        decimal(9,0);           
    DEFINE v_monto_acciones       decimal(16,6);          
    DEFINE v_monto_pesos          decimal(12,2);          
    DEFINE v_f_valor              date;                   
    DEFINE v_f_registro           date;                   
    DEFINE v_h_registro           datetime hour to second;
    DEFINE v_origen               char(20);

   FOREACH 
   	  SELECT TODAY,         
             id_derechohabiente,
             60,         
             10,   
             51,        
             46674,     
             id_referencia,     
             monto_pesos,    
             monto_pesos,       
             f_valor,           
             TODAY,        
             h_registro,        
             origen
      INTO   v_f_liquida,         
             v_id_derechohabiente,
             v_subcuenta,         
             v_fondo_inversion,
             v_movimiento,        
             v_folio_liquida,     
             v_id_referencia,     
             v_monto_acciones,    
             v_monto_pesos,       
             v_f_valor,           
             v_f_registro,        
             v_h_registro,        
             v_origen      
      FROM   cta_movimiento
      WHERE  folio_liquida = 46041
      AND    id_referencia in (8017,102940,117380)

      LET v_h_registro = CURRENT HOUR TO SECOND;          
      
      INSERT INTO cta_movimiento VALUES (
         v_f_liquida,         
         v_id_derechohabiente,
         v_subcuenta,         
         v_fondo_inversion,
         v_movimiento,        
         v_folio_liquida,     
         v_id_referencia,     
         v_monto_acciones,    
         v_monto_pesos,       
         v_f_valor,           
         v_f_registro,        
         v_h_registro,        
         v_origen                   
         );      
 
   END FOREACH;
   

   FOREACH 
   	  SELECT TODAY,         
             id_derechohabiente,
             60,         
             10,   
             51,        
             46675,     
             id_referencia,     
             monto_pesos,    
             monto_pesos,       
             f_valor,           
             TODAY,        
             h_registro,        
             origen
      INTO   v_f_liquida,         
             v_id_derechohabiente,
             v_subcuenta,         
             v_fondo_inversion,
             v_movimiento,        
             v_folio_liquida,     
             v_id_referencia,     
             v_monto_acciones,    
             v_monto_pesos,       
             v_f_valor,           
             v_f_registro,        
             v_h_registro,        
             v_origen      
      FROM   cta_movimiento
      WHERE  folio_liquida = 46264
      AND    id_referencia in (2262,4968,30381,40419,44344,44416,48810,59959,60247,67944,71415,79649,81777,92702,105951)

      LET v_h_registro = CURRENT HOUR TO SECOND;          
      
      INSERT INTO cta_movimiento VALUES (
         v_f_liquida,         
         v_id_derechohabiente,
         v_subcuenta,         
         v_fondo_inversion,
         v_movimiento,        
         v_folio_liquida,     
         v_id_referencia,     
         v_monto_acciones,    
         v_monto_pesos,       
         v_f_valor,           
         v_f_registro,        
         v_h_registro,        
         v_origen                   
         );      
 
   END FOREACH;   

END PROCEDURE                  
;


