






CREATE PROCEDURE "safreviv".sp_liquida_excep_portabilidad2()

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
             46992,     
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
      WHERE  folio_liquida = 46813
      AND    id_referencia in (968,13460,14393,19461,47410,55539,75864,75997,90856)

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
   
   LET v_f_liquida = TODAY;
   LET v_f_valor = TODAY;
   LET v_f_registro = TODAY;
   LET v_h_registro = CURRENT HOUR TO SECOND;
   
   INSERT INTO cta_movimiento VALUES (
         v_f_liquida,   -- f_liquida       
         274379,        -- id_derechohabiente
         60,            -- subcuenta    
         10,            -- fondo 
         1192,          -- movimiento     
         47427,         -- v_folio_liquida     
         75997,         -- id_referencia   
         -7585.76,      -- v_monto_acciones,    
         -7585.76,      -- v_monto_pesos,       
         v_f_valor,           
         v_f_registro,        
         v_h_registro,        
         "C4199995109"                   
         ); 
   
END PROCEDURE                  
;


