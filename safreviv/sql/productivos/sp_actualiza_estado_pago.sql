






CREATE PROCEDURE "safreviv".sp_actualiza_estado_pago(p_folio              DECIMAL(9,0)
                                         ,p_id_referencia      DECIMAL(9,0)
                                         ,p_id_derechohabiente DECIMAL(9,0)
                                         ,p_estado_pago        SMALLINT
                                         ,p_tpo_aclaracion     SMALLINT
                                         ,p_f_actualiza        DATE
                                          )
      INSERT INTO pag_ctr_pago
                (
                  folio,
                  id_referencia,
                  id_derechohabiente,
                  estado_pago,
                  tpo_aclaracion,
                  f_actualiza
                )
        VALUES (  
                p_folio,          
                p_id_referencia,  
                p_id_derechohabiente,  
                p_estado_pago,    
                p_tpo_aclaracion,
                p_f_actualiza    
               );

END PROCEDURE;


