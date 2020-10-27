






CREATE PROCEDURE "safreviv".sp_verifica_pago(p_folio         DECIMAL(9,0),
                                  p_id_referencia DECIMAL(9,0),
                                  p_tpo_archivo   SMALLINT,
                                  p_edo_pago      SMALLINT
                                 )
                                 
      INSERT INTO acl_verifica_pago
      (folio,
       id_referencia,
       tpo_archivo,
       edo_pago
       )
      VALUES
      (
       p_folio,
       p_id_referencia,
       p_tpo_archivo,  
       p_edo_pago     
      );

END PROCEDURE;


