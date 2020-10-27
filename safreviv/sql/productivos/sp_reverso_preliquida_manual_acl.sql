






CREATE PROCEDURE "safreviv".sp_reverso_preliquida_manual_acl(p_folio DECIMAL(9,0)
                                                 ,p_id_referencia  DECIMAL(9,0))
   DELETE FROM acl_preliquida
   WHERE folio_liquida = p_folio
   AND   id_referencia = p_id_referencia;
   
   DELETE FROM pag_excep_preliquida
   WHERE folio_liquida = p_folio
   AND   id_referencia = p_id_referencia;
END PROCEDURE;


