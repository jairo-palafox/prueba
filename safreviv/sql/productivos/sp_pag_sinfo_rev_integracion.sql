






CREATE PROCEDURE "safreviv".sp_pag_sinfo_rev_integracion(p_folio DECIMAL(9,0))

  -- se borran los datos de las tablas historicas de SINFO
  DELETE FROM pag_cza_sifv        WHERE folio = p_folio;
  DELETE FROM cta_his_pagos       WHERE folio = p_folio;
  DELETE FROM cta_pag_complemento WHERE folio = p_folio;
  DELETE FROM pag_sum_sifv        WHERE folio = p_folio;
  
  -- se borran las cuentas abiertas por SINFO
  -- y sus relaciones laborales
  --DELETE FROM afi_relacion_laboral
  --      WHERE folio_lote = p_folio
  --        AND id_derechohabiente IN ( SELECT id_derechohabiente
  --                                    FROM   afi_derechohabiente
  --                                    WHERE  folio_lote = p_folio );
  --DELETE FROM afi_derechohabiente  WHERE folio_lote = p_folio;
  
  UPDATE glo_folio
  SET    status = -1
  WHERE  folio  = p_folio;
  
END PROCEDURE;


