






CREATE PROCEDURE "safreviv".sp_dis_rev_casos_excep(p_folio   DECIMAL(9,0))

--Última modificación 30062014
--Declaración de variables

  DELETE 
  FROM   dis_caso_excepcion
  WHERE  folio          = p_folio;

  UPDATE dis_ctr_caso_excepcion
  SET    estado_archivo = 2
  WHERE  folio          = p_folio;

END PROCEDURE;


