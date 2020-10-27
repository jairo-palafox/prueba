






CREATE PROCEDURE "safreviv".fn_rev_pre_apo_sub(p_folio   DECIMAL(9,0))

--Última modificación 13112015
--Declaración de variables
DEFINE v_monot_apo           DECIMAL(12,2);
DEFINE v_monto_amo           DECIMAL(12,2);
DEFINE v_id_detalle          INTEGER;
      
  DELETE FROM dis_interface_ef
  WHERE folio_liquida = p_folio;

  -- Reverso Migración Aportaciones Subsecuentes a SACI (Créditos 43 BIS) 
  DELETE FROM dis_ctr_aps_tns
  WHERE folio_liquida = p_folio;

END PROCEDURE;


