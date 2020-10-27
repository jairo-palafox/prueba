






CREATE PROCEDURE "safreviv".fn_rev_folio_pag_apo(p_folio_liq_reg_pag DECIMAL (10,0))

  --Actualizar el estado_reg_pag a 2 (Archivo de Registro de Pagos LQ reversado)
   UPDATE dis_ctr_folio_apo
      SET estado_apo_sub = 2
   WHERE folio_reg_pag = p_folio_liq_reg_pag;

END PROCEDURE;


