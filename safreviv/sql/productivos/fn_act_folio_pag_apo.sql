






CREATE PROCEDURE "safreviv".fn_act_folio_pag_apo (p_folio DECIMAL (10,0))
   
   UPDATE safre_viv:dis_ctr_folio_apo
      SET folio_apo_sub = p_folio,
          estado_apo_sub = 3
   WHERE estado_reg_pag = 1
     AND folio_apo_sub IS NULL
     AND estado_apo_sub IS NULL;

END PROCEDURE;


