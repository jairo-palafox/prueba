






CREATE PROCEDURE "safreviv".sp_dis_as_rev_act_f_pag_apo (p_folio_apo_sub DECIMAL(10,0))

   UPDATE dis_ctr_folio_apo
      SET folio_apo_sub = NULL,
          estado_apo_sub = NULL 
   WHERE estado_reg_pag = 3
     AND folio_apo_sub = p_folio_apo_sub;

END PROCEDURE;


