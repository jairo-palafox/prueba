






CREATE PROCEDURE "safreviv".sp_dis_as_ins_folio_pag_apo(p_folio_reg_pago DECIMAL (10,0))

DEFINE p_estado_reg_pag SMALLINT; 
DEFINE p_folio_apo_sub  DECIMAL (10,0) ;
DEFINE p_estado_apo_sub SMALLINT;

   LET p_estado_reg_pag = 1;
   LET p_folio_apo_sub  = NULL;
   LET p_estado_apo_sub = NULL;

   INSERT INTO dis_ctr_folio_apo VALUES(p_folio_reg_pago,
                                        p_estado_reg_pag,
                                        p_folio_apo_sub,
                                        p_estado_apo_sub);

END PROCEDURE;


