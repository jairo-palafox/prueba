






CREATE PROCEDURE "safreviv".sp_dis_apo_subs3(p_folio DECIMAL(9,0))

   -- Elimina informacion de la tabla de aportaciones subsecuentes
   DELETE 
     FROM dis_apo_sub
    WHERE folio = p_folio;
   
   --Actualiza el campo estado a 40 
   --(Archivo Reversado, Aportaciones Subsecuentes)
   UPDATE dis_ctr_apo_sub       
      SET dis_ctr_apo_sub.estado      = 40
    WHERE dis_ctr_apo_sub.folio       = p_folio;

END PROCEDURE;


